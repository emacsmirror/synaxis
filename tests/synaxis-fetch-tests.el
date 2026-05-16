;;; synaxis-fetch-tests.el --- Tests for synaxis-fetch  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-fetch'.  All tests synthesise HTTP response
;; buffers and drive the response-processing pipeline directly; the
;; network is never touched.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-fetch.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defconst synaxis-fetch-tests--fixtures-dir
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun synaxis-fetch-tests--load (name)
  "Read fixture NAME as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name synaxis-fetch-tests--fixtures-dir))
    (buffer-string)))

(defun synaxis-fetch-tests--response (status headers body)
  "Return a buffer containing a synthetic HTTP response."
  (let ((buf (generate-new-buffer " *synaxis-fetch-test*")))
    (with-current-buffer buf
      (insert (format "HTTP/1.1 %s\r\n" status))
      (dolist (h headers)
        (insert (format "%s: %s\r\n" (car h) (cdr h))))
      (insert "\r\n" body))
    buf))

(defmacro synaxis-fetch-tests--with-tmp (&rest body)
  "Run BODY with a fresh DB and clean hooks."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "synaxis-fetch-test" t))
          (synaxis-db-file (expand-file-name "test.db" dir))
          (synaxis-testing t)
          (synaxis-db--connection nil)
          (synaxis-new-entry-parse-hook nil)
          (synaxis-new-entry-hook nil))
     (unwind-protect
         (progn ,@body)
       (synaxis-db-close)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

;;; Response parsing

(ert-deftest synaxis-fetch-test-parse-response-extracts-status-headers-body ()
  (let* ((buf (synaxis-fetch-tests--response
               "200 OK"
               '(("Content-Type" . "application/xml; charset=UTF-8")
                 ("ETag" . "\"v1\""))
               "hello world"))
         (parsed (synaxis-fetch--parse-response buf)))
    (kill-buffer buf)
    (should (= 200 (plist-get parsed :status)))
    (should (equal "\"v1\"" (cdr (assoc "etag" (plist-get parsed :headers)))))
    (should (equal "hello world" (plist-get parsed :body)))))

;;; Ingestion path

(ert-deftest synaxis-fetch-test-process-200-inserts-fresh-entries-as-unread ()
  (synaxis-fetch-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-fetch-tests--response
                "200 OK"
                '(("Content-Type" . "application/atom+xml"))
                (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (let ((entries (synaxis-db-list-entries nil nil)))
       (should (= 1 (length entries)))
       (let ((id (plist-get (car entries) :id)))
         (should (member "unread" (synaxis-db-get-tags id))))))))

(ert-deftest synaxis-fetch-test-process-200-back-fills-feed-title ()
  "First successful fetch sets the feed's title from the parsed `<title>'."
  (synaxis-fetch-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-fetch-tests--response
                "200 OK" nil
                (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (should (equal "Atom Test Feed"
                    (plist-get (synaxis-db-get-feed url) :title))))))

(ert-deftest synaxis-fetch-test-autotags-applied-on-fresh-insert ()
  "Tags listed in `feeds.meta.autotags' decorate every fresh entry."
  (synaxis-fetch-tests--with-tmp
   (let ((url "https://example.com/atom"))
     (synaxis-db-add-feed url '(:meta (:autotags ["emacs" "linux"])))
     (let ((buf (synaxis-fetch-tests--response
                 "200 OK" nil
                 (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (let* ((id (plist-get (car (synaxis-db-list-entries nil nil)) :id))
            (tags (synaxis-db-get-tags id)))
       (should (member "unread" tags))
       (should (member "emacs" tags))
       (should (member "linux" tags))))))

(ert-deftest synaxis-fetch-test-autotags-not-applied-on-update ()
  "Re-ingest of an existing entry does not re-tag with autotags."
  (synaxis-fetch-tests--with-tmp
   (let ((url "https://example.com/atom"))
     (synaxis-db-add-feed url '(:meta (:autotags ["emacs"])))
     (let ((buf (synaxis-fetch-tests--response
                 "200 OK" nil
                 (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (let ((id (plist-get (car (synaxis-db-list-entries nil nil)) :id)))
       ;; User strips the autotag.
       (synaxis-db-remove-tag id "emacs")
       (should-not (member "emacs" (synaxis-db-get-tags id)))
       ;; Re-ingest of same content: should NOT re-apply.
       (let ((buf (synaxis-fetch-tests--response
                   "200 OK" nil
                   (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
         (synaxis-fetch--process-response buf url)
         (kill-buffer buf))
       (should-not (member "emacs" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-fetch-test-empty-autotags-is-noop ()
  "Feed without autotags meta still inserts cleanly."
  (synaxis-fetch-tests--with-tmp
   (let ((url "https://example.com/atom"))
     (let ((buf (synaxis-fetch-tests--response
                 "200 OK" nil
                 (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (let ((id (plist-get (car (synaxis-db-list-entries nil nil)) :id)))
       (should (equal '("unread") (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-fetch-test-process-200-does-not-clobber-user-title ()
  "Back-fill only applies when feed title is currently NULL."
  (synaxis-fetch-tests--with-tmp
   (let ((url "https://example.com/atom"))
     (synaxis-db-add-feed url '(:title "My Feed"))
     (let ((buf (synaxis-fetch-tests--response
                 "200 OK" nil
                 (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (should (equal "My Feed"
                    (plist-get (synaxis-db-get-feed url) :title))))))

(ert-deftest synaxis-fetch-test-process-200-stores-etag-and-last-modified ()
  (synaxis-fetch-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-fetch-tests--response
                "200 OK"
                '(("Content-Type" . "application/atom+xml")
                  ("ETag" . "\"abc\"")
                  ("Last-Modified" . "Tue, 02 Jan 2024 03:04:05 GMT"))
                (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (let ((feed (synaxis-db-get-feed url)))
       (should (equal "\"abc\"" (plist-get feed :etag)))
       (should (equal "Tue, 02 Jan 2024 03:04:05 GMT"
                      (plist-get feed :last-modified)))
       (should (numberp (plist-get feed :last-fetched)))))))

(ert-deftest synaxis-fetch-test-process-200-upserts-existing-without-retagging ()
  (synaxis-fetch-tests--with-tmp
   (let ((url "https://example.com/atom"))
     ;; First fetch: 1 entry, tagged unread.
     (let ((buf (synaxis-fetch-tests--response
                 "200 OK" nil
                 (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (let* ((id (plist-get (car (synaxis-db-list-entries nil nil)) :id)))
       ;; User marks it read.
       (synaxis-db-remove-tag id "unread")
       (should-not (member "unread" (synaxis-db-get-tags id)))
       ;; Second fetch with the same fixture: should NOT re-tag as unread.
       (let ((buf (synaxis-fetch-tests--response
                   "200 OK" nil
                   (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
         (synaxis-fetch--process-response buf url)
         (kill-buffer buf))
       (should-not (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-fetch-test-process-304-bumps-last-fetched-only ()
  (synaxis-fetch-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-fetch-tests--response "304 Not Modified" nil "")))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (let ((feed (synaxis-db-get-feed url)))
       (should feed)
       (should (numberp (plist-get feed :last-fetched)))
       (should (= 0 (length (synaxis-db-list-entries nil nil))))))))

(ert-deftest synaxis-fetch-test-parse-error-bumps-failures ()
  (synaxis-fetch-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-fetch-tests--response "200 OK" nil "<not xml>")))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (let ((feed (synaxis-db-get-feed url)))
       (should (= 1 (plist-get feed :failures)))))))

;;; Hooks

(ert-deftest synaxis-fetch-test-parse-hook-skip-returning-nil ()
  (synaxis-fetch-tests--with-tmp
   (let* ((synaxis-new-entry-parse-hook (list (lambda (_e) nil)))
          (url "https://example.com/atom")
          (buf (synaxis-fetch-tests--response
                "200 OK" nil
                (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (should (= 0 (length (synaxis-db-list-entries nil nil)))))))

(ert-deftest synaxis-fetch-test-parse-hook-modify-title ()
  (synaxis-fetch-tests--with-tmp
   (let* ((synaxis-new-entry-parse-hook
           (list (lambda (e) (plist-put e :title "MUTATED"))))
          (url "https://example.com/atom")
          (buf (synaxis-fetch-tests--response
                "200 OK" nil
                (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (should (equal "MUTATED"
                    (plist-get (car (synaxis-db-list-entries nil nil)) :title))))))

(ert-deftest synaxis-fetch-test-new-entry-hook-called-with-id ()
  (synaxis-fetch-tests--with-tmp
   (let* ((calls nil)
          (synaxis-new-entry-hook (list (lambda (id) (push id calls))))
          (url "https://example.com/atom")
          (buf (synaxis-fetch-tests--response
                "200 OK" nil
                (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (should (= 1 (length calls)))
     (should (integerp (car calls)))
     ;; Second run with the same content: not a fresh insert, no hook call.
     (let ((buf (synaxis-fetch-tests--response
                 "200 OK" nil
                 (synaxis-fetch-tests--load "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (should (= 1 (length calls))))))

(ert-deftest synaxis-fetch-test-feed-binds-http-request-headers ()
  "`synaxis-fetch-feed' let-binds `url-request-extra-headers'."
  (synaxis-fetch-tests--with-tmp
   (let (captured)
     (cl-letf (((symbol-function 'url-queue-retrieve)
                (lambda (&rest _) (setq captured url-request-extra-headers))))
       (synaxis-fetch-feed "https://example.com/x"))
     (should (equal synaxis-http-request-headers captured)))))

(ert-deftest synaxis-fetch-test-dispatch-rss-uses-url-queue ()
  (synaxis-fetch-tests--with-tmp
   (let (queue-fired scrape-fired)
     (synaxis-db-add-feed "https://example.com/rss" '(:type "rss"))
     (cl-letf (((symbol-function 'url-queue-retrieve)
                (lambda (&rest _) (setq queue-fired t)))
               ((symbol-function 'synaxis-scrape-feed)
                (lambda (_url) (setq scrape-fired t))))
       (synaxis-fetch-feed "https://example.com/rss"))
     (should queue-fired)
     (should-not scrape-fired))))

(ert-deftest synaxis-fetch-test-dispatch-scrape-uses-scrape-feed ()
  (synaxis-fetch-tests--with-tmp
   (let (queue-fired scrape-fired)
     (synaxis-db-add-feed "https://example.com/sc" '(:type "scrape"))
     (cl-letf (((symbol-function 'url-queue-retrieve)
                (lambda (&rest _) (setq queue-fired t)))
               ((symbol-function 'synaxis-scrape-feed)
                (lambda (_url) (setq scrape-fired t))))
       (synaxis-fetch-feed "https://example.com/sc"))
     (should scrape-fired)
     (should-not queue-fired))))

(provide 'synaxis-fetch-tests)
;;; synaxis-fetch-tests.el ends here
