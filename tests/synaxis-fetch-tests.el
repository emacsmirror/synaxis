;;; synaxis-fetch-tests.el --- Tests for synaxis-fetch  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-fetch'.  All tests synthesise HTTP response
;; buffers and drive the response-processing pipeline directly; the
;; network is never touched.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-fetch.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'synaxis-test-utils)

;;; Response parsing

(ert-deftest synaxis-fetch-test-parse-response-extracts-status-headers-body ()
  (let* ((buf (synaxis-tests--http-response
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
  (synaxis-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-tests--http-response
                "200 OK"
                '(("Content-Type" . "application/atom+xml"))
                (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (let ((entries (synaxis-db-list-entries nil nil)))
       (should (= 1 (length entries)))
       (let ((id (plist-get (car entries) :id)))
         (should (member "unread" (synaxis-db-get-tags id))))))))

(ert-deftest synaxis-fetch-test-process-200-back-fills-feed-title ()
  "First successful fetch sets the feed's title from the parsed `<title>'."
  (synaxis-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-tests--http-response
                "200 OK" nil
                (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (should (equal "Atom Test Feed"
                    (plist-get (synaxis-db-get-feed url) :title))))))

(ert-deftest synaxis-fetch-test-autotags-applied-on-fresh-insert ()
  "Tags listed in `feeds.meta.autotags' decorate every fresh entry."
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/atom"))
     (synaxis-db-add-feed url '(:meta (:autotags ["emacs" "linux"])))
     (let ((buf (synaxis-tests--http-response
                 "200 OK" nil
                 (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (let* ((id (plist-get (car (synaxis-db-list-entries nil nil)) :id))
            (tags (synaxis-db-get-tags id)))
       (should (member "unread" tags))
       (should (member "emacs" tags))
       (should (member "linux" tags))))))

(ert-deftest synaxis-fetch-test-autotags-not-applied-on-update ()
  "Re-ingest of an existing entry does not re-tag with autotags."
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/atom"))
     (synaxis-db-add-feed url '(:meta (:autotags ["emacs"])))
     (let ((buf (synaxis-tests--http-response
                 "200 OK" nil
                 (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (let ((id (plist-get (car (synaxis-db-list-entries nil nil)) :id)))
       ;; User strips the autotag.
       (synaxis-db-remove-tag id "emacs")
       (should-not (member "emacs" (synaxis-db-get-tags id)))
       ;; Re-ingest of same content: should NOT re-apply.
       (let ((buf (synaxis-tests--http-response
                   "200 OK" nil
                   (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
         (synaxis-fetch--process-response buf url)
         (kill-buffer buf))
       (should-not (member "emacs" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-fetch-test-empty-autotags-is-noop ()
  "Feed without autotags meta still inserts cleanly."
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/atom"))
     (let ((buf (synaxis-tests--http-response
                 "200 OK" nil
                 (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (let ((id (plist-get (car (synaxis-db-list-entries nil nil)) :id)))
       (should (equal '("unread") (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-fetch-test-process-200-does-not-clobber-user-title ()
  "Back-fill only applies when feed title is currently NULL."
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/atom"))
     (synaxis-db-add-feed url '(:title "My Feed"))
     (let ((buf (synaxis-tests--http-response
                 "200 OK" nil
                 (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (should (equal "My Feed"
                    (plist-get (synaxis-db-get-feed url) :title))))))

(ert-deftest synaxis-fetch-test-process-200-stores-etag-and-last-modified ()
  (synaxis-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-tests--http-response
                "200 OK"
                '(("Content-Type" . "application/atom+xml")
                  ("ETag" . "\"abc\"")
                  ("Last-Modified" . "Tue, 02 Jan 2024 03:04:05 GMT"))
                (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (let ((feed (synaxis-db-get-feed url)))
       (should (equal "\"abc\"" (plist-get feed :etag)))
       (should (equal "Tue, 02 Jan 2024 03:04:05 GMT"
                      (plist-get feed :last-modified)))
       (should (numberp (plist-get feed :last-fetched)))))))

(ert-deftest synaxis-fetch-test-process-200-upserts-existing-without-retagging ()
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/atom"))
     ;; First fetch: 1 entry, tagged unread.
     (let ((buf (synaxis-tests--http-response
                 "200 OK" nil
                 (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (let* ((id (plist-get (car (synaxis-db-list-entries nil nil)) :id)))
       ;; User marks it read.
       (synaxis-db-remove-tag id "unread")
       (should-not (member "unread" (synaxis-db-get-tags id)))
       ;; Second fetch with the same fixture: should NOT re-tag as unread.
       (let ((buf (synaxis-tests--http-response
                   "200 OK" nil
                   (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
         (synaxis-fetch--process-response buf url)
         (kill-buffer buf))
       (should-not (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-fetch-test-process-304-bumps-last-fetched-only ()
  (synaxis-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-tests--http-response "304 Not Modified" nil "")))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (let ((feed (synaxis-db-get-feed url)))
       (should feed)
       (should (numberp (plist-get feed :last-fetched)))
       (should (= 0 (length (synaxis-db-list-entries nil nil))))))))

(ert-deftest synaxis-fetch-test-parse-error-bumps-failures ()
  (synaxis-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-tests--http-response "200 OK" nil "<not xml>")))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (let ((feed (synaxis-db-get-feed url)))
       (should (= 1 (plist-get feed :failures)))))))

;;; Hooks

(ert-deftest synaxis-fetch-test-parse-hook-skip-returning-nil ()
  (synaxis-tests--with-tmp
   (let* ((synaxis-new-entry-parse-hook (list (lambda (_e) nil)))
          (url "https://example.com/atom")
          (buf (synaxis-tests--http-response
                "200 OK" nil
                (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (should (= 0 (length (synaxis-db-list-entries nil nil)))))))

(ert-deftest synaxis-fetch-test-parse-hook-modify-title ()
  (synaxis-tests--with-tmp
   (let* ((synaxis-new-entry-parse-hook
           (list (lambda (e) (plist-put e :title "MUTATED"))))
          (url "https://example.com/atom")
          (buf (synaxis-tests--http-response
                "200 OK" nil
                (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (should (equal "MUTATED"
                    (plist-get (car (synaxis-db-list-entries nil nil)) :title))))))

(ert-deftest synaxis-fetch-test-new-entry-hook-called-with-id ()
  (synaxis-tests--with-tmp
   (let* ((calls nil)
          (synaxis-new-entry-hook (list (lambda (id) (push id calls))))
          (url "https://example.com/atom")
          (buf (synaxis-tests--http-response
                "200 OK" nil
                (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
     (synaxis-fetch--process-response buf url)
     (kill-buffer buf)
     (should (= 1 (length calls)))
     (should (integerp (car calls)))
     ;; Second run with the same content: not a fresh insert, no hook call.
     (let ((buf (synaxis-tests--http-response
                 "200 OK" nil
                 (synaxis-tests--load-fixture "atom-1.0-minimal.xml"))))
       (synaxis-fetch--process-response buf url)
       (kill-buffer buf))
     (should (= 1 (length calls))))))

(ert-deftest synaxis-fetch-test-feed-binds-http-request-headers ()
  "`synaxis-fetch-feed' let-binds `url-request-extra-headers'."
  (synaxis-tests--with-tmp
   (let (captured)
     (cl-letf (((symbol-function 'url-queue-retrieve)
                (lambda (&rest _) (setq captured url-request-extra-headers))))
       (synaxis-fetch-feed "https://example.com/x"))
     (should (equal synaxis-http-request-headers captured)))))

(ert-deftest synaxis-fetch-test-dispatch-rss-uses-url-queue ()
  (synaxis-tests--with-tmp
   (let (queue-fired scrape-fired)
     (synaxis-db-add-feed "https://example.com/rss" '(:type "rss"))
     (cl-letf (((symbol-function 'url-queue-retrieve)
                (lambda (&rest _) (setq queue-fired t)))
               ((symbol-function 'synaxis-scrape-feed)
                (lambda (_url &optional done-callback)
                  (setq scrape-fired t)
                  (when done-callback
                    (funcall done-callback)))))
       (synaxis-fetch-feed "https://example.com/rss"))
     (should queue-fired)
     (should-not scrape-fired))))

(ert-deftest synaxis-fetch-test-dispatch-scrape-uses-scrape-feed ()
  (synaxis-tests--with-tmp
   (let (queue-fired scrape-fired)
     (synaxis-db-add-feed "https://example.com/sc" '(:type "scrape"))
     (cl-letf (((symbol-function 'url-queue-retrieve)
                (lambda (&rest _) (setq queue-fired t)))
               ((symbol-function 'synaxis-scrape-feed)
                (lambda (_url &optional done-callback)
                  (setq scrape-fired t)
                  (when done-callback
                    (funcall done-callback)))))
       (synaxis-fetch-feed "https://example.com/sc"))
     (should scrape-fired)
     (should-not queue-fired))))

;;; Conditional GET headers

(ert-deftest synaxis-fetch-test-conditional-headers-nil-feed ()
  (should (null (synaxis-fetch--conditional-headers nil))))

(ert-deftest synaxis-fetch-test-conditional-headers-empty-feed ()
  (should (null (synaxis-fetch--conditional-headers '()))))

(ert-deftest synaxis-fetch-test-conditional-headers-etag-only ()
  (should (equal '(("If-None-Match" . "abc"))
                 (synaxis-fetch--conditional-headers '(:etag "abc")))))

(ert-deftest synaxis-fetch-test-conditional-headers-last-modified-only ()
  (should (equal '(("If-Modified-Since" . "Wed, 21 Oct 2026 07:28:00 GMT"))
                 (synaxis-fetch--conditional-headers
                  '(:last-modified "Wed, 21 Oct 2026 07:28:00 GMT")))))

(ert-deftest synaxis-fetch-test-conditional-headers-both ()
  (let ((h (synaxis-fetch--conditional-headers
            '(:etag "abc" :last-modified "Wed, 21 Oct 2026 07:28:00 GMT"))))
    (should (member '("If-None-Match" . "abc") h))
    (should (member '("If-Modified-Since" . "Wed, 21 Oct 2026 07:28:00 GMT") h))))

;;; Cache-extract advice toggle

(ert-deftest synaxis-fetch-test-cache-advice-toggle-idempotent ()
  "Toggling on twice does not double-install advice."
  (unwind-protect
      (progn
        (synaxis-fetch--cache-advice-toggle t)
        (synaxis-fetch--cache-advice-toggle t)
        (should synaxis-fetch--cache-advice-active)
        (synaxis-fetch--cache-advice-toggle nil)
        (synaxis-fetch--cache-advice-toggle nil)
        (should-not synaxis-fetch--cache-advice-active))
    (synaxis-fetch--cache-advice-toggle nil)))

(ert-deftest synaxis-fetch-test-cache-advice-safe-on-missing-file ()
  "The :around advice returns nil for a missing file without erroring."
  (let ((called-orig nil))
    (should-not
     (synaxis-fetch--url-cache-extract-safe
      (lambda (_) (setq called-orig t))
      "/tmp/synaxis-cache-definitely-not-a-file.bin"))
    (should-not called-orig)))

(ert-deftest synaxis-fetch-test-cache-advice-passes-through-when-present ()
  "Advice delegates to ORIG when the file exists."
  (let ((tmp (make-temp-file "synaxis-cache-pass-")))
    (unwind-protect
        (let ((called-with nil))
          (synaxis-fetch--url-cache-extract-safe
           (lambda (f) (setq called-with f))
           tmp)
          (should (equal tmp called-with)))
      (delete-file tmp))))

;;; Queue-drained hook

(defun synaxis-fetch-test--drain-call-p (call)
  "Non-nil when CALL schedules `synaxis-fetch-queue-drained-hook'."
  (and (equal 0 (nth 0 call))
       (null (nth 1 call))
       (eq (nth 2 call) #'run-hooks)
       (equal (nth 3 call) '(synaxis-fetch-queue-drained-hook))))

(defun synaxis-fetch-test--drain-calls (scheduled)
  "Return queue-drained hook scheduling calls from SCHEDULED."
  (cl-remove-if-not #'synaxis-fetch-test--drain-call-p scheduled))

(ert-deftest synaxis-fetch-test-callback-schedules-queue-drained-hook-on-drain ()
  "Schedule the drained hook when the callback empties the in-flight table."
  (synaxis-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (buf (synaxis-tests--http-response
                "304 Not Modified" nil ""))
          (scheduled nil))
     (clrhash synaxis-fetch--in-flight)
     (puthash url t synaxis-fetch--in-flight)
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (secs repeat fn &rest args)
                  (push (list secs repeat fn args) scheduled))))
       (with-current-buffer buf
         (synaxis-fetch--callback nil url)))
     (should (zerop (hash-table-count synaxis-fetch--in-flight)))
     (should (= 1 (length (synaxis-fetch-test--drain-calls scheduled)))))))

(ert-deftest synaxis-fetch-test-callback-skips-hook-when-queue-not-drained ()
  "Do not schedule the drained hook while another URL is in flight."
  (synaxis-tests--with-tmp
   (let* ((url "https://example.com/atom")
          (other "https://example.com/other")
          (buf (synaxis-tests--http-response
                "304 Not Modified" nil ""))
          (scheduled nil))
     (clrhash synaxis-fetch--in-flight)
     (puthash url t synaxis-fetch--in-flight)
     (puthash other t synaxis-fetch--in-flight)
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (secs repeat fn &rest args)
                  (push (list secs repeat fn args) scheduled))))
       (with-current-buffer buf
         (synaxis-fetch--callback nil url)))
     (should (= 1 (hash-table-count synaxis-fetch--in-flight)))
     (should-not (synaxis-fetch-test--drain-calls scheduled)))))

(ert-deftest synaxis-fetch-test-scrape-schedules-queue-drained-hook-on-drain ()
  "Scrape feeds should drain like queued fetches."
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/sc")
         scheduled)
     (synaxis-db-add-feed url '(:type "scrape"))
     (clrhash synaxis-fetch--in-flight)
     (cl-letf (((symbol-function 'synaxis-scrape-feed)
                (lambda (_url &optional done-callback)
                  (when done-callback
                    (funcall done-callback))))
               ((symbol-function 'run-at-time)
                (lambda (secs repeat fn &rest args)
                  (push (list secs repeat fn args) scheduled))))
       (synaxis-fetch-feed url))
     (should (zerop (hash-table-count synaxis-fetch--in-flight)))
     (should (= 1 (length (synaxis-fetch-test--drain-calls scheduled)))))))

(ert-deftest synaxis-fetch-test-scrape-completion-is-idempotent ()
  "A scrape completion callback called twice should drain only once."
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/sc")
         scheduled)
     (synaxis-db-add-feed url '(:type "scrape"))
     (clrhash synaxis-fetch--in-flight)
     (cl-letf (((symbol-function 'synaxis-scrape-feed)
                (lambda (_url &optional done-callback)
                  (when done-callback
                    (funcall done-callback)
                    (funcall done-callback))))
               ((symbol-function 'run-at-time)
                (lambda (secs repeat fn &rest args)
                  (push (list secs repeat fn args) scheduled))))
       (synaxis-fetch-feed url))
     (should (zerop (hash-table-count synaxis-fetch--in-flight)))
     (should (= 1 (length (synaxis-fetch-test--drain-calls scheduled)))))))

(ert-deftest synaxis-fetch-test-scrape-setup-error-clears-in-flight ()
  "A synchronous scrape setup error should not leave URL in flight."
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/sc")
         scheduled)
     (synaxis-db-add-feed url '(:type "scrape"))
     (clrhash synaxis-fetch--in-flight)
     (cl-letf (((symbol-function 'synaxis-scrape-feed)
                (lambda (&rest _) (error "Boom")))
               ((symbol-function 'run-at-time)
                (lambda (secs repeat fn &rest args)
                  (push (list secs repeat fn args) scheduled))))
       (synaxis-fetch-feed url))
     (should (zerop (hash-table-count synaxis-fetch--in-flight)))
     (should (= 1 (plist-get (synaxis-db-get-feed url) :failures)))
     (should (= 1 (length (synaxis-fetch-test--drain-calls scheduled)))))))

(ert-deftest synaxis-fetch-test-scrape-skips-hook-when-queue-not-drained ()
  "Scrape completion should not run the drained hook while another URL lives."
  (synaxis-tests--with-tmp
   (let ((url "https://example.com/sc")
         (other "https://example.com/other")
         scheduled)
     (synaxis-db-add-feed url '(:type "scrape"))
     (clrhash synaxis-fetch--in-flight)
     (puthash other t synaxis-fetch--in-flight)
     (cl-letf (((symbol-function 'synaxis-scrape-feed)
                (lambda (_url &optional done-callback)
                  (when done-callback
                    (funcall done-callback))))
               ((symbol-function 'run-at-time)
                (lambda (secs repeat fn &rest args)
                  (push (list secs repeat fn args) scheduled))))
       (synaxis-fetch-feed url))
     (should (= 1 (hash-table-count synaxis-fetch--in-flight)))
     (should (gethash other synaxis-fetch--in-flight))
     (should-not (synaxis-fetch-test--drain-calls scheduled)))))

(ert-deftest synaxis-fetch-test-mixed-rss-scrape-drain-clears-cache-advice ()
  "When scrape drains after RSS, cache advice is removed once."
  (synaxis-tests--with-tmp
   (let* ((rss "https://example.com/rss")
          (scrape "https://example.com/sc")
          (buf (synaxis-tests--http-response "304 Not Modified" nil ""))
          scrape-done
          scheduled)
     (unwind-protect
         (progn
           (synaxis-db-add-feed scrape '(:type "scrape"))
           (clrhash synaxis-fetch--in-flight)
           (synaxis-fetch--cache-advice-toggle t)
           (puthash rss t synaxis-fetch--in-flight)
           (cl-letf (((symbol-function 'synaxis-scrape-feed)
                      (lambda (_url &optional done-callback)
                        (setq scrape-done done-callback)))
                     ((symbol-function 'run-at-time)
                      (lambda (secs repeat fn &rest args)
                        (push (list secs repeat fn args) scheduled))))
             (synaxis-fetch-feed scrape)
             (should (= 2 (hash-table-count synaxis-fetch--in-flight)))
             (with-current-buffer buf
               (synaxis-fetch--callback nil rss))
             (should (= 1 (hash-table-count synaxis-fetch--in-flight)))
             (should synaxis-fetch--cache-advice-active)
             (funcall scrape-done)
             (should (zerop (hash-table-count synaxis-fetch--in-flight)))
             (should-not synaxis-fetch--cache-advice-active)
             (should (= 1 (length (synaxis-fetch-test--drain-calls scheduled))))))
       (synaxis-fetch--cache-advice-toggle nil)
       (when (buffer-live-p buf)
         (kill-buffer buf))))))

;;; Group: failure backoff

(ert-deftest synaxis-fetch-test-due-p-no-failures ()
  (should (synaxis-fetch--due-p '(:failures 0 :last-fetched 0)))
  (should (synaxis-fetch--due-p '())))

(ert-deftest synaxis-fetch-test-due-p-recent-failure-held ()
  (let ((synaxis-fetch-backoff-base 3600))
    (should-not (synaxis-fetch--due-p
                 (list :failures 1 :last-fetched (float-time))))))

(ert-deftest synaxis-fetch-test-due-p-old-failure-retried ()
  (let ((synaxis-fetch-backoff-base 3600))
    (should (synaxis-fetch--due-p
             (list :failures 1 :last-fetched (- (float-time) 7200))))))

(ert-deftest synaxis-fetch-test-all-respects-backoff ()
  "`synaxis-fetch-all' with backoff skips a recently-failed feed."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://ok.example/feed")
   (synaxis-db-add-feed "https://bad.example/feed")
   (synaxis-db-set-feed-cache-headers
    "https://bad.example/feed"
    (list :last-fetched (float-time) :failures 3))
   (let ((fetched nil)
         (synaxis-fetch-backoff-base 3600))
     (cl-letf (((symbol-function 'synaxis-fetch-feed)
                (lambda (url) (push url fetched))))
       (synaxis-fetch-all t))
     (should (member "https://ok.example/feed" fetched))
     (should-not (member "https://bad.example/feed" fetched))
     ;; Without backoff, the failing feed is fetched too.
     (setq fetched nil)
     (cl-letf (((symbol-function 'synaxis-fetch-feed)
                (lambda (url) (push url fetched))))
       (synaxis-fetch-all))
     (should (member "https://bad.example/feed" fetched)))))

(provide 'synaxis-fetch-tests)
;;; synaxis-fetch-tests.el ends here
