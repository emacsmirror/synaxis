;;; synaxis-scrape-tests.el --- Tests for synaxis-scrape  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-scrape'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-scrape.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defconst synaxis-scrape-tests--fixtures-dir
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun synaxis-scrape-tests--dom (name)
  "Parse fixture HTML NAME into a DOM."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name synaxis-scrape-tests--fixtures-dir))
    (libxml-parse-html-region (point-min) (point-max))))

(defun synaxis-scrape-tests--texts (nodes)
  "Return list of inner texts (or hrefs) of NODES."
  (mapcar (lambda (n)
            (or (dom-attr n 'href)
                (string-trim
                 (mapconcat (lambda (c) (if (stringp c) c "")) (dom-children n) ""))))
          nodes))

;;; Selector parser

(ert-deftest synaxis-scrape-test-parse-simple-tag ()
  (let ((p (synaxis-scrape--parse-simple "a")))
    (should (equal "a" (plist-get (cdr p) :tag)))
    (should-not (plist-get (cdr p) :classes))
    (should-not (plist-get (cdr p) :id))))

(ert-deftest synaxis-scrape-test-parse-simple-class ()
  (let ((p (synaxis-scrape--parse-simple ".foo")))
    (should-not (plist-get (cdr p) :tag))
    (should (equal '("foo") (plist-get (cdr p) :classes)))))

(ert-deftest synaxis-scrape-test-parse-simple-id ()
  (let ((p (synaxis-scrape--parse-simple "#bar")))
    (should (equal "bar" (plist-get (cdr p) :id)))))

(ert-deftest synaxis-scrape-test-parse-simple-combined ()
  (let ((p (synaxis-scrape--parse-simple "h2.entry-title.foo#hero")))
    (should (equal "h2" (plist-get (cdr p) :tag)))
    (should (equal '("entry-title" "foo") (plist-get (cdr p) :classes)))
    (should (equal "hero" (plist-get (cdr p) :id)))))

(ert-deftest synaxis-scrape-test-parse-descendant ()
  (let ((ast (synaxis-scrape--parse-selector "h2 a")))
    (should (eq 'descendant (car ast)))))

(ert-deftest synaxis-scrape-test-parse-child ()
  (let ((ast (synaxis-scrape--parse-selector "h2 > a")))
    (should (eq 'child (car ast)))))

(ert-deftest synaxis-scrape-test-parse-alternatives ()
  (let ((ast (synaxis-scrape--parse-selector "a, b")))
    (should (eq 'or-selector (car ast)))
    (should (= 2 (length (cdr ast))))))

;;; Matching against fixture

(ert-deftest synaxis-scrape-test-query-by-tag ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html"))
         (hits (synaxis-scrape--query "article" dom)))
    (should (= 3 (length hits)))))

(ert-deftest synaxis-scrape-test-query-by-class ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html"))
         (hits (synaxis-scrape--query ".entry-title" dom)))
    (should (= 3 (length hits)))))

(ert-deftest synaxis-scrape-test-query-by-id ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html"))
         (hits (synaxis-scrape--query "#sidebar" dom)))
    (should (= 1 (length hits)))))

(ert-deftest synaxis-scrape-test-query-combined ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html"))
         (hits (synaxis-scrape--query "article.featured" dom)))
    (should (= 1 (length hits)))))

(ert-deftest synaxis-scrape-test-query-descendant ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html"))
         (hits (synaxis-scrape--query "h2 a" dom))
         (hrefs (synaxis-scrape-tests--texts hits)))
    (should (equal '("/post/1" "/post/2" "/post/3") hrefs))))

(ert-deftest synaxis-scrape-test-query-direct-child ()
  "`>' excludes deeper descendants."
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html"))
         ;; article > a should match nothing (no direct <a> children of article)
         (hits (synaxis-scrape--query "article > a" dom)))
    (should (null hits)))
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html"))
         (hits (synaxis-scrape--query "h2 > a" dom)))
    (should (= 3 (length hits)))))

(ert-deftest synaxis-scrape-test-query-alternatives ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html"))
         (hits (synaxis-scrape--query "h1, aside" dom)))
    (should (= 2 (length hits)))))

(ert-deftest synaxis-scrape-test-query-empty-result ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-basic.html")))
    (should (null (synaxis-scrape--query ".nope" dom)))))

;;; URL resolution

(ert-deftest synaxis-scrape-test-resolve-url-absolute ()
  (should (equal "https://x.example/y"
                 (synaxis-scrape--resolve-url "https://base.example/foo"
                                              "https://x.example/y"))))

(ert-deftest synaxis-scrape-test-resolve-url-root-relative ()
  (should (equal "https://base.example/post/1"
                 (synaxis-scrape--resolve-url "https://base.example/blog/"
                                              "/post/1"))))

(ert-deftest synaxis-scrape-test-resolve-url-protocol-relative ()
  (should (equal "https://cdn.example/x"
                 (synaxis-scrape--resolve-url "https://base.example/"
                                              "//cdn.example/x"))))

(ert-deftest synaxis-scrape-test-resolve-url-fragment ()
  (should (equal "https://base.example/page#sec"
                 (synaxis-scrape--resolve-url "https://base.example/page"
                                              "#sec"))))

(ert-deftest synaxis-scrape-test-resolve-url-nil ()
  (should-not (synaxis-scrape--resolve-url "https://x.example/" nil))
  (should-not (synaxis-scrape--resolve-url "https://x.example/" "")))

;;; Date extraction

(ert-deftest synaxis-scrape-test-extract-date-from-datetime-attr ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-article.html"))
         (date (synaxis-scrape--extract-date dom "time")))
    (should (numberp date))
    (should (= date (float-time (encode-time (parse-time-string
                                              "2024-05-12T09:00:00Z")))))))

(ert-deftest synaxis-scrape-test-extract-date-nil-selector ()
  (should-not (synaxis-scrape--extract-date 'whatever nil)))

;;; Content cleanup

(ert-deftest synaxis-scrape-test-cleanup-removes-matching-children ()
  (let* ((dom (synaxis-scrape-tests--dom "scrape-article.html"))
         (article (car (synaxis-scrape--query "article" dom))))
    (synaxis-scrape--cleanup-content article ".ads, .related-posts")
    (should-not (synaxis-scrape--query ".ads" article))
    (should-not (synaxis-scrape--query ".related-posts" article))
    ;; The actual body paragraphs remain.
    (should (synaxis-scrape--query "p" article))))

;;; Title cleanup

(ert-deftest synaxis-scrape-test-strip-title ()
  (should (equal "Article Title"
                 (synaxis-scrape--strip-title "Article Title - Example News"
                                              " - Example News"))))

(ert-deftest synaxis-scrape-test-strip-title-handles-nils ()
  (should (equal "Title" (synaxis-scrape--strip-title "Title" nil)))
  (should (equal "" (synaxis-scrape--strip-title nil " - foo"))))

;;; Pure extraction

(defun synaxis-scrape-tests--load (name)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name synaxis-scrape-tests--fixtures-dir))
    (buffer-string)))

(ert-deftest synaxis-scrape-test-extract-basic ()
  "Extracts 3 entries from the basic blog fixture."
  (let* ((html (synaxis-scrape-tests--load "scrape-basic.html"))
         (entries (synaxis-scrape--extract
                   html "https://example.com/blog/"
                   '(:url-selector "h2.entry-title a"))))
    (should (= 3 (length entries)))
    (let ((links (mapcar (lambda (e) (plist-get e :link)) entries)))
      (should (member "https://example.com/post/1" links))
      (should (member "https://example.com/post/3" links)))
    (should (string-match-p "First Post"
                            (plist-get (car entries) :title)))))

(ert-deftest synaxis-scrape-test-extract-applies-url-pattern ()
  (let* ((html (synaxis-scrape-tests--load "scrape-basic.html"))
         (entries (synaxis-scrape--extract
                   html "https://example.com/blog/"
                   '(:url-selector "h2.entry-title a"
                                   :url-pattern "/post/2"))))
    (should (= 1 (length entries)))
    (should (string-match-p "/post/2" (plist-get (car entries) :link)))))

(ert-deftest synaxis-scrape-test-extract-applies-limit ()
  (let* ((html (synaxis-scrape-tests--load "scrape-basic.html"))
         (entries (synaxis-scrape--extract
                   html "https://example.com/blog/"
                   '(:url-selector "h2.entry-title a" :limit 2))))
    (should (= 2 (length entries)))))

(ert-deftest synaxis-scrape-test-extract-applies-title-cleanup ()
  (let* ((html (synaxis-scrape-tests--load "scrape-basic.html"))
         (entries (synaxis-scrape--extract
                   html "https://example.com/blog/"
                   '(:url-selector "h2.entry-title a"
                                   :title-cleanup " Post"))))
    (should (equal "First" (plist-get (car entries) :title)))))

(ert-deftest synaxis-scrape-test-extract-resolves-relative-links ()
  (let* ((html (synaxis-scrape-tests--load "scrape-basic.html"))
         (entries (synaxis-scrape--extract
                   html "https://example.com/blog/"
                   '(:url-selector "h2.entry-title a"))))
    (dolist (e entries)
      (should (string-prefix-p "https://example.com/" (plist-get e :link))))))

(ert-deftest synaxis-scrape-test-extract-deduplicates ()
  "Same href appearing twice in the source HTML is deduped."
  (let ((html "<html><body>
                  <a class=\"x\" href=\"/p/1\">A</a>
                  <a class=\"x\" href=\"/p/1\">A again</a>
                  <a class=\"x\" href=\"/p/2\">B</a>
                </body></html>"))
    (should (= 2 (length
                  (synaxis-scrape--extract
                   html "https://example.com/"
                   '(:url-selector ".x")))))))

(ert-deftest synaxis-scrape-test-extract-empty-on-no-match ()
  (let* ((html (synaxis-scrape-tests--load "scrape-basic.html")))
    (should (null (synaxis-scrape--extract
                   html "https://example.com/"
                   '(:url-selector ".nothing-matches"))))))

(ert-deftest synaxis-scrape-test-page-title-with-cleanup ()
  (let ((html "<html><head><title>Hello - Site</title></head></html>"))
    (should (equal "Hello"
                   (synaxis-scrape--page-title
                    html '(:title-cleanup " - Site"))))))

;;; Async per-article expansion

(defun synaxis-scrape-tests--response-buffer (body)
  "Return a buffer containing a synthetic HTTP response with BODY."
  (let ((buf (generate-new-buffer " *synaxis-scrape-test*")))
    (with-current-buffer buf
      (insert "HTTP/1.1 200 OK\r\n\r\n" body))
    buf))

(ert-deftest synaxis-scrape-test-expand-content-skipped-without-selector ()
  (let (out)
    (synaxis-scrape--expand-content
     (list (list :link "x"))
     '(:url-selector "a")
     (lambda (entries) (setq out entries)))
    (should out)
    (should (equal "x" (plist-get (car out) :link)))))

(ert-deftest synaxis-scrape-test-expand-content-applies-selector ()
  (let* ((article-html (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "scrape-article.html"
                                            synaxis-scrape-tests--fixtures-dir))
                         (buffer-string)))
         (entries (list (list :link "https://example.com/p/1"
                              :title "T" :date 1.0)))
         out)
    (cl-letf (((symbol-function 'url-queue-retrieve)
               (lambda (_url cb cbargs &rest _)
                 (with-current-buffer
                     (synaxis-scrape-tests--response-buffer article-html)
                   (apply cb nil cbargs)))))
      (synaxis-scrape--expand-content
       entries
       '(:url-selector "a"
                       :content-selector "div.content"
                       :content-cleanup ".ads, .related-posts")
       (lambda (e) (setq out e))))
    (should (= 1 (length out)))
    (let ((c (plist-get (car out) :content)))
      (should (string-match-p "Real article body" c))
      (should-not (string-match-p "Advertisement" c)))))

(ert-deftest synaxis-scrape-test-expand-content-fan-in ()
  "All N callbacks must fire before done-callback runs."
  (let* ((entries (cl-loop for i below 4
                           collect (list :link (format "https://x.example/%d" i)
                                         :title "T" :date 1.0)))
         out fired)
    (cl-letf (((symbol-function 'url-queue-retrieve)
               (lambda (_url cb cbargs &rest _)
                 (with-current-buffer
                     (synaxis-scrape-tests--response-buffer
                      "<html><body><article><p>x</p></article></body></html>")
                   (apply cb nil cbargs)))))
      (synaxis-scrape--expand-content
       entries
       '(:url-selector "a" :content-selector "article")
       (lambda (e) (setq fired t out e))))
    (should fired)
    (should (= 4 (length out)))))

;;; synaxis-scrape-test command

(ert-deftest synaxis-scrape-test-pops-buffer-without-saving ()
  "Calling `synaxis-scrape-test' renders the preview buffer and does no DB writes."
  (let* ((dir (make-temp-file "synaxis-scrape-cmd" t))
         (synaxis-db-file (expand-file-name "test.db" dir))
         (synaxis-testing t)
         (synaxis-db--connection nil)
         (display-buffer-alist '((".*" display-buffer-no-window)))
         (html (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "scrape-basic.html"
                                    synaxis-scrape-tests--fixtures-dir))
                 (buffer-string))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'synaxis-scrape--fetch-html)
                     (lambda (_url) html)))
            (synaxis-scrape-test "https://example.com/blog/"
                                 :url-selector "h2.entry-title a"))
          (let ((buf (get-buffer "*synaxis-scrape-test*")))
            (should buf)
            (with-current-buffer buf
              (should (= 3 (length tabulated-list-entries)))))
          ;; No feed or entry rows written.
          (should-not (synaxis-db-list-feeds))
          (should (zerop (caar (sqlite-select (synaxis-db--ensure-open)
                                              "SELECT COUNT(*) FROM entries;")))))
      (when (get-buffer "*synaxis-scrape-test*")
        (kill-buffer "*synaxis-scrape-test*"))
      (synaxis-db-close)
      (delete-directory dir t))))

(provide 'synaxis-scrape-tests)
;;; synaxis-scrape-tests.el ends here
