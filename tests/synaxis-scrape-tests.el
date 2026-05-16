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

(provide 'synaxis-scrape-tests)
;;; synaxis-scrape-tests.el ends here
