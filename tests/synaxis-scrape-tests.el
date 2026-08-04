;;; synaxis-scrape-tests.el --- Tests for synaxis-scrape  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-scrape'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-scrape.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'synaxis-test-utils)

(defun synaxis-scrape-tests--dom (name)
  "Parse fixture HTML NAME into a DOM."
  (with-temp-buffer
    (insert (synaxis-tests--load-fixture name))
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
    (should (equal "2024-05-12T09:00:00Z" date))))

(ert-deftest synaxis-scrape-test-extract-date-nil-selector ()
  (should-not (synaxis-scrape--extract-date 'whatever nil)))

(ert-deftest synaxis-scrape-test-extract-date-from-text-content ()
  "A bare date in text (no `datetime' attr) still parses.
Mirrors the Medscape pattern: `<span class=\"article-date\"> May 15, 2026</span>'.
Also: a second empty span with the same class is skipped."
  (let* ((dom (synaxis-scrape-tests--dom "scrape-medscape-like.html"))
         (date (synaxis-scrape--extract-date dom ".article-date")))
    (should (stringp date))
    ;; Date-only inputs encode at UTC midnight, so the day is stable
    ;; regardless of the test runner's timezone.
    (should (string-match-p "\\`2026-05-15" date))))

(ert-deftest synaxis-scrape-test-parse-time-loose-date-only ()
  "Date strings without a time component encode to start-of-day."
  (let ((t1 (synaxis-scrape--parse-time-loose "May 15, 2026"))
        (t2 (synaxis-scrape--parse-time-loose " May 15, 2026")))
    (should (stringp t1))
    (should (stringp t2))
    (should (equal t1 t2))))

(ert-deftest synaxis-scrape-test-parse-time-loose-date-only-is-utc ()
  "A bare date encodes to UTC midnight, not local midnight."
  (should (equal "2026-05-15T00:00:00Z"
                 (synaxis-scrape--parse-time-loose "May 15, 2026"))))

(ert-deftest synaxis-scrape-test-prefer-date-keeps-valid-past ()
  (should (equal "2026-01-01T00:00:00Z"
                 (synaxis-scrape--prefer-date "2026-01-01T00:00:00Z"
                                              "2026-06-18T00:00:00Z"))))

(ert-deftest synaxis-scrape-test-prefer-date-rejects-future ()
  "A future cover date falls back to the pull-time date."
  (should (equal "2026-06-18T00:00:00Z"
                 (synaxis-scrape--prefer-date "2026-12-31T00:00:00Z"
                                              "2026-06-18T00:00:00Z"))))

(ert-deftest synaxis-scrape-test-prefer-date-nil-falls-back ()
  (should (equal "2026-06-18T00:00:00Z"
                 (synaxis-scrape--prefer-date nil "2026-06-18T00:00:00Z"))))

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

(ert-deftest synaxis-scrape-test-extract-basic ()
  "Extracts 3 entries from the basic blog fixture."
  (let* ((html (synaxis-tests--load-fixture "scrape-basic.html"))
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
  (let* ((html (synaxis-tests--load-fixture "scrape-basic.html"))
         (entries (synaxis-scrape--extract
                   html "https://example.com/blog/"
                   '(:url-selector "h2.entry-title a"
                                   :url-pattern "/post/2"))))
    (should (= 1 (length entries)))
    (should (string-match-p "/post/2" (plist-get (car entries) :link)))))

(ert-deftest synaxis-scrape-test-extract-applies-limit ()
  (let* ((html (synaxis-tests--load-fixture "scrape-basic.html"))
         (entries (synaxis-scrape--extract
                   html "https://example.com/blog/"
                   '(:url-selector "h2.entry-title a" :limit 2))))
    (should (= 2 (length entries)))))

(ert-deftest synaxis-scrape-test-extract-applies-title-cleanup ()
  (let* ((html (synaxis-tests--load-fixture "scrape-basic.html"))
         (entries (synaxis-scrape--extract
                   html "https://example.com/blog/"
                   '(:url-selector "h2.entry-title a"
                                   :title-cleanup " Post"))))
    (should (equal "First" (plist-get (car entries) :title)))))

(ert-deftest synaxis-scrape-test-extract-resolves-relative-links ()
  (let* ((html (synaxis-tests--load-fixture "scrape-basic.html"))
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
  (let* ((html (synaxis-tests--load-fixture "scrape-basic.html")))
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
                                            synaxis-tests--fixtures-dir))
                         (buffer-string)))
         (entries (list (list :link "https://example.com/p/1"
                              :title "T" :date "1970-01-01T00:00:01Z")))
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
                                         :title "T" :date "1970-01-01T00:00:01Z")))
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

(ert-deftest synaxis-scrape-test-expand-content-surfaces-fetch-error ()
  "Bad article fetch messages and still completes expand without abort."
  (let* ((good (list :link "https://example.com/ok"
                     :title "ok" :date "1970-01-01T00:00:01Z"))
         (bad (list :link "https://example.com/bad"
                    :title "bad" :date "1970-01-01T00:00:01Z"))
         out msgs)
    (cl-letf (((symbol-function 'url-queue-retrieve)
               (lambda (url cb cbargs &rest _)
                 (if (string-match-p "/bad\\'" url)
                     (with-temp-buffer
                       (apply cb (list :error '(error http 404)) cbargs))
                   (with-current-buffer
                       (synaxis-scrape-tests--response-buffer
                        "<html><body><article><p>ok body</p></article></body></html>")
                     (apply cb nil cbargs)))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) msgs)
                 nil)))
      (synaxis-scrape--expand-content
       (list good bad)
       '(:url-selector "a" :content-selector "article")
       (lambda (e) (setq out e))))
    (should (= 2 (length out)))
    (let ((by-link (mapcar (lambda (e) (cons (plist-get e :link) e)) out)))
      (should (string-match-p "ok body"
                              (or (plist-get (cdr (assoc (plist-get good :link)
                                                         by-link))
                                             :content)
                                  "")))
      (should-not (plist-get (cdr (assoc (plist-get bad :link) by-link))
                             :content)))
    (should (cl-some (lambda (m)
                       (string-match-p "article expand failed.*bad" m))
                     msgs))))

(ert-deftest synaxis-scrape-test-expand-content-surfaces-parse-error ()
  "Apply/parse error messages and still completes expand without abort."
  (let* ((entries (list (list :link "https://example.com/p/1"
                              :title "T" :date "1970-01-01T00:00:01Z")
                        (list :link "https://example.com/p/2"
                              :title "U" :date "1970-01-01T00:00:01Z")))
         out msgs)
    (cl-letf (((symbol-function 'url-queue-retrieve)
               (lambda (_url cb cbargs &rest _)
                 (with-current-buffer
                     (synaxis-scrape-tests--response-buffer
                      "<html><body><article>x</article></body></html>")
                   (apply cb nil cbargs))))
              ((symbol-function 'synaxis-scrape--apply-content)
               (lambda (_buf _rules)
                 (error "forced apply failure")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) msgs)
                 nil)))
      (synaxis-scrape--expand-content
       entries
       '(:url-selector "a" :content-selector "article")
       (lambda (e) (setq out e))))
    (should (= 2 (length out)))
    (should (cl-every (lambda (e) (null (plist-get e :content))) out))
    (should (>= (length (cl-remove-if-not
                         (lambda (m)
                           (string-match-p "article expand failed" m))
                         msgs))
                2))))

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
                                    synaxis-tests--fixtures-dir))
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

(ert-deftest synaxis-scrape-test-buffer-shares-layout-with-search ()
  "The test buffer reuses `synaxis-search--columns' for its format."
  (let* ((dir (make-temp-file "synaxis-scrape-cmd" t))
         (synaxis-db-file (expand-file-name "test.db" dir))
         (synaxis-testing t)
         (synaxis-db--connection nil)
         (display-buffer-alist '((".*" display-buffer-no-window)))
         (html (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "scrape-basic.html"
                                    synaxis-tests--fixtures-dir))
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
              (should (derived-mode-p 'synaxis-scrape-test-mode))
              (should-not (derived-mode-p 'synaxis-search-mode))
              ;; Same 5-column shape as the real list.
              (should (= 5 (length tabulated-list-format)))
              ;; Buffer-local store has the plist for each row.
              (should (= 3 (length synaxis-scrape-test--entries))))))
      (when (get-buffer "*synaxis-scrape-test*")
        (kill-buffer "*synaxis-scrape-test*"))
      (synaxis-db-close)
      (delete-directory dir t))))

(ert-deftest synaxis-scrape-test-show-renders-plist-without-db ()
  "RET on a row pops `*synaxis-show*' rendering from the buffer store."
  (let* ((dir (make-temp-file "synaxis-scrape-show" t))
         (synaxis-db-file (expand-file-name "test.db" dir))
         (synaxis-testing t)
         (synaxis-db--connection nil)
         (display-buffer-alist '((".*" display-buffer-no-window)))
         (html (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "scrape-basic.html"
                                    synaxis-tests--fixtures-dir))
                 (buffer-string))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'synaxis-scrape--fetch-html)
                     (lambda (_url) html)))
            (synaxis-scrape-test "https://example.com/blog/"
                                 :url-selector "h2.entry-title a"))
          (with-current-buffer "*synaxis-scrape-test*"
            (goto-char (point-min))
            (synaxis-scrape-test-show)
            (with-current-buffer "*synaxis-show*"
              (should (string-match-p
                       "First Post"
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))))
      (dolist (b '("*synaxis-scrape-test*" "*synaxis-show*"))
        (when (get-buffer b) (kill-buffer b)))
      (synaxis-db-close)
      (delete-directory dir t))))

(ert-deftest synaxis-scrape-test-db-commands-are-read-only ()
  "Former search-buffer DB commands are shadowed in scrape previews."
  (let* ((dir (make-temp-file "synaxis-scrape-keys" t))
         (synaxis-db-file (expand-file-name "test.db" dir))
         (synaxis-testing t)
         (synaxis-db--connection nil)
         (display-buffer-alist '((".*" display-buffer-no-window)))
         (html (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "scrape-basic.html"
                                    synaxis-tests--fixtures-dir))
                 (buffer-string))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'synaxis-scrape--fetch-html)
                     (lambda (_url) html)))
            (synaxis-scrape-test "https://example.com/blog/"
                                 :url-selector "h2.entry-title a"))
          (with-current-buffer "*synaxis-scrape-test*"
            (goto-char (point-min))
            (let ((entries (copy-tree synaxis-scrape-test--entries t))
                  (rows (copy-tree tabulated-list-entries t)))
              (dolist (key '("g" "l" "r" "R" "t" ";" "A" "D" "E" "u"))
                (let ((command (key-binding (kbd key))))
                  (should (eq command #'synaxis-scrape-test--read-only-command))
                  (should-error (command-execute command) :type 'user-error)))
              (should (equal entries synaxis-scrape-test--entries))
              (should (equal rows tabulated-list-entries))))
          (should (zerop (caar (sqlite-select (synaxis-db--ensure-open)
                                              "SELECT COUNT(*) FROM entries;")))))
      (when (get-buffer "*synaxis-scrape-test*")
        (kill-buffer "*synaxis-scrape-test*"))
      (synaxis-db-close)
      (delete-directory dir t))))

(ert-deftest synaxis-scrape-test-browse-and-copy-use-preview-store ()
  "Browse/copy URL read the scrape preview row, not the database."
  (let* ((dir (make-temp-file "synaxis-scrape-url" t))
         (synaxis-db-file (expand-file-name "test.db" dir))
         (synaxis-testing t)
         (synaxis-db--connection nil)
         (display-buffer-alist '((".*" display-buffer-no-window)))
         (kill-ring nil)
         browsed
         (html (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "scrape-basic.html"
                                    synaxis-tests--fixtures-dir))
                 (buffer-string))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'synaxis-scrape--fetch-html)
                     (lambda (_url) html)))
            (synaxis-scrape-test "https://example.com/blog/"
                                 :url-selector "h2.entry-title a"))
          (with-current-buffer "*synaxis-scrape-test*"
            (goto-char (point-min))
            (cl-letf (((symbol-function 'browse-url)
                       (lambda (url &rest _args) (setq browsed url)))
                      ((symbol-function 'synaxis-db-get-entry)
                       (lambda (&rest _) (error "unexpected DB lookup"))))
              (synaxis-search-copy-link)
              (synaxis-search-browse-entry)))
          (should (equal "https://example.com/post/1" (current-kill 0 t)))
          (should (equal "https://example.com/post/1" browsed)))
      (when (get-buffer "*synaxis-scrape-test*")
        (kill-buffer "*synaxis-scrape-test*"))
      (synaxis-db-close)
      (delete-directory dir t))))

(ert-deftest synaxis-scrape-test-fetch-html-nil-buffer-user-error ()
  "Nil `url-retrieve-synchronously' raises `user-error', not a raw error."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _) nil)))
    (should-error (synaxis-scrape--fetch-html "https://example.invalid/x")
                  :type 'user-error)))

(ert-deftest synaxis-scrape-test-command-nil-fetch-user-error ()
  "`synaxis-scrape-test' inherits clear user-error on failed retrieve."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _) nil)))
    (should-error (synaxis-scrape-test "https://example.invalid/x"
                                       :url-selector "a")
                  :type 'user-error)))

(ert-deftest synaxis-scrape-test-date-format-applied ()
  "`:date-format' parses non-ISO date text on the index node."
  (let* ((html "<html><body>
<div class=\"item\"><a href=\"/p/1\">T</a><span class=\"d\">15/03/2024</span></div>
</body></html>")
         (entries (synaxis-scrape--extract
                   html "https://example.com/"
                   '(:url-selector ".item"
                     :date-selector ".d"
                     :date-format "%d/%m/%Y"))))
    (should (= 1 (length entries)))
    (should (equal "2024-03-15T00:00:00Z"
                   (plist-get (car entries) :date)))))

(provide 'synaxis-scrape-tests)
;;; synaxis-scrape-tests.el ends here
