;;; synaxis-filter-tests.el --- Tests for synaxis-filter  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-filter'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-filter.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Parse

(ert-deftest synaxis-filter-test-parse-empty ()
  (should (null (synaxis-filter-parse "")))
  (should (null (synaxis-filter-parse "   "))))

(ert-deftest synaxis-filter-test-parse-tag-and-not-tag ()
  (should (equal '((tag . "unread")) (synaxis-filter-parse "tag:unread")))
  (should (equal '((not-tag . "later")) (synaxis-filter-parse "-tag:later"))))

(ert-deftest synaxis-filter-test-parse-feed-and-not-feed ()
  (should (equal '((feed . "hackaday")) (synaxis-filter-parse "feed:hackaday")))
  (should (equal '((not-feed . "yt")) (synaxis-filter-parse "-feed:yt"))))

(ert-deftest synaxis-filter-test-parse-title-and-not-title ()
  (should (equal '((title . "rust")) (synaxis-filter-parse "title:rust")))
  (should (equal '((not-title . "draft")) (synaxis-filter-parse "-title:draft"))))

(ert-deftest synaxis-filter-test-parse-content-and-not-content ()
  (should (equal '((content . "openssh")) (synaxis-filter-parse "content:openssh")))
  (should (equal '((not-content . "spam")) (synaxis-filter-parse "-content:spam"))))

(ert-deftest synaxis-filter-test-parse-date-keyword ()
  (let ((toks (synaxis-filter-parse "date:today")))
    (should (= 1 (length toks)))
    (should (eq 'date (car (car toks))))
    (should (numberp (plist-get (cdr (car toks)) :from)))
    (should (numberp (plist-get (cdr (car toks)) :to)))))

(ert-deftest synaxis-filter-test-parse-date-relative ()
  (let ((toks (synaxis-filter-parse "date:7d")))
    (should (eq 'date (car (car toks))))
    (should (numberp (plist-get (cdr (car toks)) :from)))
    (should (numberp (plist-get (cdr (car toks)) :to)))))

(ert-deftest synaxis-filter-test-parse-date-absolute ()
  (let* ((toks (synaxis-filter-parse "date:2024-03-15"))
         (spec (cdr (car toks))))
    (should (eq 'date (car (car toks))))
    (should (= 86400 (- (plist-get spec :to) (plist-get spec :from))))))

(ert-deftest synaxis-filter-test-parse-limit ()
  (should (equal '((limit . 50)) (synaxis-filter-parse "limit:50"))))

(ert-deftest synaxis-filter-test-parse-bare-word-becomes-text ()
  (should (equal '((text . "rust")) (synaxis-filter-parse "rust"))))

(ert-deftest synaxis-filter-test-parse-multiple-bare-words ()
  (should (equal '((text . "foo") (text . "bar"))
                 (synaxis-filter-parse "foo bar"))))

(ert-deftest synaxis-filter-test-parse-mixed ()
  (let ((toks (synaxis-filter-parse "tag:unread -feed:yt title:rust limit:25 foo")))
    (should (= 5 (length toks)))
    (should (eq 'tag (car (nth 0 toks))))
    (should (eq 'not-feed (car (nth 1 toks))))
    (should (eq 'title (car (nth 2 toks))))
    (should (eq 'limit (car (nth 3 toks))))
    (should (eq 'text (car (nth 4 toks))))))

(ert-deftest synaxis-filter-test-parse-unknown-prefix-dropped ()
  (should (null (synaxis-filter-parse "weirdkey:value"))))

(ert-deftest synaxis-filter-test-parse-not-bare-word-dropped ()
  (should (null (synaxis-filter-parse "-rust"))))

(ert-deftest synaxis-filter-test-parse-not-date-dropped ()
  "Negating a date is meaningless; the token is dropped."
  (should (null (synaxis-filter-parse "-date:today"))))

(ert-deftest synaxis-filter-test-parse-not-limit-dropped ()
  (should (null (synaxis-filter-parse "-limit:10"))))

;;; Compile

(ert-deftest synaxis-filter-test-compile-empty-tautology ()
  (let ((c (synaxis-filter-compile nil)))
    (should (equal "1=1" (plist-get c :where)))
    (should (null (plist-get c :params)))
    (should (null (plist-get c :limit)))))

(ert-deftest synaxis-filter-test-compile-tag-uses-exists ()
  (let ((c (synaxis-filter-compile '((tag . "unread")))))
    (should (string-match-p "\\bEXISTS\\b" (plist-get c :where)))
    (should-not (string-match-p "NOT EXISTS" (plist-get c :where)))
    (should (equal '("unread") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-not-tag-uses-not-exists ()
  (let ((c (synaxis-filter-compile '((not-tag . "later")))))
    (should (string-match-p "NOT EXISTS" (plist-get c :where)))
    (should (equal '("later") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-feed-uses-like ()
  (let ((c (synaxis-filter-compile '((feed . "hackaday")))))
    (should (string-match-p "f\\.title LIKE" (plist-get c :where)))
    (should (equal '("%hackaday%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-not-feed-uses-not-like ()
  (let ((c (synaxis-filter-compile '((not-feed . "yt")))))
    (should (string-match-p "f\\.title NOT LIKE" (plist-get c :where)))))

(ert-deftest synaxis-filter-test-compile-title-uses-like ()
  (let ((c (synaxis-filter-compile '((title . "rust")))))
    (should (string-match-p "e\\.title LIKE" (plist-get c :where)))
    (should (equal '("%rust%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-content-uses-like ()
  (let ((c (synaxis-filter-compile '((content . "ssh")))))
    (should (string-match-p "e\\.content LIKE" (plist-get c :where)))
    (should (equal '("%ssh%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-text-binds-twice ()
  (let ((c (synaxis-filter-compile '((text . "foo")))))
    (should (equal '("%foo%" "%foo%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-emits-bounds-both ()
  (let* ((spec '(:from 1000.0 :to 2000.0))
         (c (synaxis-filter-compile (list (cons 'date spec)))))
    (should (string-match-p "e\\.date >= \\?" (plist-get c :where)))
    (should (string-match-p "e\\.date < \\?"  (plist-get c :where)))
    (should (equal '(1000.0 2000.0) (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-from-only ()
  (let* ((spec '(:from 1000.0 :to nil))
         (c (synaxis-filter-compile (list (cons 'date spec)))))
    (should (string-match-p "e\\.date >= \\?" (plist-get c :where)))
    (should-not (string-match-p "e\\.date < " (plist-get c :where)))
    (should (equal '(1000.0) (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-to-only ()
  (let* ((spec '(:from nil :to 2000.0))
         (c (synaxis-filter-compile (list (cons 'date spec)))))
    (should-not (string-match-p "e\\.date >= " (plist-get c :where)))
    (should (string-match-p "e\\.date < \\?" (plist-get c :where)))
    (should (equal '(2000.0) (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-limit-sets-plist-key ()
  (should (= 25 (plist-get (synaxis-filter-compile '((limit . 25))) :limit))))

(ert-deftest synaxis-filter-test-compile-multiple-tags-ANDed ()
  (let ((c (synaxis-filter-compile '((tag . "a") (tag . "b")))))
    (should (string-match-p " AND " (plist-get c :where)))
    (should (equal '("a" "b") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-mixed-everything ()
  (let* ((toks (synaxis-filter-parse "tag:unread -feed:yt title:rust limit:10 foo"))
         (c    (synaxis-filter-compile toks)))
    (should (= 10 (plist-get c :limit)))
    ;; tag(1) + not-feed(1) + title(1) + text(2) = 5 params
    (should (= 5 (length (plist-get c :params))))))

;;; Date spec

(ert-deftest synaxis-filter-test-date-spec-today ()
  (let ((spec (synaxis-filter-parse-date-spec "today")))
    (should (numberp (plist-get spec :from)))
    (should (numberp (plist-get spec :to)))
    (should (= 86400 (- (plist-get spec :to) (plist-get spec :from))))))

(ert-deftest synaxis-filter-test-date-spec-yesterday ()
  (let* ((y (synaxis-filter-parse-date-spec "yesterday"))
         (t1 (synaxis-filter-parse-date-spec "today")))
    (should (= 86400 (- (plist-get t1 :from) (plist-get y :from))))
    (should (= (plist-get y :to) (plist-get t1 :from)))))

(ert-deftest synaxis-filter-test-date-spec-7d ()
  (let ((spec (synaxis-filter-parse-date-spec "7d")))
    (should (numberp (plist-get spec :from)))
    (should (numberp (plist-get spec :to)))
    (should (< (abs (- 604800 (- (plist-get spec :to) (plist-get spec :from))))
               2.0))))

(ert-deftest synaxis-filter-test-date-spec-2024 ()
  (let ((spec (synaxis-filter-parse-date-spec "2024")))
    (should (= (float-time (encode-time 0 0 0 1 1 2024))
               (plist-get spec :from)))
    (should (= (float-time (encode-time 0 0 0 1 1 2025))
               (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-2024-03 ()
  (let ((spec (synaxis-filter-parse-date-spec "2024-03")))
    (should (= (float-time (encode-time 0 0 0 1 3 2024))
               (plist-get spec :from)))
    (should (= (float-time (encode-time 0 0 0 1 4 2024))
               (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-2024-03-15 ()
  (let ((spec (synaxis-filter-parse-date-spec "2024-03-15")))
    (should (= (float-time (encode-time 0 0 0 15 3 2024))
               (plist-get spec :from)))
    (should (= 86400 (- (plist-get spec :to) (plist-get spec :from))))))

(ert-deftest synaxis-filter-test-date-spec-december-rolls-year ()
  "Month-only spec for December rolls correctly to January of next year."
  (let ((spec (synaxis-filter-parse-date-spec "2024-12")))
    (should (= (float-time (encode-time 0 0 0 1 1 2025))
               (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-invalid-nil ()
  (should (null (synaxis-filter-parse-date-spec "garbage")))
  (should (null (synaxis-filter-parse-date-spec "")))
  (should (null (synaxis-filter-parse-date-spec "2024-99-99"))))

(provide 'synaxis-filter-tests)
;;; synaxis-filter-tests.el ends here
