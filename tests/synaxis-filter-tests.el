;;; synaxis-filter-tests.el --- Tests for synaxis-filter  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-filter'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-filter.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Parser

(ert-deftest synaxis-filter-test-parse-empty-string ()
  (should (equal nil (synaxis-filter-parse "")))
  (should (equal nil (synaxis-filter-parse "   "))))

(ert-deftest synaxis-filter-test-parse-plus-tag ()
  (should (equal '((plus . "unread")) (synaxis-filter-parse "+unread"))))

(ert-deftest synaxis-filter-test-parse-minus-tag ()
  (should (equal '((minus . "later")) (synaxis-filter-parse "-later"))))

(ert-deftest synaxis-filter-test-parse-since-7d ()
  (let* ((toks (synaxis-filter-parse "@7d"))
         (since (cdr (assq 'since toks))))
    (should (= 1 (length toks)))
    (should (= 604800.0 since))))

(ert-deftest synaxis-filter-test-parse-feed-substring ()
  (should (equal '((feed . "tech")) (synaxis-filter-parse "=tech"))))

(ert-deftest synaxis-filter-test-parse-limit ()
  (should (equal '((limit . 50)) (synaxis-filter-parse "#50"))))

(ert-deftest synaxis-filter-test-parse-free-text-multiple-words ()
  (should (equal '((text . "foo") (text . "bar"))
                 (synaxis-filter-parse "foo bar"))))

(ert-deftest synaxis-filter-test-parse-mixed ()
  (let ((toks (synaxis-filter-parse "+unread -later @7d =tech foo")))
    (should (equal 'plus  (car (nth 0 toks))))
    (should (equal 'minus (car (nth 1 toks))))
    (should (equal 'since (car (nth 2 toks))))
    (should (equal 'feed  (car (nth 3 toks))))
    (should (equal 'text  (car (nth 4 toks))))))

(ert-deftest synaxis-filter-test-parse-duration-units ()
  (should (= 30      (synaxis-filter-parse-duration "30s")))
  (should (= 300     (synaxis-filter-parse-duration "5m")))
  (should (= 7200    (synaxis-filter-parse-duration "2h")))
  (should (= 86400   (synaxis-filter-parse-duration "1d")))
  (should (= 604800  (synaxis-filter-parse-duration "1w")))
  (should (= 15552000 (synaxis-filter-parse-duration "6months")))
  (should (= 31536000 (synaxis-filter-parse-duration "1y"))))

;;; Compiler

(ert-deftest synaxis-filter-test-compile-empty-returns-tautology ()
  (let ((c (synaxis-filter-compile nil)))
    (should (equal "1=1" (plist-get c :where)))
    (should (null (plist-get c :params)))
    (should (null (plist-get c :limit)))))

(ert-deftest synaxis-filter-test-compile-single-plus-tag ()
  (let ((c (synaxis-filter-compile '((plus . "unread")))))
    (should (string-match-p "EXISTS" (plist-get c :where)))
    (should (equal '("unread") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-minus-tag-uses-not-exists ()
  (let ((c (synaxis-filter-compile '((minus . "later")))))
    (should (string-match-p "NOT EXISTS" (plist-get c :where)))
    (should (equal '("later") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-feed-uses-like ()
  (let ((c (synaxis-filter-compile '((feed . "tech")))))
    (should (string-match-p "LIKE" (plist-get c :where)))
    (should (equal '("%tech%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-text-binds-twice ()
  (let ((c (synaxis-filter-compile '((text . "foo")))))
    (should (equal '("%foo%" "%foo%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-limit-set ()
  (let ((c (synaxis-filter-compile '((limit . 25)))))
    (should (= 25 (plist-get c :limit)))))

(ert-deftest synaxis-filter-test-compile-mixed-and-joins-everything ()
  (let* ((toks (synaxis-filter-parse "+unread -later =tech foo #10"))
         (c (synaxis-filter-compile toks)))
    (should (string-match-p " AND " (plist-get c :where)))
    (should (= 10 (plist-get c :limit)))
    ;; +unread -later =tech text("foo")×2: 5 params total.
    (should (= 5 (length (plist-get c :params))))))

(provide 'synaxis-filter-tests)
;;; synaxis-filter-tests.el ends here
