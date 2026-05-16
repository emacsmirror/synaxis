;;; synaxis-edit-tests.el --- Tests for synaxis-edit  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-edit'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-edit.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defmacro synaxis-edit-tests--with-tmp (&rest body)
  "Run BODY with a fresh DB and a registered scrape feed."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "synaxis-edit-test" t))
          (synaxis-db-file (expand-file-name "test.db" dir))
          (synaxis-testing t)
          (synaxis-db--connection nil)
          (synaxis-edit--current-url "https://example.com/sc"))
     (unwind-protect
         (progn
           (synaxis-db-add-feed "https://example.com/sc"
                                '(:title "Test Feed"
                                         :type "scrape"
                                         :meta (:autotags ["alpha" "beta"])))
           (synaxis-db-add-scrape-rule
            "https://example.com/sc"
            '(:url-selector "h2 a"
                            :content-selector "article"
                            :title-cleanup " - Site"))
           ,@body)
       (synaxis-db-close)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

;;; Predicates

(ert-deftest synaxis-edit-test-scrape-feed-p-true-for-scrape ()
  (synaxis-edit-tests--with-tmp
   (should (synaxis-edit--scrape-feed-p))))

(ert-deftest synaxis-edit-test-scrape-feed-p-false-for-rss ()
  (synaxis-edit-tests--with-tmp
   (let ((synaxis-edit--current-url "https://example.com/rss"))
     (synaxis-db-add-feed "https://example.com/rss" '(:type "rss"))
     (should-not (synaxis-edit--scrape-feed-p)))))

(ert-deftest synaxis-edit-test-has-content-selector-p ()
  (synaxis-edit-tests--with-tmp
   (should (synaxis-edit--has-content-selector-p))
   (synaxis-db-update-scrape-rule-field
    "https://example.com/sc" :content-selector nil)
   (should-not (synaxis-edit--has-content-selector-p))))

;;; Formatters

(ert-deftest synaxis-edit-test-field-line-formats-value ()
  (let ((line (synaxis-edit--field-line "X" "hello")))
    (should (string-match-p "X" line))
    (should (string-match-p "hello" line))))

(ert-deftest synaxis-edit-test-field-line-handles-nil ()
  (let ((line (synaxis-edit--field-line "X" nil)))
    (should (string-match-p "(unset)" line))))

(ert-deftest synaxis-edit-test-format-autotags ()
  (synaxis-edit-tests--with-tmp
   (should (equal "alpha beta" (synaxis-edit--format-autotags)))))

;;; Setters

(ert-deftest synaxis-edit-test-set-url-selector-updates-rule ()
  (synaxis-edit-tests--with-tmp
   (cl-letf (((symbol-function 'read-string)
              (lambda (&rest _) "div.new"))
             ((symbol-function 'keymap-popup) (lambda (&rest _) nil)))
     (call-interactively 'synaxis-edit-set-url-selector))
   (should (equal "div.new"
                  (plist-get (synaxis-db-get-scrape-rule
                              "https://example.com/sc")
                             :url-selector)))))

(ert-deftest synaxis-edit-test-set-url-selector-empty-clears ()
  "Empty input clears the field to nil."
  (synaxis-edit-tests--with-tmp
   (cl-letf (((symbol-function 'read-string)
              (lambda (&rest _) ""))
             ((symbol-function 'keymap-popup) (lambda (&rest _) nil)))
     (call-interactively 'synaxis-edit-set-url-pattern))
   (should-not (plist-get (synaxis-db-get-scrape-rule
                           "https://example.com/sc")
                          :url-pattern))))

(ert-deftest synaxis-edit-test-set-title-overwrites ()
  (synaxis-edit-tests--with-tmp
   (cl-letf (((symbol-function 'read-string)
              (lambda (&rest _) "New Title"))
             ((symbol-function 'keymap-popup) (lambda (&rest _) nil)))
     (call-interactively 'synaxis-edit-set-title))
   (should (equal "New Title"
                  (plist-get (synaxis-db-get-feed
                              "https://example.com/sc")
                             :title)))))

(ert-deftest synaxis-edit-test-set-autotags-replaces ()
  (synaxis-edit-tests--with-tmp
   (cl-letf (((symbol-function 'completing-read-multiple)
              (lambda (&rest _) '("gamma" "delta")))
             ((symbol-function 'keymap-popup) (lambda (&rest _) nil)))
     (call-interactively 'synaxis-edit-set-autotags))
   (let* ((feed (synaxis-db-get-feed "https://example.com/sc"))
          (tags (append (plist-get (plist-get feed :meta) :autotags) nil)))
     (should (equal '("gamma" "delta") tags)))))

(ert-deftest synaxis-edit-test-set-limit-parses-integer ()
  (synaxis-edit-tests--with-tmp
   (cl-letf (((symbol-function 'read-string)
              (lambda (&rest _) "20"))
             ((symbol-function 'keymap-popup) (lambda (&rest _) nil)))
     (call-interactively 'synaxis-edit-set-limit))
   (should (eq 20 (plist-get (synaxis-db-get-scrape-rule
                              "https://example.com/sc")
                             :limit)))))

;;; Entry / quit

(ert-deftest synaxis-edit-test-quit-clears-state ()
  (synaxis-edit-tests--with-tmp
   (synaxis-edit-quit)
   (should-not synaxis-edit--current-url)))

(provide 'synaxis-edit-tests)
;;; synaxis-edit-tests.el ends here
