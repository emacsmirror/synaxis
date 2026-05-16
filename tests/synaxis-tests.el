;;; synaxis-tests.el --- Tests for synaxis  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the top-level glue in `synaxis.el'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defmacro synaxis-tests--with-tmp (&rest body)
  "Run BODY with a fresh DB, cleaned-up timer, and isolated interval."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "synaxis-test" t))
          (synaxis-db-file (expand-file-name "test.db" dir))
          (synaxis-testing t)
          (synaxis-db--connection nil)
          (synaxis-update-interval nil)
          (synaxis--update-timer nil))
     (unwind-protect
         (progn ,@body)
       (synaxis--update-cancel-timer)
       (synaxis-db-close)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

;;; Update timer

(ert-deftest synaxis-test-timer-not-set-when-interval-nil ()
  (synaxis-tests--with-tmp
   (let ((synaxis-update-interval nil)
         (synaxis-testing nil))
     (synaxis--update-maybe-start-timer)
     (should (null synaxis--update-timer)))))

(ert-deftest synaxis-test-timer-set-when-interval-positive ()
  (synaxis-tests--with-tmp
   (let ((synaxis-update-interval 3600)
         (synaxis-testing nil))
     (synaxis--update-maybe-start-timer)
     (should synaxis--update-timer)
     (synaxis--update-cancel-timer))))

(ert-deftest synaxis-test-timer-canceled-by-cancel-fn ()
  (synaxis-tests--with-tmp
   (let ((synaxis-update-interval 3600)
         (synaxis-testing nil))
     (synaxis--update-maybe-start-timer)
     (synaxis--update-cancel-timer)
     (should (null synaxis--update-timer)))))

(ert-deftest synaxis-test-timer-respects-synaxis-testing ()
  (synaxis-tests--with-tmp
   (let ((synaxis-update-interval 3600)
         (synaxis-testing t))
     (synaxis--update-maybe-start-timer)
     (should (null synaxis--update-timer)))))

(ert-deftest synaxis-test-timer-maybe-start-is-idempotent ()
  "Calling maybe-start twice doesn't leak a second timer."
  (synaxis-tests--with-tmp
   (let ((synaxis-update-interval 3600)
         (synaxis-testing nil))
     (synaxis--update-maybe-start-timer)
     (let ((first synaxis--update-timer))
       (synaxis--update-maybe-start-timer)
       (should-not (eq first synaxis--update-timer))
       (should synaxis--update-timer))
     (synaxis--update-cancel-timer))))

(ert-deftest synaxis-test-update-background-noop-without-feeds ()
  "Background update is silent and never calls fetch-all without feeds."
  (synaxis-tests--with-tmp
   (let ((calls 0))
     (cl-letf (((symbol-function 'synaxis-fetch-all)
                (lambda () (cl-incf calls))))
       (synaxis--update-background)
       (should (= 0 calls))))))

(ert-deftest synaxis-test-update-background-calls-fetch-when-feeds-exist ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/x")
   (let ((calls 0))
     (cl-letf (((symbol-function 'synaxis-fetch-all)
                (lambda () (cl-incf calls))))
       (synaxis--update-background)
       (should (= 1 calls))))))

;;; Tag rules

(defun synaxis-tests--seed-entry (feed-url source-id title date &optional unread)
  "Insert a feed + entry, optionally tagged unread.  Returns the entry id."
  (synaxis-db-add-feed feed-url '(:title "F"))
  (let ((id (synaxis-db-upsert-entry
             (list :feed-url feed-url :source-id source-id
                   :title title :date date))))
    (when unread (synaxis-db-add-tag id "unread"))
    id))

(ert-deftest synaxis-test-rules-apply-entry-adds-tag-when-filter-matches ()
  (synaxis-tests--with-tmp
   (let* ((id (synaxis-tests--seed-entry
               "https://example.com/x" "1" "Rust async runtime" 1.0 t))
          (synaxis-tag-rules
           '((:filter "title:rust" :add ("rust" "lang")))))
     (synaxis-tag-rules-apply-entry id)
     (let ((tags (synaxis-db-get-tags id)))
       (should (member "rust" tags))
       (should (member "lang" tags))))))

(ert-deftest synaxis-test-rules-apply-entry-skips-when-filter-does-not-match ()
  (synaxis-tests--with-tmp
   (let* ((id (synaxis-tests--seed-entry
               "https://example.com/x" "1" "Plain title" 1.0 t))
          (synaxis-tag-rules
           '((:filter "title:rust" :add ("rust")))))
     (synaxis-tag-rules-apply-entry id)
     (should-not (member "rust" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-test-rules-apply-entry-removes-tag ()
  (synaxis-tests--with-tmp
   (let* ((id (synaxis-tests--seed-entry
               "https://example.com/x" "1" "T" 1.0 t))
          (synaxis-tag-rules
           '((:filter "tag:unread" :remove ("unread")))))
     (synaxis-tag-rules-apply-entry id)
     (should-not (member "unread" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-test-rules-apply-entry-add-and-remove-in-same-rule ()
  (synaxis-tests--with-tmp
   (let* ((id (synaxis-tests--seed-entry
               "https://example.com/x" "1" "rust topic" 1.0 t))
          (synaxis-tag-rules
           '((:filter "title:rust" :add ("rust") :remove ("unread")))))
     (synaxis-tag-rules-apply-entry id)
     (let ((tags (synaxis-db-get-tags id)))
       (should (member "rust" tags))
       (should-not (member "unread" tags))))))

(ert-deftest synaxis-test-rules-apply-entry-empty-rule-noop ()
  (synaxis-tests--with-tmp
   (let* ((id (synaxis-tests--seed-entry
               "https://example.com/x" "1" "T" 1.0 t))
          (synaxis-tag-rules '((:filter "title:T"))))   ;; no :add, no :remove
     (synaxis-tag-rules-apply-entry id)
     (should (equal '("unread") (synaxis-db-get-tags id))))))

(ert-deftest synaxis-test-rules-apply-all-applies-across-db ()
  (synaxis-tests--with-tmp
   (let ((id1 (synaxis-tests--seed-entry
               "https://example.com/x" "1" "Rust 101" 1.0 t))
         (id2 (synaxis-tests--seed-entry
               "https://example.com/x" "2" "Lua 101" 2.0 t)))
     (let ((synaxis-tag-rules
            '((:filter "title:rust" :add ("rust")))))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
         (call-interactively 'synaxis-tag-rules-apply-all)))
     (should     (member "rust" (synaxis-db-get-tags id1)))
     (should-not (member "rust" (synaxis-db-get-tags id2))))))

(ert-deftest synaxis-test-rules-apply-all-errors-when-no-rules ()
  (synaxis-tests--with-tmp
   (let ((synaxis-tag-rules nil))
     (should-error (call-interactively 'synaxis-tag-rules-apply-all)
                   :type 'user-error))))

(ert-deftest synaxis-test-rules-apply-all-respects-cancel ()
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
              "https://example.com/x" "1" "Rust" 1.0 t)))
     (let ((synaxis-tag-rules
            '((:filter "title:rust" :add ("rust")))))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
         (call-interactively 'synaxis-tag-rules-apply-all)))
     (should-not (member "rust" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-test-rules-wires-into-new-entry-hook ()
  (should (memq #'synaxis-tag-rules-apply-entry synaxis-new-entry-hook)))

(provide 'synaxis-tests)
;;; synaxis-tests.el ends here
