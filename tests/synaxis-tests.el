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

(provide 'synaxis-tests)
;;; synaxis-tests.el ends here
