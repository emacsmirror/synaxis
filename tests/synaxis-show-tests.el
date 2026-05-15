;;; synaxis-show-tests.el --- Tests for synaxis-show  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-show'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-show.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defmacro synaxis-show-tests--with-tmp (&rest body)
  "Run BODY with a fresh DB and a no-op display-buffer."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "synaxis-show-test" t))
          (synaxis-db-file (expand-file-name "test.db" dir))
          (synaxis-testing t)
          (synaxis-db--connection nil)
          (display-buffer-alist '((".*" display-buffer-no-window))))
     (unwind-protect
         (progn ,@body)
       (synaxis-db-close)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

(ert-deftest synaxis-show-test-render-shr-inserts-title-and-content ()
  (with-temp-buffer
    (synaxis-show--render-shr
     '(:title "Hello"
              :feed-title "F"
              :date 1704164645.0
              :content "<p>Body text.</p>"
              :content-type "html"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Hello" text))
      (should (string-match-p "Body text" text)))))

(ert-deftest synaxis-show-test-render-handles-text-content-type ()
  (with-temp-buffer
    (synaxis-show--render-shr
     '(:title "T"
              :feed-title "F"
              :date 1.0
              :content "plain content"
              :content-type "text"))
    (should (string-match-p "plain content"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest synaxis-show-test-render-handles-missing-content ()
  (with-temp-buffer
    (synaxis-show--render-shr
     '(:title "T" :feed-title "F" :date 1.0 :content nil :content-type nil))
    (should (string-match-p "T"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest synaxis-show-test-show-entry-marks-read ()
  (synaxis-show-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/x")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/x" :source-id "1"
                          :title "T" :date 1.0 :content "<p>x</p>"
                          :content-type "html"))))
     (synaxis-db-add-tag id "unread")
     (should (member "unread" (synaxis-db-get-tags id)))
     (synaxis-show-entry id)
     (should-not (member "unread" (synaxis-db-get-tags id))))))

(provide 'synaxis-show-tests)
;;; synaxis-show-tests.el ends here
