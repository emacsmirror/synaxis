;;; synaxis-search-tests.el --- Tests for synaxis-search  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-search'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-search.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defmacro synaxis-search-tests--with-tmp (&rest body)
  "Run BODY with a fresh DB and a no-window display buffer."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "synaxis-search-test" t))
          (synaxis-db-file (expand-file-name "test.db" dir))
          (synaxis-testing t)
          (synaxis-db--connection nil)
          (display-buffer-alist '((".*" display-buffer-no-window))))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer "*synaxis*") (kill-buffer "*synaxis*"))
       (synaxis-db-close)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

(defun synaxis-search-tests--add-entry (feed-url source-id title date &optional unread)
  "Insert one entry and optionally tag it unread."
  (synaxis-db-add-feed feed-url '(:title "F"))
  (let ((id (synaxis-db-upsert-entry
             (list :feed-url feed-url :source-id source-id
                   :title title :date date))))
    (when unread (synaxis-db-add-tag id "unread"))
    id))

(ert-deftest synaxis-search-test-entry-columns-include-date-and-title ()
  (synaxis-search-tests--with-tmp
   (let* ((id (synaxis-search-tests--add-entry
               "https://example.com/x" "1" "Hello world" 1704164645.0 t))
          (entry (synaxis-db-get-entry id))
          (cols (synaxis-search--entry-columns entry)))
     (should (string-match-p "2024" (aref cols 0)))
     (should (equal "*" (aref cols 1)))
     (should (string-match-p "Hello world" (aref cols 3))))))

(ert-deftest synaxis-search-test-refresh-populates-tabulated-list ()
  (synaxis-search-tests--with-tmp
   (synaxis-search-tests--add-entry "https://example.com/x" "1" "A" 1.0 t)
   (synaxis-search-tests--add-entry "https://example.com/x" "2" "B" 2.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (should (= 2 (length tabulated-list-entries))))))

(ert-deftest synaxis-search-test-current-entry-returns-id-at-point ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "Only" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (should (equal id (synaxis-search-current-entry)))))))

(ert-deftest synaxis-search-test-toggle-read-removes-unread-tag ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "T" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (should (member "unread" (synaxis-db-get-tags id)))
       (synaxis-search-toggle-read)
       (should-not (member "unread" (synaxis-db-get-tags id)))
       (synaxis-search-toggle-read)
       (should (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-set-filter-changes-buffer-local ()
  (synaxis-search-tests--with-tmp
   (synaxis-search-tests--add-entry "https://example.com/x" "1" "T" 1.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (synaxis-search-set-filter "tag:unread")
     (should (equal "tag:unread" synaxis-search--filter)))))

(ert-deftest synaxis-search-test-default-filter-shows-unread-only ()
  (synaxis-search-tests--with-tmp
   (synaxis-search-tests--add-entry "https://example.com/x" "1" "Unread" 2.0 t)
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "2" "Read" 1.0 t)))
     (synaxis-db-remove-tag id "unread"))
   (synaxis-search)
   (with-current-buffer "*synaxis*"
     (let ((titles (mapcar (lambda (e)
                             (substring-no-properties (aref (cadr e) 3)))
                           tabulated-list-entries)))
       (should (equal '("Unread") titles))))))

(ert-deftest synaxis-search-test-replace-entry-updates-row-in-place ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "T" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       ;; Marker column reflects unread state.
       (should (equal "*" (aref (tabulated-list-get-entry) 1)))
       (synaxis-db-remove-tag id "unread")
       (synaxis-search--redraw-current)
       (goto-char (point-min))
       (should (equal " " (aref (tabulated-list-get-entry) 1)))))))

(ert-deftest synaxis-search-test-set-filter-via-completing-read-multiple ()
  "Calling `synaxis-search-set-filter' interactively pulls from CRM."
  (synaxis-search-tests--with-tmp
   (synaxis-search-tests--add-entry "https://example.com/x" "1" "T" 1.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (cl-letf (((symbol-function 'completing-read-multiple)
                (lambda (&rest _) (list "tag:starred" "feed:hackaday"))))
       (call-interactively 'synaxis-search-set-filter))
     (should (equal "tag:starred feed:hackaday" synaxis-search--filter)))))

(ert-deftest synaxis-search-test-tag-entry-uses-completing-read ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "T" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) "starred")))
         (call-interactively 'synaxis-search-tag-entry))
       (should (member "starred" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-untag-entry-uses-completing-read ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "T" 1.0 t)))
     (synaxis-db-add-tag id "starred")
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) "starred")))
         (call-interactively 'synaxis-search-untag-entry))
       (should-not (member "starred" (synaxis-db-get-tags id)))
       (should (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-untag-entry-errors-when-no-tags ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "T" 1.0 nil)))
     (ignore id)
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (should-error (call-interactively 'synaxis-search-untag-entry)
                     :type 'user-error)))))

(ert-deftest synaxis-search-test-mark-all-read-marks-visible-entries ()
  (synaxis-search-tests--with-tmp
   (let ((id1 (synaxis-search-tests--add-entry
               "https://example.com/x" "1" "A" 1.0 t))
         (id2 (synaxis-search-tests--add-entry
               "https://example.com/x" "2" "B" 2.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
         (call-interactively 'synaxis-search-mark-all-read))
       (should-not (member "unread" (synaxis-db-get-tags id1)))
       (should-not (member "unread" (synaxis-db-get-tags id2)))))))

(ert-deftest synaxis-search-test-mark-all-read-respects-cancel ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "T" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
         (call-interactively 'synaxis-search-mark-all-read))
       (should (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-mark-all-read-errors-when-empty ()
  (synaxis-search-tests--with-tmp
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (should-error (call-interactively 'synaxis-search-mark-all-read)
                   :type 'user-error))))

(provide 'synaxis-search-tests)
;;; synaxis-search-tests.el ends here
