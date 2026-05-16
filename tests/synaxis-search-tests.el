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
     (should (string-match-p "Hello world" (aref cols 4))))))

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
                             (substring-no-properties (aref (cadr e) 4)))
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

(ert-deftest synaxis-search-test-entry-columns-renders-tags-excluding-unread ()
  "Tags cell shows other tags but omits `unread'."
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/tg" "1" "T" 1.0 t)))
     (synaxis-db-add-tag id "starred")
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (let* ((row (cadr (car tabulated-list-entries)))
              (tags-cell (substring-no-properties (aref row 3))))
         (should (string-match-p "starred" tags-cell))
         (should-not (string-match-p "unread" tags-cell)))))))

(ert-deftest synaxis-search-test-entry-columns-applies-tag-face ()
  "Tag face from the registry is applied to the rendered tag."
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/face" "1" "T" 1.0 nil)))
     (synaxis-db-add-tag id "highlight")
     (synaxis-db-set-tag-face "highlight" 'warning)
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (let* ((row (cadr (car tabulated-list-entries)))
              (cell (aref row 3))
              (idx (string-match "highlight" cell))
              (face (get-text-property idx 'face cell)))
         (should idx)
         (should (eq face 'warning)))))))

(ert-deftest synaxis-search-test-refresh-rebuilds-tag-face-cache ()
  "Setting a tag face and refreshing picks up the change.
Before set-tag-face the cell uses `synaxis-search-tag-face';
after, it uses the registry override."
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/r" "1" "T" 1.0 nil)))
     (synaxis-db-add-tag id "wip")
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (let* ((row (cadr (car tabulated-list-entries)))
              (cell (aref row 3))
              (idx (string-match "wip" cell)))
         (should idx)
         (should (eq 'synaxis-search-tag-face
                     (get-text-property idx 'face cell))))
       (synaxis-db-set-tag-face "wip" 'success)
       (synaxis-search-refresh)
       (let* ((row (cadr (car tabulated-list-entries)))
              (cell (aref row 3))
              (idx (string-match "wip" cell)))
         (should idx)
         (should (eq 'success (get-text-property idx 'face cell))))))))

(ert-deftest synaxis-search-test-edit-tags-adds-with-plus-prefix ()
  (synaxis-search-tests--with-tmp
    (let ((id (synaxis-search-tests--add-entry
               "https://example.com/e" "1" "T" 1.0 nil)))
      (let ((synaxis-search-default-filter ""))
        (synaxis-search))
      (with-current-buffer "*synaxis*"
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) (list "+rust"))))
          (call-interactively 'synaxis-search-edit-tags))
        (should (member "rust" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-edit-tags-removes-with-minus-prefix ()
  (synaxis-search-tests--with-tmp
    (let ((id (synaxis-search-tests--add-entry
               "https://example.com/e" "1" "T" 1.0 t)))
      (synaxis-db-add-tag id "starred")
      (let ((synaxis-search-default-filter ""))
        (synaxis-search))
      (with-current-buffer "*synaxis*"
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) (list "-starred"))))
          (call-interactively 'synaxis-search-edit-tags))
        (should-not (member "starred" (synaxis-db-get-tags id)))
        (should (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-edit-tags-mixed-add-and-remove ()
  (synaxis-search-tests--with-tmp
    (let ((id (synaxis-search-tests--add-entry
               "https://example.com/e" "1" "T" 1.0 t)))
      (let ((synaxis-search-default-filter ""))
        (synaxis-search))
      (with-current-buffer "*synaxis*"
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) (list "+rust" "-unread" "+hot"))))
          (call-interactively 'synaxis-search-edit-tags))
        (let ((tags (synaxis-db-get-tags id)))
          (should     (member "rust" tags))
          (should     (member "hot"  tags))
          (should-not (member "unread" tags)))))))

(ert-deftest synaxis-search-test-edit-tags-bare-name-adds ()
  "A bare typed name (no `+`/`-`) is treated as add."
  (synaxis-search-tests--with-tmp
    (let ((id (synaxis-search-tests--add-entry
               "https://example.com/e" "1" "T" 1.0 nil)))
      (let ((synaxis-search-default-filter ""))
        (synaxis-search))
      (with-current-buffer "*synaxis*"
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) (list "fresh-tag"))))
          (call-interactively 'synaxis-search-edit-tags))
        (should (member "fresh-tag" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-edit-tags-empty-input-noop ()
  (synaxis-search-tests--with-tmp
    (let ((id (synaxis-search-tests--add-entry
               "https://example.com/e" "1" "T" 1.0 t)))
      (let ((synaxis-search-default-filter ""))
        (synaxis-search))
      (with-current-buffer "*synaxis*"
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) nil)))
          (call-interactively 'synaxis-search-edit-tags))
        (should (equal '("unread") (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-tag-candidates-marks-current-with-minus ()
  (synaxis-search-tests--with-tmp
    (let ((id (synaxis-search-tests--add-entry
               "https://example.com/c" "1" "T" 1.0 t)))
      (synaxis-db-add-tag id "alpha")
      ;; Register an unattached tag too.
      (let ((db (synaxis-db--ensure-open)))
        (sqlite-execute db "INSERT INTO tags (tag) VALUES ('beta');"))
      (let ((cands (synaxis-search--tag-candidates id)))
        (should (member "-alpha"  cands))
        (should (member "-unread" cands))
        (should (member "+beta"   cands))))))

(ert-deftest synaxis-search-test-edit-feed-picks-row-url ()
  "`synaxis-search-edit-feed' passes the row's feed-url to the editor."
  (synaxis-search-tests--with-tmp
   (synaxis-search-tests--add-entry
    "https://example.com/ef" "1" "T" 1.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (goto-char (point-min))
     (let (captured)
       (cl-letf (((symbol-function 'synaxis-edit-feed)
                  (lambda (&optional url) (setq captured url))))
         (call-interactively 'synaxis-search-edit-feed))
       (should (equal "https://example.com/ef" captured))))))

(provide 'synaxis-search-tests)
;;; synaxis-search-tests.el ends here
