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

(ert-deftest synaxis-search-test-pp-formats-entry-line ()
  (synaxis-search-tests--with-tmp
   (let* ((id (synaxis-search-tests--add-entry
               "https://example.com/x" "1" "Hello world" 1704164645.0 t))
          (entry (synaxis-db-get-entry id)))
     (with-temp-buffer
       (synaxis-search--pp entry)
       (let ((text (buffer-substring-no-properties (point-min) (point-max))))
         (should (string-match-p "2024" text))
         (should (string-match-p "Hello world" text)))))))

(ert-deftest synaxis-search-test-refresh-populates-ewoc-from-db ()
  (synaxis-search-tests--with-tmp
   (synaxis-search-tests--add-entry "https://example.com/x" "1" "A" 1.0 t)
   (synaxis-search-tests--add-entry "https://example.com/x" "2" "B" 2.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (should (= 2 (length (ewoc-collect synaxis-search--ewoc #'identity)))))))

(ert-deftest synaxis-search-test-current-entry-returns-id-at-point ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "Only" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (ewoc-goto-node synaxis-search--ewoc
                       (ewoc-nth synaxis-search--ewoc 0))
       (should (equal id (synaxis-search-current-entry)))))))

(ert-deftest synaxis-search-test-toggle-read-removes-unread-tag ()
  (synaxis-search-tests--with-tmp
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "1" "T" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (ewoc-goto-node synaxis-search--ewoc
                       (ewoc-nth synaxis-search--ewoc 0))
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
     (synaxis-search-set-filter "+unread")
     (should (equal "+unread" synaxis-search--filter)))))

(ert-deftest synaxis-search-test-default-filter-shows-unread-only ()
  (synaxis-search-tests--with-tmp
   (synaxis-search-tests--add-entry "https://example.com/x" "1" "Unread" 2.0 t)
   (let ((id (synaxis-search-tests--add-entry
              "https://example.com/x" "2" "Read" 1.0 t)))
     (synaxis-db-remove-tag id "unread"))
   (synaxis-search)
   (with-current-buffer "*synaxis*"
     (let ((titles (mapcar (lambda (e) (plist-get e :title))
                           (ewoc-collect synaxis-search--ewoc #'identity))))
       (should (equal '("Unread") titles))))))

(provide 'synaxis-search-tests)
;;; synaxis-search-tests.el ends here
