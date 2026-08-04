;;; synaxis-search-tests.el --- Tests for synaxis-search  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-search'.

;;; Code:

(require 'ert)
(require 'bookmark)

(load (expand-file-name "../lisp/synaxis-search.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'synaxis-test-utils)

(ert-deftest synaxis-search-test-entry-columns-include-date-and-title ()
  (synaxis-tests--with-tmp
   (let* ((id (synaxis-tests--seed-entry
               "https://example.com/x" "1" "Hello world" 1704164645.0 t))
          (entry (synaxis-db-get-entry id))
          (cols (synaxis-search--entry-columns entry)))
     (should (string-match-p "2024" (aref cols 0)))
     (should (equal "*" (aref cols 1)))
     (should (string-match-p "Hello world" (aref cols 4))))))

(ert-deftest synaxis-search-test-refresh-populates-tabulated-list ()
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry "https://example.com/x" "1" "A" 1.0 t)
   (synaxis-tests--seed-entry "https://example.com/x" "2" "B" 2.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (should (= 2 (length tabulated-list-entries))))))

(ert-deftest synaxis-search-test-current-entry-returns-id-at-point ()
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
              "https://example.com/x" "1" "Only" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (should (equal id (synaxis-search-current-entry)))))))

(ert-deftest synaxis-search-test-toggle-read-removes-unread-tag ()
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry "https://example.com/x" "1" "T" 1.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (synaxis-search-set-filter "tag:unread")
     (should (equal "tag:unread" synaxis-search--filter)))))

(ert-deftest synaxis-search-test-default-filter-shows-unread-only ()
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry "https://example.com/x" "1" "Unread" 2.0 t)
   (let ((id (synaxis-tests--seed-entry
              "https://example.com/x" "2" "Read" 1.0 t)))
     (synaxis-db-remove-tag id "unread"))
   (synaxis-search)
   (with-current-buffer "*synaxis*"
     (let ((titles (mapcar (lambda (e)
                             (substring-no-properties (aref (cadr e) 4)))
                           tabulated-list-entries)))
       (should (equal '("Unread") titles))))))

(ert-deftest synaxis-search-test-replace-entry-updates-row-in-place ()
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
       (should (equal " " (aref (tabulated-list-get-entry) 1)))
       (should (equal " " (aref (cadr (car tabulated-list-entries)) 1)))))))

(ert-deftest synaxis-search-test-show-entry-reprints-without-refresh ()
  "Opening an unread row keeps it visible but clears the unread marker."
  (synaxis-tests--with-tmp
   (require 'synaxis-show)
   (let ((id (synaxis-tests--seed-entry
              "https://example.com/x" "1" "T" 1.0 t)))
     (synaxis-search)
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (should (equal id (tabulated-list-get-id)))
       (cl-letf (((symbol-function 'synaxis-show-entry)
                  (lambda (entry-id peers)
                    (should (equal id entry-id))
                    (should (equal (list id) peers))
                    (synaxis-db-remove-tag entry-id "unread")))
                 ((symbol-function 'synaxis-db-list-entries)
                  (lambda (&rest _) (error "unexpected refresh"))))
         (synaxis-search-show-entry))
       (goto-char (point-min))
       (should (equal id (tabulated-list-get-id)))
       (should (equal " " (aref (tabulated-list-get-entry) 1)))
       (should (equal " " (aref (cadr (car tabulated-list-entries)) 1)))))))

(ert-deftest synaxis-search-test-show-entry-preserves-non-first-row ()
  "Opening a non-first row keeps point on that row after repaint."
  (synaxis-tests--with-tmp
   (require 'synaxis-show)
   (let ((first-id (synaxis-tests--seed-entry
                    "https://example.com/x" "1" "First" 2.0 t))
         (opened-id (synaxis-tests--seed-entry
                     "https://example.com/x" "2" "Second" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (should (equal first-id (tabulated-list-get-id)))
       (forward-line 1)
       (should (equal opened-id (tabulated-list-get-id)))
       (cl-letf (((symbol-function 'synaxis-show-entry)
                  (lambda (entry-id peers)
                    (should (equal opened-id entry-id))
                    (should (equal (list first-id opened-id) peers))
                    (synaxis-db-remove-tag entry-id "unread")
                    (with-current-buffer "*synaxis*"
                      (goto-char (point-min)))
                    (pop-to-buffer (get-buffer-create "*synaxis-test-show*"))))
                 ((symbol-function 'synaxis-db-list-entries)
                  (lambda (&rest _) (error "unexpected refresh"))))
         (synaxis-search-show-entry))
       (with-current-buffer "*synaxis*"
         (should (equal opened-id (tabulated-list-get-id))))))))

(ert-deftest synaxis-search-test-format-uses-displayed-search-window ()
  "Column widths are based on the search window, not the selected window."
  (with-temp-buffer
    (synaxis-search-mode)
    (cl-letf (((symbol-function 'get-buffer-window)
               (lambda (&rest _) 'search-window))
              ((symbol-function 'window-width)
               (lambda (&optional window)
                 (if (eq window 'search-window) 100 20))))
      (let ((format (synaxis-search--format)))
        (should (= 12 (nth 1 (aref format 2))))
        (should (= 50 (nth 1 (aref format 4))))))))

(ert-deftest synaxis-search-test-set-filter-via-completing-read-multiple ()
  "Calling `synaxis-search-set-filter' interactively pulls from CRM."
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry "https://example.com/x" "1" "T" 1.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (cl-letf (((symbol-function 'completing-read-multiple)
                (lambda (&rest _) (list "tag:starred" "feed:hackaday"))))
       (call-interactively 'synaxis-search-set-filter))
     (should (equal "tag:starred feed:hackaday" synaxis-search--filter)))))

(ert-deftest synaxis-search-test-set-filter-crm-initial-preserves-quoted-value ()
  "Filter CRM initial input keeps quoted multi-word values together."
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry "https://example.com/x" "1" "T" 1.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (setq synaxis-search--filter "feed:\"PubMed Trending\" +med")
     (let (initial)
       (cl-letf (((symbol-function 'completing-read-multiple)
                  (lambda (_prompt _candidates &optional _predicate _require-match
                                   initial-input &rest _)
                    (setq initial initial-input)
                    (split-string initial-input "," t))))
         (call-interactively 'synaxis-search-set-filter))
       (should (equal "feed:\"PubMed Trending\",+med" initial))
       (should (equal "feed:\"PubMed Trending\" +med"
                      synaxis-search--filter))))))

(ert-deftest synaxis-search-test-tag-entry-uses-completing-read ()
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
              "https://example.com/x" "1" "T" 1.0 nil)))
     (ignore id)
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (should-error (call-interactively 'synaxis-search-untag-entry)
                     :type 'user-error)))))

(ert-deftest synaxis-search-test-mark-all-read-marks-visible-entries ()
  (synaxis-tests--with-tmp
   (let ((id1 (synaxis-tests--seed-entry
               "https://example.com/x" "1" "A" 1.0 t))
         (id2 (synaxis-tests--seed-entry
               "https://example.com/x" "2" "B" 2.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
         (call-interactively 'synaxis-search-mark-all-read))
       (should-not (member "unread" (synaxis-db-get-tags id1)))
       (should-not (member "unread" (synaxis-db-get-tags id2)))))))

(ert-deftest synaxis-search-test-mark-all-read-respects-cancel ()
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
              "https://example.com/x" "1" "T" 1.0 t)))
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
         (call-interactively 'synaxis-search-mark-all-read))
       (should (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-search-test-mark-all-read-errors-when-empty ()
  (synaxis-tests--with-tmp
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (should-error (call-interactively 'synaxis-search-mark-all-read)
                   :type 'user-error))))

(ert-deftest synaxis-search-test-entry-columns-renders-tags-excluding-unread ()
  "Tags cell shows other tags but omits `unread'."
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry
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
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry
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

(ert-deftest synaxis-search-test-browse-entry-from-search-mode ()
  "`synaxis-search-browse-entry' opens the row's link via `browse-url'."
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry
    "https://example.com/b" "1" "T" 1.0 t)
   (synaxis-db-upsert-entry
    '(:feed-url "https://example.com/b" :source-id "1"
                :title "T" :link "https://example.com/article" :date "1970-01-01T00:00:01Z"))
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (goto-char (point-min))
     (let (browsed)
       (cl-letf (((symbol-function 'browse-url)
                  (lambda (u &rest _) (setq browsed u))))
         (synaxis-search-browse-entry))
       (should (equal "https://example.com/article" browsed))))))

(ert-deftest synaxis-search-test-browse-entry-errors-without-row ()
  "Browse errors with `user-error' when no entry is at point."
  (synaxis-tests--with-tmp
   (with-temp-buffer
     (synaxis-search-mode)
     (should-error (synaxis-search-browse-entry) :type 'user-error))))

;;; Auto-refresh after queue drain

(ert-deftest synaxis-search-test-auto-refresh-reflects-new-entries ()
  "After new entries land in the DB, `synaxis-search--auto-refresh'
should reprint live search buffers so the new rows appear."
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry "https://example.com/x" "1" "A" 1.0 t)
   (synaxis-tests--seed-entry "https://example.com/x" "2" "B" 2.0 t)
   (let ((synaxis-search-default-filter ""))
     (synaxis-search))
   (with-current-buffer "*synaxis*"
     (should (= 2 (length tabulated-list-entries)))
     (synaxis-tests--seed-entry "https://example.com/x" "3" "C" 3.0 t)
     (synaxis-tests--seed-entry "https://example.com/x" "4" "D" 4.0 t)
     (synaxis-search--auto-refresh)
     (should (= 4 (length tabulated-list-entries))))))

(ert-deftest synaxis-search-test-auto-refresh-preserves-point-on-existing-entry ()
  "When the entry at point still exists after refresh, point should
land on the same row even though new entries shifted it."
  (synaxis-tests--with-tmp
   (let ((id-a (synaxis-tests--seed-entry "https://example.com/x" "1" "A" 1.0 t))
         (id-b (synaxis-tests--seed-entry "https://example.com/x" "2" "B" 2.0 t)))
     (ignore id-b)
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (while (and (not (eobp))
                   (not (equal id-a (tabulated-list-get-id))))
         (forward-line 1))
       (should (equal id-a (tabulated-list-get-id)))
       (synaxis-tests--seed-entry "https://example.com/x" "3" "C" 99.0 t)
       (synaxis-search--auto-refresh)
       (should (equal id-a (tabulated-list-get-id)))))))

(ert-deftest synaxis-search-test-auto-refresh-falls-back-when-entry-gone ()
  "When the entry at point has been deleted, refresh must not error
and point should land at the start of the buffer."
  (synaxis-tests--with-tmp
   (let ((id-a (synaxis-tests--seed-entry "https://example.com/x" "1" "A" 1.0 t)))
     (synaxis-tests--seed-entry "https://example.com/x" "2" "B" 2.0 t)
     (let ((synaxis-search-default-filter ""))
       (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (while (and (not (eobp))
                   (not (equal id-a (tabulated-list-get-id))))
         (forward-line 1))
       (should (equal id-a (tabulated-list-get-id)))
       (synaxis-db-delete-entry id-a)
       (synaxis-search--auto-refresh)
       (should-not (equal id-a (tabulated-list-get-id)))
       (should (= (point) (point-min)))))))

;;; Group: marking

(ert-deftest synaxis-search-test-toggle-mark-toggles ()
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry "https://e/a" "1" "T" 1.0 t)))
     (let ((synaxis-search-default-filter "")) (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (synaxis-search-toggle-mark)
       (should (member id synaxis-search--marked))
       (goto-char (point-min))
       (synaxis-search-toggle-mark)
       (should-not (member id synaxis-search--marked))))))

(ert-deftest synaxis-search-test-target-ids-marked-or-point ()
  (synaxis-tests--with-tmp
   (let ((id (synaxis-tests--seed-entry "https://e/a" "1" "T" 1.0 t)))
     (let ((synaxis-search-default-filter "")) (synaxis-search))
     (with-current-buffer "*synaxis*"
       (goto-char (point-min))
       (should (equal (list id) (synaxis-search--target-ids)))
       (setq synaxis-search--marked (list id))
       (should (equal (list id) (synaxis-search--target-ids)))))))

(ert-deftest synaxis-search-test-toggle-read-bulk-on-marks ()
  "With marks, `synaxis-search-toggle-read' marks all of them read."
  (synaxis-tests--with-tmp
   (let ((id1 (synaxis-tests--seed-entry "https://e/a" "1" "A" 2.0 t))
         (id2 (synaxis-tests--seed-entry "https://e/a" "2" "B" 1.0 t)))
     (let ((synaxis-search-default-filter "")) (synaxis-search))
     (with-current-buffer "*synaxis*"
       (setq synaxis-search--marked (list id1 id2))
       (synaxis-search-toggle-read)
       (should-not (member "unread" (synaxis-db-get-tags id1)))
       (should-not (member "unread" (synaxis-db-get-tags id2)))
       (should-not synaxis-search--marked)))))

(ert-deftest synaxis-search-test-edit-tags-bulk-on-marks ()
  "With marks, tag edits apply to every marked entry, then marks clear."
  (synaxis-tests--with-tmp
   (let ((id1 (synaxis-tests--seed-entry "https://e/a" "1" "A" 2.0 t))
         (id2 (synaxis-tests--seed-entry "https://e/a" "2" "B" 1.0 t)))
     (let ((synaxis-search-default-filter "")) (synaxis-search))
     (with-current-buffer "*synaxis*"
       (setq synaxis-search--marked (list id1 id2))
       (cl-letf (((symbol-function 'completing-read-multiple)
                  (lambda (&rest _) (list "+foo"))))
         (synaxis-search-edit-tags))
       (should (member "foo" (synaxis-db-get-tags id1)))
       (should (member "foo" (synaxis-db-get-tags id2)))
       (should-not synaxis-search--marked)))))

(ert-deftest synaxis-search-test-bookmark-record-stores-filter ()
  "The bookmark record carries the current filter as `location'."
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry "https://example.com/x" "1" "T" 1.0 t)
   (let ((synaxis-search-default-filter "")) (synaxis-search))
   (with-current-buffer "*synaxis*"
     (synaxis-search-set-filter "tag:unread foo")
     (let ((record (synaxis-search--bookmark-make-record)))
       (should (equal "tag:unread foo"
                      (bookmark-prop-get record 'location)))
       (should (eq #'synaxis-search-bookmark-handler
                   (bookmark-prop-get record 'handler)))))))

(ert-deftest synaxis-search-test-bookmark-handler-applies-filter ()
  "Jumping to a bookmark opens the buffer and applies the stored filter."
  (synaxis-tests--with-tmp
   (synaxis-tests--seed-entry "https://example.com/x" "1" "T" 1.0 t)
   (let ((synaxis-search-default-filter "")) (synaxis-search))
   (synaxis-search-bookmark-handler '("synaxis tag:unread"
                                      (location . "tag:unread")))
   (with-current-buffer "*synaxis*"
     (should (equal "tag:unread" synaxis-search--filter)))))

(ert-deftest synaxis-search-test-entry-columns-tolerates-bad-date ()
  (let* ((entry '(:title "T" :feed-title "F" :date "not-a-date"
                         :unread t :tags ("unread")))
         (cols (synaxis-search--entry-columns entry)))
    (should (string-match-p "not-a-date" (aref cols 0)))))

(provide 'synaxis-search-tests)
;;; synaxis-search-tests.el ends here
