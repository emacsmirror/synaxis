;;; synaxis-search.el --- Entry list buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>
;; Keywords: news, hypermedia, rss, atom
;; URL: https://codeberg.org/thanosapollo/emacs-synaxis

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The entry list buffer.  Pure renderer: holds no derived state.
;; Every refresh re-runs a SQL `SELECT' and feeds rows into
;; `tabulated-list-mode' via `synaxis-tl-print'.  Tag commands write
;; through to the database and replace just the affected row.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'keymap-popup)
(require 'synaxis-db)
(require 'synaxis-filter)
(require 'synaxis-tl)
(require 'parse-time)

(declare-function synaxis-show-entry "synaxis-show" (entry-id &optional peers))
(declare-function synaxis-fetch-all "synaxis-fetch" ())
(declare-function synaxis-add-feed "synaxis" (url &optional title))
(declare-function synaxis-remove-feed "synaxis" (url))
(declare-function synaxis-edit-feed "synaxis-edit" (&optional url))
(declare-function synaxis-tag-rules-apply-all "synaxis" ())
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))

(defvar bookmark-make-record-function)
(defvar crm-separator)
(defvar synaxis-scrape-test--entries)

;;; Customisation

(defcustom synaxis-search-default-filter "tag:unread"
  "Initial filter for the list buffer.
See `synaxis-filter-parse' for the syntax."
  :type 'string
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defcustom synaxis-search-default-limit nil
  "Default maximum number of entries shown in the list buffer.
Nil means no limit -- the filter returns every matching row.
Override per query with `#N' in the filter string."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defconst synaxis-search--columns
  `(("Date"  10        t)
    (""      1         nil)
    ("Feed"  ,(/ 1.0 8) t)
    ("Tags"  ,(/ 1.0 8) nil)
    ("Title" ,(/ 1.0 2) t))
  "Column spec for the entry list.
Each element is (NAME WIDTH-OR-FLOAT SORT . PROPS).
Float widths are multiplied by `window-width' at render time;
integer widths are absolute.")

;;; Faces

(defface synaxis-search-unread-face
  '((t :inherit default :weight bold))
  "Face for unread entry titles."
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defface synaxis-search-read-face
  '((t :inherit font-lock-comment-face))
  "Face for already-read entry titles."
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defface synaxis-search-feed-face
  '((t :inherit font-lock-type-face))
  "Face for the feed-name column."
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defface synaxis-search-date-face
  '((t :inherit font-lock-comment-face))
  "Face for the date column."
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defface synaxis-search-tag-face
  '((t :inherit font-lock-keyword-face))
  "Default face for entry tags in the list buffer.
Per-tag faces from the registry (set via `synaxis-db-set-tag-face')
override this default."
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defface synaxis-search-marked-face
  '((t :inherit region))
  "Face for marked rows in the list buffer."
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

;;; Buffer-local state

(defvar-local synaxis-search--filter nil
  "Current filter string for the list buffer.")

(defvar-local synaxis-search--tag-face-cache nil
  "Hash mapping tag string to face symbol.
Rebuilt on each `synaxis-search-refresh' from the `tags' registry.
Tag-face changes made while the buffer is open take effect on `g'.")

(defvar-local synaxis-search--marked nil
  "List of marked entry ids, most-recently-marked first.
Bulk commands act on these when non-nil; cleared on refresh.")

(defun synaxis-search--rebuild-tag-face-cache ()
  "Populate the buffer-local tag-face cache from the registry."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (row (synaxis-db-list-tags))
      (when-let* ((face-str (plist-get row :face)))
        (puthash (plist-get row :tag) (intern face-str) h)))
    (setq synaxis-search--tag-face-cache h)))

;;; Filter compilation

(defun synaxis-search--compile-filter (filter)
  "Compile FILTER string to a plist (:where :params :limit).
The :limit default is filled in from `synaxis-search-default-limit'."
  (let ((c (synaxis-filter-compile (synaxis-filter-parse filter))))
    (list :where  (plist-get c :where)
          :params (plist-get c :params)
          :limit  (or (plist-get c :limit) synaxis-search-default-limit))))

;;; Row formatting

(defun synaxis-search--render-tags (tag-list)
  "Render TAG-LIST as a propertized cell, excluding `unread'.
Each tag picks up its face from `synaxis-search--tag-face-cache'
when set, otherwise falls back to `synaxis-search-tag-face'."
  (mapconcat
   (lambda (tag)
     (let ((face (or (and synaxis-search--tag-face-cache
                          (gethash tag synaxis-search--tag-face-cache))
                     'synaxis-search-tag-face)))
       (propertize tag 'face face)))
   (cl-remove "unread" (sort (copy-sequence tag-list) #'string<)
              :test #'string=)
   " "))

(defun synaxis-search--entry-columns (entry)
  "Convert ENTRY plist to the column vector used by `tabulated-list-mode'.
Reads `:unread' and `:tags' from ENTRY rather than re-querying the DB."
  (let* ((unread (plist-get entry :unread))
         (date-iso (plist-get entry :date))
         (date   (if date-iso
                     (format-time-string
                      "%Y-%m-%d"
                      (parse-iso8601-time-string date-iso))
                   ""))
         (mark   (if unread "*" " "))
         (feed   (or (plist-get entry :feed-title) "?"))
         (tags   (synaxis-search--render-tags (plist-get entry :tags)))
         (title  (or (plist-get entry :title) "(untitled)"))
         (title-face (if unread
                         'synaxis-search-unread-face
                       'synaxis-search-read-face)))
    (vector (propertize date 'face 'synaxis-search-date-face)
            mark
            (propertize feed 'face 'synaxis-search-feed-face)
            tags
            (propertize title 'face title-face))))

(defun synaxis-search--window-width ()
  "Return the displayed search window width in columns."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (window-width window)
    (window-width)))

(defun synaxis-search--format ()
  "Build `tabulated-list-format' from `synaxis-search--columns'.
Float widths in the spec are scaled by the displayed search window
at call time, so the format reflects the current window size."
  (let ((w (synaxis-search--window-width)))
    (apply #'vector
           (mapcar (pcase-lambda (`(,name ,spec ,sort . ,props))
                     (let ((width (if (floatp spec)
                                      (max 1 (truncate (* w spec)))
                                    spec)))
                       (append (list name width sort) props)))
                   synaxis-search--columns))))

;;; Mode and keymap

(keymap-popup-define synaxis-search-mode-map
  "Keymap for `synaxis-search-mode'."
  :parent synaxis-tl-list-mode-map
  :description
  (lambda ()
    (with-current-buffer (or (get-buffer "*synaxis*") (current-buffer))
      (format "synaxis  [%s]  %d entries"
              (or (and (boundp 'synaxis-search--filter)
                       synaxis-search--filter)
                  "")
              (if (boundp 'tabulated-list-entries)
                  (length tabulated-list-entries)
                0))))
  :group "Navigation"
  "n" ("Next" next-line :stay-open t)
  "p" ("Previous" previous-line :stay-open t)
  "RET" ("Open" synaxis-search-show-entry)
  "b" ("Browse URL" synaxis-search-browse-entry)
  "c" ("Copy URL" synaxis-search-copy-link)
  :group "Tags"
  "r" ("Toggle read" synaxis-search-toggle-read :stay-open t)
  "R" ("Mark all read" synaxis-search-mark-all-read)
  "t" ("Edit tags" synaxis-search-edit-tags :stay-open t)
  ";" ("Apply tag rules" synaxis-tag-rules-apply-all)
  :group "Mark"
  "m" ("Mark / unmark" synaxis-search-toggle-mark :stay-open t)
  "U" ("Unmark all" synaxis-search-unmark-all :stay-open t)
  :group "Feeds"
  "A" ("Add feed" synaxis-add-feed)
  "D" ("Remove feed" synaxis-remove-feed)
  "E" ("Edit feed" synaxis-search-edit-feed)
  "u" ("Update feeds" synaxis-search-update)
  :group "View"
  "l" ("Filter" synaxis-search-set-filter)
  "g" ("Refresh" synaxis-search-refresh)
  "q" ("Quit" quit-window))

(define-derived-mode synaxis-search-mode tabulated-list-mode "Synaxis"
  "Major mode for the synaxis entry list buffer."
  (setq tabulated-list-format (synaxis-search--format))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (synaxis-search-refresh)))
  (setq-local bookmark-make-record-function
              #'synaxis-search--bookmark-make-record)
  (tabulated-list-init-header))

;;; Commands

;;;###autoload
(defun synaxis-search ()
  "Open or switch to the synaxis entry list buffer."
  (interactive)
  (let ((buf (get-buffer-create "*synaxis*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'synaxis-search-mode)
        (synaxis-search-mode)
        (setq synaxis-search--filter synaxis-search-default-filter))
      (synaxis-search-refresh))
    (pop-to-buffer-same-window buf)))

(defun synaxis-search--rebuild-format ()
  "Recompute the tabulated-list column format for the current window."
  (setq tabulated-list-format (synaxis-search--format))
  (tabulated-list-init-header))

(defun synaxis-search--reprint-view (&optional entry-id)
  "Reprint current rows without re-querying the database.
When ENTRY-ID is non-nil, restore point to that row explicitly."
  (synaxis-search--rebuild-format)
  (synaxis-tl-print)
  (synaxis-search--refresh-mark-overlays)
  (when entry-id
    (synaxis-search--restore-entry entry-id)))

(defun synaxis-search-refresh ()
  "Re-run the current filter's query and repopulate the buffer.
Also recomputes the column format from `synaxis-search--columns' so
window resizes are picked up automatically, and rebuilds the
tag-face cache from the registry."
  (interactive)
  (when (derived-mode-p 'synaxis-search-mode)
    (setq synaxis-search--marked nil)
    (synaxis-search--rebuild-format)
    (synaxis-search--rebuild-tag-face-cache)
    (let* ((spec (synaxis-search--compile-filter
                  (or synaxis-search--filter "")))
           (where (plist-get spec :where))
           (params (plist-get spec :params))
           (limit (plist-get spec :limit))
           (entries (synaxis-db-list-entries where params limit)))
      (setq tabulated-list-entries
            (mapcar (lambda (e)
                      (list (plist-get e :id)
                            (synaxis-search--entry-columns e)))
                    entries))
      (let ((header (format "synaxis  [%s]  %d entries"
                            (or synaxis-search--filter "")
                            (length entries))))
        (setq-local mode-line-buffer-identification
                    (list (propertize header 'face 'mode-line-buffer-id))))
      (synaxis-tl-print t))))

(defun synaxis-search-current-entry ()
  "Return the entry id at point, or nil."
  (tabulated-list-get-id))

(defun synaxis-search--redraw-current (&optional entry-id)
  "Re-render ENTRY-ID, or the row at point, from the latest DB state."
  (when-let* ((id (or entry-id (synaxis-search-current-entry)))
              (entry (synaxis-db-get-entry id)))
    (let ((columns (synaxis-search--entry-columns entry)))
      (setq tabulated-list-entries
            (mapcar (lambda (row)
                      (if (equal id (car row))
                          (list id columns)
                        row))
                    tabulated-list-entries))
      (synaxis-tl-replace-entry id columns))))

(defun synaxis-search-show-entry ()
  "Open the entry at point in the show buffer.
Hands the show buffer the current view's id sequence so its `n'
and `p' can step between sibling entries.  Repaints the just-opened
row so the unread marker clears (the row itself stays visible until
the next `g')."
  (interactive)
  (when-let* ((id (synaxis-search-current-entry))
              (origin (current-buffer)))
    (require 'synaxis-show)
    (synaxis-show-entry id (mapcar #'car tabulated-list-entries))
    (with-current-buffer origin
      (synaxis-search--redraw-current id)
      (synaxis-search--reprint-view id))))

(defun synaxis-search--entry-at-point ()
  "Return the entry plist at point, dispatching by current mode.
DB-backed in `synaxis-search-mode'; buffer-local store in
`synaxis-scrape-test-mode'."
  (and-let* ((id (tabulated-list-get-id)))
    (if (derived-mode-p 'synaxis-scrape-test-mode)
        (cl-find id synaxis-scrape-test--entries
                 :key (lambda (e) (plist-get e :id))
                 :test #'equal)
      (synaxis-db-get-entry id))))

(defun synaxis-search-browse-entry ()
  "Open the entry at point's article URL in a browser."
  (interactive)
  (if-let* ((entry (synaxis-search--entry-at-point))
            (link  (plist-get entry :link)))
      (browse-url link)
    (user-error "No link for this entry")))

(defun synaxis-search-copy-link ()
  "Copy the entry at point's article URL to the kill ring."
  (interactive)
  (if-let* ((entry (synaxis-search--entry-at-point))
            (link  (plist-get entry :link)))
      (progn (kill-new link)
             (message "synaxis: copied %s" link))
    (user-error "No link for this entry")))

(defun synaxis-search-mark-all-read ()
  "Mark every entry currently visible in the buffer as read."
  (interactive)
  (let* ((ids (mapcar #'car tabulated-list-entries))
         (n   (length ids)))
    (unless ids (user-error "No entries to mark"))
    (when (y-or-n-p (format "Mark %d entries as read? " n))
      (synaxis-db-bulk-remove-tag ids "unread")
      (synaxis-search-refresh)
      (message "synaxis: marked %d entries as read" n))))

(defun synaxis-search-toggle-read ()
  "Toggle the `unread' tag on the entry at point.
When entries are marked, mark all of them read instead."
  (interactive)
  (if synaxis-search--marked
      (let ((ids (synaxis-search--target-ids)))
        (synaxis-db-bulk-remove-tag ids "unread")
        (synaxis-search--after-bulk ids))
    (when-let* ((id (synaxis-search-current-entry)))
      (if (member "unread" (synaxis-db-get-tags id))
          (synaxis-db-remove-tag id "unread")
        (synaxis-db-add-tag id "unread"))
      (synaxis-search--redraw-current))))

(defun synaxis-search--tag-candidates (entry-id)
  "Return `+absent' / `-present' candidate strings for ENTRY-ID.
Pulls all registered tag names from `synaxis-db-list-tags' and
marks each as add (`+') or remove (`-') based on the entry's
current tag membership."
  (let* ((all (sort (mapcar (lambda (r) (plist-get r :tag))
                            (synaxis-db-list-tags))
                    #'string<))
         (current (synaxis-db-get-tags entry-id)))
    (mapcar (lambda (name)
              (concat (if (member name current) "-" "+") name))
            all)))

(defun synaxis-search--apply-tag-edits (entry-id selections)
  "Apply tag-edit SELECTIONS to ENTRY-ID inside a single transaction.
Each SELECTION is `+NAME' (add), `-NAME' (remove), or a bare NAME
\(treated as add, so new tag names typed at the prompt work)."
  (synaxis-db--with-transaction (synaxis-db--ensure-open)
    (cl-loop for sel in selections
             when (string-match "\\`\\([-+]?\\)\\(.+\\)\\'" sel)
             for name = (match-string 2 sel)
             do (if (equal "-" (match-string 1 sel))
                    (synaxis-db-remove-tag entry-id name)
                  (synaxis-db-add-tag entry-id name)))))

(defun synaxis-search-edit-tags ()
  "Add or remove tags on the entry at point via `completing-read-multiple'.
When entries are marked, the chosen edits apply to all of them.
Candidates are prefixed `+' (absent) or `-' (already on the entry);
new tag names may also be typed.  Separator is `,'."
  (interactive)
  (let ((ids (synaxis-search--target-ids)))
    (when ids
      (let* ((cands (synaxis-search--tag-candidates (car ids)))
             (crm-separator ",")
             (selections (mapcar #'string-trim
                                 (completing-read-multiple
                                  "Tags (+add, -remove): " cands nil nil))))
        (when selections
          (dolist (id ids)
            (synaxis-search--apply-tag-edits id selections))
          (synaxis-search--after-bulk ids))))))

(defun synaxis-search-tag-entry (tag)
  "Add TAG to the entry at point.
Completes against existing tags but accepts new ones."
  (interactive
   (list (completing-read "Add tag: " (synaxis-filter--db-tags) nil nil)))
  (when-let* ((id (synaxis-search-current-entry))
              ((not (string-empty-p tag))))
    (synaxis-db-add-tag id tag)
    (synaxis-search--redraw-current)))

(defun synaxis-search-untag-entry (tag)
  "Remove TAG from the entry at point.
Completes against the entry's current tags only."
  (interactive
   (let* ((id (synaxis-search-current-entry))
          (tags (and id (synaxis-db-get-tags id))))
     (unless tags (user-error "No tags on this entry"))
     (list (completing-read "Remove tag: " tags nil t))))
  (when-let* ((id (synaxis-search-current-entry))
              ((not (string-empty-p tag))))
    (synaxis-db-remove-tag id tag)
    (synaxis-search--redraw-current)))

;;; Marking

(defun synaxis-search--marked-p (id)
  "Non-nil when entry ID is marked."
  (and (member id synaxis-search--marked) t))

(defun synaxis-search--target-ids ()
  "Return the marked ids (oldest first), or the id at point as a list."
  (or (reverse synaxis-search--marked)
      (and-let* ((id (synaxis-search-current-entry)))
        (list id))))

(defun synaxis-search--clear-mark-overlays ()
  "Remove all mark overlays from the buffer."
  (remove-overlays (point-min) (point-max) 'synaxis-mark t))

(defun synaxis-search--refresh-mark-overlays ()
  "Recreate row overlays for every visible marked entry."
  (synaxis-search--clear-mark-overlays)
  (when synaxis-search--marked
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (synaxis-search--marked-p (tabulated-list-get-id))
          (let ((ov (make-overlay (line-beginning-position)
                                  (line-beginning-position 2))))
            (overlay-put ov 'synaxis-mark t)
            (overlay-put ov 'face 'synaxis-search-marked-face)
            (overlay-put ov 'evaporate t)))
        (forward-line 1)))))

(defun synaxis-search--after-bulk (ids)
  "Redraw rows for IDS and clear every mark."
  (dolist (id ids)
    (synaxis-search--redraw-current id))
  (setq synaxis-search--marked nil)
  (synaxis-search--clear-mark-overlays))

(defun synaxis-search-toggle-mark ()
  "Toggle the mark on the entry at point, then move to the next line."
  (interactive)
  (when-let* ((id (synaxis-search-current-entry)))
    (setq synaxis-search--marked
          (if (synaxis-search--marked-p id)
              (delete id synaxis-search--marked)
            (cons id synaxis-search--marked)))
    (synaxis-search--refresh-mark-overlays)
    (forward-line 1)))

(defun synaxis-search-unmark-all ()
  "Clear every mark in the list buffer."
  (interactive)
  (setq synaxis-search--marked nil)
  (synaxis-search--clear-mark-overlays)
  (message "synaxis: marks cleared"))


(defun synaxis-search--read-filter (default)
  "Read a filter string with `completing-read-multiple' and `,' separator.
DEFAULT is the current filter (whitespace-separated); spaces are
swapped for commas so CRM splits the initial value into items.
The returned string is whitespace-joined for the parser."
  (let* ((candidates (synaxis-filter-completions))
         (crm-separator ",")
         (initial (replace-regexp-in-string " " "," (or default ""))))
    (string-join
     (completing-read-multiple "Filter: " candidates nil nil initial)
     " ")))

(defun synaxis-search-set-filter (filter)
  "Set the buffer's filter to FILTER and refresh.
Interactively, prompts with `completing-read' and prefix-aware
completion against `synaxis-filter-completions'."
  (interactive
   (list (synaxis-search--read-filter (or synaxis-search--filter ""))))
  (setq synaxis-search--filter filter)
  (synaxis-search-refresh))

;;; Bookmarks

(defun synaxis-search--bookmark-make-record ()
  "Return a bookmark record for the current synaxis filter.
The filter string is stored as the record's `location'.  The list
buffer has no file and no meaningful point context, so neither is
recorded."
  `(,(format "synaxis %s" (or synaxis-search--filter ""))
    ,@(bookmark-make-record-default 'no-file 'no-context)
    (location . ,(or synaxis-search--filter ""))
    (handler . ,#'synaxis-search-bookmark-handler)))

;;;###autoload
(defun synaxis-search-bookmark-handler (record)
  "Jump to a synaxis search bookmark RECORD.
Opens the list buffer and applies the stored filter."
  (synaxis-search)
  (synaxis-search-set-filter (bookmark-prop-get record 'location)))

(put 'synaxis-search-bookmark-handler 'bookmark-handler-type "Synaxis")

(defun synaxis-search-edit-feed ()
  "Open `synaxis-edit-feed' on the feed of the entry at point.
With point on no row, falls back to `synaxis-edit-feed's prompt."
  (interactive)
  (require 'synaxis-edit)
  (let* ((id (synaxis-search-current-entry))
         (entry (and id (synaxis-db-get-entry id)))
         (url (and entry (plist-get entry :feed-url))))
    (if url
        (synaxis-edit-feed url)
      (call-interactively #'synaxis-edit-feed))))

(defun synaxis-search-update ()
  "Kick off `synaxis-fetch-all'.  Press `g' to refresh once it finishes."
  (interactive)
  (require 'synaxis-fetch)
  (synaxis-fetch-all))

;;; Auto-refresh on fetch-queue drain

(defvar synaxis-fetch-queue-drained-hook)

(defun synaxis-search--goto-entry (id)
  "Move point to the tabulated-list row whose id equals ID.
Return the row position, or nil when no row matches."
  (let ((target (save-excursion
                  (goto-char (point-min))
                  (cl-loop until (eobp)
                           for row-id = (tabulated-list-get-id)
                           when (equal id row-id) return (point)
                           do (forward-line 1)))))
    (when target
      (goto-char target)
      target)))

(defun synaxis-search--restore-entry (id &optional fallback)
  "Move buffer and visible window point to entry ID.
When ID is not present and FALLBACK is non-nil, move buffer and
visible window point to FALLBACK.  Return the final position, or
nil when ID is missing and no fallback was given."
  (let ((pos (or (synaxis-search--goto-entry id) fallback)))
    (when pos
      (goto-char pos)
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (set-window-point window pos))
      pos)))

(defun synaxis-search--auto-refresh ()
  "Refresh every live `synaxis-search-mode' buffer.
Captures the entry id at point before reprint and restores it
after, so the user does not lose their place when new entries
arrive at the top of the list.  When the entry has been deleted,
point falls back to `point-min'."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'synaxis-search-mode)
          (let ((id (synaxis-search-current-entry)))
            (synaxis-search-refresh)
            (unless (and id (synaxis-search--goto-entry id))
              (goto-char (point-min)))))))))

(add-hook 'synaxis-fetch-queue-drained-hook #'synaxis-search--auto-refresh)

(provide 'synaxis-search)
;;; synaxis-search.el ends here
