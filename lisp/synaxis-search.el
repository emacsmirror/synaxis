;;; synaxis-search.el --- Entry list buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: news, hypermedia, rss, atom
;; URL: https://codeberg.org/thanosapollo/synaxis

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

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

(declare-function synaxis-show-entry "synaxis-show" (entry-id &optional peers))
(declare-function synaxis-fetch-all "synaxis-fetch" ())
(declare-function synaxis-add-feed "synaxis" (url &optional title))
(declare-function synaxis-remove-feed "synaxis" (url))
(declare-function synaxis-edit-feed "synaxis-edit" (&optional url))

(defvar crm-separator)

;;; Customisation

(defcustom synaxis-search-default-filter "tag:unread"
  "Initial filter for the list buffer.
See `synaxis-filter-parse' for the syntax."
  :type 'string
  :group 'synaxis)

(defcustom synaxis-search-default-limit 200
  "Maximum number of entries shown in the list buffer."
  :type 'integer
  :group 'synaxis)

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
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for unread entry titles."
  :group 'synaxis)

(defface synaxis-search-read-face
  '((t :inherit font-lock-comment-face))
  "Face for already-read entry titles."
  :group 'synaxis)

(defface synaxis-search-feed-face
  '((t :inherit font-lock-type-face))
  "Face for the feed-name column."
  :group 'synaxis)

(defface synaxis-search-date-face
  '((t :inherit font-lock-comment-face))
  "Face for the date column."
  :group 'synaxis)

(defface synaxis-search-tag-face
  '((t :inherit font-lock-keyword-face))
  "Default face for entry tags in the list buffer.
Per-tag faces from the registry (set via `synaxis-db-set-tag-face')
override this default."
  :group 'synaxis)

;;; Buffer-local state

(defvar-local synaxis-search--filter nil
  "Current filter string for the list buffer.")

(defvar-local synaxis-search--tag-face-cache nil
  "Hash mapping tag string to face symbol.
Rebuilt on each `synaxis-search-refresh' from the `tags' registry.
Tag-face changes made while the buffer is open take effect on `g'.")

(defun synaxis-search--rebuild-tag-face-cache ()
  "Populate the buffer-local tag-face cache from the registry."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (row (synaxis-db-list-tags))
      (when-let* ((face-str (plist-get row :face)))
        (puthash (plist-get row :tag) (intern face-str) h)))
    (setq synaxis-search--tag-face-cache h)))

;;; Filter compilation

(defun synaxis-search--compile-filter (filter)
  "Compile FILTER string to (WHERE PARAMS LIMIT) for the DB layer."
  (let ((c (synaxis-filter-compile (synaxis-filter-parse filter))))
    (list (plist-get c :where)
          (plist-get c :params)
          (or (plist-get c :limit) synaxis-search-default-limit))))

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
         (date   (format-time-string
                  "%Y-%m-%d"
                  (seconds-to-time (or (plist-get entry :date) 0))))
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

(defun synaxis-search--format ()
  "Build `tabulated-list-format' from `synaxis-search--columns'.
Float widths in the spec are scaled by `window-width' at call time,
so the format reflects the current window size."
  (let ((w (window-width)))
    (apply #'vector
           (mapcar (lambda (col)
                     (let* ((name  (car col))
                            (spec  (nth 1 col))
                            (sort  (nth 2 col))
                            (props (nthcdr 3 col))
                            (width (if (floatp spec)
                                       (max 1 (truncate (* w spec)))
                                     spec)))
                       (append (list name width sort) props)))
                   synaxis-search--columns))))

;;; Mode and keymap

(keymap-popup-define synaxis-search-mode-map
  "Keymap for `synaxis-search-mode'."
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
  "n" ("Next"         next-line :stay-open t)
  "p" ("Previous"     previous-line :stay-open t)
  "RET" ("Open"       synaxis-search-show-entry)
  :group "Tags"
  "r" ("Toggle read"     synaxis-search-toggle-read :stay-open t)
  "R" ("Mark all read"   synaxis-search-mark-all-read)
  "t" ("Edit tags"       synaxis-search-edit-tags :stay-open t)
  :group "Feeds"
  "A" ("Add feed"     synaxis-add-feed)
  "D" ("Remove feed"  synaxis-remove-feed)
  "E" ("Edit feed"    synaxis-search-edit-feed)
  "u" ("Update feeds" synaxis-search-update)
  :group "View"
  "l" ("Filter"       synaxis-search-set-filter)
  "g" ("Refresh"      synaxis-search-refresh)
  "q" ("Quit"         quit-window))

(define-derived-mode synaxis-search-mode tabulated-list-mode "Synaxis"
  "Major mode for the synaxis entry list buffer."
  (setq tabulated-list-format (synaxis-search--format))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (synaxis-search-refresh)))
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

(defun synaxis-search-refresh ()
  "Re-run the current filter's query and repopulate the buffer.
Also recomputes the column format from `synaxis-search--columns' so
window resizes are picked up automatically, and rebuilds the
tag-face cache from the registry."
  (interactive)
  (when (derived-mode-p 'synaxis-search-mode)
    (setq tabulated-list-format (synaxis-search--format))
    (tabulated-list-init-header)
    (synaxis-search--rebuild-tag-face-cache)
    (let* ((spec    (synaxis-search--compile-filter
                     (or synaxis-search--filter "")))
           (where   (nth 0 spec))
           (params  (nth 1 spec))
           (limit   (nth 2 spec))
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

(defun synaxis-search--redraw-current ()
  "Re-render the row at point from the latest DB state."
  (when-let* ((id (synaxis-search-current-entry))
              (entry (synaxis-db-get-entry id)))
    (synaxis-tl-replace-entry id (synaxis-search--entry-columns entry))))

(defun synaxis-search-show-entry ()
  "Open the entry at point in the show buffer.
Hands the show buffer the current view's id sequence so its `n'
and `p' can step between sibling entries."
  (interactive)
  (when-let* ((id (synaxis-search-current-entry)))
    (require 'synaxis-show)
    (synaxis-show-entry id (mapcar #'car tabulated-list-entries))
    (synaxis-search--redraw-current)))

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
  "Toggle the `unread' tag on the entry at point."
  (interactive)
  (when-let* ((id (synaxis-search-current-entry)))
    (if (member "unread" (synaxis-db-get-tags id))
        (synaxis-db-remove-tag id "unread")
      (synaxis-db-add-tag id "unread"))
    (synaxis-search--redraw-current)))

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
  (let ((db (synaxis-db--ensure-open)))
    (synaxis-db--with-transaction db
      (dolist (sel selections)
        (cond
         ((string-prefix-p "+" sel)
          (let ((name (substring sel 1)))
            (unless (string-empty-p name)
              (synaxis-db-add-tag entry-id name))))
         ((string-prefix-p "-" sel)
          (let ((name (substring sel 1)))
            (unless (string-empty-p name)
              (synaxis-db-remove-tag entry-id name))))
         ((not (string-empty-p sel))
          (synaxis-db-add-tag entry-id sel)))))))

(defun synaxis-search-edit-tags ()
  "Add or remove tags on the entry at point via `completing-read-multiple'.
Candidates are prefixed `+' (absent) or `-' (already on the entry);
new tag names may also be typed.  Separator is `,'."
  (interactive)
  (when-let* ((id (synaxis-search-current-entry)))
    (let* ((cands (synaxis-search--tag-candidates id))
           (crm-separator ",")
           (selections (mapcar #'string-trim
                               (completing-read-multiple
                                "Tags (+add, -remove): " cands nil nil))))
      (when selections
        (synaxis-search--apply-tag-edits id selections)
        (synaxis-search--redraw-current)))))

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
      (call-interactively 'synaxis-edit-feed))))

(defun synaxis-search-update ()
  "Kick off `synaxis-fetch-all'.  Press `g' to refresh once it finishes."
  (interactive)
  (require 'synaxis-fetch)
  (synaxis-fetch-all)
  (message "synaxis: fetching feeds -- press `g' to refresh."))

(provide 'synaxis-search)
;;; synaxis-search.el ends here
