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

(declare-function synaxis-show-entry "synaxis-show" (entry-id))
(declare-function synaxis-fetch-all "synaxis-fetch" ())

;;; Customisation

(defcustom synaxis-search-default-filter "+unread"
  "Initial filter for the list buffer."
  :type 'string
  :group 'synaxis)

(defcustom synaxis-search-default-limit 200
  "Maximum number of entries shown in the list buffer."
  :type 'integer
  :group 'synaxis)

(defcustom synaxis-search-title-width 200
  "Width budget for the title column.
Long titles are clipped at the window edge via `truncate-lines',
not truncated to this width.  A large value here means the column
spec never forces an ellipsis."
  :type 'integer
  :group 'synaxis)

;;; Faces

(defface synaxis-search-unread-face
  '((t :weight bold))
  "Face for unread entry titles."
  :group 'synaxis)

(defface synaxis-search-read-face
  '((t :inherit shadow))
  "Face for already-read entry titles."
  :group 'synaxis)

(defface synaxis-search-feed-face
  '((t :inherit font-lock-type-face))
  "Face for the feed-name column."
  :group 'synaxis)

(defface synaxis-search-date-face
  '((t :inherit shadow))
  "Face for the date column."
  :group 'synaxis)

;;; Buffer-local state

(defvar-local synaxis-search--filter nil
  "Current filter string for the list buffer.")

;;; Filter compilation

(defun synaxis-search--compile-filter (filter)
  "Compile FILTER string to (WHERE PARAMS LIMIT) for the DB layer."
  (let ((c (synaxis-filter-compile (synaxis-filter-parse filter))))
    (list (plist-get c :where)
          (plist-get c :params)
          (or (plist-get c :limit) synaxis-search-default-limit))))

;;; Row formatting

(defun synaxis-search--entry-columns (entry)
  "Convert ENTRY plist to the column vector used by `tabulated-list-mode'."
  (let* ((id     (plist-get entry :id))
         (tags   (and id (synaxis-db-get-tags id)))
         (unread (and (member "unread" tags) t))
         (date   (format-time-string
                  "%Y-%m-%d"
                  (seconds-to-time (or (plist-get entry :date) 0))))
         (mark   (if unread "*" " "))
         (feed   (or (plist-get entry :feed-title) "?"))
         (title  (or (plist-get entry :title) "(untitled)"))
         (title-face (if unread
                         'synaxis-search-unread-face
                       'synaxis-search-read-face)))
    (vector (propertize date 'face 'synaxis-search-date-face)
            mark
            (propertize feed 'face 'synaxis-search-feed-face)
            (propertize title 'face title-face))))

(defun synaxis-search--format ()
  "Return the `tabulated-list-format' vector."
  (vector (list "Date" 10 t)
          (list ""     1  nil)
          (list "Feed" 16 t)
          (list "Title" synaxis-search-title-width t)))

;;; Mode and keymap

(keymap-popup-define synaxis-search-mode-map
  "Keymap for `synaxis-search-mode'."
  :group "Navigation"
  "n" ("Next"         next-line :stay-open t)
  "p" ("Previous"     previous-line :stay-open t)
  "RET" ("Open"       synaxis-search-show-entry)
  :group "Tags"
  "r" ("Toggle read"  synaxis-search-toggle-read :stay-open t)
  "+" ("Add tag"      synaxis-search-tag-entry :stay-open t)
  "-" ("Remove tag"   synaxis-search-untag-entry :stay-open t)
  :group "View"
  "s" ("Filter"       synaxis-search-set-filter)
  "g" ("Refresh"      synaxis-search-refresh)
  "u" ("Update feeds" synaxis-search-update)
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
    (display-buffer buf)))

(defun synaxis-search-refresh ()
  "Re-run the current filter's query and repopulate the buffer."
  (interactive)
  (when (derived-mode-p 'synaxis-search-mode)
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
        (setq mode-line-buffer-identification
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
  "Open the entry at point in the show buffer."
  (interactive)
  (when-let* ((id (synaxis-search-current-entry)))
    (require 'synaxis-show)
    (synaxis-show-entry id)
    (synaxis-search--redraw-current)))

(defun synaxis-search-toggle-read ()
  "Toggle the `unread' tag on the entry at point."
  (interactive)
  (when-let* ((id (synaxis-search-current-entry)))
    (if (member "unread" (synaxis-db-get-tags id))
        (synaxis-db-remove-tag id "unread")
      (synaxis-db-add-tag id "unread"))
    (synaxis-search--redraw-current)))

(defun synaxis-search-tag-entry (tag)
  "Add TAG to the entry at point."
  (interactive (list (read-string "Add tag: ")))
  (when-let* ((id (synaxis-search-current-entry))
              ((not (string-empty-p tag))))
    (synaxis-db-add-tag id tag)
    (synaxis-search--redraw-current)))

(defun synaxis-search-untag-entry (tag)
  "Remove TAG from the entry at point."
  (interactive (list (read-string "Remove tag: ")))
  (when-let* ((id (synaxis-search-current-entry))
              ((not (string-empty-p tag))))
    (synaxis-db-remove-tag id tag)
    (synaxis-search--redraw-current)))

(defun synaxis-search-set-filter (filter)
  "Set the buffer's filter to FILTER and refresh."
  (interactive (list (read-string "Filter: "
                                  (or synaxis-search--filter ""))))
  (setq synaxis-search--filter filter)
  (synaxis-search-refresh))

(defun synaxis-search-update ()
  "Kick off `synaxis-fetch-all'.  Press `g' to refresh once it finishes."
  (interactive)
  (require 'synaxis-fetch)
  (synaxis-fetch-all)
  (message "synaxis: fetching feeds -- press `g' to refresh."))

(provide 'synaxis-search)
;;; synaxis-search.el ends here
