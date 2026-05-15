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

;; The entry list buffer.  A pure renderer: holds no derived state,
;; every refresh re-runs a SQL `SELECT' and replays ewoc nodes.  Tag
;; commands write through to the database immediately and invalidate
;; the affected node so it redraws.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'keymap-popup)
(require 'synaxis-db)

(declare-function synaxis-show-entry "synaxis-show" (entry-id))
(declare-function synaxis-fetch-all "synaxis-fetch" ())

;;; Customisation

(defcustom synaxis-search-default-filter "+unread"
  "Initial filter for the list buffer.
For v0.1 the recognised values are the empty string, `+unread', and
`-unread'.  The full filter mini-language lands in `synaxis-filter'."
  :type 'string
  :group 'synaxis)

(defcustom synaxis-search-default-limit 200
  "Maximum number of entries shown in the list buffer."
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

(defvar-local synaxis-search--ewoc nil
  "Ewoc instance for the current list buffer.")

(defvar-local synaxis-search--filter nil
  "Current filter string for the list buffer.")

;;; Filter compilation (stub replaced in synaxis-filter)

(defun synaxis-search--compile-filter (filter)
  "Compile FILTER string to (WHERE PARAMS LIMIT).
Stub: handles only empty, `+unread', and `-unread' until
`synaxis-filter' lands."
  (let ((limit synaxis-search-default-limit))
    (cond
     ((or (null filter) (string-empty-p filter))
      (list "1=1" nil limit))
     ((string= filter "+unread")
      (list (concat "EXISTS (SELECT 1 FROM entry_tags t"
                    " WHERE t.entry_id = e.id AND t.tag = 'unread')")
            nil limit))
     ((string= filter "-unread")
      (list (concat "NOT EXISTS (SELECT 1 FROM entry_tags t"
                    " WHERE t.entry_id = e.id AND t.tag = 'unread')")
            nil limit))
     (t (list "1=1" nil limit)))))

;;; Pretty-printer

(defun synaxis-search--pp (entry)
  "Ewoc printer for an entry plist ENTRY."
  (let* ((id      (plist-get entry :id))
         (date    (format-time-string
                   "%Y-%m-%d"
                   (seconds-to-time (or (plist-get entry :date) 0))))
         (tags    (and id (synaxis-db-get-tags id)))
         (unread  (member "unread" tags))
         (mark    (if unread "*" " "))
         (feed    (truncate-string-to-width
                   (or (plist-get entry :feed-title) "?") 16 nil ?\s "…"))
         (title   (or (plist-get entry :title) "(untitled)"))
         (face    (if unread 'synaxis-search-unread-face 'synaxis-search-read-face)))
    (insert (propertize date 'face 'synaxis-search-date-face)
            " " mark " "
            (propertize feed 'face 'synaxis-search-feed-face)
            "  "
            (propertize title 'face face))))

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

(define-derived-mode synaxis-search-mode special-mode "Synaxis"
  "Major mode for the synaxis entry list buffer."
  (buffer-disable-undo)
  (setq-local revert-buffer-function (lambda (&rest _) (synaxis-search-refresh))))

;;; Commands

;;;###autoload
(defun synaxis-search ()
  "Open or switch to the synaxis entry list buffer."
  (interactive)
  (let ((buf (get-buffer-create "*synaxis*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'synaxis-search-mode)
        (synaxis-search-mode)
        (setq synaxis-search--filter synaxis-search-default-filter)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq synaxis-search--ewoc
              (ewoc-create #'synaxis-search--pp nil nil t)))
      (synaxis-search-refresh))
    (display-buffer buf)))

(defun synaxis-search-refresh ()
  "Re-run the current filter's query and repopulate the ewoc."
  (interactive)
  (when synaxis-search--ewoc
    (let* ((spec    (synaxis-search--compile-filter
                     (or synaxis-search--filter "")))
           (where   (nth 0 spec))
           (params  (nth 1 spec))
           (limit   (nth 2 spec))
           (entries (synaxis-db-list-entries where params limit))
           (inhibit-read-only t))
      (ewoc-filter synaxis-search--ewoc (lambda (_) nil))
      (ewoc-set-hf synaxis-search--ewoc
                   (format "synaxis  [%s]  %d entries\n\n"
                           (or synaxis-search--filter "")
                           (length entries))
                   "")
      (dolist (e entries)
        (ewoc-enter-last synaxis-search--ewoc e)))))

(defun synaxis-search-current-entry ()
  "Return the entry id at point, or nil."
  (let ((node (and synaxis-search--ewoc
                   (ewoc-locate synaxis-search--ewoc))))
    (and node (plist-get (ewoc-data node) :id))))

(defun synaxis-search--redraw-current ()
  "Invalidate the ewoc node at point, forcing a redraw."
  (let ((node (and synaxis-search--ewoc
                   (ewoc-locate synaxis-search--ewoc))))
    (when node (ewoc-invalidate synaxis-search--ewoc node))))

(defun synaxis-search-show-entry ()
  "Open the entry at point in the show buffer."
  (interactive)
  (let ((id (synaxis-search-current-entry)))
    (when id
      (require 'synaxis-show)
      (synaxis-show-entry id)
      (synaxis-search--redraw-current))))

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
