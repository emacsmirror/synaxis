;;; synaxis-show.el --- Entry display buffer  -*- lexical-binding: t; -*-

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

;; The entry display buffer: renders one entry into `*synaxis-show*'
;; via a configurable display function (`shr' by default).  Opening
;; an entry through `synaxis-show-entry' clears its `unread' tag.

;;; Code:

(require 'cl-lib)
(require 'shr)
(require 'keymap-popup)
(require 'synaxis-db)

(declare-function synaxis-search-refresh "synaxis-search" ())

;;; Customisation

(defcustom synaxis-show-display-function #'synaxis-show--render-shr
  "Function called to render an entry into the current buffer.
The function takes one argument, the entry plist."
  :type 'function
  :group 'synaxis)

;;; Faces

(defface synaxis-show-title-face
  '((t :inherit variable-pitch :weight bold :height 1.3))
  "Face for the entry title in the show buffer."
  :group 'synaxis)

(defface synaxis-show-meta-face
  '((t :inherit shadow))
  "Face for entry metadata (feed title, date) in the show buffer."
  :group 'synaxis)

;;; Buffer-local state

(defvar-local synaxis-show--entry-id nil
  "Database id of the entry currently displayed.")

(defvar-local synaxis-show--peers nil
  "List of entry ids in the originating list view.
Order matches the displayed order at the time of opening.  Stale
if the originating filter changes after opening; the user can
press `g' on the list buffer and reopen to refresh.")

(defvar-local synaxis-show--current-link nil
  "Original article URL for the entry shown in this buffer.
Set by `synaxis-show--render-into-buffer' from the entry's
`:link' so `synaxis-show-browse-entry' works in both DB-backed
and plist-preview render paths.")

;;; Rendering

(defun synaxis-show--render-shr (entry)
  "Insert ENTRY's metadata and content into the current buffer."
  (insert (propertize (or (plist-get entry :title) "(untitled)")
                      'face 'synaxis-show-title-face)
          "\n"
          (propertize
           (format "%s -- %s"
                   (or (plist-get entry :feed-title) "")
                   (format-time-string
                    "%Y-%m-%d %H:%M"
                    (seconds-to-time (or (plist-get entry :date) 0))))
           'face 'synaxis-show-meta-face)
          "\n\n")
  (let ((content (plist-get entry :content))
        (ctype   (plist-get entry :content-type)))
    (cond
     ((null content)
      (insert (propertize "(no content)" 'face 'synaxis-show-meta-face)))
     ((string= ctype "text")
      (insert content))
     (t
      (let ((shr-use-fonts nil)
            (shr-width nil)
            (start (point)))
        (insert content)
        (shr-render-region start (point)))))))

;;; Keymap

(keymap-popup-define synaxis-show-mode-map
  "Keymap for `synaxis-show-mode'."
  :description
  (lambda ()
    (if-let* ((id synaxis-show--entry-id)
              (entry (synaxis-db-get-entry id)))
        (let* ((title (or (plist-get entry :title) "(untitled)"))
               (feed  (or (plist-get entry :feed-title) ""))
               (idx (or (and-let* ((peers synaxis-show--peers)
                                   (pos (cl-position id peers)))
                          (format " [%d/%d]" (1+ pos) (length peers)))
                        "")))
          (format "%s%s: %s"
                  (propertize feed 'face 'font-lock-type-face)
                  idx
                  (propertize title 'face 'font-lock-keyword-face)))
      "synaxis-show (no entry)"))
  :group "Navigate"
  "n" ("Next entry"     synaxis-show-next-entry)
  "p" ("Previous entry" synaxis-show-prev-entry)
  :group "Entry"
  "g" ("Reload"         synaxis-show-revert)
  "b" ("Browse URL"     synaxis-show-browse-entry)
  "q" ("Quit"           quit-window))

;;; Mode

(define-derived-mode synaxis-show-mode special-mode "Synaxis-Show"
  "Major mode for the synaxis entry display buffer."
  (setq-local revert-buffer-function (lambda (&rest _) (synaxis-show-revert))))

;;; Commands

(defun synaxis-show-revert ()
  "Re-render the entry currently shown in this buffer."
  (interactive)
  (when synaxis-show--entry-id
    (synaxis-show-entry synaxis-show--entry-id)))

(defun synaxis-show--render-into-buffer (entry entry-id peers)
  "Render ENTRY into `*synaxis-show*' and seed its buffer-local state.
ENTRY-ID and PEERS are stored as-is (either may be nil for preview
paths with no DB row).  Returns the buffer."
  (let ((buf (get-buffer-create "*synaxis-show*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'synaxis-show-mode)
        (synaxis-show-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall synaxis-show-display-function entry)
        (goto-char (point-min)))
      (setq synaxis-show--entry-id entry-id
            synaxis-show--peers peers
            synaxis-show--current-link (plist-get entry :link)))
    buf))

(defun synaxis-show-entry (entry-id &optional peers)
  "Display the entry with database id ENTRY-ID.
When PEERS is non-nil it is stored buffer-local so that `n' / `p'
in the show buffer can navigate between sibling entries; otherwise
any peers already recorded in the show buffer are preserved."
  (let* ((entry (or (synaxis-db-get-entry entry-id)
                    (user-error "No entry with id %s" entry-id)))
         (existing (and-let* ((buf (get-buffer "*synaxis-show*")))
                     (buffer-local-value 'synaxis-show--peers buf)))
         (buf (synaxis-show--render-into-buffer
               entry entry-id (or peers existing))))
    (synaxis-db-remove-tag entry-id "unread")
    (pop-to-buffer buf)))

(defun synaxis-show-entry-plist (entry)
  "Display ENTRY plist in `*synaxis-show*' without DB lookup or side effects.
Used by scrape-test and other preview paths that have an entry
plist in hand but no DB row to refer to."
  (pop-to-buffer (synaxis-show--render-into-buffer entry nil nil)))

(defun synaxis-show--sync-list (id)
  "Refresh `*synaxis*' and move point to the row whose id is ID.
No-op if the buffer is gone or not in `synaxis-search-mode'.  If ID
is filtered out after refresh, point stays where the refresh landed."
  (and-let* ((buf (get-buffer "*synaxis*"))
             ((buffer-live-p buf)))
    (with-current-buffer buf
      (when (derived-mode-p 'synaxis-search-mode)
        (require 'synaxis-search)
        (synaxis-search-refresh)
        (goto-char (point-min))
        (while (and (not (eobp))
                    (not (equal id (tabulated-list-get-id))))
          (forward-line 1))
        (when (eobp) (goto-char (point-min)))
        (when-let* ((win (get-buffer-window buf 'visible)))
          (set-window-point win (point)))))))

(defun synaxis-show--walk (delta)
  "Show the peer entry at DELTA from the current one.
DELTA is +1 (next) or -1 (previous).  Errors at the ends or when
the buffer has no recorded peers.  After navigating, refreshes the
originating `*synaxis*' buffer and lands point on the new entry."
  (let* ((peers (or synaxis-show--peers
                    (user-error "Not in a navigable view")))
         (target (+ (cl-position synaxis-show--entry-id peers) delta)))
    (unless (and (<= 0 target) (< target (length peers)))
      (user-error "No more entries"))
    (let ((new-id (nth target peers)))
      (synaxis-show-entry new-id peers)
      (synaxis-show--sync-list new-id))))

(defun synaxis-show-next-entry ()
  "Show the next entry in the originating list view."
  (interactive)
  (synaxis-show--walk +1))

(defun synaxis-show-prev-entry ()
  "Show the previous entry in the originating list view."
  (interactive)
  (synaxis-show--walk -1))

(defun synaxis-show-browse-entry ()
  "Open the current entry's article URL in a browser."
  (interactive)
  (if synaxis-show--current-link
      (browse-url synaxis-show--current-link)
    (user-error "No link for this entry")))

(provide 'synaxis-show)
;;; synaxis-show.el ends here
