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

(require 'shr)
(require 'keymap-popup)
(require 'synaxis-db)

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
     ((equal ctype "text")
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
  :group "Entry"
  "g" ("Reload"   synaxis-show-revert)
  "q" ("Quit"     quit-window))

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

(defun synaxis-show-entry (entry-id)
  "Display the entry with database id ENTRY-ID."
  (let ((entry (synaxis-db-get-entry entry-id)))
    (unless entry
      (user-error "No entry with id %s" entry-id))
    (let ((buf (get-buffer-create "*synaxis-show*")))
      (with-current-buffer buf
        (unless (derived-mode-p 'synaxis-show-mode)
          (synaxis-show-mode))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (funcall synaxis-show-display-function entry)
          (goto-char (point-min)))
        (setq synaxis-show--entry-id entry-id))
      (synaxis-db-remove-tag entry-id "unread")
      (display-buffer buf))))

(provide 'synaxis-show)
;;; synaxis-show.el ends here
