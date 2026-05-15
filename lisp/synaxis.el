;;; synaxis.el --- Feed reader with SQLite storage  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: news, hypermedia, rss, atom
;; URL: https://codeberg.org/thanosapollo/synaxis
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (keymap-popup "0.2.1"))

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

;; synaxis (Greek σύναξις, "gathering") is a small feed reader for
;; Emacs.  It reads RSS, Atom, and JSON feeds and stores them in
;; SQLite -- the single source of truth for all displayed state.
;;
;; Top-level commands:
;;   M-x synaxis            open the entry list buffer
;;   M-x synaxis-add-feed   register a new feed URL
;;   M-x synaxis-update     fetch all feeds asynchronously
;;   M-x synaxis-remove-feed delete a feed and its entries

;;; Code:

(defgroup synaxis nil
  "Feed reader with SQLite storage."
  :group 'applications
  :prefix "synaxis-"
  :link '(url-link "https://codeberg.org/thanosapollo/synaxis"))

(defconst synaxis-version "0.1.0"
  "Current synaxis version.")

(require 'synaxis-db)
(require 'synaxis-fetch)
(require 'synaxis-search)
(require 'synaxis-show)

;;; Background update timer

(defcustom synaxis-update-interval nil
  "Seconds between automatic feed updates, or nil to disable.
Set to e.g. 1800 (30 minutes) to fetch in the background while
synaxis is open.  Changes take effect the next time `synaxis'
runs."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'synaxis)

(defvar synaxis--update-timer nil
  "Active timer scheduled by `synaxis--update-maybe-start-timer'.")

(defun synaxis--update-cancel-timer ()
  "Cancel any active autoupdate timer."
  (when synaxis--update-timer
    (cancel-timer synaxis--update-timer)
    (setq synaxis--update-timer nil)))

(defun synaxis--update-background ()
  "Fetch all feeds without messaging.  No-op when no feeds exist."
  (when (synaxis-db-list-feeds)
    (synaxis-fetch-all)))

(defun synaxis--update-maybe-start-timer ()
  "Schedule the autoupdate timer if configured.
No-op when `synaxis-testing' is non-nil, when
`synaxis-update-interval' is nil, or when a timer is already
running.  Always cancels an existing timer first so a re-call is
idempotent."
  (synaxis--update-cancel-timer)
  (when (and (not synaxis-testing)
             (integerp synaxis-update-interval)
             (> synaxis-update-interval 0))
    (setq synaxis--update-timer
          (run-with-timer synaxis-update-interval
                          synaxis-update-interval
                          #'synaxis--update-background))))

;;; Completing-read wrapper

(defun synaxis-completing-read (prompt collection &rest args)
  "Wrapper around `completing-read' for synaxis prompts.
PROMPT, COLLECTION, and ARGS are passed through.  Defined here so
the user's completion framework is respected uniformly."
  (apply #'completing-read prompt collection args))

;;; Top-level commands

;;;###autoload
(defun synaxis ()
  "Open or switch to the synaxis entry list buffer.
Starts the background autoupdate timer when configured."
  (interactive)
  (synaxis--update-maybe-start-timer)
  (synaxis-search))

;;;###autoload
(defun synaxis-add-feed (url &optional title)
  "Add the feed at URL to the database.
TITLE is optional and only used as the displayed name until a real
fetch overwrites it from the feed's own `<title>'."
  (interactive
   (let* ((u (read-string "Feed URL: "))
          (raw-title (read-string "Title (optional): ")))
     (list u (and (not (string-empty-p raw-title)) raw-title))))
  (synaxis-db-add-feed url (and title (list :title title)))
  (message "synaxis: added %s" url))

(defun synaxis-remove-feed (url)
  "Remove the feed at URL after confirmation.
Cascades to its entries and tags."
  (interactive
   (let ((urls (mapcar (lambda (f) (plist-get f :url))
                       (synaxis-db-list-feeds))))
     (unless urls (user-error "No feeds to remove"))
     (list (synaxis-completing-read "Remove feed: " urls nil t))))
  (when (y-or-n-p (format "Remove %s and all its entries? " url))
    (synaxis-db-remove-feed url)
    (message "synaxis: removed %s" url)
    (when (get-buffer "*synaxis*")
      (with-current-buffer "*synaxis*"
        (synaxis-search-refresh)))))

;;;###autoload
(defun synaxis-update ()
  "Fetch all known feeds asynchronously.
Press `g' in the list buffer to redraw once entries have landed."
  (interactive)
  (let ((n (length (synaxis-db-list-feeds))))
    (when (zerop n) (user-error "No feeds to update"))
    (message "synaxis: updating %d feed%s..."
             n (if (= n 1) "" "s")))
  (synaxis-fetch-all))

;;; Unload

(defun synaxis-unload-function ()
  "Cancel the autoupdate timer and close the database on `unload-feature'."
  (synaxis--update-cancel-timer)
  (synaxis-db-close)
  nil)

(provide 'synaxis)
;;; synaxis.el ends here
