;;; synaxis.el --- Feed reader with SQLite storage  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>
;; Keywords: news, hypermedia, rss, atom
;; URL: https://codeberg.org/thanosapollo/emacs-synaxis
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (keymap-popup "0.2.1"))

;; This file is NOT part of GNU Emacs.

;; Assisted-by: Hermes:multi-model

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
  :link '(url-link "https://codeberg.org/thanosapollo/emacs-synaxis"))

(defconst synaxis-version "0.1.0"
  "Current synaxis version.")

(require 'cl-lib)
(require 'synaxis-db)
(require 'synaxis-fetch)
(require 'synaxis-filter)
(require 'synaxis-search)
(require 'synaxis-show)

;;; Background update timer

(defcustom synaxis-update-interval nil
  "Seconds between automatic feed updates, or nil to disable.
Set to e.g. 1800 (30 minutes) to fetch in the background while
synaxis is open.  Changes take effect the next time `synaxis'
runs."
  :type '(choice (const :tag "Disabled" nil)
                 (natnum :tag "Seconds"))
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defvar synaxis--update-timer nil
  "Active timer scheduled by `synaxis--update-maybe-start-timer'.")

(defun synaxis--update-cancel-timer ()
  "Cancel any active autoupdate timer."
  (when synaxis--update-timer
    (cancel-timer synaxis--update-timer)
    (setq synaxis--update-timer nil)))

(defun synaxis--update-background ()
  "Fetch all due feeds.  No-op when no feeds are registered.
Honours failure backoff so chronically-failing feeds are not
re-fetched every cycle (see `synaxis-fetch--due-p')."
  (when (synaxis-db-list-feeds)
    (synaxis-fetch-all t)))

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

;;; Tag rules

(defcustom synaxis-tag-rules nil
  "Rules auto-applied to entries on insert and via `synaxis-tag-rules-apply-all'.
Each rule is a plist with keys:
  :filter STRING -- filter expression (see `synaxis-filter-parse')
  :add    LIST   -- tag strings to add to matching entries (optional)
  :remove LIST   -- tag strings to remove from matching entries (optional)

Rules fire on every fresh entry insert when Synaxis is loaded.
Run `synaxis-tag-rules-apply-all' to back-apply across the whole DB.

Values with whitespace must be double-quoted so they parse as one
token: `feed:\"PubMed Trending\"', `tag:\"slow read\"'.

Example:
  (setq synaxis-tag-rules
        \\='((:filter \"feed:hackaday\" :add (\"hardware\"))
          (:filter \"feed:\\\"PubMed Trending\\\"\" :add (\"medicine\"))
          (:filter \"feed:promo\"    :remove (\"unread\"))
          (:filter \"title:rust\"    :add (\"rust\") :remove (\"later\"))))"
  :type '(repeat (plist :options
                        ((:filter (string :tag "Filter"))
                         (:add    (repeat (string :tag "Tag to add")))
                         (:remove (repeat (string :tag "Tag to remove"))))))
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

(defun synaxis-tag-rules--filter-sql (filter)
  "Compile FILTER to (WHERE . PARAMS)."
  (let ((c (synaxis-filter-compile (synaxis-filter-parse filter))))
    (cons (plist-get c :where) (plist-get c :params))))

(defun synaxis-tag-rules--matching-ids (filter)
  "Return ids of entries matching FILTER, or nil."
  (pcase-let ((`(,where . ,params) (synaxis-tag-rules--filter-sql filter)))
    (mapcar #'car
            (sqlite-select (synaxis-db--ensure-open)
                           (concat "SELECT e.id FROM entries e
                                    JOIN feeds f ON f.url = e.feed_url
                                    WHERE " where ";")
                           params))))

(defun synaxis-tag-rules--rule-matches-entry-p (rule entry-id)
  "Non-nil if RULE's :filter matches ENTRY-ID."
  (pcase-let ((`(,where . ,params)
               (synaxis-tag-rules--filter-sql (plist-get rule :filter))))
    (sqlite-select (synaxis-db--ensure-open)
                   (concat "SELECT 1 FROM entries e
                            JOIN feeds f ON f.url = e.feed_url
                            WHERE (" where ") AND e.id = ?
                            LIMIT 1;")
                   (append params (list entry-id)))))

(defun synaxis-tag-rules-apply-entry (entry-id)
  "Apply each rule in `synaxis-tag-rules' to ENTRY-ID."
  (dolist (rule synaxis-tag-rules)
    (let ((add    (plist-get rule :add))
          (remove (plist-get rule :remove)))
      (when (and (or add remove)
                 (synaxis-tag-rules--rule-matches-entry-p rule entry-id))
        (dolist (tag add)    (synaxis-db-add-tag    entry-id tag))
        (dolist (tag remove) (synaxis-db-remove-tag entry-id tag))))))

(defun synaxis-tag-rules-apply-all ()
  "Apply every rule in `synaxis-tag-rules' across all entries.
Re-adds any :add tag previously removed by hand on matching entries."
  (interactive)
  (unless synaxis-tag-rules (user-error "No rules in synaxis-tag-rules"))
  (when (y-or-n-p
         (format "Apply %d rules across ALL entries? \
This will re-add tags removed by hand on matching entries.  Continue? "
                 (length synaxis-tag-rules)))
    (cl-loop for rule in synaxis-tag-rules
             for add    = (plist-get rule :add)
             for remove = (plist-get rule :remove)
             when (or add remove)
             do (let ((ids (synaxis-tag-rules--matching-ids
                            (plist-get rule :filter))))
                  (cl-loop for tag in add    do (synaxis-db-bulk-add-tag    ids tag))
                  (cl-loop for tag in remove do (synaxis-db-bulk-remove-tag ids tag))))
    (message "synaxis: applied %d rules" (length synaxis-tag-rules))))

(defun synaxis-tag-rules-test (filter)
  "Show how many entries match FILTER plus a small sample.
Read-only; no tags are changed."
  (interactive (list (read-string "Test filter: ")))
  (let* ((c      (synaxis-filter-compile (synaxis-filter-parse filter)))
         (where  (plist-get c :where))
         (params (plist-get c :params))
         (db     (synaxis-db--ensure-open))
         (count  (caar (sqlite-select
                        db
                        (concat "SELECT COUNT(*) FROM entries e
                                 JOIN feeds f ON f.url = e.feed_url
                                 WHERE " where ";")
                        params)))
         (sample (mapcar #'car
                         (sqlite-select
                          db
                          (concat "SELECT e.title FROM entries e
                                   JOIN feeds f ON f.url = e.feed_url
                                   WHERE " where "
                                   ORDER BY e.date DESC LIMIT 5;")
                          params))))
    (message "synaxis: %d match%s%s"
             count
             (if (= count 1) "" "es")
             (if sample (concat ": " (string-join sample "; ")) ""))))

;;; Scrape feed creation

(defconst synaxis-create-feed--rule-keys
  '(:url-selector :url-pattern :content-selector :content-cleanup
                  :title-cleanup :date-selector :date-format :limit)
  "Keyword arguments passed straight to `synaxis-db-add-scrape-rule'.")

(defun synaxis-create-feed--split (args)
  "Split ARGS plist into (FEED-PLIST . RULE-PLIST)."
  (let (feed rule)
    (cl-loop for (k v) on args by #'cddr do
             (if (memq k synaxis-create-feed--rule-keys)
                 (setq rule (plist-put rule k v))
               (setq feed (plist-put feed k v))))
    (cons feed rule)))

;;;###autoload
(defun synaxis-create-feed (url &rest rules)
  "Register URL as a scrape feed with RULES (a keyword plist).
Required: :url-selector.  Optional feed-level keys: :title, :tags.
Optional rule keys: :url-pattern, :content-selector, :content-cleanup,
:title-cleanup, :date-selector, :date-format, :limit."
  (unless (plist-get rules :url-selector)
    (user-error "Missing :url-selector for synaxis-create-feed"))
  (pcase-let* ((`(,feed-args . ,rule-args) (synaxis-create-feed--split rules))
               (title (plist-get feed-args :title))
               (tags  (plist-get feed-args :tags)))
    (synaxis-db-add-feed
     url (append (and title `(:title ,title))
                 '(:type "scrape")
                 (and tags `(:meta (:autotags ,(vconcat tags))))))
    (synaxis-db-add-scrape-rule url rule-args)
    (message "synaxis: scrape feed registered %s" url)))

(declare-function synaxis-scrape-test "synaxis-scrape")

;;;###autoload
(defalias 'synaxis-create-feed-test #'synaxis-scrape-test
  "Preview a scrape rule without registering the feed.
Sibling to `synaxis-create-feed' for discoverability.")

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

;;;###autoload
(defun synaxis-remove-feed (url)
  "Remove the feed at URL after confirmation.
Cascades to its entries and tags."
  (interactive
   (let ((urls (mapcar (lambda (f) (plist-get f :url))
                       (synaxis-db-list-feeds))))
     (unless urls (user-error "No feeds to remove"))
     (list (completing-read "Remove feed: " urls nil t))))
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
