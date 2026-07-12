;;; synaxis-ol.el --- Org-link integration for synaxis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>
;; Keywords: news, hypermedia, rss, atom
;; URL: https://git.thanosapollo.org/synaxis

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

;; Org-link type `synaxis:' for storing and following links to feed
;; entries.  The link path is the entry's original article URL, which
;; doubles as a stable identifier across machines and as a graceful
;; `browse-url' fallback when the entry is not in the local DB.
;;
;; Format: synaxis:ARTICLE-URL
;; Example: [[synaxis:https://hackaday.com/2026/05/15/foo][Hackaday: foo]]

;;; Code:

(require 'cl-lib)
(require 'ol)
(require 'tabulated-list)
(require 'synaxis-db)

(declare-function synaxis-show-entry "synaxis-show" (entry-id &optional peers))

(defvar synaxis-show--entry-id)
(defvar synaxis-scrape-test--entries)

;;; Helpers

(defun synaxis-ol--entry-at-point ()
  "Return the entry plist at point, or nil.
Picks the source per current mode: DB lookup in `synaxis-search-mode',
the buffer-local preview store in `synaxis-scrape-test-mode', and the
show buffer's recorded id in `synaxis-show-mode'."
  (cond
   ((derived-mode-p 'synaxis-scrape-test-mode)
    (and-let* ((id (tabulated-list-get-id)))
      (cl-find id synaxis-scrape-test--entries
               :key (lambda (e) (plist-get e :id))
               :test #'equal)))
   ((derived-mode-p 'synaxis-search-mode)
    (and-let* ((id (tabulated-list-get-id)))
      (synaxis-db-get-entry id)))
   ((derived-mode-p 'synaxis-show-mode)
    (and synaxis-show--entry-id
         (synaxis-db-get-entry synaxis-show--entry-id)))))

(defun synaxis-ol--description (entry)
  "Format ENTRY as an org-link description: FEED: TITLE."
  (format "%s: %s"
          (or (plist-get entry :feed-title) "synaxis")
          (or (plist-get entry :title) "(untitled)")))

;;; Store / follow / export

;;;###autoload
(defun synaxis-ol-store-link (&optional _interactive)
  "Store an org link to the synaxis entry at point.
Active in `synaxis-search-mode', `synaxis-show-mode', and
`synaxis-scrape-test-mode'."
  (when (derived-mode-p 'synaxis-search-mode 'synaxis-show-mode
                        'synaxis-scrape-test-mode)
    (and-let* ((entry (synaxis-ol--entry-at-point))
               (link  (plist-get entry :link)))
      (org-link-store-props
       :type "synaxis"
       :link (concat "synaxis:" link)
       :description (synaxis-ol--description entry)))))

;;;###autoload
(defun synaxis-ol-follow (path _prefix)
  "Follow a synaxis: link.  PATH is the original article URL.
If PATH matches an entry in the DB, open it in `synaxis-show';
otherwise hand off to `browse-url' so the link still resolves on
machines that do not have the entry cached locally."
  (require 'synaxis-show)
  (if-let* ((row (car (synaxis-db-list-entries "e.link = ?" (list path) 1))))
      (synaxis-show-entry (plist-get row :id))
    (browse-url path)))

;;;###autoload
(defun synaxis-ol-export (path desc backend _channel)
  "Export a synaxis: link to BACKEND.
PATH is the article URL; DESC the user-visible label (falls back
to PATH)."
  (let ((desc (or desc path)))
    (pcase backend
      ('html  (format "<a href=\"%s\">%s</a>" path desc))
      ('md    (format "[%s](%s)" desc path))
      ('latex (format "\\href{%s}{%s}" path desc))
      ('ascii desc)
      (_      desc))))

;;;###autoload
(org-link-set-parameters "synaxis"
                         :store #'synaxis-ol-store-link
                         :follow #'synaxis-ol-follow
                         :export #'synaxis-ol-export)

(provide 'synaxis-ol)
;;; synaxis-ol.el ends here
