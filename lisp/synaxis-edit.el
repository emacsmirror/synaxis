;;; synaxis-edit.el --- Interactive feed editor  -*- lexical-binding: t; -*-

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

;; A keymap-popup-driven editor for feed metadata and scrape rules.
;; Pressing a key shows the current value, prompts for a new one,
;; saves to the DB, and re-opens the popup so the change is visible.
;;
;; Patterns lifted from yeetube and emacs-jabber: dynamic-value
;; description lambdas with `font-lock-constant-face', `:if'
;; predicates hiding irrelevant rows, tail-call to `keymap-popup' so
;; the menu stays "live."

;;; Code:

(require 'cl-lib)
(require 'keymap-popup)
(require 'synaxis-db)
(require 'synaxis-filter)

(declare-function synaxis-scrape-test "synaxis-scrape" (url &rest rules))

(defvar crm-separator)
(defvar synaxis-edit-map)

;;; State

(defvar synaxis-edit--current-url nil
  "URL of the feed currently being edited.
Bound by `synaxis-edit-feed' for the duration of the popup session
and cleared by `synaxis-edit-quit'.")

;;; Predicates

(defun synaxis-edit--scrape-feed-p ()
  "Non-nil if the current feed has type=scrape."
  (and synaxis-edit--current-url
       (let ((feed (synaxis-db-get-feed synaxis-edit--current-url)))
         (and feed (string= "scrape" (plist-get feed :type))))))

(defun synaxis-edit--has-rule-p (key)
  "Non-nil when the current scrape rule has KEY set."
  (and (synaxis-edit--scrape-feed-p)
       (plist-get (synaxis-db-get-scrape-rule synaxis-edit--current-url)
                  key)))

;;; Field formatters

(defun synaxis-edit--field-line (label value)
  "Format LABEL: VALUE for popup descriptions.
Whitespace on both sides of VALUE is trimmed for display only; the
underlying stored value is untouched."
  (format "%s: %s"
          label
          (if value
              (propertize (string-trim (format "%s" value))
                          'face 'font-lock-constant-face)
            (propertize "(unset)" 'face 'shadow))))

(defun synaxis-edit--rule-line (label key)
  "Format scrape-rule KEY as a field line under LABEL."
  (synaxis-edit--field-line
   label
   (plist-get (synaxis-db-get-scrape-rule synaxis-edit--current-url) key)))

(defun synaxis-edit--format-autotags ()
  "Render autotags from the current feed as a space-joined string."
  (let* ((meta (plist-get (synaxis-db-get-feed synaxis-edit--current-url) :meta))
         (tags (append (plist-get meta :autotags) nil)))
    (if tags (string-join tags " ") nil)))

;;; Set-* functions

(defun synaxis-edit--prompt-rule (key prompt)
  "Read a value for rule KEY from the user with PROMPT and save it."
  (let* ((current (plist-get (synaxis-db-get-scrape-rule
                              synaxis-edit--current-url)
                             key))
         (new (read-string (format "%s: " prompt) (or current ""))))
    (synaxis-db-update-scrape-rule-field
     synaxis-edit--current-url
     key
     (if (string-empty-p new) nil new)))
  (keymap-popup synaxis-edit-map))

(defun synaxis-edit-set-title ()
  "Set the title of the feed being edited."
  (interactive)
  (let* ((current (plist-get (synaxis-db-get-feed
                              synaxis-edit--current-url)
                             :title))
         (new (read-string "Title: " (or current ""))))
    (synaxis-db-set-feed-title
     synaxis-edit--current-url
     (if (string-empty-p new) nil new)))
  (keymap-popup synaxis-edit-map))

(defun synaxis-edit-set-autotags ()
  "Set the autotags list (CRM with `,' separator)."
  (interactive)
  (let* ((meta (plist-get (synaxis-db-get-feed
                           synaxis-edit--current-url)
                          :meta))
         (current (append (plist-get meta :autotags) nil))
         (crm-separator ",")
         (new (completing-read-multiple
               "Autotags: " (synaxis-filter--db-tags) nil nil
               (string-join current ","))))
    (synaxis-db-set-feed-autotags synaxis-edit--current-url new))
  (keymap-popup synaxis-edit-map))

(defun synaxis-edit-set-url-selector ()
  "Set the rule's url-selector."
  (interactive)
  (synaxis-edit--prompt-rule :url-selector "URL selector"))

(defun synaxis-edit-set-url-pattern ()
  "Set the rule's url-pattern regex."
  (interactive)
  (synaxis-edit--prompt-rule :url-pattern "URL pattern"))

(defun synaxis-edit-set-content-selector ()
  "Set the rule's content-selector."
  (interactive)
  (synaxis-edit--prompt-rule :content-selector "Content selector"))

(defun synaxis-edit-set-content-cleanup ()
  "Set the rule's content-cleanup selectors."
  (interactive)
  (synaxis-edit--prompt-rule :content-cleanup "Content cleanup"))

(defun synaxis-edit-set-title-cleanup ()
  "Set the rule's title-cleanup string."
  (interactive)
  (synaxis-edit--prompt-rule :title-cleanup "Title cleanup"))

(defun synaxis-edit-set-date-selector ()
  "Set the rule's date-selector."
  (interactive)
  (synaxis-edit--prompt-rule :date-selector "Date selector"))

(defun synaxis-edit-set-date-format ()
  "Set the rule's date-format."
  (interactive)
  (synaxis-edit--prompt-rule :date-format "Date format"))

(defun synaxis-edit-set-limit ()
  "Set the rule's limit (integer or empty for nil)."
  (interactive)
  (let* ((current (plist-get (synaxis-db-get-scrape-rule
                              synaxis-edit--current-url)
                             :limit))
         (new (read-string "Limit: " (and current (format "%s" current)))))
    (synaxis-db-update-scrape-rule-field
     synaxis-edit--current-url
     :limit
     (if (string-empty-p new) nil (string-to-number new))))
  (keymap-popup synaxis-edit-map))

;;; Actions

(defun synaxis-edit-test ()
  "Re-run `synaxis-scrape-test' against the currently-saved rule."
  (interactive)
  (require 'synaxis-scrape)
  (let* ((url synaxis-edit--current-url)
         (rule (synaxis-db-get-scrape-rule url))
         (args (cl-loop for (k v) on rule by #'cddr
                        when v append (list k v))))
    (apply #'synaxis-scrape-test url args)))

(defun synaxis-edit-quit ()
  "Close the editor and clear the current URL."
  (interactive)
  (setq synaxis-edit--current-url nil))

;;; Popup

(keymap-popup-define synaxis-edit-map
  "Edit a feed's title, autotags, and scrape rule."
  :description
  (lambda ()
    (if synaxis-edit--current-url
        (format "Edit %s"
                (propertize (string-trim synaxis-edit--current-url)
                            'face 'font-lock-string-face))
      "Edit (no feed selected)"))

  :group "Feed"
  "n" ((lambda ()
         (synaxis-edit--field-line
          "Title"
          (plist-get (synaxis-db-get-feed synaxis-edit--current-url)
                     :title)))
       synaxis-edit-set-title)
  "a" ((lambda ()
         (synaxis-edit--field-line "Autotags"
                                   (synaxis-edit--format-autotags)))
       synaxis-edit-set-autotags)

  :group "Rule"
  "u" ((lambda () (synaxis-edit--rule-line "URL selector" :url-selector))
       synaxis-edit-set-url-selector
       :if (lambda () (synaxis-edit--scrape-feed-p)))
  "p" ((lambda () (synaxis-edit--rule-line "URL pattern" :url-pattern))
       synaxis-edit-set-url-pattern
       :if (lambda () (synaxis-edit--scrape-feed-p)))
  "c" ((lambda () (synaxis-edit--rule-line "Content selector" :content-selector))
       synaxis-edit-set-content-selector
       :if (lambda () (synaxis-edit--scrape-feed-p)))
  "C" ((lambda () (synaxis-edit--rule-line "Content cleanup" :content-cleanup))
       synaxis-edit-set-content-cleanup
       :if (lambda () (synaxis-edit--has-rule-p :content-selector)))
  "T" ((lambda () (synaxis-edit--rule-line "Title cleanup" :title-cleanup))
       synaxis-edit-set-title-cleanup
       :if (lambda () (synaxis-edit--scrape-feed-p)))
  "d" ((lambda () (synaxis-edit--rule-line "Date selector" :date-selector))
       synaxis-edit-set-date-selector
       :if (lambda () (synaxis-edit--scrape-feed-p)))
  "D" ((lambda () (synaxis-edit--rule-line "Date format" :date-format))
       synaxis-edit-set-date-format
       :if (lambda () (synaxis-edit--has-rule-p :date-selector)))
  "L" ((lambda () (synaxis-edit--rule-line "Limit" :limit))
       synaxis-edit-set-limit
       :if (lambda () (synaxis-edit--scrape-feed-p)))

  :group "Actions"
  "t" ("Test current rules" synaxis-edit-test
       :if (lambda () (synaxis-edit--scrape-feed-p)))
  "q" ("Quit" synaxis-edit-quit))

;;; Entry point

(defun synaxis-edit--read-feed-url ()
  "Prompt for a feed URL via `completing-read'."
  (let ((urls (mapcar (lambda (f) (plist-get f :url))
                      (synaxis-db-list-feeds))))
    (unless urls (user-error "No feeds in the DB"))
    (completing-read "Edit feed: " urls nil t)))

;;;###autoload
(defun synaxis-edit-feed (&optional url)
  "Open the feed editor on a feed URL.
With no URL, prompts via `completing-read'."
  (interactive (list (synaxis-edit--read-feed-url)))
  (setq synaxis-edit--current-url url)
  (keymap-popup synaxis-edit-map))

(provide 'synaxis-edit)
;;; synaxis-edit.el ends here
