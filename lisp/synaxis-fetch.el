;;; synaxis-fetch.el --- Feed fetching  -*- lexical-binding: t; -*-

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

;; Async feed retrieval via `url-queue-retrieve' with conditional GET,
;; dispatching to the parser and the database.  Fresh inserts are
;; tagged `unread' and announced through `synaxis-new-entry-hook'.

;;; Code:

(require 'url-queue)
(require 'synaxis-db)
(require 'synaxis-parse)

;;; Customisation

(defcustom synaxis-fetch-max-parallel 4
  "Maximum number of in-flight feed fetches.
Bound around the `url-queue-retrieve' call as
`url-queue-parallel-processes'."
  :type 'integer
  :group 'synaxis)

(defcustom synaxis-fetch-timeout 30
  "Per-request timeout in seconds, bound as `url-queue-timeout'."
  :type 'integer
  :group 'synaxis)

;;; Hooks

(defvar synaxis-new-entry-parse-hook nil
  "Functions run on each entry just after parsing.
Each function takes one entry plist and must return a (possibly
modified) plist, or nil to skip the entry.")

(defvar synaxis-new-entry-hook nil
  "Functions run when a brand-new entry is inserted.
Each function takes the new entry's id.  Not called when an existing
entry is updated.")

;;; In-flight tracking

(defvar synaxis-fetch--in-flight (make-hash-table :test 'equal)
  "Set of feed URLs currently being fetched.")

(defun synaxis-fetch--in-flight-p (url)
  "Non-nil if URL is currently being fetched."
  (gethash url synaxis-fetch--in-flight))

;;; Response parsing

(defun synaxis-fetch--parse-response (buffer)
  "Parse BUFFER as an HTTP response.
Return a plist with `:status' (integer), `:headers' (alist of
lowercased keys), and `:body' (string)."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((sep-end    (and (re-search-forward "\r?\n\r?\n" nil t)
                              (match-end 0)))
             (head-end   (and sep-end (match-beginning 0)))
             (head-text  (and head-end
                              (buffer-substring-no-properties
                               (point-min) head-end)))
             (body       (if sep-end
                             (buffer-substring-no-properties
                              sep-end (point-max))
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
             status headers)
        (when head-text
          (let ((lines (split-string head-text "\r?\n")))
            (when (string-match "\\` *HTTP/[0-9.]+ \\([0-9]+\\)" (car lines))
              (setq status (string-to-number (match-string 1 (car lines)))))
            (dolist (line (cdr lines))
              (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*?\\)\\'" line)
                (push (cons (downcase (match-string 1 line))
                            (match-string 2 line))
                      headers)))))
        (list :status status
              :headers (nreverse headers)
              :body body)))))

(defun synaxis-fetch--decode-body (body _headers)
  "Return BODY as a multibyte string for parsing.
v0.1 assumes UTF-8 for unibyte input and passes multibyte through."
  (if (multibyte-string-p body)
      body
    (decode-coding-string body 'utf-8)))

;;; Hook plumbing

(defun synaxis-fetch--apply-parse-hook (entry)
  "Pass ENTRY through `synaxis-new-entry-parse-hook'.
Each hook function may return a modified plist or nil to skip."
  (cl-loop with current = entry
           for fn in synaxis-new-entry-parse-hook
           while current
           do (setq current (funcall fn current))
           finally return current))

;;; Top-level pipeline

(defun synaxis-fetch--process-response (buffer url)
  "Process the HTTP response in BUFFER for feed URL.
Updates the DB, runs hooks, and tracks cache headers and failures."
  (let* ((resp    (synaxis-fetch--parse-response buffer))
         (status  (plist-get resp :status))
         (headers (plist-get resp :headers)))
    (unless (synaxis-db-get-feed url)
      (synaxis-db-add-feed url))
    (cond
     ((eql status 304)
      (synaxis-db-set-feed-cache-headers
       url (list :last-fetched (float-time))))
     ((and (integerp status) (>= status 200) (< status 300))
      (synaxis-fetch--ingest url (plist-get resp :body) headers))
     (t
      (synaxis-fetch--record-failure url)))))

(defun synaxis-fetch--record-failure (url)
  "Increment URL's `failures' counter and stamp `last_fetched'."
  (let* ((feed (synaxis-db-get-feed url))
         (failures (1+ (or (plist-get feed :failures) 0))))
    (synaxis-db-set-feed-cache-headers
     url (list :last-fetched (float-time)
               :failures failures))))

(defun synaxis-fetch--ingest (url body headers)
  "Parse BODY as a feed for URL and upsert entries.
HEADERS' ETag and Last-Modified are persisted on success."
  (condition-case _err
      (let* ((parsed  (synaxis-parse-string
                       (synaxis-fetch--decode-body body headers)))
             (entries (plist-get parsed :entries))
             (etag    (cdr (assoc "etag" headers)))
             (lm      (cdr (assoc "last-modified" headers))))
        (synaxis-db-set-feed-title-if-empty url (plist-get parsed :title))
        (dolist (raw entries)
          (let ((entry (synaxis-fetch--apply-parse-hook raw)))
            (when entry
              (setq entry (plist-put entry :feed-url url))
              (let* ((source-id (plist-get entry :source-id))
                     (existing  (synaxis-db-find-entry url source-id))
                     (id        (synaxis-db-upsert-entry entry)))
                (unless existing
                  (synaxis-db-add-tag id "unread")
                  (run-hook-with-args 'synaxis-new-entry-hook id))))))
        (synaxis-db-set-feed-cache-headers
         url (list :last-fetched (float-time)
                   :etag etag
                   :last-modified lm
                   :failures 0)))
    (error
     (synaxis-fetch--record-failure url))))

;;; Public fetch entry points

(defun synaxis-fetch-feed (url)
  "Asynchronously fetch URL via `url-queue-retrieve'.
No-op if a fetch for URL is already in flight."
  (unless (synaxis-fetch--in-flight-p url)
    (let* ((feed (synaxis-db-get-feed url))
           (etag (and feed (plist-get feed :etag)))
           (lm   (and feed (plist-get feed :last-modified)))
           (url-request-extra-headers
            (delq nil
                  (list (and etag (cons "If-None-Match" etag))
                        (and lm   (cons "If-Modified-Since" lm)))))
           (url-queue-parallel-processes synaxis-fetch-max-parallel)
           (url-queue-timeout synaxis-fetch-timeout))
      (puthash url t synaxis-fetch--in-flight)
      (url-queue-retrieve url #'synaxis-fetch--callback (list url) t t))))

(defun synaxis-fetch--callback (_status url)
  "Callback for `url-queue-retrieve' bound to URL."
  (let ((buf (current-buffer)))
    (unwind-protect
        (synaxis-fetch--process-response buf url)
      (remhash url synaxis-fetch--in-flight)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun synaxis-fetch-all ()
  "Kick off `synaxis-fetch-feed' for every known feed."
  (dolist (feed (synaxis-db-list-feeds))
    (synaxis-fetch-feed (plist-get feed :url))))

(provide 'synaxis-fetch)
;;; synaxis-fetch.el ends here
