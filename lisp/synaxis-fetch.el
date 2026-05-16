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

(declare-function synaxis-scrape-feed "synaxis-scrape" (url))

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

(defcustom synaxis-http-request-headers
  '(("Accept-Language" . "en-US,en;q=0.9")
    ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0")
    ("Cookie" . "ucbcb=1; gdpr=1; cookieconsent_status=allow"))
  "HTTP headers sent by feed fetch and scrape requests.
Bound as `url-request-extra-headers' around each call.  Defaults
impersonate a recent Firefox and pre-set common EU-cookie-consent
bypass values (`ucbcb=1', `gdpr=1', `cookieconsent_status=allow')
so consent gates are skipped on the major CMPs.  Adjust
`Accept-Language' to bias content to your locale (e.g.
\"el-GR,en;q=0.5\")."
  :type '(alist :key-type string :value-type string)
  :group 'synaxis)

;;; Hooks

(defvar synaxis-new-entry-parse-hook nil
  "Functions run on each entry just after parsing.
Each function takes one entry plist and must return a (possibly
modified) plist, or nil to skip the entry.")

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

;;; Hook plumbing

(defun synaxis-fetch--apply-parse-hook (entry)
  "Pass ENTRY through `synaxis-new-entry-parse-hook'.
Each hook function may return a modified plist or nil to skip."
  (seq-reduce (lambda (e fn) (and e (funcall fn e)))
              synaxis-new-entry-parse-hook
              entry))

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

(defun synaxis-fetch--body-as-string (body)
  "Return BODY as a multibyte string, decoding unibyte as UTF-8."
  (if (multibyte-string-p body) body
    (decode-coding-string body 'utf-8)))

(defun synaxis-fetch--ingest (url body headers)
  "Parse BODY as a feed for URL and upsert entries.
HEADERS' ETag and Last-Modified are persisted on success.
Tags listed in the feed's `meta.autotags' are applied to each
fresh insert in addition to `unread'."
  (condition-case _err
      (let* ((parsed   (synaxis-parse-string
                        (synaxis-fetch--body-as-string body)))
             (feed     (synaxis-db-get-feed url))
             (autotags (append (plist-get (plist-get feed :meta) :autotags)
                               nil)))
        (synaxis-db-set-feed-title-if-empty url (plist-get parsed :title))
        (cl-loop for raw in (plist-get parsed :entries)
                 for entry = (synaxis-fetch--apply-parse-hook raw)
                 when entry
                 do (synaxis-db-upsert-with-tags url autotags entry))
        (synaxis-db-set-feed-cache-headers
         url (list :last-fetched (float-time)
                   :etag (cdr (assoc "etag" headers))
                   :last-modified (cdr (assoc "last-modified" headers))
                   :failures 0)))
    (error
     (synaxis-fetch--record-failure url))))

;;; Public fetch entry points

(defun synaxis-fetch-feed (url)
  "Asynchronously fetch URL via `url-queue-retrieve'.
No-op if a fetch for URL is already in flight.

Conditional GET is intentionally NOT sent: `url-http' unconditionally
calls `url-cache-extract' on a 304 response, which errors when the
url-cache directory has not been populated (synaxis stores ETags in
its own DB, not in url-cache).  We still record the response's ETag
and Last-Modified for a future, custom HTTP path."
  (unless (synaxis-fetch--in-flight-p url)
    (let* ((feed (synaxis-db-get-feed url))
           (type (and feed (plist-get feed :type))))
      (cond
       ((equal type "scrape")
        (require 'synaxis-scrape)
        (condition-case err
            (synaxis-scrape-feed url)
          (error
           (message "synaxis: scrape failed for %s: %S" url err)
           (synaxis-fetch--record-failure url))))
       (t
        (let ((url-queue-parallel-processes synaxis-fetch-max-parallel)
              (url-queue-timeout             synaxis-fetch-timeout)
              (url-request-extra-headers     synaxis-http-request-headers))
          (puthash url t synaxis-fetch--in-flight)
          (url-queue-retrieve url #'synaxis-fetch--callback (list url) t t)))))))

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
