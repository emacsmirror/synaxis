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

;;; Conditional GET workaround

;; Background -- why this advice exists, in case a reviewer asks:
;;
;; `url-http' (Emacs core, lisp/url/url-http.el line ~728) handles a
;; 304 Not Modified response by unconditionally calling
;; (url-cache-extract (url-cache-create-filename (url-view-url t))).
;; That helper does:
;;   (erase-buffer)
;;   (set-buffer-multibyte nil)
;;   (insert-file-contents-literally fnam)
;; with no existence check.
;;
;; Synaxis keeps cache headers (ETag, Last-Modified) in its own
;; SQLite DB and never writes anything to `url-cache-directory'.  So
;; on a 304, `url-cache-extract' tries to read a file that does not
;; exist and raises an error, which surfaces as a fetch failure in
;; our callback even though the conditional GET succeeded.
;;
;; The fix is :around advice that treats a missing cache file as a
;; no-op instead of an error.  When the file exists (i.e. some other
;; package did populate url-cache for this URL) the original
;; behaviour runs unchanged.  When the file is missing we leave the
;; response buffer alone so its HTTP headers remain readable, and
;; `synaxis-fetch--parse-response' detects the 304 from the status
;; line and dispatches our "no new content" branch.
;;
;; Scope: the advice is installed only while we have at least one
;; fetch in flight (see `synaxis-fetch-feed' and
;; `synaxis-fetch--callback') so it does not affect non-synaxis
;; url.el callers during idle periods.
;;
;; cl-letf cannot be used here: `url-queue-retrieve' enqueues the
;; request and returns synchronously, but `url-cache-extract' is
;; called later in url-http's async response handler, by which time
;; cl-letf's dynamic binding has unwound.

(defvar synaxis-fetch--cache-advice-active nil
  "Non-nil while our `url-cache-extract' :around advice is installed.")

(defun synaxis-fetch--url-cache-extract-safe (orig fnam)
  "Around-advice on `url-cache-extract' tolerating a missing cache file.
See the commentary above this function for the full reasoning."
  (if (file-exists-p fnam)
      (funcall orig fnam)
    nil))

(defun synaxis-fetch--cache-advice-toggle (on)
  "Install (ON non-nil) or remove the cache-extract :around advice.
Idempotent: tracks state via `synaxis-fetch--cache-advice-active'."
  (cond
   ((and on (not synaxis-fetch--cache-advice-active))
    (advice-add 'url-cache-extract :around
                #'synaxis-fetch--url-cache-extract-safe)
    (setq synaxis-fetch--cache-advice-active t))
   ((and (not on) synaxis-fetch--cache-advice-active)
    (advice-remove 'url-cache-extract
                   #'synaxis-fetch--url-cache-extract-safe)
    (setq synaxis-fetch--cache-advice-active nil))))

(defun synaxis-fetch--conditional-headers (feed)
  "Return `If-None-Match' / `If-Modified-Since' headers from FEED, or nil.
FEED is a feed plist as returned by `synaxis-db-get-feed'."
  (let (h)
    (when-let* ((etag (and feed (plist-get feed :etag))))
      (push (cons "If-None-Match" etag) h))
    (when-let* ((lm (and feed (plist-get feed :last-modified))))
      (push (cons "If-Modified-Since" lm) h))
    h))

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
       url (list :last-fetched (float-time) :failures 0)))
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
      (let* ((parsed (synaxis-parse-string
                      (synaxis-fetch--body-as-string body)
                      url))
             (feed (synaxis-db-get-feed url))
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

When the feed has a stored ETag or Last-Modified from a prior
fetch, those are sent as `If-None-Match' / `If-Modified-Since'.
A 304 Not Modified response is handled in
`synaxis-fetch--process-response'.  See
`synaxis-fetch--url-cache-extract-safe' for the workaround that
keeps url-http's hardcoded `url-cache-extract' call on 304 from
crashing when synaxis does not warm `url-cache-directory'."
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
        (let* ((cond-headers (synaxis-fetch--conditional-headers feed))
               (url-queue-parallel-processes synaxis-fetch-max-parallel)
               (url-queue-timeout             synaxis-fetch-timeout)
               (url-request-extra-headers
                (append cond-headers synaxis-http-request-headers)))
          (when (zerop (hash-table-count synaxis-fetch--in-flight))
            (synaxis-fetch--cache-advice-toggle t))
          (puthash url t synaxis-fetch--in-flight)
          (url-queue-retrieve url #'synaxis-fetch--callback (list url) t t)))))))

(defun synaxis-fetch--callback (_status url)
  "Callback for `url-queue-retrieve' bound to URL."
  (let ((buf (current-buffer)))
    (unwind-protect
        (synaxis-fetch--process-response buf url)
      (remhash url synaxis-fetch--in-flight)
      (when (zerop (hash-table-count synaxis-fetch--in-flight))
        (synaxis-fetch--cache-advice-toggle nil))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun synaxis-fetch-all ()
  "Kick off `synaxis-fetch-feed' for every known feed."
  (dolist (feed (synaxis-db-list-feeds))
    (synaxis-fetch-feed (plist-get feed :url))))

(provide 'synaxis-fetch)
;;; synaxis-fetch.el ends here
