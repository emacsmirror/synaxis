;;; synaxis-scrape.el --- HTML scraping to synthetic feeds  -*- lexical-binding: t; -*-

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

;; Generate synthetic feeds from arbitrary HTML pages using a small
;; CSS-selector subset.  Two halves:
;;
;;   Pure   selector parsing, DOM matching, item extraction.
;;   Impure HTTP fetch, async per-article expansion, DB writes.
;;
;; Supported selector subset: tag, .class, #id, combinations
;; (tag.class, tag#id), descendant (a b), direct child (a > b), and
;; comma-separated alternatives (a, b).

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'keymap-popup)
(require 'tabulated-list)
(require 'url-parse)
(require 'parse-time)
(require 'url-queue)
(require 'synaxis-parse)
(require 'synaxis-tl)

(defvar synaxis-http-request-headers)

;;; Customisation

(defcustom synaxis-scrape-max-parallel 4
  "Maximum concurrent article fetches per scrape cycle.
Bound around `url-queue-retrieve' as `url-queue-parallel-processes'."
  :type 'natnum
  :group 'synaxis
  :package-version '(synaxis . "0.1"))

;;; Selector AST
;;
;; A parsed selector is one of:
;;   (simple :tag STRING-OR-NIL :classes (STRING ...) :id STRING-OR-NIL)
;;   (descendant LEFT RIGHT)
;;   (child LEFT RIGHT)
;;   (or-selector BRANCH1 BRANCH2 ...)

(defun synaxis-scrape--parse-simple (token)
  "Parse one TOKEN like `a.foo#bar' into a simple-selector plist."
  (let ((tag nil) (classes nil) (id nil)
        (i 0) (len (length token)))
    (when (and (> len 0)
               (not (memq (aref token 0) '(?. ?#))))
      (let ((end (or (string-match "[.#]" token) len)))
        (setq tag (substring token 0 end) i end)))
    (while (< i len)
      (let* ((sigil (aref token i))
             (start (1+ i))
             (end (or (string-match "[.#]" token start) len)))
        (pcase-exhaustive sigil
          (?. (push (substring token start end) classes))
          (?# (setq id (substring token start end))))
        (setq i end)))
    (list 'simple :tag tag :classes (nreverse classes) :id id)))

(defun synaxis-scrape--tokenize-branch (branch)
  "Split BRANCH string into (TOKEN . COMBINATOR) pairs.
COMBINATOR is `descendant', `child', or nil for the last token."
  (let ((parts (split-string branch "[ \t\n]+" t))
        result)
    (while parts
      (cond
       ((string= (car parts) ">")
        (when result (setcdr (car result) 'child))
        (setq parts (cdr parts)))
       (t
        (push (cons (car parts) 'descendant) result)
        (setq parts (cdr parts)))))
    (when result (setcdr (car result) nil))
    (nreverse result)))

(defun synaxis-scrape--parse-branch (branch)
  "Parse a non-comma BRANCH string into a selector AST."
  (let* ((tokens (synaxis-scrape--tokenize-branch branch))
         (first (synaxis-scrape--parse-simple (car (car tokens))))
         (combinators (mapcar #'cdr tokens))
         (rest (mapcar (lambda (p) (synaxis-scrape--parse-simple (car p)))
                       (cdr tokens))))
    (cl-loop with ast = first
             for next in rest
             for combinator in combinators
             do (setq ast (list (if (eq combinator 'child) 'child 'descendant)
                                ast next))
             finally return ast)))

(defun synaxis-scrape--parse-selector (selector)
  "Parse SELECTOR string into the AST documented above."
  (let ((branches (mapcar #'string-trim (split-string selector "," t))))
    (if (= 1 (length branches))
        (synaxis-scrape--parse-branch (car branches))
      (cons 'or-selector
            (mapcar #'synaxis-scrape--parse-branch branches)))))

;;; Matching

(defun synaxis-scrape--match-simple (simple node)
  "Return non-nil if NODE matches SIMPLE selector plist."
  (and (listp node) (symbolp (dom-tag node))
       (let ((tag (plist-get (cdr simple) :tag))
             (classes (plist-get (cdr simple) :classes))
             (id (plist-get (cdr simple) :id))
             (node-classes (split-string (or (dom-attr node 'class) "")
                                         "[ \t]+" t)))
         (and (or (null tag) (eq (intern tag) (dom-tag node)))
              (or (null id)  (equal id (dom-attr node 'id)))
              (cl-every (lambda (c) (member c node-classes)) classes)))))

(defun synaxis-scrape--descendants-matching (simple node)
  "Return all descendants of NODE (excluding NODE) that match SIMPLE."
  (let (matches)
    (cl-labels ((walk (n)
                  (when (and (listp n) (symbolp (dom-tag n)))
                    (dolist (child (dom-children n))
                      (when (and (listp child) (symbolp (dom-tag child)))
                        (when (synaxis-scrape--match-simple simple child)
                          (push child matches))
                        (walk child))))))
      (walk node))
    (nreverse matches)))

(defun synaxis-scrape--children-matching (simple node)
  "Return direct children of NODE matching SIMPLE."
  (cl-loop for child in (dom-children node)
           when (and (listp child) (symbolp (dom-tag child))
                     (synaxis-scrape--match-simple simple child))
           collect child))

(defun synaxis-scrape--match-from (ast roots)
  "Return list of nodes matching AST starting from ROOTS list."
  (pcase-exhaustive (car ast)
    ('simple
     (cl-loop for root in roots
              when (synaxis-scrape--match-simple ast root)
              collect root))
    ('descendant
     (let* ((left  (nth 1 ast))
            (right (nth 2 ast))
            (lefts (synaxis-scrape--match-from left roots)))
       (cl-loop for l in lefts
                append (synaxis-scrape--descendants-matching right l))))
    ('child
     (let* ((left  (nth 1 ast))
            (right (nth 2 ast))
            (lefts (synaxis-scrape--match-from left roots)))
       (cl-loop for l in lefts
                append (synaxis-scrape--children-matching right l))))
    ('or-selector
     (cl-loop for branch in (cdr ast)
              append (synaxis-scrape--match-from branch roots)))))

(defun synaxis-scrape--all-nodes (root)
  "Return ROOT plus every descendant element node, depth-first."
  (let (out)
    (cl-labels ((walk (n)
                  (when (and (listp n) (symbolp (dom-tag n)))
                    (push n out)
                    (dolist (c (dom-children n)) (walk c)))))
      (walk root))
    (nreverse out)))

(defun synaxis-scrape--query (selector dom)
  "Return list of nodes in DOM matching SELECTOR string."
  (let ((ast (synaxis-scrape--parse-selector selector))
        (all (synaxis-scrape--all-nodes dom)))
    (synaxis-scrape--match-from ast all)))

;;; URL resolution

(defalias 'synaxis-scrape--resolve-url #'synaxis-parse--resolve-url
  "Alias kept so existing scrape callers and tests need no rename.")

;;; HTML decoding

(defun synaxis-scrape--decode-html (buffer)
  "Return the decoded HTML body of BUFFER (response from `url-retrieve').
Strips HTTP headers if present.  Coding is chosen from the response
Content-Type charset, an XML/HTML declaration in the body, or
UTF-8 (see `synaxis-parse--decode-bytes')."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((body-start
              (if (re-search-forward "\r?\n\r?\n" nil t)
                  (match-end 0)
                (point-min)))
             (headers (buffer-substring-no-properties (point-min) body-start))
             (content-type
              (and (string-match "[Cc]ontent-[Tt]ype:[ \t]*\\([^\r\n]*\\)" headers)
                   (match-string 1 headers)))
             (body (buffer-substring-no-properties body-start (point-max))))
        (synaxis-parse--decode-bytes body content-type)))))

;;; Date extraction

(defun synaxis-scrape--parse-with-format (s fmt)
  "Parse date string S with strftime-like FMT.
Supports %Y, %m, %d, %H, %M, %S.  Returns canonical ISO or nil."
  (and (stringp s) (stringp fmt) (not (string-empty-p s))
       (let ((subs nil)
             (parts nil)
             (i 0)
             (len (length fmt)))
         (while (< i len)
           (if (and (= (aref fmt i) ?%)
                    (< (1+ i) len)
                    (memq (aref fmt (1+ i)) '(?Y ?m ?d ?H ?M ?S)))
               (progn
                 (pcase (aref fmt (1+ i))
                   (?Y (push 'year subs) (push "\\([0-9]\\{4\\}\\)" parts))
                   (?m (push 'month subs) (push "\\([0-9]\\{1,2\\}\\)" parts))
                   (?d (push 'day subs) (push "\\([0-9]\\{1,2\\}\\)" parts))
                   (?H (push 'hour subs) (push "\\([0-9]\\{1,2\\}\\)" parts))
                   (?M (push 'minute subs) (push "\\([0-9]\\{1,2\\}\\)" parts))
                   (?S (push 'second subs) (push "\\([0-9]\\{1,2\\}\\)" parts)))
                 (setq i (+ i 2)))
             (push (regexp-quote (char-to-string (aref fmt i))) parts)
             (setq i (1+ i))))
         (let* ((re (concat "\\`" (apply #'concat (nreverse parts)) "\\'"))
                (keys (nreverse subs))
                (text (string-trim s)))
           (and (string-match re text)
                (let ((year 1970) (month 1) (day 1)
                      (hour 0) (minute 0) (second 0)
                      (n 1))
                  (dolist (k keys)
                    (let ((v (string-to-number (match-string n text))))
                      (pcase k
                        ('year (setq year v))
                        ('month (setq month v))
                        ('day (setq day v))
                        ('hour (setq hour v))
                        ('minute (setq minute v))
                        ('second (setq second v)))
                      (setq n (1+ n))))
                  (and (>= year 1) (<= month 12) (<= day 31)
                       (synaxis-parse--time-to-iso
                        (encode-time second minute hour day month year
                                     t)))))))))

(defun synaxis-scrape--parse-time-loose (s &optional date-format)
  "Best-effort date parse on S, returning a canonical ISO string or nil.
When DATE-FORMAT is non-nil, try it first (strftime-like subset).
Fills in zeros for missing hour/minute/second so date-only strings
like \"May 15, 2026\" (no time component) still encode.  Preserves
the timezone from the decoded time when present (e.g. trailing
letter Z).  A value with no timezone (a bare date) is treated as UTC, so
date-only inputs do not shift a day under a non-UTC local zone."
  (and (stringp s) (not (string-empty-p s))
       (or (and date-format (synaxis-scrape--parse-with-format s date-format))
           (condition-case nil
               (let ((decoded (parse-time-string s)))
                 (and (decoded-time-year decoded)
                      (decoded-time-month decoded)
                      (decoded-time-day decoded)
                      (synaxis-parse--time-to-iso
                       (encode-time (or (decoded-time-second decoded) 0)
                                    (or (decoded-time-minute decoded) 0)
                                    (or (decoded-time-hour decoded) 0)
                                    (decoded-time-day decoded)
                                    (decoded-time-month decoded)
                                    (decoded-time-year decoded)
                                    (or (decoded-time-zone decoded) t)))))
             (error nil)))))

(defun synaxis-scrape--children-text (node)
  "Concatenate all descendant text of NODE, trimmed.  Empty when NODE is nil."
  (if node (string-trim (synaxis-parse--text-of node)) ""))

(defun synaxis-scrape--node-date (node &optional date-format)
  "Return ISO date string from NODE's `datetime' attribute or text content.
DATE-FORMAT, when non-nil, is tried on the text first."
  (or (synaxis-scrape--parse-time-loose (dom-attr node 'datetime) date-format)
      (synaxis-scrape--parse-time-loose (synaxis-scrape--children-text node)
                                        date-format)))

(defun synaxis-scrape--extract-date (dom selector &optional date-format)
  "Extract an ISO date string from DOM using SELECTOR string.
SELECTOR may be nil; returns nil if no match.  DATE-FORMAT is an
optional strftime-like pattern applied to the matched text."
  (and selector
       (let ((nodes (synaxis-scrape--query selector dom)))
         (cl-loop for n in nodes
                  for d = (synaxis-scrape--node-date n date-format)
                  when d return d))))

(defun synaxis-scrape--prefer-date (candidate fallback)
  "Return CANDIDATE when it is a usable date, else FALLBACK.
FALLBACK is the entry's pull-time date.  CANDIDATE comes from a
`date-selector' and may be nil, or a future cover date (PubMed
issue dates can be months ahead); only a non-nil value no later
than FALLBACK is honoured.  Both are canonical UTC ISO strings, so
the comparison is a plain lexical one."
  (if (and candidate (not (string< fallback candidate))) candidate fallback))

;;; Content cleanup

(defun synaxis-scrape--cleanup-content (node cleanup-selector)
  "Remove from NODE every child matching CLEANUP-SELECTOR.
CLEANUP-SELECTOR may be nil (no-op).  Mutates NODE in place;
returns NODE for chaining."
  (when (and node cleanup-selector)
    (dolist (victim (synaxis-scrape--query cleanup-selector node))
      (dom-remove-node node victim)))
  node)

;;; Title cleanup

(defun synaxis-scrape--strip-title (title cleanup)
  "Remove CLEANUP substring from TITLE.  Trims the result.
Either argument may be nil."
  (let ((s (or title "")))
    (when (and cleanup (not (string-empty-p cleanup)))
      (setq s (string-replace cleanup "" s)))
    (string-trim s)))

;;; Pure extraction

(defun synaxis-scrape--find-anchor (node)
  "Return (TITLE . HREF) from NODE.
If NODE is an `a' element, use it directly; otherwise descend to
the first `a' inside.  Returns nil if no anchor is found."
  (let ((anchor (if (eq (dom-tag node) 'a)
                    node
                  (car (dom-by-tag node 'a)))))
    (and anchor
         (cons (synaxis-scrape--children-text anchor)
               (dom-attr anchor 'href)))))

(defun synaxis-scrape--node-html (node)
  "Serialise NODE's children as an HTML string."
  (if (and node (listp node))
      (with-temp-buffer
        (dolist (child (dom-children node))
          (cond ((stringp child) (insert child))
                ((listp child)   (dom-print child))))
        (string-trim (buffer-string)))
    ""))

(defun synaxis-scrape--build-entry (node base-url rules)
  "Build an entry plist for an extracted NODE.
BASE-URL resolves relative hrefs.  RULES is the rule plist."
  (and-let* ((pair (synaxis-scrape--find-anchor node))
             (raw-title (car pair))
             (raw-href  (cdr pair))
             (link (synaxis-scrape--resolve-url base-url raw-href)))
    (list :source-id    link
          :title        (synaxis-scrape--strip-title
                         raw-title (plist-get rules :title-cleanup))
          :link         link
          :date         (synaxis-scrape--prefer-date
                         (synaxis-scrape--extract-date
                          node (plist-get rules :date-selector)
                          (plist-get rules :date-format))
                         (synaxis-parse--time-to-iso (current-time)))
          :content      (synaxis-scrape--node-html node)
          :content-type "html")))

(defun synaxis-scrape--filter-urls (entries pattern limit)
  "Filter ENTRIES by url PATTERN regexp, then truncate to LIMIT count."
  (let ((filtered
         (if (and pattern (not (string-empty-p pattern)))
             (cl-remove-if-not
              (lambda (e) (string-match-p pattern (plist-get e :link)))
              entries)
           entries)))
    (if (and (integerp limit) (> limit 0))
        (take limit filtered)
      filtered)))

(defun synaxis-scrape--dedupe (entries)
  "Drop duplicate ENTRIES sharing the same :link.
Keeps the first occurrence."
  (let ((seen (make-hash-table :test 'equal))
        out)
    (dolist (e entries)
      (let ((k (plist-get e :link)))
        (unless (gethash k seen)
          (puthash k t seen)
          (push e out))))
    (nreverse out)))

(defun synaxis-scrape--extract (html base-url rules)
  "Extract entries from HTML string with BASE-URL using RULES plist.
Pure: no HTTP, no DB.  Returns a list of entry plists."
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (selector (plist-get rules :url-selector))
         (nodes    (and selector (synaxis-scrape--query selector dom)))
         (built    (delq nil
                         (mapcar (lambda (n)
                                   (synaxis-scrape--build-entry n base-url rules))
                                 nodes)))
         (deduped  (synaxis-scrape--dedupe built)))
    (synaxis-scrape--filter-urls deduped
                                 (plist-get rules :url-pattern)
                                 (plist-get rules :limit))))

(defun synaxis-scrape--page-title (html rules)
  "Return the document <title> from HTML, stripped per RULES."
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (title-node (car (dom-by-tag dom 'title)))
         (raw (and title-node (synaxis-scrape--children-text title-node))))
    (synaxis-scrape--strip-title raw (plist-get rules :title-cleanup))))

;;; Async per-article expansion

(defun synaxis-scrape--apply-content (buffer rules)
  "Extract content and date from BUFFER (an HTTP response) per RULES.
Returns a plist `(:content STR-OR-NIL :date STR-OR-NIL)' so the
caller can update both fields on the entry.  `:date-selector' is
applied against the article DOM (where per-article dates live),
not the index DOM."
  (let* ((html (synaxis-scrape--decode-html buffer))
         (dom  (with-temp-buffer
                 (insert html)
                 (libxml-parse-html-region (point-min) (point-max))))
         (content-selector (plist-get rules :content-selector))
         (node (and content-selector
                    (car (synaxis-scrape--query content-selector dom))))
         (content (when node
                    (synaxis-scrape--cleanup-content
                     node (plist-get rules :content-cleanup))
                    (synaxis-scrape--node-html node)))
         (date (synaxis-scrape--extract-date
                dom (plist-get rules :date-selector)
                (plist-get rules :date-format))))
    (list :content content :date date)))

(defun synaxis-scrape--queue-article (entry rules tracker done-callback)
  "Fetch ENTRY's :link asynchronously, apply RULES, update TRACKER.
Calls DONE-CALLBACK with the entry list once all pending fetches return."
  (let ((url-request-extra-headers synaxis-http-request-headers)
        (url-queue-parallel-processes synaxis-scrape-max-parallel))
    (url-queue-retrieve
     (plist-get entry :link)
     (lambda (_status entry rules tracker done-callback)
       (let ((buf (current-buffer)))
         (unwind-protect
             (let ((result
                    (condition-case err
                        (synaxis-scrape--apply-content buf rules)
                      (error
                       (message "synaxis: article expand failed for %s: %S"
                                (plist-get entry :link) err)
                       nil))))
               (when (plist-get result :content)
                 (setq entry (plist-put entry :content (plist-get result :content))))
               (when (plist-get result :date)
                 (setq entry (plist-put entry :date
                                        (synaxis-scrape--prefer-date
                                         (plist-get result :date)
                                         (plist-get entry :date))))))
           (when (buffer-live-p buf)
             (kill-buffer buf))
           (synaxis-scrape--tracker-tick tracker entry done-callback))))
     (list entry rules tracker done-callback)
     t t)))

(defun synaxis-scrape--tracker-tick (tracker entry done-callback)
  "Mark ENTRY done in TRACKER; fire DONE-CALLBACK if all are in."
  (let* ((pending (plist-get tracker :pending))
         (new-pending (1- pending)))
    (plist-put tracker :done
               (cons entry (plist-get tracker :done)))
    (plist-put tracker :pending new-pending)
    (when (zerop new-pending)
      (funcall done-callback (nreverse (plist-get tracker :done))))))

(defun synaxis-scrape--expand-content (entries rules callback)
  "Fire async per-article fetches for ENTRIES, then call CALLBACK.
CALLBACK receives the augmented entry list.  When RULES lacks
`:content-selector', skips fetching and calls CALLBACK synchronously."
  (cond
   ((null entries) (funcall callback nil))
   ((not (plist-get rules :content-selector))
    (funcall callback entries))
   (t (let ((tracker (list :pending (length entries) :done nil)))
        (dolist (e entries)
          (synaxis-scrape--queue-article e rules tracker callback))))))

;;; Top-level orchestration

(declare-function synaxis-db--ensure-open "synaxis-db" ())
(declare-function synaxis-db-upsert-with-tags "synaxis-db" (url autotags entry &optional preserve-date))
(declare-function synaxis-db-set-feed-cache-headers "synaxis-db" (url plist))
(declare-function synaxis-db-set-feed-title-if-empty "synaxis-db" (url title))
(declare-function synaxis-db-get-scrape-rule "synaxis-db" (url))
(declare-function synaxis-db-get-feed "synaxis-db" (url))

(defun synaxis-scrape--fetch-html (url)
  "Synchronously fetch URL and return its decoded HTML body.
Used by `synaxis-scrape-test'.

Signal `user-error' when the request yields no buffer (network failure
or similar).  401s from the server are surfaced as the response body
rather than triggering Emacs's interactive auth prompt."
  (let ((url-request-noninteractive t)
        (url-request-extra-headers synaxis-http-request-headers))
    (let ((buf (url-retrieve-synchronously url t t)))
      (unless buf
        (user-error "Failed to retrieve %s" url))
      (unwind-protect (synaxis-scrape--decode-html buf)
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun synaxis-scrape--fetch-html-async (url callback)
  "Asynchronously fetch URL and call CALLBACK with HTML or an error.
CALLBACK is called with two arguments: HTML and ERR.  Exactly one
of them is non-nil."
  (let ((url-request-extra-headers synaxis-http-request-headers))
    (url-queue-retrieve
     url
     (lambda (status callback)
       (let ((buf (current-buffer))
             (status-error (plist-get status :error)))
         (unwind-protect
             (if status-error
                 (funcall callback nil status-error)
               (let (html err)
                 (condition-case e
                     (setq html (synaxis-scrape--decode-html buf))
                   (error (setq err e)))
                 (funcall callback html err)))
           (when (buffer-live-p buf) (kill-buffer buf)))))
     (list callback)
     t t)))

(defun synaxis-scrape--record-failure (url)
  "Increment URL's scrape failure counter and stamp `last_fetched'."
  (let* ((feed (synaxis-db-get-feed url))
         (failures (1+ (or (plist-get feed :failures) 0))))
    (synaxis-db-set-feed-cache-headers
     url (list :last-fetched (float-time)
               :failures failures))))

(defun synaxis-scrape--save-entries (url entries)
  "Upsert ENTRIES under feed URL; fire `synaxis-new-entry-hook' for new rows."
  (let* ((feed (synaxis-db-get-feed url))
         (autotags (append (plist-get (plist-get feed :meta) :autotags) nil)))
    (dolist (entry entries)
      ;; Scraped dates are pull-time; preserve first-seen across re-fetches.
      (synaxis-db-upsert-with-tags url autotags entry t))))

(defun synaxis-scrape--process-html (url html rules callback)
  "Extract entries from HTML at URL under RULES and pass them to CALLBACK."
  (let ((entries (synaxis-scrape--extract html url rules)))
    (synaxis-db-set-feed-title-if-empty
     url (synaxis-scrape--page-title html rules))
    (synaxis-scrape--expand-content entries rules callback)))

(defun synaxis-scrape--store-success (url entries)
  "Save ENTRIES for URL and mark the scrape feed successful."
  (synaxis-scrape--save-entries url entries)
  (synaxis-db-set-feed-cache-headers
   url (list :last-fetched (float-time) :failures 0)))

(defun synaxis-scrape-feed (url &optional done-callback)
  "Run the scrape pipeline for URL asynchronously.
Fetches the index page, extracts entry candidates, optionally fetches each
article to expand content/date, then upserts into the DB.

DONE-CALLBACK, when non-nil, is called once when the scrape cycle finishes.

Routed to from `synaxis-fetch-feed' when the feed's type is `scrape'."
  (let ((rules (synaxis-db-get-scrape-rule url))
        (finished nil))
    (unless rules
      (user-error "No scrape rule for %s" url))
    (cl-labels ((finish ()
                  (unless finished
                    (setq finished t)
                    (when done-callback
                      (funcall done-callback))))
                (fail (err)
                  (message "synaxis: scrape failed for %s: %S" url err)
                  (ignore-errors (synaxis-scrape--record-failure url))
                  (finish))
                (store (entries)
                  (condition-case err
                      (progn
                        (synaxis-scrape--store-success url entries)
                        (finish))
                    (error (fail err)))))
      (synaxis-scrape--fetch-html-async
       url
       (lambda (html err)
         (if err
             (fail err)
           (condition-case err
               (synaxis-scrape--process-html url html rules #'store)
             (error (fail err)))))))))

;;; Dry-run test command

(declare-function synaxis-search--format "synaxis-search" ())
(declare-function synaxis-search--entry-columns "synaxis-search" (entry))
(declare-function synaxis-search-browse-entry "synaxis-search" ())
(declare-function synaxis-search-copy-link "synaxis-search" ())
(declare-function synaxis-show-entry-plist "synaxis-show" (entry))

(defvar synaxis-scrape-test--buffer-name "*synaxis-scrape-test*"
  "Buffer name used by `synaxis-scrape-test'.")

(defvar-local synaxis-scrape-test--entries nil
  "Buffer-local list of preview entry plists.
RET in `synaxis-scrape-test-mode' looks up the entry at point here
and renders it via `synaxis-show-entry-plist'.  Inspect with
`describe-variable' on `synaxis-scrape-test--entries' to inspect.")

(defun synaxis-scrape-test--prep-entry (entry feed-title)
  "Augment ENTRY plist with FEED-TITLE and search-list view fields."
  (list :id          (plist-get entry :source-id)
        :source-id   (plist-get entry :source-id)
        :feed-title  feed-title
        :feed-url    nil
        :title       (plist-get entry :title)
        :link        (plist-get entry :link)
        :date        (plist-get entry :date)
        :content     (plist-get entry :content)
        :content-type (plist-get entry :content-type)
        :unread      t
        :tags        nil))

(defun synaxis-scrape-test-show ()
  "Render the entry at point from the buffer-local preview store."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (entry (cl-find id synaxis-scrape-test--entries
                              :key (lambda (e) (plist-get e :id))
                              :test #'equal)))
    (require 'synaxis-show)
    (synaxis-show-entry-plist entry)))

(defun synaxis-scrape-test--read-only-command ()
  "Reject DB-backed commands in scrape preview buffers."
  (interactive)
  (user-error "Scrape previews are read-only; this command uses the Synaxis database"))

(keymap-popup-define synaxis-scrape-test-mode-map
  "Keymap for `synaxis-scrape-test-mode'."
  :parent synaxis-tl-list-mode-map
  :description
  (lambda ()
    (format "scrape preview  [%d entries]"
            (if (boundp 'tabulated-list-entries)
                (length tabulated-list-entries)
              0)))
  :group "Navigation"
  "n" ("Next" next-line :stay-open t)
  "p" ("Previous" previous-line :stay-open t)
  "RET" ("Open" synaxis-scrape-test-show)
  "b" ("Browse URL" synaxis-search-browse-entry)
  "c" ("Copy URL" synaxis-search-copy-link)
  :group "View"
  "q" ("Quit" quit-window))

(dolist (key '("g" "l" "r" "R" "t" ";" "A" "D" "E" "u"))
  (keymap-set synaxis-scrape-test-mode-map
              key #'synaxis-scrape-test--read-only-command))

(define-derived-mode synaxis-scrape-test-mode tabulated-list-mode "Synax-Test"
  "Preview scraped entries without writing to the database.
The mode keeps the shared search-list layout, but uses a preview
keymap and buffer-local rows so refresh and mutation keys cannot
query or write to the real Synaxis database."
  (setq tabulated-list-format (synaxis-search--format))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _)
                (user-error "Re-invoke synaxis-scrape-test to refresh")))
  (tabulated-list-init-header))

(defun synaxis-scrape--render-test-buffer (url entries)
  "Pop the scrape-test buffer with ENTRIES extracted from URL."
  (require 'synaxis-search)
  (let ((buf (get-buffer-create synaxis-scrape-test--buffer-name))
        (feed-title (format "(test) %s" url)))
    (with-current-buffer buf
      (synaxis-scrape-test-mode)
      (setq synaxis-scrape-test--entries
            (mapcar (lambda (e) (synaxis-scrape-test--prep-entry e feed-title))
                    entries))
      (setq tabulated-list-format (synaxis-search--format))
      (setq tabulated-list-padding 1)
      (tabulated-list-init-header)
      (setq tabulated-list-entries
            (mapcar (lambda (e)
                      (list (plist-get e :id)
                            (synaxis-search--entry-columns e)))
                    synaxis-scrape-test--entries))
      (setq-local mode-line-buffer-identification
                  (list (format "scrape-test  [%d items from %s]"
                                (length entries) url)))
      (tabulated-list-print))
    (pop-to-buffer buf)))

;;;###autoload
(defun synaxis-scrape-test (url &rest rules)
  "Preview scrape RULES against URL without saving.
Synchronous: fetches the page, applies rules, optionally expands
per-article content (capped at 5 items for test mode), and pops
`*synaxis-scrape-test*' with the result."
  (interactive
   (list (read-string "Test URL: ")
         :url-selector (read-string "URL selector: ")))
  (let* ((rules (plist-put rules :limit (min 5 (or (plist-get rules :limit) 5))))
         (html (synaxis-scrape--fetch-html url))
         (entries (synaxis-scrape--extract html url rules))
         (expanded (synaxis-scrape--expand-sync-test entries rules)))
    (synaxis-scrape--render-test-buffer url expanded)))

(defun synaxis-scrape--expand-sync-test (entries rules)
  "Synchronously expand per-article content and date for test mode.
Returns ENTRIES with `:content' and `:date' filled per RULES.
Inhibits Emacs's auth prompt on 401 responses."
  (if (not (plist-get rules :content-selector))
      entries
    (mapcar
     (lambda (e)
       (condition-case nil
           (let* ((url-request-noninteractive t)
                  (url-request-extra-headers synaxis-http-request-headers)
                  (buf (url-retrieve-synchronously
                        (plist-get e :link) t t)))
             (unwind-protect
                 (let ((result (synaxis-scrape--apply-content buf rules)))
                   (when (plist-get result :content)
                     (setq e (plist-put e :content (plist-get result :content))))
                   (when (plist-get result :date)
                     (setq e (plist-put e :date
                                        (synaxis-scrape--prefer-date
                                         (plist-get result :date)
                                         (plist-get e :date)))))
                   e)
               (when (buffer-live-p buf)
                 (kill-buffer buf))))
         (error e)))
     entries)))

(provide 'synaxis-scrape)
;;; synaxis-scrape.el ends here
