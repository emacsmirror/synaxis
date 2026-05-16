;;; synaxis-scrape.el --- HTML scraping to synthetic feeds  -*- lexical-binding: t; -*-

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
(require 'url-parse)
(require 'parse-time)
(require 'url-queue)

(defvar synaxis-http-request-headers)

;;; Customisation

(defcustom synaxis-scrape-max-parallel 4
  "Maximum concurrent article fetches per scrape cycle.
Bound around `url-queue-retrieve' as `url-queue-parallel-processes'."
  :type 'integer
  :group 'synaxis)

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
        (pcase sigil
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
  (pcase (car ast)
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

(defun synaxis-scrape--resolve-url (base path)
  "Resolve PATH against BASE URL into an absolute URL string.
Handles absolute, page-relative, root-relative, protocol-relative,
and fragment-only paths.  Returns nil for nil or empty PATH."
  (cond
   ((or (null path) (string-empty-p path)) nil)
   ((string-prefix-p "http://"  path) path)
   ((string-prefix-p "https://" path) path)
   ((string-prefix-p "//" path)
    (let ((scheme (url-type (url-generic-parse-url base))))
      (concat scheme ":" path)))
   ((string-prefix-p "#" path)
    (concat (replace-regexp-in-string "#.*\\'" "" base) path))
   (t (url-expand-file-name path base))))

;;; HTML decoding

(defun synaxis-scrape--decode-html (buffer)
  "Return the decoded HTML body of BUFFER (response from `url-retrieve').
Strips HTTP headers if present, decodes as UTF-8 fallback."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((body-start
             (if (re-search-forward "\r?\n\r?\n" nil t)
                 (match-end 0)
               (point-min))))
        (let ((body (buffer-substring-no-properties body-start (point-max))))
          (if (multibyte-string-p body)
              body
            (decode-coding-string body 'utf-8)))))))

;;; Date extraction

(defun synaxis-scrape--parse-time-loose (s)
  "Best-effort date parse on S, returning float-time or nil.
Fills in zeros for missing hour/minute/second so date-only strings
like \"May 15, 2026\" (no time component) still encode.  Preserves
the timezone from the decoded time when present (e.g. trailing
`Z'), so an explicit UTC stays UTC."
  (and (stringp s) (not (string-empty-p s))
       (condition-case nil
           (let ((decoded (parse-time-string s)))
             (and (decoded-time-year decoded)
                  (decoded-time-month decoded)
                  (decoded-time-day decoded)
                  (float-time
                   (encode-time (or (decoded-time-second decoded) 0)
                                (or (decoded-time-minute decoded) 0)
                                (or (decoded-time-hour decoded) 0)
                                (decoded-time-day decoded)
                                (decoded-time-month decoded)
                                (decoded-time-year decoded)
                                (decoded-time-zone decoded)))))
         (error nil))))

(defun synaxis-scrape--node-date (node)
  "Return float-time from NODE's `datetime' attribute or text content."
  (or (synaxis-scrape--parse-time-loose (dom-attr node 'datetime))
      (synaxis-scrape--parse-time-loose
       (string-trim (or (and node (mapconcat
                                   (lambda (c) (if (stringp c) c ""))
                                   (dom-children node) ""))
                        "")))))

(defun synaxis-scrape--extract-date (dom selector)
  "Extract a float-time from DOM using SELECTOR string.
SELECTOR may be nil; returns nil if no match."
  (and selector
       (let ((nodes (synaxis-scrape--query selector dom)))
         (cl-loop for n in nodes
                  for d = (synaxis-scrape--node-date n)
                  when d return d))))

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
      (setq s (replace-regexp-in-string (regexp-quote cleanup) "" s)))
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
         (cons (string-trim
                (mapconcat (lambda (c) (if (stringp c) c ""))
                           (dom-children anchor) ""))
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
          :date         (or (synaxis-scrape--extract-date
                             node (plist-get rules :date-selector))
                            (float-time))
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
        (cl-subseq filtered 0 (min limit (length filtered)))
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
         (raw (and title-node
                   (string-trim
                    (mapconcat (lambda (c) (if (stringp c) c ""))
                               (dom-children title-node) "")))))
    (synaxis-scrape--strip-title raw (plist-get rules :title-cleanup))))

;;; Async per-article expansion

(defun synaxis-scrape--apply-content (buffer rules)
  "Return the content STRING extracted from BUFFER per RULES.
Reads `:content-selector' and `:content-cleanup' from RULES."
  (let* ((html (synaxis-scrape--decode-html buffer))
         (dom  (with-temp-buffer
                 (insert html)
                 (libxml-parse-html-region (point-min) (point-max))))
         (selector (plist-get rules :content-selector))
         (node     (and selector (car (synaxis-scrape--query selector dom)))))
    (when node
      (synaxis-scrape--cleanup-content
       node (plist-get rules :content-cleanup))
      (synaxis-scrape--node-html node))))

(defun synaxis-scrape--queue-article (entry rules tracker done-callback)
  "Fetch ENTRY's :link asynchronously, apply RULES, update TRACKER.
Calls DONE-CALLBACK with the entry list once all pending fetches return."
  (let ((url-request-extra-headers synaxis-http-request-headers)
        (url-queue-parallel-processes synaxis-scrape-max-parallel))
    (url-queue-retrieve
     (plist-get entry :link)
     (lambda (_status entry rules tracker done-callback)
       (unwind-protect
           (let ((content (ignore-errors
                            (synaxis-scrape--apply-content
                             (current-buffer) rules))))
             (when content
               (plist-put entry :content content)))
         (kill-buffer (current-buffer))
         (synaxis-scrape--tracker-tick tracker entry done-callback)))
     (list entry rules tracker done-callback)
     t t)))

(defun synaxis-scrape--tracker-tick (tracker entry done-callback)
  "Mark ENTRY done in TRACKER; fire DONE-CALLBACK if all are in."
  (let ((pending (plist-get tracker :pending)))
    (plist-put tracker :done
               (cons entry (plist-get tracker :done)))
    (plist-put tracker :pending (1- pending))
    (when (zerop (1- pending))
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
(declare-function synaxis-db-add-feed "synaxis-db" (url &optional plist))
(declare-function synaxis-db-find-entry "synaxis-db" (feed-url source-id))
(declare-function synaxis-db-upsert-entry "synaxis-db" (plist))
(declare-function synaxis-db-add-tag "synaxis-db" (entry-id tag))
(declare-function synaxis-db-set-feed-cache-headers "synaxis-db" (url plist))
(declare-function synaxis-db-set-feed-title-if-empty "synaxis-db" (url title))
(declare-function synaxis-db-get-scrape-rule "synaxis-db" (url))
(declare-function synaxis-db-get-feed "synaxis-db" (url))

(defvar synaxis-new-entry-hook)

(defun synaxis-scrape--fetch-html (url)
  "Synchronously fetch URL and return its decoded HTML body.
401s from the server are surfaced as the response body rather than
triggering Emacs's interactive auth prompt."
  (let ((url-request-extra-headers synaxis-http-request-headers))
    (cl-letf (((symbol-function 'url-get-authentication)
               (lambda (&rest _) nil)))
      (let ((buf (url-retrieve-synchronously url t t)))
        (unwind-protect (synaxis-scrape--decode-html buf)
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(defun synaxis-scrape--save-entries (url entries)
  "Upsert ENTRIES under feed URL; fire `synaxis-new-entry-hook' for inserts."
  (let* ((feed (synaxis-db-get-feed url))
         (autotags (append (plist-get (plist-get feed :meta) :autotags) nil)))
    (dolist (raw entries)
      (let* ((entry (plist-put raw :feed-url url))
             (source-id (plist-get entry :source-id))
             (existing  (synaxis-db-find-entry url source-id))
             (id        (synaxis-db-upsert-entry entry)))
        (unless existing
          (synaxis-db-add-tag id "unread")
          (dolist (tag autotags) (synaxis-db-add-tag id tag))
          (run-hook-with-args 'synaxis-new-entry-hook id))))))

(defun synaxis-scrape-feed (url)
  "Run the scrape pipeline for URL: fetch + extract + (optional) expand + save.
Routed to from `synaxis-fetch-feed' when the feed's type is `scrape'."
  (let ((rules (synaxis-db-get-scrape-rule url)))
    (unless rules
      (user-error "No scrape rule for %s" url))
    (let* ((html (synaxis-scrape--fetch-html url))
           (entries (synaxis-scrape--extract html url rules)))
      (synaxis-db-set-feed-title-if-empty
       url (synaxis-scrape--page-title html rules))
      (synaxis-scrape--expand-content
       entries rules
       (lambda (final)
         (synaxis-scrape--save-entries url final)
         (synaxis-db-set-feed-cache-headers
          url (list :last-fetched (float-time) :failures 0)))))))

;;; Dry-run test command

(declare-function synaxis-search-mode "synaxis-search" ())
(declare-function synaxis-search--format "synaxis-search" ())
(declare-function synaxis-search--entry-columns "synaxis-search" (entry))
(declare-function synaxis-show-entry-plist "synaxis-show" (entry))

(defvar synaxis-scrape-test--buffer-name "*synaxis-scrape-test*"
  "Buffer name used by `synaxis-scrape-test'.")

(defvar-local synaxis-scrape-test--entries nil
  "Buffer-local list of preview entry plists.
RET in `synaxis-scrape-test-mode' looks up the entry at point here
and renders it via `synaxis-show-entry-plist'.  Inspect with
`M-x describe-variable RET synaxis-scrape-test--entries'.")

(defun synaxis-scrape-test--prep-entry (entry feed-title)
  "Augment ENTRY plist with the fields needed by the search list view."
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

(define-derived-mode synaxis-scrape-test-mode synaxis-search-mode "Synax-Test"
  "Preview scraped entries without writing to the database.
Inherits everything from `synaxis-search-mode' so the layout
matches the real list; RET on a row renders the entry via
`synaxis-show-entry-plist' using `synaxis-scrape-test--entries' as
the source instead of the DB."
  (setq-local revert-buffer-function
              (lambda (&rest _)
                (user-error "Re-invoke synaxis-scrape-test to refresh"))))

(defun synaxis-scrape-test-show ()
  "Render the entry at point from the buffer-local preview store."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (entry (cl-find id synaxis-scrape-test--entries
                              :key (lambda (e) (plist-get e :id))
                              :test #'equal)))
    (require 'synaxis-show)
    (synaxis-show-entry-plist entry)))

(define-key synaxis-scrape-test-mode-map (kbd "RET")
  #'synaxis-scrape-test-show)

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
  "Synchronously expand per-article content for test mode.
Returns ENTRIES with `:content' filled when `:content-selector' is set.
Inhibits Emacs's auth prompt on 401 responses."
  (if (not (plist-get rules :content-selector))
      entries
    (cl-letf (((symbol-function 'url-get-authentication)
               (lambda (&rest _) nil)))
      (mapcar
       (lambda (e)
         (condition-case nil
             (let* ((url-request-extra-headers synaxis-http-request-headers)
                    (buf (url-retrieve-synchronously
                          (plist-get e :link) t t))
                    (content (synaxis-scrape--apply-content buf rules)))
               (when (buffer-live-p buf) (kill-buffer buf))
               (if content (plist-put e :content content) e))
           (error e)))
       entries))))

(provide 'synaxis-scrape)
;;; synaxis-scrape.el ends here
