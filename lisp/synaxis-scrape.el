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
  "Best-effort date parse on S, returning float-time or nil."
  (and (stringp s) (not (string-empty-p s))
       (condition-case nil
           (let ((decoded (parse-time-string s)))
             (and (decoded-time-year decoded)
                  (float-time (encode-time decoded))))
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

(provide 'synaxis-scrape)
;;; synaxis-scrape.el ends here
