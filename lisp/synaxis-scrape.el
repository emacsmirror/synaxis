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

(provide 'synaxis-scrape)
;;; synaxis-scrape.el ends here
