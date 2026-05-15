;;; synaxis-filter.el --- Filter mini-language  -*- lexical-binding: t; -*-

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

;; Filter mini-language modelled on elfeed's syntax, compiled to a
;; SQL `WHERE' clause and a parameter list.  Supported tokens:
;;
;;   +TAG          must have the tag
;;   -TAG          must not have the tag
;;   @DURATION     entry newer than DURATION (e.g. @7d, @2h, @6months)
;;   =FEED-MATCH   feed title contains the substring (case-insensitive)
;;   #LIMIT        cap the result count at LIMIT
;;   FREE-TEXT     entry title or content contains the word
;;
;; `=' uses `LIKE COLLATE NOCASE' rather than REGEXP -- SQLite does not
;; ship a REGEXP function by default and v0.1 keeps the dependency
;; surface clean.

;;; Code:

(require 'cl-lib)

;;; Duration parsing

(defun synaxis-filter-parse-duration (s)
  "Parse duration string S into seconds.
Accepted suffixes: `s', `m', `h', `d', `w', `months', `y'.  Returns
nil for unrecognised input."
  (when (and (stringp s)
             (string-match
              "\\`\\([0-9]+\\)\\(months\\|s\\|m\\|h\\|d\\|w\\|y\\)\\'"
              s))
    (let* ((n    (string-to-number (match-string 1 s)))
           (unit (match-string 2 s))
           (mult (pcase unit
                   ("s"      1)
                   ("m"      60)
                   ("h"      3600)
                   ("d"      86400)
                   ("w"      604800)
                   ("months" 2592000)   ; 30-day month
                   ("y"      31536000)))) ; 365-day year
      (* n mult))))

;;; Tokenizer

(defun synaxis-filter--classify (tok)
  "Classify token TOK as (TYPE . VALUE) or nil to drop."
  (cond
   ((string-empty-p tok) nil)
   ((string-prefix-p "+" tok)
    (let ((s (substring tok 1)))
      (and (not (string-empty-p s)) (cons 'plus s))))
   ((string-prefix-p "-" tok)
    (let ((s (substring tok 1)))
      (and (not (string-empty-p s)) (cons 'minus s))))
   ((string-prefix-p "@" tok)
    (let ((d (synaxis-filter-parse-duration (substring tok 1))))
      (and d (cons 'since d))))
   ((string-prefix-p "=" tok)
    (let ((s (substring tok 1)))
      (and (not (string-empty-p s)) (cons 'feed s))))
   ((string-prefix-p "#" tok)
    (let ((n (string-to-number (substring tok 1))))
      (and (> n 0) (cons 'limit n))))
   (t (cons 'text tok))))

(defun synaxis-filter-parse (s)
  "Tokenise filter string S into a list of (TYPE . VALUE) cells."
  (delq nil
        (mapcar #'synaxis-filter--classify
                (split-string (or s "") "[ \t\n]+" t))))

;;; Compiler

(defconst synaxis-filter--exists-template
  "EXISTS (SELECT 1 FROM entry_tags t WHERE t.entry_id = e.id AND t.tag = ?)"
  "SQL fragment for a tag presence check.")

(defconst synaxis-filter--not-exists-template
  (concat "NOT " synaxis-filter--exists-template)
  "SQL fragment for a tag absence check.")

(defun synaxis-filter-compile (tokens)
  "Compile parsed TOKENS into a plist `:where :params :limit'."
  (let ((parts nil) (params nil) (limit nil))
    (dolist (tok tokens)
      (pcase (car tok)
        ('plus
         (push synaxis-filter--exists-template parts)
         (push (cdr tok) params))
        ('minus
         (push synaxis-filter--not-exists-template parts)
         (push (cdr tok) params))
        ('since
         (push "e.date > ?" parts)
         (push (- (float-time) (cdr tok)) params))
        ('feed
         (push "f.title LIKE ? COLLATE NOCASE" parts)
         (push (format "%%%s%%" (cdr tok)) params))
        ('text
         (push (concat "(e.title LIKE ? COLLATE NOCASE"
                       " OR e.content LIKE ? COLLATE NOCASE)")
               parts)
         (push (format "%%%s%%" (cdr tok)) params)
         (push (format "%%%s%%" (cdr tok)) params))
        ('limit
         (setq limit (cdr tok)))))
    (list :where (if parts
                     (string-join (nreverse parts) " AND ")
                   "1=1")
          :params (nreverse params)
          :limit limit)))

(provide 'synaxis-filter)
;;; synaxis-filter.el ends here
