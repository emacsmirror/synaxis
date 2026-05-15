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

;; Filter mini-language with notmuch/forgejo-style `KEY:VALUE' tokens,
;; compiled to a SQL `WHERE' clause and parameter list.
;;
;; Supported tokens:
;;
;;   tag:K        -tag:K        require / forbid tag K
;;   feed:V       -feed:V       feed title contains / does not contain V
;;   title:V      -title:V      entry title contains / does not contain V
;;   content:V    -content:V    entry content contains / does not contain V
;;   date:SPEC                  see `synaxis-filter-parse-date-spec'
;;   limit:N                    cap the result count
;;   WORD                       free text in (title OR content); AND-ed
;;
;; All non-empty whitespace-separated tokens are AND-ed.  Unknown
;; prefixes are silently dropped.  `-date:' and `-limit:' are
;; meaningless and ignored.  Bare-word negation (`-WORD') is not
;; supported in v0.1.1 -- use `-title:' or `-content:' instead.
;;
;; `=' uses `LIKE COLLATE NOCASE'.  SQLite does not ship a REGEXP
;; function; rich pattern matching is deferred to v1+.

;;; Code:

(require 'cl-lib)

;;; Date parsing

(defun synaxis-filter--day-bounds (year month day)
  "Return (FROM . TO) float-time for the day starting YEAR-MONTH-DAY."
  (let* ((from (float-time (encode-time 0 0 0 day month year)))
         (to   (+ from 86400)))
    (cons from to)))

(defun synaxis-filter--keyword-date (s)
  "Parse keyword date S (today / yesterday); nil otherwise."
  (pcase s
    ("today"
     (let* ((d (decode-time))
            (bounds (synaxis-filter--day-bounds
                     (decoded-time-year d)
                     (decoded-time-month d)
                     (decoded-time-day d))))
       (list :from (car bounds) :to (cdr bounds))))
    ("yesterday"
     (let* ((d (decode-time (time-subtract (current-time) 86400)))
            (bounds (synaxis-filter--day-bounds
                     (decoded-time-year d)
                     (decoded-time-month d)
                     (decoded-time-day d))))
       (list :from (car bounds) :to (cdr bounds))))))

(defun synaxis-filter--absolute-date (s)
  "Parse absolute date S: YYYY, YYYY-MM, or YYYY-MM-DD; nil otherwise.
Rejects months outside 1-12 and days outside 1-31 (`encode-time'
would otherwise silently normalise out-of-range values)."
  (cond
   ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)\\'" s)
    (let ((year  (string-to-number (match-string 1 s)))
          (month (string-to-number (match-string 2 s)))
          (day   (string-to-number (match-string 3 s))))
      (when (and (<= 1 month 12) (<= 1 day 31))
        (let ((bounds (synaxis-filter--day-bounds year month day)))
          (list :from (car bounds) :to (cdr bounds))))))
   ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)\\'" s)
    (let ((year  (string-to-number (match-string 1 s)))
          (month (string-to-number (match-string 2 s))))
      (when (<= 1 month 12)
        (let* ((from  (float-time (encode-time 0 0 0 1 month year)))
               (next-month (if (= month 12) 1 (1+ month)))
               (next-year  (if (= month 12) (1+ year) year))
               (to (float-time (encode-time 0 0 0 1 next-month next-year))))
          (list :from from :to to)))))
   ((string-match "\\`\\([0-9]\\{4\\}\\)\\'" s)
    (let* ((year (string-to-number (match-string 1 s)))
           (from (float-time (encode-time 0 0 0 1 1 year)))
           (to   (float-time (encode-time 0 0 0 1 1 (1+ year)))))
      (list :from from :to to)))))

(defun synaxis-filter--duration-seconds (n unit)
  "Return seconds for N units of UNIT (string).
Recognised units: s, m, h, d, w, months, y.  nil otherwise."
  (let ((mult (pcase unit
                ("s"      1)
                ("m"      60)
                ("h"      3600)
                ("d"      86400)
                ("w"      604800)
                ("months" 2592000)
                ("y"      31536000))))
    (and mult (* n mult))))

(defun synaxis-filter--relative-date (s)
  "Parse relative date S (e.g. `7d', `1y'); nil otherwise."
  (when (string-match
         "\\`\\([0-9]+\\)\\(months\\|s\\|m\\|h\\|d\\|w\\|y\\)\\'"
         s)
    (let ((secs (synaxis-filter--duration-seconds
                 (string-to-number (match-string 1 s))
                 (match-string 2 s))))
      (and secs
           (let ((now (float-time)))
             (list :from (- now secs) :to now))))))

(defun synaxis-filter--simple-date-spec (s)
  "Dispatch S to one of the simple-date sub-parsers."
  (or (synaxis-filter--keyword-date s)
      (synaxis-filter--absolute-date s)
      (synaxis-filter--relative-date s)))

(defun synaxis-filter-parse-date-spec (s)
  "Parse date spec S into a plist `(:from F :to T)' or nil.
F and T are float-time bounds; either may be nil (open end)."
  (and (stringp s)
       (not (string-empty-p s))
       (synaxis-filter--simple-date-spec s)))

;;; Token classification

(defun synaxis-filter--token-for (key val negated)
  "Build a token cell for KEY=VAL, optionally NEGATED."
  (pcase key
    ('tag     (cons (if negated 'not-tag     'tag)     val))
    ('feed    (cons (if negated 'not-feed    'feed)    val))
    ('title   (cons (if negated 'not-title   'title)   val))
    ('content (cons (if negated 'not-content 'content) val))
    ('date    (and (not negated)
                   (let ((spec (synaxis-filter-parse-date-spec val)))
                     (and spec (cons 'date spec)))))
    ('limit   (and (not negated)
                   (let ((n (string-to-number val)))
                     (and (> n 0) (cons 'limit n)))))))

(defun synaxis-filter--classify-prefixed (tok negated)
  "Classify TOK as a `prefix:value' token.
If TOK has no `:', treat as bare word (or drop when NEGATED)."
  (cond
   ((string-match "\\`\\([a-z]+\\):\\(.+\\)\\'" tok)
    (synaxis-filter--token-for
     (intern (match-string 1 tok))
     (match-string 2 tok)
     negated))
   (negated nil)              ;; `-WORD' unsupported
   (t (cons 'text tok))))

(defun synaxis-filter--classify (tok)
  "Classify a single whitespace-separated TOK; return token cell or nil."
  (cond
   ((string-empty-p tok) nil)
   ((string-prefix-p "-" tok)
    (synaxis-filter--classify-prefixed (substring tok 1) t))
   (t (synaxis-filter--classify-prefixed tok nil))))

(defun synaxis-filter-parse (s)
  "Tokenise filter string S into a list of token cells."
  (delq nil
        (mapcar #'synaxis-filter--classify
                (split-string (or s "") "[ \t\n]+" t))))

;;; Compiler

(defconst synaxis-filter--exists-sql
  "EXISTS (SELECT 1 FROM entry_tags t WHERE t.entry_id = e.id AND t.tag = ?)")

(defconst synaxis-filter--not-exists-sql
  (concat "NOT " synaxis-filter--exists-sql))

(defun synaxis-filter--like-clause (col negated)
  "SQL fragment matching COL with `LIKE'.  Negate when NEGATED."
  (format "%s %s ? COLLATE NOCASE" col (if negated "NOT LIKE" "LIKE")))

(defun synaxis-filter--date-clause (spec parts-cell params-cell)
  "Append SQL fragments for date SPEC into PARTS-CELL and PARAMS-CELL."
  (let ((from (plist-get spec :from))
        (to   (plist-get spec :to)))
    (when from
      (push "e.date >= ?" (car parts-cell))
      (push from           (car params-cell)))
    (when to
      (push "e.date < ?"   (car parts-cell))
      (push to             (car params-cell)))))

(defun synaxis-filter-compile (tokens)
  "Compile parsed TOKENS into a plist `(:where S :params P :limit L)'."
  (let ((parts (list nil)) (params (list nil)) (limit nil))
    (dolist (tok tokens)
      (pcase (car tok)
        ('tag
         (push synaxis-filter--exists-sql (car parts))
         (push (cdr tok) (car params)))
        ('not-tag
         (push synaxis-filter--not-exists-sql (car parts))
         (push (cdr tok) (car params)))
        ('feed
         (push (synaxis-filter--like-clause "f.title" nil) (car parts))
         (push (format "%%%s%%" (cdr tok)) (car params)))
        ('not-feed
         (push (synaxis-filter--like-clause "f.title" t) (car parts))
         (push (format "%%%s%%" (cdr tok)) (car params)))
        ('title
         (push (synaxis-filter--like-clause "e.title" nil) (car parts))
         (push (format "%%%s%%" (cdr tok)) (car params)))
        ('not-title
         (push (synaxis-filter--like-clause "e.title" t) (car parts))
         (push (format "%%%s%%" (cdr tok)) (car params)))
        ('content
         (push (synaxis-filter--like-clause "e.content" nil) (car parts))
         (push (format "%%%s%%" (cdr tok)) (car params)))
        ('not-content
         (push (synaxis-filter--like-clause "e.content" t) (car parts))
         (push (format "%%%s%%" (cdr tok)) (car params)))
        ('text
         (push (concat "(e.title LIKE ? COLLATE NOCASE"
                       " OR e.content LIKE ? COLLATE NOCASE)")
               (car parts))
         (push (format "%%%s%%" (cdr tok)) (car params))
         (push (format "%%%s%%" (cdr tok)) (car params)))
        ('date
         (synaxis-filter--date-clause (cdr tok) parts params))
        ('limit
         (setq limit (cdr tok)))))
    (list :where (if (car parts)
                     (string-join (nreverse (car parts)) " AND ")
                   "1=1")
          :params (nreverse (car params))
          :limit limit)))

(provide 'synaxis-filter)
;;; synaxis-filter.el ends here
