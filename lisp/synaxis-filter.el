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
(require 'synaxis-db)

;;; Date parsing

(defun synaxis-filter--day-bounds (year month day)
  "Return (FROM . TO) float-time for the day starting YEAR-MONTH-DAY."
  (let* ((from (float-time (encode-time 0 0 0 day month year)))
         (to   (+ from 86400)))
    (cons from to)))

(defun synaxis-filter--keyword-date (s)
  "Parse keyword date S; nil otherwise.
Recognised: today, yesterday, thisweek, thismonth, thisyear."
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
       (list :from (car bounds) :to (cdr bounds))))
    ("thisweek"
     (let* ((now (current-time))
            (d   (decode-time now))
            (dow (decoded-time-weekday d))   ; 0=Sunday..6=Saturday
            (mondays-back (if (zerop dow) 6 (1- dow)))
            (monday (time-subtract now (* mondays-back 86400)))
            (md (decode-time monday))
            (bounds (synaxis-filter--day-bounds
                     (decoded-time-year md)
                     (decoded-time-month md)
                     (decoded-time-day md))))
       (list :from (car bounds) :to (float-time now))))
    ("thismonth"
     (let* ((now (current-time))
            (d   (decode-time now))
            (from (float-time (encode-time 0 0 0 1
                                           (decoded-time-month d)
                                           (decoded-time-year d)))))
       (list :from from :to (float-time now))))
    ("thisyear"
     (let* ((now (current-time))
            (d   (decode-time now))
            (from (float-time (encode-time 0 0 0 1 1
                                           (decoded-time-year d)))))
       (list :from from :to (float-time now))))))

(defun synaxis-filter--absolute-date (s)
  "Parse absolute date S: YYYY, YYYY-MM, or YYYY-MM-DD; nil otherwise.
Rejects months outside 1-12 and days outside 1-31 (`encode-time'
would otherwise silently normalise out-of-range values)."
  (cond
   ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)\\'" s)
    (let ((year  (string-to-number (match-string 1 s)))
          (month (string-to-number (match-string 2 s)))
          (day   (string-to-number (match-string 3 s))))
      (and (<= 1 month 12) (<= 1 day 31)
           (let ((bounds (synaxis-filter--day-bounds year month day)))
             (list :from (car bounds) :to (cdr bounds))))))
   ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)\\'" s)
    (let ((year  (string-to-number (match-string 1 s)))
          (month (string-to-number (match-string 2 s))))
      (and (<= 1 month 12)
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
  (and (string-match
        "\\`\\([0-9]+\\)\\(months\\|s\\|m\\|h\\|d\\|w\\|y\\)\\'"
        s)
       (and-let* ((secs (synaxis-filter--duration-seconds
                         (string-to-number (match-string 1 s))
                         (match-string 2 s)))
                  (now (float-time)))
         (list :from (- now secs) :to now))))

(defun synaxis-filter--simple-date-spec (s)
  "Dispatch S to one of the simple-date sub-parsers."
  (or (synaxis-filter--keyword-date s)
      (synaxis-filter--absolute-date s)
      (synaxis-filter--relative-date s)))

;;; Regex token helpers

(defun synaxis-filter--regex-shape-p (s)
  "Return non-nil when S is `/REGEX/' with a non-empty body.
Requires both leading and trailing slash and at least one char between."
  (and (stringp s)
       (> (length s) 2)
       (eq (aref s 0) ?/)
       (eq (aref s (1- (length s))) ?/)))

(defun synaxis-filter--strip-slashes (s)
  "Return S with one leading and one trailing slash removed."
  (substring s 1 (1- (length s))))

(defun synaxis-filter--validate-regex (pattern)
  "Probe PATTERN with `string-match-p'.
Return PATTERN on success; signal `user-error' on invalid regex."
  (condition-case err
      (progn (string-match-p pattern "") pattern)
    (invalid-regexp
     (user-error "Invalid regex /%s/: %s" pattern (cadr err)))))

(defun synaxis-filter-parse-date-spec (s)
  "Parse date spec S into a plist `(:from F :to T)' or nil.
F and T are float-time bounds; either may be nil (open end).
Supports ranges of the form LO..HI, LO.., and ..HI."
  (cond
   ((not (stringp s)) nil)
   ((string-empty-p s) nil)
   ((string-match "\\`\\(.*\\)\\.\\.\\(.*\\)\\'" s)
    (let* ((lo-str (match-string 1 s))
           (hi-str (match-string 2 s))
           (lo (and (not (string-empty-p lo-str))
                    (synaxis-filter--simple-date-spec lo-str)))
           (hi (and (not (string-empty-p hi-str))
                    (synaxis-filter--simple-date-spec hi-str)))
           (from (and lo (plist-get lo :from)))
           (to   (and hi (plist-get hi :to))))
      (and (or from to) (list :from from :to to))))
   (t (synaxis-filter--simple-date-spec s))))

;;; Token classification

(defun synaxis-filter--token-for (key val negated)
  "Build a token cell for KEY=VAL, optionally NEGATED.
For `title', `content', and `feed', a VAL shaped `/RE/' produces a
regex-FIELD (or not-regex-FIELD) cell with the stripped, validated
pattern.  `tag' values are always treated as literal strings."
  (pcase key
    ((or 'feed 'title 'content)
     (let* ((regex? (synaxis-filter--regex-shape-p val))
            (pat (if regex?
                     (synaxis-filter--validate-regex
                      (synaxis-filter--strip-slashes val))
                   val))
            (sym (intern (format "%s%s%s"
                                 (if negated "not-" "")
                                 (if regex? "regex-" "")
                                 key))))
       (cons sym pat)))
    ('tag   (cons (if negated 'not-tag 'tag) val))
    ('date  (and (not negated)
                 (let ((spec (synaxis-filter-parse-date-spec val)))
                   (and spec (cons 'date spec)))))
    ('limit (and (not negated)
                 (let ((n (string-to-number val)))
                   (and (> n 0) (cons 'limit n)))))))

(defun synaxis-filter--classify-prefixed (tok negated)
  "Classify TOK as a `prefix:value' token.
If TOK has no `:', treat as bare word, bare regex (when `/RE/'-shaped),
or drop when NEGATED and not regex-shaped."
  (cond
   ((string-match "\\`\\([a-z]+\\):\\(.+\\)\\'" tok)
    (synaxis-filter--token-for
     (intern (match-string 1 tok))
     (match-string 2 tok)
     negated))
   ((synaxis-filter--regex-shape-p tok)
    (let ((pat (synaxis-filter--validate-regex
                (synaxis-filter--strip-slashes tok))))
      (cons (if negated 'not-regex-text 'regex-text) pat)))
   (negated nil)              ;; `-WORD' unsupported
   (t (cons 'text tok))))

(defun synaxis-filter--classify (tok)
  "Classify a single whitespace-separated TOK; return token cell or nil."
  (cond
   ((string-empty-p tok) nil)
   ((string-prefix-p "-" tok)
    (synaxis-filter--classify-prefixed (substring tok 1) t))
   (t (synaxis-filter--classify-prefixed tok nil))))

(defun synaxis-filter--tokenize (s)
  "Split S into tokens, respecting double-quoted segments.
Whitespace inside `\"...\"' is preserved.  `\\\"' inside a quoted
segment becomes a literal `\"'.  Empty tokens are dropped.  An
unmatched opening quote is treated as if closed at end-of-string."
  (named-let walk ((i 0) (in-quote nil) (cur nil) (out nil))
    (let ((flush (lambda (acc xs)
                   (let ((tok (and acc (apply #'string (nreverse acc)))))
                     (if (and tok (not (string-empty-p tok)))
                         (cons tok xs)
                       xs)))))
      (cond
       ((>= i (length s))
        (nreverse (funcall flush cur out)))
       ((and in-quote (eq (aref s i) ?\\)
             (< (1+ i) (length s))
             (eq (aref s (1+ i)) ?\"))
        (walk (+ i 2) t (cons ?\" cur) out))
       ((eq (aref s i) ?\")
        (walk (1+ i) (not in-quote) cur out))
       ((and (not in-quote) (memq (aref s i) '(?\s ?\t ?\n)))
        (walk (1+ i) nil nil (funcall flush cur out)))
       (t
        (walk (1+ i) in-quote (cons (aref s i) cur) out))))))

(defun synaxis-filter-parse (s)
  "Tokenise filter string S into a list of token cells.
Double-quoted segments are taken as literal values; whitespace
inside quotes is preserved.  See `synaxis-filter--tokenize'."
  (delq nil
        (mapcar #'synaxis-filter--classify
                (synaxis-filter--tokenize (or s "")))))

;;; Compiler

(defun synaxis-filter--regex-match (pattern s)
  "Case-insensitive `string-match-p' of PATTERN on S.
Returns nil when S is nil rather than erroring."
  (and (stringp s)
       (let ((case-fold-search t))
         (string-match-p pattern s))))

(defun synaxis-filter--regex-predicate (kind pattern)
  "Return a single-token predicate for regex token KIND with PATTERN.
KIND is one of `regex-title', `not-regex-title', `regex-content',
`not-regex-content', `regex-feed', `not-regex-feed', `regex-text',
`not-regex-text'.  Returned closure takes an entry plist."
  (pcase kind
    ('regex-title       (lambda (e) (synaxis-filter--regex-match pattern (plist-get e :title))))
    ('not-regex-title   (lambda (e) (not (synaxis-filter--regex-match pattern (plist-get e :title)))))
    ('regex-content     (lambda (e) (synaxis-filter--regex-match pattern (plist-get e :content))))
    ('not-regex-content (lambda (e) (not (synaxis-filter--regex-match pattern (plist-get e :content)))))
    ('regex-feed        (lambda (e) (synaxis-filter--regex-match pattern (plist-get e :feed-title))))
    ('not-regex-feed    (lambda (e) (not (synaxis-filter--regex-match pattern (plist-get e :feed-title)))))
    ('regex-text        (lambda (e)
                          (or (synaxis-filter--regex-match pattern (plist-get e :title))
                              (synaxis-filter--regex-match pattern (plist-get e :content)))))
    ('not-regex-text    (lambda (e)
                          (not (or (synaxis-filter--regex-match pattern (plist-get e :title))
                                   (synaxis-filter--regex-match pattern (plist-get e :content))))))))

(defconst synaxis-filter--exists-sql
  "EXISTS (SELECT 1 FROM entry_tags t WHERE t.entry_id = e.id AND t.tag = ?)")

(defconst synaxis-filter--not-exists-sql
  (concat "NOT " synaxis-filter--exists-sql))

(defun synaxis-filter--like-clause (col negated)
  "SQL fragment matching COL with `LIKE'.  Negate when NEGATED."
  (format "%s %s ? COLLATE NOCASE" col (if negated "NOT LIKE" "LIKE")))

(defun synaxis-filter--like-token (col tok negated)
  "Return (CLAUSE . PARAM) for a LIKE token on COL with value TOK.
Negated when NEGATED."
  (cons (synaxis-filter--like-clause col negated)
        (format "%%%s%%" tok)))

(defun synaxis-filter-compile (tokens)
  "Compile parsed TOKENS into a plist.
Returns (:where S :params P :limit L :post-filter PRED-OR-NIL).
PRED-OR-NIL is a closure (entry) -> boolean composed AND-wise from
all regex tokens, or nil when no regex tokens are present."
  (let (parts params limit post-preds)
    (cl-flet ((emit (clause &rest ps)
                (push clause parts)
                (dolist (p ps) (push p params)))
              (emit-pred (kind val)
                (push (synaxis-filter--regex-predicate kind val) post-preds)))
      (dolist (tok tokens)
        (pcase-exhaustive (car tok)
          ('tag         (emit synaxis-filter--exists-sql     (cdr tok)))
          ('not-tag     (emit synaxis-filter--not-exists-sql (cdr tok)))
          ('feed        (pcase-let ((`(,c . ,p) (synaxis-filter--like-token "f.title"   (cdr tok) nil)))
                          (emit c p)))
          ('not-feed    (pcase-let ((`(,c . ,p) (synaxis-filter--like-token "f.title"   (cdr tok) t)))
                          (emit c p)))
          ('title       (pcase-let ((`(,c . ,p) (synaxis-filter--like-token "e.title"   (cdr tok) nil)))
                          (emit c p)))
          ('not-title   (pcase-let ((`(,c . ,p) (synaxis-filter--like-token "e.title"   (cdr tok) t)))
                          (emit c p)))
          ('content     (pcase-let ((`(,c . ,p) (synaxis-filter--like-token "e.content" (cdr tok) nil)))
                          (emit c p)))
          ('not-content (pcase-let ((`(,c . ,p) (synaxis-filter--like-token "e.content" (cdr tok) t)))
                          (emit c p)))
          ('text
           (let ((wild (format "%%%s%%" (cdr tok))))
             (emit "(e.title LIKE ? COLLATE NOCASE OR e.content LIKE ? COLLATE NOCASE)"
                   wild wild)))
          ('date
           (let ((from (plist-get (cdr tok) :from))
                 (to   (plist-get (cdr tok) :to)))
             (when from (emit "e.date >= ?" from))
             (when to   (emit "e.date < ?"  to))))
          ('limit (setq limit (cdr tok)))
          ((and kind (guard (memq kind '(regex-title     not-regex-title
							 regex-content   not-regex-content
							 regex-feed      not-regex-feed
							 regex-text      not-regex-text))))
           (emit-pred kind (cdr tok))))))
    (list :where       (if parts (string-join (nreverse parts) " AND ") "1=1")
          :params      (nreverse params)
          :limit       limit
          :post-filter (and post-preds
                            (let ((preds (nreverse post-preds)))
                              (lambda (e) (cl-every (lambda (p) (funcall p e)) preds)))))))

;;; Completions

(defcustom synaxis-filter-title-completion-limit 200
  "Maximum number of entry titles offered as `title:' completions."
  :type 'integer
  :group 'synaxis)

(defconst synaxis-filter--static-completions
  '("tag:" "-tag:" "feed:" "-feed:" "title:" "-title:"
    "content:" "-content:" "date:" "limit:"
    "date:today" "date:yesterday"
    "date:thisweek" "date:thismonth" "date:thisyear"
    "date:7d" "date:30d" "date:1y")
  "Static completion strings independent of DB state.")

(defun synaxis-filter--db-tags ()
  "Return the list of registered tag strings, alphabetically.
Reads from the `tags' registry (schema v2+); no DISTINCT scan."
  (let ((db (synaxis-db--ensure-open)))
    (mapcar #'car
            (sqlite-select
             db "SELECT tag FROM tags ORDER BY tag COLLATE NOCASE;"))))

(defun synaxis-filter--db-feed-titles ()
  "Return the list of non-empty feed titles in the database."
  (let ((db (synaxis-db--ensure-open)))
    (mapcar #'car
            (sqlite-select
             db "SELECT title FROM feeds
                 WHERE title IS NOT NULL AND title <> ''
                 ORDER BY title COLLATE NOCASE;"))))

(defun synaxis-filter--db-recent-entry-titles (limit)
  "Return the LIMIT most-recent distinct entry titles."
  (let ((db (synaxis-db--ensure-open)))
    (mapcar #'car
            (sqlite-select
             db "SELECT DISTINCT title FROM entries
                 WHERE title IS NOT NULL AND title <> ''
                 ORDER BY date DESC LIMIT ?;"
             (list limit)))))

(defun synaxis-filter--quote-if-needed (s)
  "Wrap S in double quotes when it contains whitespace."
  (if (and (stringp s) (string-match-p "[ \t]" s))
      (format "\"%s\"" s)
    s))

(defun synaxis-filter--prefix-each (prefix values)
  "Return VALUES with PREFIX prepended, quoting whitespace-containing items."
  (mapcar (lambda (v)
            (concat prefix (synaxis-filter--quote-if-needed v)))
          values))

(defun synaxis-filter-completions ()
  "Return a list of completion candidate strings for the filter prompt."
  (let ((tags  (synaxis-filter--db-tags))
        (feeds (synaxis-filter--db-feed-titles))
        (titles (synaxis-filter--db-recent-entry-titles
                 synaxis-filter-title-completion-limit)))
    (append synaxis-filter--static-completions
            (synaxis-filter--prefix-each "tag:"   tags)
            (synaxis-filter--prefix-each "-tag:"  tags)
            (synaxis-filter--prefix-each "feed:"  feeds)
            (synaxis-filter--prefix-each "-feed:" feeds)
            (synaxis-filter--prefix-each "title:" titles))))

(provide 'synaxis-filter)
;;; synaxis-filter.el ends here
