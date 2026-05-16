;;; synaxis-db.el --- SQLite storage layer  -*- lexical-binding: t; -*-

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

;; SQLite connection, schema, and CRUD primitives for feeds, entries,
;; and tags.  All persistent state lives here; other modules go
;; through this API.

;;; Code:

(require 'cl-lib)
(require 'sqlite)

(defvar synaxis-testing nil
  "Non-nil while a test run is in progress.
Code that registers global side effects (timers, `kill-emacs-hook'
entries, auto-save) honours this flag.  Defined here because
`synaxis-db' is loaded first; `synaxis.el' rebinds the docstring.")

;;; Customisation

(defcustom synaxis-db-file
  (expand-file-name "synaxis/synaxis.db" user-emacs-directory)
  "Path to the synaxis SQLite database."
  :type 'file
  :group 'synaxis)

;;; Transactions

(defmacro synaxis-db--with-transaction (db &rest body)
  "Run BODY inside a SQLite transaction on DB; roll back on non-local exit."
  (declare (indent 1) (debug t))
  (let ((db-sym (make-symbol "db"))
        (committed (make-symbol "committed")))
    `(let ((,db-sym ,db)
           (,committed nil))
       (sqlite-transaction ,db-sym)
       (unwind-protect
           (prog1 (progn ,@body)
             (sqlite-commit ,db-sym)
             (setq ,committed t))
         (unless ,committed
           (ignore-errors (sqlite-rollback ,db-sym)))))))

;;; Connection

(defvar synaxis-db--connection nil
  "Cached SQLite connection, or nil if not yet opened.")

(defconst synaxis-db--schema-target-version 1
  "Schema version the bootstrapper migrates databases up to.
Bump this and append a new entry to `synaxis-db--migrations' when
adding a schema change.")

(defconst synaxis-db--v1-statements
  '("CREATE TABLE IF NOT EXISTS feeds (
       url           TEXT    PRIMARY KEY,
       title         TEXT,
       type          TEXT    NOT NULL DEFAULT 'rss',
       last_fetched  REAL,
       last_modified TEXT,
       etag          TEXT,
       failures      INTEGER NOT NULL DEFAULT 0,
       meta          TEXT
     ) STRICT;"
    "CREATE TABLE IF NOT EXISTS entries (
       id           INTEGER PRIMARY KEY,
       feed_url     TEXT    NOT NULL REFERENCES feeds(url) ON DELETE CASCADE,
       source_id    TEXT    NOT NULL,
       title        TEXT    NOT NULL DEFAULT '',
       link         TEXT,
       date         REAL    NOT NULL,
       content      TEXT,
       content_type TEXT,
       meta         TEXT,
       UNIQUE (feed_url, source_id)
     ) STRICT;"
    "CREATE INDEX IF NOT EXISTS entries_date ON entries (date DESC);"
    "CREATE INDEX IF NOT EXISTS entries_feed ON entries (feed_url);"
    "CREATE TABLE IF NOT EXISTS entry_tags (
       entry_id INTEGER NOT NULL REFERENCES entries(id) ON DELETE CASCADE,
       tag      TEXT    NOT NULL,
       PRIMARY KEY (entry_id, tag)
     ) STRICT;"
    "CREATE INDEX IF NOT EXISTS entry_tags_tag ON entry_tags (tag);")
  "DDL statements for v1 (baseline feeds/entries/entry_tags schema).")

(defun synaxis-db--migration-0-to-1 (db)
  "Apply the v1 baseline schema to DB."
  (dolist (stmt synaxis-db--v1-statements)
    (sqlite-execute db stmt)))

(defvar synaxis-db--migrations
  '((1 . synaxis-db--migration-0-to-1))
  "Alist of (TARGET-VERSION . FUNCTION).
FUNCTION takes the open DB and moves the schema from TARGET-VERSION-1
to TARGET-VERSION.  Each call is wrapped in its own transaction by
`synaxis-db--migrate'; functions need not manage transactions
themselves.  Append a new entry when bumping
`synaxis-db--schema-target-version'.")

(defun synaxis-db--migrate (db from to)
  "Run pending migrations on DB for versions in (FROM, TO]."
  (cl-loop for v from (1+ from) to to
           for fn = (cdr (assq v synaxis-db--migrations))
           when fn
           do (synaxis-db--with-transaction db
                (funcall fn db)
                (sqlite-execute db
                                "UPDATE schema_version SET version = ?;"
                                (list v)))))

(defun synaxis-db--bootstrap (db)
  "Ensure DB is at `synaxis-db--schema-target-version'.
Records v0 as the starting point for fresh installs and applies
every pending entry in `synaxis-db--migrations' in order."
  (sqlite-execute db
                  "CREATE TABLE IF NOT EXISTS schema_version (version INTEGER NOT NULL);")
  (unless (sqlite-select db "SELECT version FROM schema_version LIMIT 1;")
    (sqlite-execute db "INSERT INTO schema_version (version) VALUES (0);"))
  (let ((current (caar (sqlite-select db "SELECT version FROM schema_version;"))))
    (when (< current synaxis-db--schema-target-version)
      (synaxis-db--migrate db current synaxis-db--schema-target-version))))

(defun synaxis-db--open ()
  "Open `synaxis-db-file', enable foreign keys, bootstrap schema."
  (let* ((file (expand-file-name synaxis-db-file))
         (dir  (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((db (sqlite-open file)))
      (sqlite-pragma db "foreign_keys = ON")
      (sqlite-pragma db "journal_mode = WAL")
      (synaxis-db--bootstrap db)
      db)))

(defun synaxis-db--ensure-open ()
  "Return the cached connection, opening it on first call."
  (or synaxis-db--connection
      (let ((db (synaxis-db--open)))
        (unless synaxis-testing
          (add-hook 'kill-emacs-hook #'synaxis-db-close))
        (setq synaxis-db--connection db))))

(defun synaxis-db-close ()
  "Close the cached SQLite connection, if any."
  (when synaxis-db--connection
    (ignore-errors (sqlite-close synaxis-db--connection))
    (setq synaxis-db--connection nil)))

;;; JSON helpers

(defun synaxis-db--encode-meta (plist)
  "Encode PLIST to a JSON string, or return nil for empty input."
  (and plist (json-serialize plist)))

(defun synaxis-db--decode-meta (text)
  "Decode TEXT (JSON) to a plist; nil for empty or missing input."
  (and text (not (string-empty-p text))
       (json-parse-string text :object-type 'plist :array-type 'list)))

;;; Feeds

(defun synaxis-db--row-to-feed-plist (row)
  "Convert ROW (column order matches `synaxis-db--feed-select') to a plist."
  (pcase-let ((`(,url ,title ,type ,last-fetched ,last-modified ,etag ,failures ,meta) row))
    (list :url url
          :title title
          :type type
          :last-fetched last-fetched
          :last-modified last-modified
          :etag etag
          :failures failures
          :meta (synaxis-db--decode-meta meta))))

(defconst synaxis-db--feed-columns
  "url, title, type, last_fetched, last_modified, etag, failures, meta"
  "Column list used to project feed rows into plists.")

(defun synaxis-db-add-feed (url &optional plist)
  "Insert or replace feed URL.
PLIST may contain `:title', `:type', `:meta'."
  (let ((db (synaxis-db--ensure-open)))
    (sqlite-execute
     db
     "INSERT INTO feeds (url, title, type, meta) VALUES (?, ?, ?, ?)
      ON CONFLICT(url) DO UPDATE SET
        title = excluded.title,
        type  = excluded.type,
        meta  = excluded.meta;"
     (list url
           (plist-get plist :title)
           (or (plist-get plist :type) "rss")
           (synaxis-db--encode-meta (plist-get plist :meta))))))

(defun synaxis-db-remove-feed (url)
  "Delete the feed at URL.  Cascades to entries and tags."
  (let ((db (synaxis-db--ensure-open)))
    (sqlite-execute db "DELETE FROM feeds WHERE url = ?;" (list url))))

(defun synaxis-db-get-feed (url)
  "Return the feed plist for URL, or nil."
  (let* ((db (synaxis-db--ensure-open))
         (row (car (sqlite-select
                    db
                    (concat "SELECT " synaxis-db--feed-columns
                            " FROM feeds WHERE url = ?;")
                    (list url)))))
    (and row (synaxis-db--row-to-feed-plist row))))

(defun synaxis-db-list-feeds ()
  "Return all feeds as plists, ordered alphabetically by title."
  (let ((db (synaxis-db--ensure-open)))
    (mapcar #'synaxis-db--row-to-feed-plist
            (sqlite-select
             db
             (concat "SELECT " synaxis-db--feed-columns
                     " FROM feeds ORDER BY title COLLATE NOCASE ASC, url ASC;")))))

(defun synaxis-db-set-feed-title-if-empty (url title)
  "Set feed URL's title to TITLE only if it is currently NULL or empty.
Used to back-fill the title from a parsed feed without clobbering
a title the user supplied at `synaxis-add-feed' time."
  (when (and title (not (string-empty-p title)))
    (let ((db (synaxis-db--ensure-open)))
      (sqlite-execute
       db
       "UPDATE feeds SET title = ?
        WHERE url = ? AND (title IS NULL OR title = '');"
       (list title url)))))

(defun synaxis-db-set-feed-cache-headers (url plist)
  "Update cache header fields on feed URL from PLIST.
Recognised keys: `:last-fetched', `:last-modified', `:etag', `:failures'.
Keys absent or nil leave the corresponding column unchanged."
  (let ((db (synaxis-db--ensure-open)))
    (sqlite-execute
     db
     "UPDATE feeds
      SET last_fetched  = COALESCE(?, last_fetched),
          last_modified = COALESCE(?, last_modified),
          etag          = COALESCE(?, etag),
          failures      = COALESCE(?, failures)
      WHERE url = ?;"
     (list (plist-get plist :last-fetched)
           (plist-get plist :last-modified)
           (plist-get plist :etag)
           (plist-get plist :failures)
           url))))

;;; Entries

(defconst synaxis-db--entry-columns
  "e.id, e.feed_url, e.source_id, e.title, e.link, e.date,
   e.content, e.content_type, e.meta, f.title,
   EXISTS (SELECT 1 FROM entry_tags t
           WHERE t.entry_id = e.id AND t.tag = 'unread') AS unread"
  "Column list used to project entry rows into plists (joined with feeds).
The trailing EXISTS column lets callers know `unread' status without a
second query.")

(defun synaxis-db--row-to-entry-plist (row)
  "Convert ROW (column order matches `synaxis-db--entry-columns') to a plist."
  (pcase-let ((`(,id ,feed-url ,source-id ,title ,link ,date
                     ,content ,ctype ,meta ,feed-title ,unread)
               row))
    (list :id id
          :feed-url feed-url
          :feed-title feed-title
          :source-id source-id
          :title title
          :link link
          :date date
          :content content
          :content-type ctype
          :meta (synaxis-db--decode-meta meta)
          :unread (not (zerop (or unread 0))))))

(defun synaxis-db-upsert-entry (plist)
  "Insert or update an entry described by PLIST.
Required keys: `:feed-url', `:source-id', `:title', `:date'.
Optional keys: `:link', `:content', `:content-type', `:meta'.
Returns the entry's primary key."
  (let* ((db (synaxis-db--ensure-open))
         (feed-url  (plist-get plist :feed-url))
         (source-id (plist-get plist :source-id))
         (title     (or (plist-get plist :title) ""))
         (link      (plist-get plist :link))
         (date      (plist-get plist :date))
         (content   (plist-get plist :content))
         (ctype     (plist-get plist :content-type))
         (meta      (synaxis-db--encode-meta (plist-get plist :meta))))
    (sqlite-execute
     db
     "INSERT INTO entries
        (feed_url, source_id, title, link, date, content, content_type, meta)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(feed_url, source_id) DO UPDATE SET
        title        = excluded.title,
        link         = excluded.link,
        date         = excluded.date,
        content      = excluded.content,
        content_type = excluded.content_type,
        meta         = excluded.meta;"
     (list feed-url source-id title link date content ctype meta))
    (synaxis-db-find-entry feed-url source-id)))

(defun synaxis-db-find-entry (feed-url source-id)
  "Return the entry id for FEED-URL and SOURCE-ID, or nil."
  (let* ((db (synaxis-db--ensure-open))
         (row (car (sqlite-select
                    db
                    "SELECT id FROM entries WHERE feed_url = ? AND source_id = ?;"
                    (list feed-url source-id)))))
    (and row (car row))))

(defun synaxis-db-get-entry (id)
  "Return the entry plist for ID, or nil."
  (let* ((db (synaxis-db--ensure-open))
         (row (car (sqlite-select
                    db
                    (concat "SELECT " synaxis-db--entry-columns
                            " FROM entries e
                              JOIN feeds f ON f.url = e.feed_url
                              WHERE e.id = ?;")
                    (list id)))))
    (and row (synaxis-db--row-to-entry-plist row))))

(defun synaxis-db-list-entries (where params &optional limit)
  "Return entries matching WHERE / PARAMS.
WHERE is a SQL fragment over aliases `e' (entries) and `f' (feeds);
nil means no filter.  PARAMS is its bind list.  LIMIT, when non-nil,
caps the result count.  Results are ordered by date descending."
  (let* ((db  (synaxis-db--ensure-open))
         (sql (concat "SELECT " synaxis-db--entry-columns
                      " FROM entries e
                        JOIN feeds f ON f.url = e.feed_url
                        WHERE " (or where "1=1")
                      " ORDER BY e.date DESC"
                      (if limit (format " LIMIT %d" limit) "")
                      ";")))
    (mapcar #'synaxis-db--row-to-entry-plist
            (sqlite-select db sql params))))

(defun synaxis-db-delete-entry (id)
  "Delete the entry with ID.  Cascades to tags."
  (let ((db (synaxis-db--ensure-open)))
    (sqlite-execute db "DELETE FROM entries WHERE id = ?;" (list id))))

;;; Tags

(defun synaxis-db-add-tag (entry-id tag)
  "Add TAG to ENTRY-ID.  Idempotent."
  (let ((db (synaxis-db--ensure-open)))
    (sqlite-execute
     db
     "INSERT OR IGNORE INTO entry_tags (entry_id, tag) VALUES (?, ?);"
     (list entry-id tag))))

(defun synaxis-db-remove-tag (entry-id tag)
  "Remove TAG from ENTRY-ID."
  (let ((db (synaxis-db--ensure-open)))
    (sqlite-execute
     db
     "DELETE FROM entry_tags WHERE entry_id = ? AND tag = ?;"
     (list entry-id tag))))

(defun synaxis-db-bulk-add-tag (entry-ids tag)
  "Add TAG to each id in ENTRY-IDS in a single transaction.
No-op when ENTRY-IDS is nil.  Idempotent via INSERT OR IGNORE."
  (when entry-ids
    (let ((db (synaxis-db--ensure-open)))
      (synaxis-db--with-transaction db
        (dolist (id entry-ids)
          (sqlite-execute
           db
           "INSERT OR IGNORE INTO entry_tags (entry_id, tag) VALUES (?, ?);"
           (list id tag)))))))

(defun synaxis-db-bulk-remove-tag (entry-ids tag)
  "Remove TAG from each id in ENTRY-IDS in a single transaction.
No-op when ENTRY-IDS is nil."
  (when entry-ids
    (let ((db (synaxis-db--ensure-open)))
      (synaxis-db--with-transaction db
        (dolist (id entry-ids)
          (sqlite-execute
           db
           "DELETE FROM entry_tags WHERE entry_id = ? AND tag = ?;"
           (list id tag)))))))

(defun synaxis-db-get-tags (entry-id)
  "Return the list of tag strings on ENTRY-ID."
  (let ((db (synaxis-db--ensure-open)))
    (mapcar #'car
            (sqlite-select
             db
             "SELECT tag FROM entry_tags WHERE entry_id = ?;"
             (list entry-id)))))

(provide 'synaxis-db)
;;; synaxis-db.el ends here
