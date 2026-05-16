;;; synaxis-db-tests.el --- Tests for synaxis-db  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-db'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-db.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Setup helpers

(defmacro synaxis-db-tests--with-tmp (&rest body)
  "Run BODY with a fresh temporary database, then clean up."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "synaxis-db-test" t))
          (synaxis-db-file (expand-file-name "test.db" dir))
          (synaxis-testing t)
          (synaxis-db--connection nil))
     (unwind-protect
         (progn ,@body)
       (synaxis-db-close)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

;;; Migration framework

(ert-deftest synaxis-db-test-bootstrap-records-target-version ()
  "Fresh install lands at `synaxis-db--schema-target-version'."
  (synaxis-db-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     (should (= synaxis-db--schema-target-version
                (caar (sqlite-select db "SELECT version FROM schema_version;")))))))

(ert-deftest synaxis-db-test-bootstrap-idempotent ()
  "Re-bootstrapping an up-to-date DB doesn't run migrations again."
  (synaxis-db-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     ;; Run bootstrap a second time.
     (synaxis-db--bootstrap db)
     (should (= synaxis-db--schema-target-version
                (caar (sqlite-select db "SELECT version FROM schema_version;")))))))

(ert-deftest synaxis-db-test-migrate-advances-one-step ()
  "A pending migration bumps version exactly to its target."
  (synaxis-db-tests--with-tmp
   (let* ((db (synaxis-db--ensure-open))
          (saw 0)
          (synaxis-db--schema-target-version 99)
          (synaxis-db--migrations
           `((50 . ,(lambda (_db) (setq saw 50)))
             (51 . ,(lambda (_db) (setq saw 51))))))
     (sqlite-execute db "UPDATE schema_version SET version = 49;")
     (synaxis-db--migrate db 49 51)
     (should (= 51 saw))
     (should (= 51 (caar (sqlite-select db "SELECT version FROM schema_version;")))))))

;;; v2 schema and migration

(ert-deftest synaxis-db-test-v2-tags-table-present ()
  "Fresh install has the `tags' registry table."
  (synaxis-db-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     (should (sqlite-select
              db "SELECT name FROM sqlite_master
                  WHERE type='table' AND name='tags';")))))

(ert-deftest synaxis-db-test-entry-tags-fk-enforces-tag-existence ()
  "Inserting into entry_tags with an unregistered tag errors."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/fk")
   (let ((db (synaxis-db--ensure-open))
         (id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/fk" :source-id "1"
                          :title "T" :date 1.0))))
     (should-error
      (sqlite-execute db
                      "INSERT INTO entry_tags (entry_id, tag) VALUES (?, ?);"
                      (list id "unregistered"))))))

(ert-deftest synaxis-db-test-tag-rename-via-update-cascades ()
  "Updating tags.tag propagates to entry_tags via FK cascade."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/c")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/c" :source-id "1"
                          :title "T" :date 1.0))))
     (synaxis-db-add-tag id "rust")
     (let ((db (synaxis-db--ensure-open)))
       (sqlite-execute db "UPDATE tags SET tag = 'Rust' WHERE tag = 'rust';"))
     (should (member "Rust" (synaxis-db-get-tags id)))
     (should-not (member "rust" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-tag-delete-cascades ()
  "Deleting a row in tags cascades to entry_tags."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/d")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/d" :source-id "1"
                          :title "T" :date 1.0))))
     (synaxis-db-add-tag id "tmp")
     (let ((db (synaxis-db--ensure-open)))
       (sqlite-execute db "DELETE FROM tags WHERE tag = 'tmp';"))
     (should-not (member "tmp" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-migration-1-to-2-preserves-data ()
  "Seeding a v1 DB then bootstrapping migrates entry_tags rows intact."
  (synaxis-db-tests--with-tmp
   ;; Build a v1 database manually, side-stepping the bootstrap.
   (let* ((file (expand-file-name "v1.db"
                                  (file-name-directory synaxis-db-file)))
          (synaxis-db-file file)
          (db (sqlite-open file)))
     (sqlite-pragma db "foreign_keys = ON")
     (sqlite-execute db "CREATE TABLE schema_version (version INTEGER NOT NULL);")
     (sqlite-execute db "INSERT INTO schema_version (version) VALUES (1);")
     (dolist (stmt synaxis-db--v1-statements)
       (sqlite-execute db stmt))
     ;; Seed feed + entry + two tags.
     (sqlite-execute db "INSERT INTO feeds (url) VALUES ('https://x');")
     (sqlite-execute db
                     "INSERT INTO entries (feed_url, source_id, date)
                      VALUES ('https://x', 'a', 1.0);")
     (sqlite-execute db
                     "INSERT INTO entry_tags (entry_id, tag) VALUES (1, 'unread');")
     (sqlite-execute db
                     "INSERT INTO entry_tags (entry_id, tag) VALUES (1, 'rust');")
     (sqlite-close db)
     ;; Now reopen via the synaxis bootstrap, which should migrate to v2.
     (setq synaxis-db--connection nil)
     (let ((db (synaxis-db--ensure-open)))
       (should (= 2 (caar (sqlite-select db "SELECT version FROM schema_version;"))))
       (should (equal '(("rust") ("unread"))
                      (sqlite-select
                       db "SELECT tag FROM tags ORDER BY tag;")))
       ;; system flag set for `unread' only.
       (should (= 1 (caar (sqlite-select
                           db "SELECT system FROM tags WHERE tag = 'unread';"))))
       (should (= 0 (caar (sqlite-select
                           db "SELECT system FROM tags WHERE tag = 'rust';"))))
       ;; entry_tags rows preserved.
       (should (= 2 (caar (sqlite-select db "SELECT COUNT(*) FROM entry_tags;"))))))))

;;; Bootstrap

(ert-deftest synaxis-db-test-bootstrap-creates-schema-version ()
  "On first open the schema_version row is set to the current target."
  (synaxis-db-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     (should (= synaxis-db--schema-target-version
                (caar (sqlite-select db "SELECT version FROM schema_version;")))))))

;;; Feeds

(ert-deftest synaxis-db-test-add-and-get-feed ()
  "Adding a feed and reading it back round-trips all fields."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/feed.xml"
                        '(:title "Example" :type "atom"
                                 :meta (:author "Alice")))
   (let ((feed (synaxis-db-get-feed "https://example.com/feed.xml")))
     (should feed)
     (should (equal (plist-get feed :url) "https://example.com/feed.xml"))
     (should (equal (plist-get feed :title) "Example"))
     (should (equal (plist-get feed :type) "atom"))
     (should (equal (plist-get (plist-get feed :meta) :author) "Alice"))
     (should (equal (plist-get feed :failures) 0)))))

(ert-deftest synaxis-db-test-add-feed-defaults-type-to-rss ()
  "Omitting :type defaults to rss."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a" '(:title "A"))
   (should (equal "rss" (plist-get (synaxis-db-get-feed "https://example.com/a") :type)))))

(ert-deftest synaxis-db-test-list-feeds-ordered-by-title ()
  "Feeds are listed alphabetically by title (case-insensitive)."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://a" '(:title "Charlie"))
   (synaxis-db-add-feed "https://b" '(:title "alpha"))
   (synaxis-db-add-feed "https://c" '(:title "Bravo"))
   (let ((titles (mapcar (lambda (f) (plist-get f :title))
                         (synaxis-db-list-feeds))))
     (should (equal titles '("alpha" "Bravo" "Charlie"))))))

(ert-deftest synaxis-db-test-remove-feed-cascades-entries-and-tags ()
  "Removing a feed deletes its entries and entry tags."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/f")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/f"
                          :source-id "x" :title "T" :date 1.0))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-remove-feed "https://example.com/f")
     (should-not (synaxis-db-get-feed "https://example.com/f"))
     (should-not (synaxis-db-get-entry id))
     (should-not (synaxis-db-get-tags id)))))

(ert-deftest synaxis-db-test-set-feed-cache-headers-updates-fields ()
  "Cache header fields round-trip."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/h")
   (synaxis-db-set-feed-cache-headers
    "https://example.com/h"
    '(:last-fetched 100.0 :etag "W/\"abc\"" :last-modified "Tue, 02 Jan 2024 03:04:05 GMT"
                    :failures 2))
   (let ((feed (synaxis-db-get-feed "https://example.com/h")))
     (should (equal (plist-get feed :last-fetched) 100.0))
     (should (equal (plist-get feed :etag) "W/\"abc\""))
     (should (equal (plist-get feed :last-modified) "Tue, 02 Jan 2024 03:04:05 GMT"))
     (should (equal (plist-get feed :failures) 2)))))

(ert-deftest synaxis-db-test-set-feed-title-if-empty-fills-null ()
  "Back-fill applies when current title is NULL."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t1")
   (synaxis-db-set-feed-title-if-empty "https://example.com/t1" "Discovered")
   (should (equal "Discovered"
                  (plist-get (synaxis-db-get-feed "https://example.com/t1") :title)))))

(ert-deftest synaxis-db-test-set-feed-title-if-empty-preserves-existing ()
  "Back-fill is a no-op when a title is already set."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t2" '(:title "User Title"))
   (synaxis-db-set-feed-title-if-empty "https://example.com/t2" "Other")
   (should (equal "User Title"
                  (plist-get (synaxis-db-get-feed "https://example.com/t2") :title)))))

;;; Entries

(ert-deftest synaxis-db-test-upsert-entry-insert-and-update ()
  "First upsert inserts; second with same key updates."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/u")
   (let ((id1 (synaxis-db-upsert-entry
               '(:feed-url "https://example.com/u" :source-id "1"
                           :title "First" :date 1.0))))
     (should (integerp id1))
     (let ((id2 (synaxis-db-upsert-entry
                 '(:feed-url "https://example.com/u" :source-id "1"
                             :title "Second" :date 2.0))))
       (should (equal id1 id2))
       (let ((row (synaxis-db-get-entry id1)))
         (should (equal "Second" (plist-get row :title)))
         (should (equal 2.0 (plist-get row :date))))))))

(ert-deftest synaxis-db-test-get-entry-by-id ()
  "All entry columns round-trip, including JSON meta."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/g")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/g" :source-id "abc"
                          :title "Hello" :link "https://example.com/h" :date 10.0
                          :content "<p>hi</p>" :content-type "html"
                          :meta (:authors ["Alice" "Bob"])))))
     (let ((e (synaxis-db-get-entry id)))
       (should (equal "Hello" (plist-get e :title)))
       (should (equal "https://example.com/h" (plist-get e :link)))
       (should (equal "<p>hi</p>" (plist-get e :content)))
       (should (equal "html" (plist-get e :content-type)))
       (should (equal '("Alice" "Bob")
                      (plist-get (plist-get e :meta) :authors)))))))

(ert-deftest synaxis-db-test-find-entry-by-feed-and-source-id ()
  "find-entry returns the id or nil."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/l")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/l" :source-id "k"
                          :title "T" :date 1.0))))
     (should (equal id (synaxis-db-find-entry "https://example.com/l" "k")))
     (should-not (synaxis-db-find-entry "https://example.com/l" "missing")))))

(ert-deftest synaxis-db-test-list-entries-with-where ()
  "WHERE clauses scope the result."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a")
   (synaxis-db-add-feed "https://example.com/b")
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/a" :source-id "1"
                                        :title "A1" :date 1.0))
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/a" :source-id "2"
                                        :title "A2" :date 2.0))
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/b" :source-id "1"
                                        :title "B1" :date 3.0))
   (let ((titles (mapcar (lambda (e) (plist-get e :title))
                         (synaxis-db-list-entries "e.feed_url = ?"
                                                  '("https://example.com/a")))))
     (should (equal titles '("A2" "A1"))))))

(ert-deftest synaxis-db-test-list-entries-respects-limit ()
  "LIMIT caps the result count."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/lim")
   (dotimes (i 5)
     (synaxis-db-upsert-entry
      `(:feed-url "https://example.com/lim" :source-id ,(format "%d" i)
                  :title ,(format "T%d" i) :date ,(float i))))
   (should (= 2 (length (synaxis-db-list-entries nil nil 2))))))

(ert-deftest synaxis-db-test-list-entries-includes-unread-flag ()
  "Each row's :unread reflects the tag without a second query."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/u")
   (let ((id1 (synaxis-db-upsert-entry
               '(:feed-url "https://example.com/u" :source-id "1"
                           :title "U" :date 1.0)))
         (id2 (synaxis-db-upsert-entry
               '(:feed-url "https://example.com/u" :source-id "2"
                           :title "R" :date 2.0))))
     (synaxis-db-add-tag id1 "unread")
     (let* ((entries (synaxis-db-list-entries nil nil))
            (by-id (lambda (id) (seq-find (lambda (e) (eq id (plist-get e :id)))
                                          entries))))
       (should (plist-get (funcall by-id id1) :unread))
       (should-not (plist-get (funcall by-id id2) :unread))))))

(ert-deftest synaxis-db-test-list-entries-includes-feed-title ()
  "Joined query exposes feed title under :feed-title."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/j" '(:title "JFeed"))
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/j" :source-id "x"
                                        :title "T" :date 1.0))
   (let ((e (car (synaxis-db-list-entries nil nil))))
     (should (equal "JFeed" (plist-get e :feed-title))))))

(ert-deftest synaxis-db-test-delete-entry-cascades-tags ()
  "Deleting an entry deletes its tag rows."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/d")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/d" :source-id "1"
                          :title "T" :date 1.0))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-delete-entry id)
     (should-not (synaxis-db-get-entry id))
     (should-not (synaxis-db-get-tags id)))))

;;; Tags

(ert-deftest synaxis-db-test-add-tag-idempotent ()
  "Adding the same tag twice is fine."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/t" :source-id "1"
                          :title "T" :date 1.0))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "unread")
     (should (equal '("unread") (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-get-tags-returns-strings ()
  "get-tags returns a list of tag strings."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t2")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/t2" :source-id "1"
                          :title "T" :date 1.0))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "starred")
     (let ((tags (sort (synaxis-db-get-tags id) #'string<)))
       (should (equal tags '("starred" "unread")))))))

(ert-deftest synaxis-db-test-bulk-add-tag ()
  "Bulk-add-tag adds TAG only to the listed ids."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a")
   (let ((ids (cl-loop for i below 5
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/a"
                                            :source-id ,(format "%d" i)
                                            :title "T" :date ,(float i))))))
     (synaxis-db-bulk-add-tag (cl-subseq ids 0 3) "foo")
     (dolist (id (cl-subseq ids 0 3))
       (should (member "foo" (synaxis-db-get-tags id))))
     (dolist (id (cl-subseq ids 3))
       (should-not (member "foo" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-db-test-bulk-add-tag-idempotent ()
  "Adding the same tag twice via bulk yields no duplicates."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/i")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/i" :source-id "1"
                          :title "T" :date 1.0))))
     (synaxis-db-add-tag id "foo")
     (synaxis-db-bulk-add-tag (list id) "foo")
     (should (equal '("foo") (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-bulk-add-tag-empty-ids-noop ()
  "Empty ENTRY-IDS is a silent no-op."
  (synaxis-db-tests--with-tmp
   (should-not (synaxis-db-bulk-add-tag nil "foo"))))

(ert-deftest synaxis-db-test-bulk-add-tag-survives-large-batch ()
  "Bulk-add of >chunk-size ids still tags every entry."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/big")
   ;; Pretend the parameter limit is tiny so we exercise the chunking loop.
   (let ((synaxis-db--max-vars 8))
     (let ((ids (cl-loop for i below 50
                         collect (synaxis-db-upsert-entry
                                  `(:feed-url "https://example.com/big"
                                              :source-id ,(format "%d" i)
                                              :title "T" :date ,(float i))))))
       (synaxis-db-bulk-add-tag ids "mass")
       (dolist (id ids)
         (should (member "mass" (synaxis-db-get-tags id))))))))

(ert-deftest synaxis-db-test-bulk-remove-tag ()
  "Bulk-remove-tag clears TAG only from the listed ids."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/b")
   (let ((ids (cl-loop for i below 5
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/b"
                                            :source-id ,(format "%d" i)
                                            :title "T" :date ,(float i))))))
     (dolist (id ids) (synaxis-db-add-tag id "unread"))
     (synaxis-db-bulk-remove-tag (cl-subseq ids 0 3) "unread")
     (dolist (id (cl-subseq ids 0 3))
       (should-not (member "unread" (synaxis-db-get-tags id))))
     (dolist (id (cl-subseq ids 3))
       (should (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-db-test-bulk-remove-tag-empty-ids-noop ()
  "Passing nil as ENTRY-IDS is a silent no-op."
  (synaxis-db-tests--with-tmp
   (should-not (synaxis-db-bulk-remove-tag nil "unread"))))

(ert-deftest synaxis-db-test-remove-tag ()
  "remove-tag deletes only the named tag."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/rt")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/rt" :source-id "1"
                          :title "T" :date 1.0))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "starred")
     (synaxis-db-remove-tag id "unread")
     (should (equal '("starred") (synaxis-db-get-tags id))))))

;;; Transactions

(ert-deftest synaxis-db-test-with-transaction-rolls-back-on-error ()
  "An error inside the transaction rolls back partial inserts."
  (synaxis-db-tests--with-tmp
   (synaxis-db-add-feed "https://a.example/feed")
   (should-error
    (let ((db (synaxis-db--ensure-open)))
      (synaxis-db--with-transaction db
	(sqlite-execute db
			"INSERT INTO feeds (url) VALUES (?);"
			(list "https://b.example/feed"))
	(error "boom"))))
   (should-not (synaxis-db-get-feed "https://b.example/feed"))))

(provide 'synaxis-db-tests)
;;; synaxis-db-tests.el ends here
