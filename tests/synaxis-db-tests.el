;;; synaxis-db-tests.el --- Tests for synaxis-db  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-db'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-db.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'synaxis-test-utils)

;;; Migration framework

(ert-deftest synaxis-db-test-bootstrap-records-target-version ()
  "Fresh install lands at `synaxis-db--schema-target-version'."
  (synaxis-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     (should (= synaxis-db--schema-target-version
                (caar (sqlite-select db "SELECT version FROM schema_version;")))))))

(ert-deftest synaxis-db-test-bootstrap-idempotent ()
  "Re-bootstrapping an up-to-date DB doesn't run migrations again."
  (synaxis-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     ;; Run bootstrap a second time.
     (synaxis-db--bootstrap db)
     (should (= synaxis-db--schema-target-version
                (caar (sqlite-select db "SELECT version FROM schema_version;")))))))

(ert-deftest synaxis-db-test-migrate-advances-one-step ()
  "A pending migration bumps version exactly to its target."
  (synaxis-tests--with-tmp
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

;;; Tags registry

(ert-deftest synaxis-db-test-tags-table-present ()
  "Fresh install has the `tags' registry table."
  (synaxis-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     (should (sqlite-select
              db "SELECT name FROM sqlite_master
                  WHERE type='table' AND name='tags';")))))

(ert-deftest synaxis-db-test-entry-tags-fk-enforces-tag-existence ()
  "Inserting into entry_tags with an unregistered tag errors."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/fk")
   (let ((db (synaxis-db--ensure-open))
         (id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/fk" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (should-error
      (sqlite-execute db
                      "INSERT INTO entry_tags (entry_id, tag) VALUES (?, ?);"
                      (list id "unregistered"))))))

(ert-deftest synaxis-db-test-tag-rename-via-update-cascades ()
  "Updating tags.tag propagates to entry_tags via FK cascade."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/c")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/c" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "rust")
     (let ((db (synaxis-db--ensure-open)))
       (sqlite-execute db "UPDATE tags SET tag = 'Rust' WHERE tag = 'rust';"))
     (should (member "Rust" (synaxis-db-get-tags id)))
     (should-not (member "rust" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-tag-delete-cascades ()
  "Deleting a row in tags cascades to entry_tags."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/d")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/d" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "tmp")
     (let ((db (synaxis-db--ensure-open)))
       (sqlite-execute db "DELETE FROM tags WHERE tag = 'tmp';"))
     (should-not (member "tmp" (synaxis-db-get-tags id))))))

;;; Bootstrap

(ert-deftest synaxis-db-test-bootstrap-creates-schema-version ()
  "On first open the schema_version row is set to the current target."
  (synaxis-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     (should (= synaxis-db--schema-target-version
                (caar (sqlite-select db "SELECT version FROM schema_version;")))))))

;;; Feeds

(ert-deftest synaxis-db-test-add-and-get-feed ()
  "Adding a feed and reading it back round-trips all fields."
  (synaxis-tests--with-tmp
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
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a" '(:title "A"))
   (should (equal "rss" (plist-get (synaxis-db-get-feed "https://example.com/a") :type)))))

(ert-deftest synaxis-db-test-list-feeds-ordered-by-title ()
  "Feeds are listed alphabetically by title (case-insensitive)."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://a" '(:title "Charlie"))
   (synaxis-db-add-feed "https://b" '(:title "alpha"))
   (synaxis-db-add-feed "https://c" '(:title "Bravo"))
   (let ((titles (mapcar (lambda (f) (plist-get f :title))
                         (synaxis-db-list-feeds))))
     (should (equal titles '("alpha" "Bravo" "Charlie"))))))

(ert-deftest synaxis-db-test-remove-feed-cascades-entries-and-tags ()
  "Removing a feed deletes its entries and entry tags."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/f")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/f"
                          :source-id "x" :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-remove-feed "https://example.com/f")
     (should-not (synaxis-db-get-feed "https://example.com/f"))
     (should-not (synaxis-db-get-entry id))
     (should-not (synaxis-db-get-tags id)))))

(ert-deftest synaxis-db-test-set-feed-cache-headers-updates-fields ()
  "Cache header fields round-trip."
  (synaxis-tests--with-tmp
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
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t1")
   (synaxis-db-set-feed-title-if-empty "https://example.com/t1" "Discovered")
   (should (equal "Discovered"
                  (plist-get (synaxis-db-get-feed "https://example.com/t1") :title)))))

(ert-deftest synaxis-db-test-set-feed-title-if-empty-preserves-existing ()
  "Back-fill is a no-op when a title is already set."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t2" '(:title "User Title"))
   (synaxis-db-set-feed-title-if-empty "https://example.com/t2" "Other")
   (should (equal "User Title"
                  (plist-get (synaxis-db-get-feed "https://example.com/t2") :title)))))

;;; Entries

(ert-deftest synaxis-db-test-upsert-entry-insert-and-update ()
  "First upsert inserts; second with same key updates."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/u")
   (let ((id1 (synaxis-db-upsert-entry
               '(:feed-url "https://example.com/u" :source-id "1"
                           :title "First" :date "1970-01-01T00:00:01Z"))))
     (should (integerp id1))
     (let ((id2 (synaxis-db-upsert-entry
                 '(:feed-url "https://example.com/u" :source-id "1"
                             :title "Second" :date "1970-01-01T00:00:02Z"))))
       (should (equal id1 id2))
       (let ((row (synaxis-db-get-entry id1)))
         (should (equal "Second" (plist-get row :title)))
         (should (equal "1970-01-01T00:00:02Z" (plist-get row :date))))))))

(ert-deftest synaxis-db-test-upsert-with-tags-preserve-date-keeps-first-seen ()
  "With PRESERVE-DATE, a re-upsert keeps the original stored date."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/s")
   (synaxis-db-upsert-with-tags
    "https://example.com/s" nil
    (list :source-id "x" :title "First" :date "2026-06-01T00:00:00Z") t)
   (synaxis-db-upsert-with-tags
    "https://example.com/s" nil
    (list :source-id "x" :title "Second" :date "2026-06-10T00:00:00Z") t)
   (let* ((id (synaxis-db-find-entry "https://example.com/s" "x"))
          (row (synaxis-db-get-entry id)))
     ;; Date held at first-seen; other fields still update.
     (should (equal "2026-06-01T00:00:00Z" (plist-get row :date)))
     (should (equal "Second" (plist-get row :title))))))

(ert-deftest synaxis-db-test-upsert-with-tags-without-preserve-updates-date ()
  "Without PRESERVE-DATE, a re-upsert updates the date (default)."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/s")
   (synaxis-db-upsert-with-tags
    "https://example.com/s" nil
    (list :source-id "x" :title "First" :date "2026-06-01T00:00:00Z"))
   (synaxis-db-upsert-with-tags
    "https://example.com/s" nil
    (list :source-id "x" :title "Second" :date "2026-06-10T00:00:00Z"))
   (let* ((id (synaxis-db-find-entry "https://example.com/s" "x"))
          (row (synaxis-db-get-entry id)))
     (should (equal "2026-06-10T00:00:00Z" (plist-get row :date))))))

(ert-deftest synaxis-db-test-get-entry-by-id ()
  "All entry columns round-trip, including JSON meta."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/g")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/g" :source-id "abc"
                          :title "Hello" :link "https://example.com/h" :date "1970-01-01T00:00:10Z"
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
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/l")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/l" :source-id "k"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (should (equal id (synaxis-db-find-entry "https://example.com/l" "k")))
     (should-not (synaxis-db-find-entry "https://example.com/l" "missing")))))

(ert-deftest synaxis-db-test-list-entries-with-where ()
  "WHERE clauses scope the result."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a")
   (synaxis-db-add-feed "https://example.com/b")
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/a" :source-id "1"
                                        :title "A1" :date "1970-01-01T00:00:01Z"))
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/a" :source-id "2"
                                        :title "A2" :date "1970-01-01T00:00:02Z"))
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/b" :source-id "1"
                                        :title "B1" :date "1970-01-01T00:00:03Z"))
   (let ((titles (mapcar (lambda (e) (plist-get e :title))
                         (synaxis-db-list-entries "e.feed_url = ?"
                                                  '("https://example.com/a")))))
     (should (equal titles '("A2" "A1"))))))

(ert-deftest synaxis-db-test-list-entries-respects-limit ()
  "LIMIT caps the result count."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/lim")
   (dotimes (i 5)
     (synaxis-db-upsert-entry
      `(:feed-url "https://example.com/lim" :source-id ,(format "%d" i)
                  :title ,(format "T%d" i) :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" i t))))
   (should (= 2 (length (synaxis-db-list-entries nil nil 2))))))

(ert-deftest synaxis-db-test-list-entries-includes-unread-flag ()
  "Each row's :unread reflects the tag without a second query."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/u")
   (let ((id1 (synaxis-db-upsert-entry
               '(:feed-url "https://example.com/u" :source-id "1"
                           :title "U" :date "1970-01-01T00:00:01Z")))
         (id2 (synaxis-db-upsert-entry
               '(:feed-url "https://example.com/u" :source-id "2"
                           :title "R" :date "1970-01-01T00:00:02Z"))))
     (synaxis-db-add-tag id1 "unread")
     (let* ((entries (synaxis-db-list-entries nil nil))
            (by-id (lambda (id) (seq-find (lambda (e) (eq id (plist-get e :id)))
                                          entries))))
       (should (plist-get (funcall by-id id1) :unread))
       (should-not (plist-get (funcall by-id id2) :unread))))))

(ert-deftest synaxis-db-test-list-entries-includes-tags-list ()
  "Each row's :tags reflects every tag attached to the entry."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/tt")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/tt" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "starred")
     (let* ((entry (car (synaxis-db-list-entries nil nil)))
            (tags  (plist-get entry :tags)))
       (should (member "unread" tags))
       (should (member "starred" tags))))))

(ert-deftest synaxis-db-test-list-entries-empty-tags-yields-nil ()
  "An untagged entry has :tags nil and :unread nil."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/empty")
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/empty"
                                        :source-id "1"
                                        :title "T" :date "1970-01-01T00:00:01Z"))
   (let ((entry (car (synaxis-db-list-entries nil nil))))
     (should-not (plist-get entry :tags))
     (should-not (plist-get entry :unread)))))

(ert-deftest synaxis-db-test-list-entries-includes-feed-title ()
  "Joined query exposes feed title under :feed-title."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/j" '(:title "JFeed"))
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/j" :source-id "x"
                                        :title "T" :date "1970-01-01T00:00:01Z"))
   (let ((e (car (synaxis-db-list-entries nil nil))))
     (should (equal "JFeed" (plist-get e :feed-title))))))

(ert-deftest synaxis-db-test-delete-entry-cascades-tags ()
  "Deleting an entry deletes its tag rows."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/d")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/d" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-delete-entry id)
     (should-not (synaxis-db-get-entry id))
     (should-not (synaxis-db-get-tags id)))))

;;; Scrape rules CRUD

(ert-deftest synaxis-db-test-scrape-rule-round-trip ()
  "Add then read back every supported scrape-rule field."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/sc" '(:type "scrape"))
   (synaxis-db-add-scrape-rule
    "https://example.com/sc"
    '(:url-selector     "h2 a"
                        :url-pattern      "/post/"
                        :content-selector "article.post"
                        :content-cleanup  ".ads"
                        :title-cleanup    " - Site"
                        :date-selector    "time"
                        :date-format      "%Y-%m-%d"
                        :limit            5))
   (let ((r (synaxis-db-get-scrape-rule "https://example.com/sc")))
     (should (equal "h2 a"           (plist-get r :url-selector)))
     (should (equal "/post/"         (plist-get r :url-pattern)))
     (should (equal "article.post"   (plist-get r :content-selector)))
     (should (equal ".ads"           (plist-get r :content-cleanup)))
     (should (equal " - Site"        (plist-get r :title-cleanup)))
     (should (equal "time"           (plist-get r :date-selector)))
     (should (equal "%Y-%m-%d"       (plist-get r :date-format)))
     (should (eq 5                   (plist-get r :limit))))))

(ert-deftest synaxis-db-test-scrape-rule-replace-on-conflict ()
  "Re-adding a rule for the same feed updates rather than errors."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/sc" '(:type "scrape"))
   (synaxis-db-add-scrape-rule "https://example.com/sc"
                               '(:url-selector "a"))
   (synaxis-db-add-scrape-rule "https://example.com/sc"
                               '(:url-selector "h2 a"))
   (should (equal "h2 a"
                  (plist-get (synaxis-db-get-scrape-rule
                              "https://example.com/sc")
                             :url-selector)))))

(ert-deftest synaxis-db-test-scrape-rule-nil-for-unknown ()
  (synaxis-tests--with-tmp
   (should-not (synaxis-db-get-scrape-rule "https://nope/"))))

(ert-deftest synaxis-db-test-list-scrape-rules ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a" '(:type "scrape"))
   (synaxis-db-add-feed "https://example.com/b" '(:type "scrape"))
   (synaxis-db-add-scrape-rule "https://example.com/a" '(:url-selector "x"))
   (synaxis-db-add-scrape-rule "https://example.com/b" '(:url-selector "y"))
   (let ((rs (synaxis-db-list-scrape-rules)))
     (should (= 2 (length rs)))
     (should (assoc "https://example.com/a" rs)))))

;;; Partial-update helpers

(ert-deftest synaxis-db-test-set-feed-title-force-overwrites ()
  "set-feed-title overwrites the existing title unconditionally."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t" '(:title "Old"))
   (synaxis-db-set-feed-title "https://example.com/t" "New")
   (should (equal "New"
                  (plist-get (synaxis-db-get-feed "https://example.com/t")
                             :title)))))

(ert-deftest synaxis-db-test-set-feed-autotags-replaces-list ()
  "set-feed-autotags replaces autotags but keeps other meta keys."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a"
                        '(:meta (:autotags ["old"] :author "Alice")))
   (synaxis-db-set-feed-autotags "https://example.com/a" '("new" "fresh"))
   (let* ((feed (synaxis-db-get-feed "https://example.com/a"))
          (meta (plist-get feed :meta))
          (tags (append (plist-get meta :autotags) nil)))
     (should (equal '("new" "fresh") tags))
     ;; Other meta preserved.
     (should (equal "Alice" (plist-get meta :author))))))

(ert-deftest synaxis-db-test-update-scrape-rule-field ()
  "update-scrape-rule-field changes one key only."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/sc" '(:type "scrape"))
   (synaxis-db-add-scrape-rule
    "https://example.com/sc"
    '(:url-selector "h2 a" :content-selector "article"
                    :title-cleanup " - Site"))
   (synaxis-db-update-scrape-rule-field
    "https://example.com/sc" :url-pattern "/post/")
   (let ((r (synaxis-db-get-scrape-rule "https://example.com/sc")))
     ;; Updated.
     (should (equal "/post/" (plist-get r :url-pattern)))
     ;; Preserved.
     (should (equal "h2 a" (plist-get r :url-selector)))
     (should (equal "article" (plist-get r :content-selector)))
     (should (equal " - Site" (plist-get r :title-cleanup))))))

(ert-deftest synaxis-db-test-update-scrape-rule-field-errors-when-missing ()
  (synaxis-tests--with-tmp
   (should-error (synaxis-db-update-scrape-rule-field
                  "https://nope/" :url-selector "x")
                 :type 'user-error)))

;;; Tag registry CRUD

(ert-deftest synaxis-db-test-list-tags-returns-registry ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/r")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/r" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "rust"))
   (let* ((rows (synaxis-db-list-tags))
          (by-tag (mapcar (lambda (r) (cons (plist-get r :tag) r)) rows)))
     (should (assoc "rust" by-tag))
     (should (assoc "unread" by-tag))
     (should (plist-get (cdr (assoc "unread" by-tag)) :system))
     (should-not (plist-get (cdr (assoc "rust" by-tag)) :system)))))

(ert-deftest synaxis-db-test-rename-tag-no-conflict-uses-cascade ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/rn")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/rn" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "rust")
     (synaxis-db-rename-tag "rust" "Rust")
     (should (member "Rust" (synaxis-db-get-tags id)))
     (should-not (member "rust" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-rename-tag-merges-on-conflict ()
  "Renaming OLD to an existing NEW merges memberships and removes OLD."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/m")
   (let ((id1 (synaxis-db-upsert-entry
               '(:feed-url "https://example.com/m" :source-id "1"
                           :title "T" :date "1970-01-01T00:00:01Z")))
         (id2 (synaxis-db-upsert-entry
               '(:feed-url "https://example.com/m" :source-id "2"
                           :title "T" :date "1970-01-01T00:00:02Z"))))
     (synaxis-db-add-tag id1 "alpha")
     (synaxis-db-add-tag id2 "beta")
     ;; id2 already tagged beta; renaming alpha -> beta merges.
     (synaxis-db-rename-tag "alpha" "beta")
     (should (member "beta" (synaxis-db-get-tags id1)))
     (should (member "beta" (synaxis-db-get-tags id2)))
     (should-not (member "alpha" (synaxis-db-get-tags id1)))
     (let ((tags (mapcar (lambda (r) (plist-get r :tag))
                         (synaxis-db-list-tags))))
       (should (member "beta" tags))
       (should-not (member "alpha" tags))))))

(ert-deftest synaxis-db-test-rename-tag-rejects-empty ()
  (synaxis-tests--with-tmp
   (should-error (synaxis-db-rename-tag "" "new")  :type 'user-error)
   (should-error (synaxis-db-rename-tag "old" "")  :type 'user-error)
   (should-error (synaxis-db-rename-tag "x" "x")   :type 'user-error)))

(ert-deftest synaxis-db-test-delete-tag-cascades-to-entry-tags ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/del")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/del" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "doomed")
     (synaxis-db-delete-tag "doomed")
     (should-not (member "doomed" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-set-tag-face-round-trips ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/f")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/f" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "x"))
   (synaxis-db-set-tag-face "x" 'warning)
   (let ((entry (cl-find "x" (synaxis-db-list-tags)
                         :test (lambda (a b) (equal a (plist-get b :tag))))))
     (should (equal "warning" (plist-get entry :face))))
   (synaxis-db-set-tag-face "x" nil)
   (let ((entry (cl-find "x" (synaxis-db-list-tags)
                         :test (lambda (a b) (equal a (plist-get b :tag))))))
     (should (null (plist-get entry :face))))))

;;; Tags

(ert-deftest synaxis-db-test-add-tag-idempotent ()
  "Adding the same tag twice is fine."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/t" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "unread")
     (should (equal '("unread") (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-get-tags-returns-strings ()
  "get-tags returns a list of tag strings."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/t2")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/t2" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "starred")
     (let ((tags (sort (synaxis-db-get-tags id) #'string<)))
       (should (equal tags '("starred" "unread")))))))

(ert-deftest synaxis-db-test-bulk-add-tag ()
  "Bulk-add-tag adds TAG only to the listed ids."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a")
   (let ((ids (cl-loop for i below 5
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/a"
                                            :source-id ,(format "%d" i)
                                            :title "T" :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" i t))))))
     (synaxis-db-bulk-add-tag (cl-subseq ids 0 3) "foo")
     (dolist (id (cl-subseq ids 0 3))
       (should (member "foo" (synaxis-db-get-tags id))))
     (dolist (id (cl-subseq ids 3))
       (should-not (member "foo" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-db-test-bulk-add-tag-idempotent ()
  "Adding the same tag twice via bulk yields no duplicates."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/i")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/i" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "foo")
     (synaxis-db-bulk-add-tag (list id) "foo")
     (should (equal '("foo") (synaxis-db-get-tags id))))))

(ert-deftest synaxis-db-test-bulk-add-tag-empty-ids-noop ()
  "Empty ENTRY-IDS is a silent no-op."
  (synaxis-tests--with-tmp
   (should-not (synaxis-db-bulk-add-tag nil "foo"))))

(ert-deftest synaxis-db-test-bulk-add-tag-survives-large-batch ()
  "Bulk-add of >chunk-size ids still tags every entry."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/big")
   ;; Pretend the parameter limit is tiny so we exercise the chunking loop.
   (let ((synaxis-db--max-vars 8))
     (let ((ids (cl-loop for i below 50
                         collect (synaxis-db-upsert-entry
                                  `(:feed-url "https://example.com/big"
                                              :source-id ,(format "%d" i)
                                              :title "T" :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" i t))))))
       (synaxis-db-bulk-add-tag ids "mass")
       (dolist (id ids)
         (should (member "mass" (synaxis-db-get-tags id))))))))

(ert-deftest synaxis-db-test-bulk-remove-tag ()
  "Bulk-remove-tag clears TAG only from the listed ids."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/b")
   (let ((ids (cl-loop for i below 5
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/b"
                                            :source-id ,(format "%d" i)
                                            :title "T" :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" i t))))))
     (dolist (id ids) (synaxis-db-add-tag id "unread"))
     (synaxis-db-bulk-remove-tag (cl-subseq ids 0 3) "unread")
     (dolist (id (cl-subseq ids 0 3))
       (should-not (member "unread" (synaxis-db-get-tags id))))
     (dolist (id (cl-subseq ids 3))
       (should (member "unread" (synaxis-db-get-tags id)))))))

(ert-deftest synaxis-db-test-bulk-remove-tag-empty-ids-noop ()
  "Passing nil as ENTRY-IDS is a silent no-op."
  (synaxis-tests--with-tmp
   (should-not (synaxis-db-bulk-remove-tag nil "unread"))))

(ert-deftest synaxis-db-test-remove-tag ()
  "remove-tag deletes only the named tag."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/rt")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/rt" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "starred")
     (synaxis-db-remove-tag id "unread")
     (should (equal '("starred") (synaxis-db-get-tags id))))))

;;; Transactions

(ert-deftest synaxis-db-test-with-transaction-rolls-back-on-error ()
  "An error inside the transaction rolls back partial inserts."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://a.example/feed")
   (should-error
    (let ((db (synaxis-db--ensure-open)))
      (synaxis-db--with-transaction db
	(sqlite-execute db
			"INSERT INTO feeds (url) VALUES (?);"
			(list "https://b.example/feed"))
	(error "boom"))))
   (should-not (synaxis-db-get-feed "https://b.example/feed"))))

(ert-deftest synaxis-db-test-add-feed-title-only-preserves-scrape-type ()
  "Title-only re-add must not clobber an existing scrape type."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/sc" '(:type "scrape" :title "S"))
   (synaxis-db-add-feed "https://example.com/sc" '(:title "S2"))
   (let ((feed (synaxis-db-get-feed "https://example.com/sc")))
     (should (equal "scrape" (plist-get feed :type)))
     (should (equal "S2" (plist-get feed :title))))))

(ert-deftest synaxis-db-test-set-feed-cache-headers-clears-etag ()
  "Explicit nil etag/last-modified clears; omit-key preserves."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/h")
   (synaxis-db-set-feed-cache-headers
    "https://example.com/h"
    '(:etag "W/\"abc\"" :last-modified "Tue, 02 Jan 2024 03:04:05 GMT"
            :failures 1))
   (synaxis-db-set-feed-cache-headers
    "https://example.com/h"
    '(:etag nil :last-modified nil))
   (let ((feed (synaxis-db-get-feed "https://example.com/h")))
     (should-not (plist-get feed :etag))
     (should-not (plist-get feed :last-modified))
     (should (equal 1 (plist-get feed :failures))))
   (synaxis-db-set-feed-cache-headers
    "https://example.com/h"
    '(:etag "keep-me"))
   (synaxis-db-set-feed-cache-headers
    "https://example.com/h"
    '(:failures 0))
   (let ((feed (synaxis-db-get-feed "https://example.com/h")))
     (should (equal "keep-me" (plist-get feed :etag)))
     (should (equal 0 (plist-get feed :failures))))))

(provide 'synaxis-db-tests)
;;; synaxis-db-tests.el ends here
