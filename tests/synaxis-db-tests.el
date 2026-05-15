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

;;; Bootstrap

(ert-deftest synaxis-db-test-bootstrap-creates-schema-version ()
  "On first open the schema_version row is set to 1."
  (synaxis-db-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     (should (equal '((1)) (sqlite-select db "SELECT version FROM schema_version;"))))))

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
