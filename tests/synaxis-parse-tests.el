;;; synaxis-parse-tests.el --- Tests for synaxis-parse  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-parse'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-parse.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'synaxis-test-utils)

;;; Format detection

(ert-deftest synaxis-parse-test-detect-atom ()
  (should (eq 'atom (synaxis-parse--detect-format-string
                     (synaxis-tests--load-fixture "atom-1.0-minimal.xml")))))

(ert-deftest synaxis-parse-test-detect-rss ()
  (should (eq 'rss (synaxis-parse--detect-format-string
                    (synaxis-tests--load-fixture "rss-2.0-minimal.xml")))))

(ert-deftest synaxis-parse-test-detect-rss1 ()
  (should (eq 'rss1 (synaxis-parse--detect-format-string
                     (synaxis-tests--load-fixture "rss-1.0-rdf.xml")))))

(ert-deftest synaxis-parse-test-detect-json ()
  (should (eq 'json (synaxis-parse--detect-format-string
                     (synaxis-tests--load-fixture "json-feed-1.1.json")))))

;;; Date helpers

(ert-deftest synaxis-parse-test-iso8601-parses-common-shapes ()
  (should (numberp (synaxis-parse--decode-iso8601 "2024-01-02T03:04:05Z")))
  (should (numberp (synaxis-parse--decode-iso8601 "2024-01-02T03:04:05+02:00"))))

(ert-deftest synaxis-parse-test-rfc822-parses-common-shapes ()
  (should (numberp (synaxis-parse--decode-rfc822 "Tue, 02 Jan 2024 03:04:05 GMT")))
  (should (numberp (synaxis-parse--decode-rfc822 "02 Jan 2024 03:04:05 +0200"))))

(ert-deftest synaxis-parse-test-decode-date-falls-back-to-now ()
  (let ((before (float-time)))
    (should (>= (synaxis-parse--decode-date "completely bogus") before))))

;;; Atom

(ert-deftest synaxis-parse-test-atom-1.0-minimal ()
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "atom-1.0-minimal.xml")))
         (entries (plist-get feed :entries))
         (e (car entries)))
    (should (eq 'atom (plist-get feed :type)))
    (should (equal "Atom Test Feed" (plist-get feed :title)))
    (should (= 1 (length entries)))
    (should (equal "First Post" (plist-get e :title)))
    (should (equal "tag:example.com,2024:1" (plist-get e :source-id)))
    (should (equal "https://example.com/posts/1" (plist-get e :link)))
    (should (numberp (plist-get e :date)))
    (should (string-match-p "Hello" (or (plist-get e :content) "")))
    (should (equal "html" (plist-get e :content-type)))))

(ert-deftest synaxis-parse-test-atom-1.0-xhtml-content ()
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "atom-1.0-xhtml-content.xml")))
         (e (car (plist-get feed :entries))))
    (should (string-match-p "Hi there" (or (plist-get e :content) "")))))

(ert-deftest synaxis-parse-test-atom-no-id-synthesises-source-id ()
  (let* ((raw (synaxis-tests--load-fixture "atom-no-id.xml"))
         (e1 (car (plist-get (synaxis-parse-string raw) :entries)))
         (e2 (car (plist-get (synaxis-parse-string raw) :entries))))
    (should (stringp (plist-get e1 :source-id)))
    (should (not (string-empty-p (plist-get e1 :source-id))))
    (should (equal (plist-get e1 :source-id) (plist-get e2 :source-id)))))

(ert-deftest synaxis-parse-test-atom-xml-base-resolves-entry-link ()
  "Feed-level xml:base resolves an entry's relative href."
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "atom-1.0-xml-base.xml")
                "https://wherever.example.org/atom.xml"))
         (e (car (plist-get feed :entries))))
    (should (equal "https://feed.example.com/posts/entry-1"
                   (plist-get e :link)))))

(ert-deftest synaxis-parse-test-atom-entry-xml-base-overrides-feed ()
  "Entry-level xml:base shadows the feed-level one."
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "atom-1.0-xml-base.xml")
                "https://wherever.example.org/atom.xml"))
         (e (nth 1 (plist-get feed :entries))))
    (should (equal "https://override.example.com/entry-2"
                   (plist-get e :link)))))

(ert-deftest synaxis-parse-test-atom-protocol-relative-link-resolved ()
  "An Atom href like `//cdn.example/x' inherits the feed's scheme."
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "atom-1.0-protocol-relative-link.xml")
                "https://feeds.example.org/atom.xml"))
         (e (car (plist-get feed :entries))))
    (should (equal "https://cdn.example.com/post/1"
                   (plist-get e :link)))))

;;; RSS

(ert-deftest synaxis-parse-test-rss-2.0-minimal ()
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "rss-2.0-minimal.xml")))
         (e (car (plist-get feed :entries))))
    (should (eq 'rss (plist-get feed :type)))
    (should (equal "RSS Test" (plist-get feed :title)))
    (should (equal "RSS Item 1" (plist-get e :title)))
    (should (equal "https://example.com/r/1" (plist-get e :link)))
    (should (equal "https://example.com/r/1" (plist-get e :source-id)))
    (should (numberp (plist-get e :date)))
    (should (string-match-p "plain desc" (or (plist-get e :content) "")))))

(ert-deftest synaxis-parse-test-rss-2.0-prefers-content-encoded ()
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "rss-2.0-content-encoded.xml")))
         (e (car (plist-get feed :entries))))
    (should (string-match-p "Full HTML content" (or (plist-get e :content) "")))
    (should-not (string-match-p "short summary" (or (plist-get e :content) "")))))

(ert-deftest synaxis-parse-test-rss-protocol-relative-link-resolved ()
  "RSS `<link>//cdn.../1</link>' inherits the feed URL's scheme."
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "rss-2.0-protocol-relative-link.xml")
                "https://feeds.example.org/rss.xml"))
         (e (car (plist-get feed :entries))))
    (should (equal "https://cdn.example.com/r/1" (plist-get e :link)))))

(ert-deftest synaxis-parse-test-rss-cdata-wrapped-link ()
  "CDATA around a `<link>' value is unwrapped to the bare URL."
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "rss-2.0-cdata-link.xml")))
         (e (car (plist-get feed :entries))))
    (should (equal "http://nullprogram.com/" (plist-get e :link)))))

;;; RSS 1.0 (RDF)

(ert-deftest synaxis-parse-test-rss-1.0-rdf ()
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "rss-1.0-rdf.xml")))
         (e (car (plist-get feed :entries))))
    (should (eq 'rss1 (plist-get feed :type)))
    (should (equal "RSS 1.0 Test" (plist-get feed :title)))
    (should (equal "RDF Item" (plist-get e :title)))
    (should (equal "https://example.com/rdf/1" (plist-get e :link)))
    (should (numberp (plist-get e :date)))))

;;; JSON Feed

(ert-deftest synaxis-parse-test-json-feed-1.1 ()
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "json-feed-1.1.json")))
         (e (car (plist-get feed :entries))))
    (should (eq 'json (plist-get feed :type)))
    (should (equal "JSON Feed Test" (plist-get feed :title)))
    (should (equal "JSON Item" (plist-get e :title)))
    (should (equal "1" (plist-get e :source-id)))
    (should (equal "https://example.com/j/1" (plist-get e :link)))
    (should (numberp (plist-get e :date)))
    (should (string-match-p "JSON content" (or (plist-get e :content) "")))))

(provide 'synaxis-parse-tests)
;;; synaxis-parse-tests.el ends here
