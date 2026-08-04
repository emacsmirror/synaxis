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
  (should (equal "2024-01-02T03:04:05Z"
                 (synaxis-parse--decode-iso8601 "2024-01-02T03:04:05Z")))
  (should (equal "2024-01-02T01:04:05Z"
                 (synaxis-parse--decode-iso8601 "2024-01-02T03:04:05+02:00"))))

(ert-deftest synaxis-parse-test-rfc822-parses-common-shapes ()
  (should (equal "2024-01-02T03:04:05Z"
                 (synaxis-parse--decode-rfc822 "Tue, 02 Jan 2024 03:04:05 GMT")))
  (should (equal "2024-01-02T01:04:05Z"
                 (synaxis-parse--decode-rfc822 "02 Jan 2024 03:04:05 +0200"))))

(ert-deftest synaxis-parse-test-decode-date-falls-back-to-now ()
  (let ((before (synaxis-parse--time-to-iso (current-time))))
    (should (not (string< (synaxis-parse--decode-date "completely bogus") before)))))

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
    (should (stringp (plist-get e :date)))
    (should (string-match-p "Hello" (or (plist-get e :content) "")))
    (should (equal "html" (plist-get e :content-type)))))

(ert-deftest synaxis-parse-test-atom-1.0-xhtml-content ()
  (let* ((feed (synaxis-parse-string
                (synaxis-tests--load-fixture "atom-1.0-xhtml-content.xml")))
         (e (car (plist-get feed :entries))))
    (should (string-match-p "Hi there" (or (plist-get e :content) "")))))

(ert-deftest synaxis-parse-test-atom-uses-direct-children-not-nested ()
  "Entry fields come from direct children, not same-named tags nested
in xhtml content.  Here `content' precedes `title' and embeds a
`<title>' element; with the old recursive `dom-by-tag' the entry
title would resolve to the nested \"NESTED\" node."
  (let* ((xml "<?xml version=\"1.0\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>Feed</title>
  <entry>
    <content type=\"xhtml\"><div xmlns=\"http://www.w3.org/1999/xhtml\"><title>NESTED</title>body</div></content>
    <title>Real Title</title>
    <id>urn:x:1</id>
    <updated>2024-01-01T00:00:00Z</updated>
  </entry>
</feed>")
         (e (car (plist-get (synaxis-parse-string xml) :entries))))
    (should (equal "Real Title" (plist-get e :title)))
    (should (equal "urn:x:1" (plist-get e :source-id)))))

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
    (should (stringp (plist-get e :date)))
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
    (should (stringp (plist-get e :date)))))

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
    (should (stringp (plist-get e :date)))
    (should (string-match-p "JSON content" (or (plist-get e :content) "")))))

;;; Group: charset decoding

(ert-deftest synaxis-parse-test-charset->coding ()
  (should (eq 'utf-8 (synaxis-parse--charset->coding "UTF-8")))
  (should (eq 'iso-8859-7 (synaxis-parse--charset->coding "ISO-8859-7")))
  (should (eq 'iso-8859-1 (synaxis-parse--charset->coding "latin1")))
  (should-not (synaxis-parse--charset->coding "no-such-charset-xyz")))

(ert-deftest synaxis-parse-test-decode-bytes-content-type-wins ()
  "An explicit Content-Type charset is honoured."
  (let* ((s "Εστία")
         (bytes (encode-coding-string s 'iso-8859-7)))
    (should (equal s (synaxis-parse--decode-bytes
                      bytes "text/xml; charset=ISO-8859-7")))))

(ert-deftest synaxis-parse-test-decode-bytes-xml-declaration ()
  "Without a Content-Type, the XML encoding declaration is used."
  (let* ((doc "<?xml version=\"1.0\" encoding=\"ISO-8859-7\"?>\n<x>Εστία</x>")
         (bytes (encode-coding-string doc 'iso-8859-7)))
    (should (string-match-p "Εστία" (synaxis-parse--decode-bytes bytes nil)))))

(ert-deftest synaxis-parse-test-decode-bytes-html-meta ()
  "Without a Content-Type, an HTML <meta charset> is used."
  (let* ((doc "<html><head><meta charset=\"iso-8859-7\"></head><body>Εστία</body></html>")
         (bytes (encode-coding-string doc 'iso-8859-7)))
    (should (string-match-p "Εστία" (synaxis-parse--decode-bytes bytes nil)))))

(ert-deftest synaxis-parse-test-decode-bytes-utf-8-fallback ()
  "With no charset hint at all, UTF-8 is assumed."
  (let* ((s "café ☕")
         (bytes (encode-coding-string s 'utf-8)))
    (should (equal s (synaxis-parse--decode-bytes bytes nil)))))

(ert-deftest synaxis-parse-test-date-only-iso-is-midnight-utc ()
  (should (equal "2024-01-01T00:00:00Z"
                 (synaxis-parse--decode-date "2024-01-01"))))

(ert-deftest synaxis-parse-test-json-date-modified-and-summary-external-url ()
  (let* ((raw "{\"version\":\"https://jsonfeed.org/version/1.1\",\"items\":[{\"id\":\"1\",\"title\":\"T\",\"external_url\":\"https://example.com/x\",\"date_modified\":\"2020-06-15T12:00:00Z\",\"summary\":\"SUM\"}]}")
         (e (car (plist-get (synaxis-parse-string raw) :entries))))
    (should (equal "2020-06-15T12:00:00Z" (plist-get e :date)))
    (should (equal "https://example.com/x" (plist-get e :link)))
    (should (equal "SUM" (plist-get e :content)))))

(ert-deftest synaxis-parse-test-empty-guid-falls-back-to-link ()
  (let* ((xml "<?xml version=\"1.0\"?><rss version=\"2.0\"><channel>
<item><title>A</title><link>https://example.com/a</link><guid></guid></item>
<item><title>B</title><link>https://example.com/b</link><guid></guid></item>
</channel></rss>")
         (entries (plist-get (synaxis-parse-string xml) :entries)))
    (should (= 2 (length entries)))
    (should (equal "https://example.com/a" (plist-get (nth 0 entries) :source-id)))
    (should (equal "https://example.com/b" (plist-get (nth 1 entries) :source-id)))))

(ert-deftest synaxis-parse-test-orphan-source-id-stable-without-date ()
  (let* ((e1 (list :title "Orphan" :date "2024-01-01T00:00:00Z"))
         (e2 (list :title "Orphan" :date "2024-01-02T00:00:00Z"))
         (id1 (synaxis-parse--source-id e1 "https://f.example/x"))
         (id2 (synaxis-parse--source-id e2 "https://f.example/x")))
    (should (equal id1 id2))
    (should (string-prefix-p "synaxis:sha1:" id1))))

(ert-deftest synaxis-parse-test-rss1-about-attr-without-prefix ()
  (let* ((xml "<?xml version=\"1.0\"?>
<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
         xmlns=\"http://purl.org/rss/1.0/\">
  <channel rdf:about=\"https://example.com/\"><title>C</title><link>https://example.com/</link></channel>
  <item rdf:about=\"https://example.com/about-only\">
    <title>About Only</title>
  </item>
</rdf:RDF>")
         (e (car (plist-get (synaxis-parse-string xml) :entries))))
    (should (equal "https://example.com/about-only" (plist-get e :source-id)))))

(ert-deftest synaxis-parse-test-atom-empty-content-falls-back-to-summary ()
  (let* ((xml "<?xml version=\"1.0\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>F</title>
  <entry>
    <title>T</title>
    <id>urn:x:1</id>
    <updated>2024-01-01T00:00:00Z</updated>
    <content type=\"text\"></content>
    <summary type=\"text\">SUMBODY</summary>
  </entry>
</feed>")
         (e (car (plist-get (synaxis-parse-string xml) :entries))))
    (should (equal "SUMBODY" (plist-get e :content)))))

(ert-deftest synaxis-parse-test-undated-marks-date-synthetic ()
  (let* ((xml "<?xml version=\"1.0\"?><rss version=\"2.0\"><channel>
<item><title>U</title><guid>g1</guid><link>https://example.com/u</link></item>
</channel></rss>")
         (e (car (plist-get (synaxis-parse-string xml) :entries))))
    (should (plist-get e :date-synthetic))
    (should (stringp (plist-get e :date)))))

(provide 'synaxis-parse-tests)
;;; synaxis-parse-tests.el ends here
