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
