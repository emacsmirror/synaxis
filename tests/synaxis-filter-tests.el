;;; synaxis-filter-tests.el --- Tests for synaxis-filter  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-filter'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-filter.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'synaxis-test-utils)

;;; Parse

(ert-deftest synaxis-filter-test-parse-empty ()
  (should (null (synaxis-filter-parse "")))
  (should (null (synaxis-filter-parse "   "))))

(ert-deftest synaxis-filter-test-parse-tag-and-not-tag ()
  (should (equal '((tag . "unread")) (synaxis-filter-parse "tag:unread")))
  (should (equal '((not-tag . "later")) (synaxis-filter-parse "-tag:later"))))

(ert-deftest synaxis-filter-test-parse-feed-and-not-feed ()
  (should (equal '((feed . "hackaday")) (synaxis-filter-parse "feed:hackaday")))
  (should (equal '((not-feed . "yt")) (synaxis-filter-parse "-feed:yt"))))

(ert-deftest synaxis-filter-test-parse-title-and-not-title ()
  (should (equal '((title . "rust")) (synaxis-filter-parse "title:rust")))
  (should (equal '((not-title . "draft")) (synaxis-filter-parse "-title:draft"))))

(ert-deftest synaxis-filter-test-parse-content-and-not-content ()
  (should (equal '((content . "openssh")) (synaxis-filter-parse "content:openssh")))
  (should (equal '((not-content . "spam")) (synaxis-filter-parse "-content:spam"))))

(ert-deftest synaxis-filter-test-parse-date-keyword ()
  (let ((toks (synaxis-filter-parse "date:today")))
    (should (= 1 (length toks)))
    (should (eq 'date (car (car toks))))
    (should (numberp (plist-get (cdr (car toks)) :from)))
    (should (numberp (plist-get (cdr (car toks)) :to)))))

(ert-deftest synaxis-filter-test-parse-date-relative ()
  (let ((toks (synaxis-filter-parse "date:7d")))
    (should (eq 'date (car (car toks))))
    (should (numberp (plist-get (cdr (car toks)) :from)))
    (should (numberp (plist-get (cdr (car toks)) :to)))))

(ert-deftest synaxis-filter-test-parse-date-absolute ()
  (let* ((toks (synaxis-filter-parse "date:2024-03-15"))
         (spec (cdr (car toks))))
    (should (eq 'date (car (car toks))))
    (should (= 86400 (- (plist-get spec :to) (plist-get spec :from))))))

(ert-deftest synaxis-filter-test-parse-limit ()
  (should (equal '((limit . 50)) (synaxis-filter-parse "limit:50"))))

(ert-deftest synaxis-filter-test-parse-bare-word-becomes-text ()
  (should (equal '((text . "rust")) (synaxis-filter-parse "rust"))))

(ert-deftest synaxis-filter-test-parse-multiple-bare-words ()
  (should (equal '((text . "foo") (text . "bar"))
                 (synaxis-filter-parse "foo bar"))))

(ert-deftest synaxis-filter-test-parse-mixed ()
  (let ((toks (synaxis-filter-parse "tag:unread -feed:yt title:rust limit:25 foo")))
    (should (= 5 (length toks)))
    (should (eq 'tag (car (nth 0 toks))))
    (should (eq 'not-feed (car (nth 1 toks))))
    (should (eq 'title (car (nth 2 toks))))
    (should (eq 'limit (car (nth 3 toks))))
    (should (eq 'text (car (nth 4 toks))))))

(ert-deftest synaxis-filter-test-parse-unknown-prefix-dropped ()
  (should (null (synaxis-filter-parse "weirdkey:value"))))

(ert-deftest synaxis-filter-test-parse-not-bare-word-dropped ()
  (should (null (synaxis-filter-parse "-rust"))))

(ert-deftest synaxis-filter-test-parse-not-date-dropped ()
  "Negating a date is meaningless; the token is dropped."
  (should (null (synaxis-filter-parse "-date:today"))))

(ert-deftest synaxis-filter-test-parse-not-limit-dropped ()
  (should (null (synaxis-filter-parse "-limit:10"))))

;;; Compile

(ert-deftest synaxis-filter-test-compile-empty-tautology ()
  (let ((c (synaxis-filter-compile nil)))
    (should (equal "1=1" (plist-get c :where)))
    (should (null (plist-get c :params)))
    (should (null (plist-get c :limit)))))

(ert-deftest synaxis-filter-test-compile-tag-uses-exists ()
  (let ((c (synaxis-filter-compile '((tag . "unread")))))
    (should (string-match-p "\\bEXISTS\\b" (plist-get c :where)))
    (should-not (string-match-p "NOT EXISTS" (plist-get c :where)))
    (should (equal '("unread") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-not-tag-uses-not-exists ()
  (let ((c (synaxis-filter-compile '((not-tag . "later")))))
    (should (string-match-p "NOT EXISTS" (plist-get c :where)))
    (should (equal '("later") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-feed-uses-like ()
  (let ((c (synaxis-filter-compile '((feed . "hackaday")))))
    (should (string-match-p "f\\.title LIKE" (plist-get c :where)))
    (should (equal '("%hackaday%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-not-feed-uses-not-like ()
  (let ((c (synaxis-filter-compile '((not-feed . "yt")))))
    (should (string-match-p "f\\.title NOT LIKE" (plist-get c :where)))))

(ert-deftest synaxis-filter-test-compile-title-uses-like ()
  (let ((c (synaxis-filter-compile '((title . "rust")))))
    (should (string-match-p "e\\.title LIKE" (plist-get c :where)))
    (should (equal '("%rust%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-content-uses-like ()
  (let ((c (synaxis-filter-compile '((content . "ssh")))))
    (should (string-match-p "e\\.content LIKE" (plist-get c :where)))
    (should (equal '("%ssh%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-text-binds-twice ()
  (let ((c (synaxis-filter-compile '((text . "foo")))))
    (should (equal '("%foo%" "%foo%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-emits-bounds-both ()
  (let* ((spec '(:from 1000.0 :to 2000.0))
         (c (synaxis-filter-compile (list (cons 'date spec)))))
    (should (string-match-p "e\\.date >= \\?" (plist-get c :where)))
    (should (string-match-p "e\\.date < \\?"  (plist-get c :where)))
    (should (equal '("1970-01-01T00:16:40Z" "1970-01-01T00:33:20Z")
                   (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-from-only ()
  (let* ((spec '(:from 1000.0 :to nil))
         (c (synaxis-filter-compile (list (cons 'date spec)))))
    (should (string-match-p "e\\.date >= \\?" (plist-get c :where)))
    (should-not (string-match-p "e\\.date < " (plist-get c :where)))
    (should (equal '("1970-01-01T00:16:40Z") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-to-only ()
  (let* ((spec '(:from nil :to 2000.0))
         (c (synaxis-filter-compile (list (cons 'date spec)))))
    (should-not (string-match-p "e\\.date >= " (plist-get c :where)))
    (should (string-match-p "e\\.date < \\?" (plist-get c :where)))
    (should (equal '("1970-01-01T00:33:20Z") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-limit-sets-plist-key ()
  (should (= 25 (plist-get (synaxis-filter-compile '((limit . 25))) :limit))))

(ert-deftest synaxis-filter-test-compile-multiple-tags-ANDed ()
  (let ((c (synaxis-filter-compile '((tag . "a") (tag . "b")))))
    (should (string-match-p " AND " (plist-get c :where)))
    (should (equal '("a" "b") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-mixed-everything ()
  (let* ((toks (synaxis-filter-parse "tag:unread -feed:yt title:rust limit:10 foo"))
         (c    (synaxis-filter-compile toks)))
    (should (= 10 (plist-get c :limit)))
    ;; tag(1) + not-feed(1) + title(1) + text(2) = 5 params
    (should (= 5 (length (plist-get c :params))))))

;;; Date spec

(ert-deftest synaxis-filter-test-date-spec-today ()
  (let ((spec (synaxis-filter-parse-date-spec "today")))
    (should (numberp (plist-get spec :from)))
    (should (numberp (plist-get spec :to)))
    (should (= 86400 (- (plist-get spec :to) (plist-get spec :from))))))

(ert-deftest synaxis-filter-test-date-spec-yesterday ()
  (let* ((y (synaxis-filter-parse-date-spec "yesterday"))
         (t1 (synaxis-filter-parse-date-spec "today")))
    (should (= 86400 (- (plist-get t1 :from) (plist-get y :from))))
    (should (= (plist-get y :to) (plist-get t1 :from)))))

(ert-deftest synaxis-filter-test-date-spec-7d ()
  (let ((spec (synaxis-filter-parse-date-spec "7d")))
    (should (numberp (plist-get spec :from)))
    (should (numberp (plist-get spec :to)))
    (should (< (abs (- 604800 (- (plist-get spec :to) (plist-get spec :from))))
               2.0))))

(ert-deftest synaxis-filter-test-date-spec-2024 ()
  (let ((spec (synaxis-filter-parse-date-spec "2024")))
    (should (= (float-time (encode-time 0 0 0 1 1 2024))
               (plist-get spec :from)))
    (should (= (float-time (encode-time 0 0 0 1 1 2025))
               (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-2024-03 ()
  (let ((spec (synaxis-filter-parse-date-spec "2024-03")))
    (should (= (float-time (encode-time 0 0 0 1 3 2024))
               (plist-get spec :from)))
    (should (= (float-time (encode-time 0 0 0 1 4 2024))
               (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-2024-03-15 ()
  (let ((spec (synaxis-filter-parse-date-spec "2024-03-15")))
    (should (= (float-time (encode-time 0 0 0 15 3 2024))
               (plist-get spec :from)))
    (should (= 86400 (- (plist-get spec :to) (plist-get spec :from))))))

(ert-deftest synaxis-filter-test-date-spec-december-rolls-year ()
  "Month-only spec for December rolls correctly to January of next year."
  (let ((spec (synaxis-filter-parse-date-spec "2024-12")))
    (should (= (float-time (encode-time 0 0 0 1 1 2025))
               (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-thisweek ()
  (let ((spec (synaxis-filter-parse-date-spec "thisweek")))
    (should (numberp (plist-get spec :from)))
    (should (numberp (plist-get spec :to)))
    (should (<= (plist-get spec :from) (plist-get spec :to)))
    ;; Window covers at most 7 days.
    (should (<= (- (plist-get spec :to) (plist-get spec :from))
                (* 7 86400)))))

(ert-deftest synaxis-filter-test-date-spec-thismonth ()
  (let* ((spec (synaxis-filter-parse-date-spec "thismonth"))
         (d (decode-time))
         (expected-from
          (float-time (encode-time 0 0 0 1
                                   (decoded-time-month d)
                                   (decoded-time-year d)))))
    (should (= expected-from (plist-get spec :from)))))

(ert-deftest synaxis-filter-test-date-spec-thisyear ()
  (let* ((spec (synaxis-filter-parse-date-spec "thisyear"))
         (d (decode-time))
         (expected-from
          (float-time (encode-time 0 0 0 1 1 (decoded-time-year d)))))
    (should (= expected-from (plist-get spec :from)))))

(ert-deftest synaxis-filter-test-date-spec-range-explicit ()
  (let ((spec (synaxis-filter-parse-date-spec "2024-01..2024-03")))
    (should (= (float-time (encode-time 0 0 0 1 1 2024))
               (plist-get spec :from)))
    (should (= (float-time (encode-time 0 0 0 1 4 2024))
               (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-range-open-end ()
  (let ((spec (synaxis-filter-parse-date-spec "2024-01..")))
    (should (= (float-time (encode-time 0 0 0 1 1 2024))
               (plist-get spec :from)))
    (should (null (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-range-open-start ()
  (let ((spec (synaxis-filter-parse-date-spec "..2024-12-31")))
    (should (null (plist-get spec :from)))
    (should (= (float-time (encode-time 0 0 0 1 1 2025))
               (plist-get spec :to)))))

(ert-deftest synaxis-filter-test-date-spec-range-double-dots-only-nil ()
  "Bare `..' with no bounds is invalid."
  (should (null (synaxis-filter-parse-date-spec ".."))))

(ert-deftest synaxis-filter-test-date-spec-invalid-nil ()
  (should (null (synaxis-filter-parse-date-spec "garbage")))
  (should (null (synaxis-filter-parse-date-spec "")))
  (should (null (synaxis-filter-parse-date-spec "2024-99-99"))))

;;; Completions

(ert-deftest synaxis-filter-test-completions-include-static-prefixes ()
  (synaxis-tests--with-tmp
   (let ((c (synaxis-filter-completions)))
     (should (member "tag:" c))
     (should (member "-tag:" c))
     (should (member "date:" c))
     (should (member "limit:" c))
     (should (member "date:today" c))
     (should (member "date:thisweek" c)))))

(ert-deftest synaxis-filter-test-completions-pull-from-tags-registry ()
  "Registered tags appear in completion even when no entry carries them."
  (synaxis-tests--with-tmp
   (let ((db (synaxis-db--ensure-open)))
     (sqlite-execute db "INSERT INTO tags (tag) VALUES (?);" '("orphan-tag")))
   (let ((c (synaxis-filter-completions)))
     (should (member "tag:orphan-tag" c)))))

(ert-deftest synaxis-filter-test-completions-include-tag-values ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/x")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/x" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-db-add-tag id "unread")
     (synaxis-db-add-tag id "starred"))
   (let ((c (synaxis-filter-completions)))
     (should (member "tag:unread" c))
     (should (member "tag:starred" c))
     (should (member "-tag:unread" c))
     (should (member "-tag:starred" c)))))

(ert-deftest synaxis-filter-test-completions-include-feed-titles ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/a" '(:title "Alpha"))
   (synaxis-db-add-feed "https://example.com/b" '(:title "Bravo"))
   (let ((c (synaxis-filter-completions)))
     (should (member "feed:Alpha" c))
     (should (member "feed:Bravo" c))
     (should (member "-feed:Alpha" c)))))

(ert-deftest synaxis-filter-test-completions-omit-entry-titles ()
  "Entry titles are not offered as completions: `title:' is free-text LIKE."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/x")
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/x" :source-id "1"
                                        :title "Recent Post" :date "1970-01-01T00:01:40Z"))
   (let ((c (synaxis-filter-completions)))
     (should-not (member "title:\"Recent Post\"" c))
     (should-not (member "title:Recent Post" c))
     ;; The bare prefix is still offered.
     (should (member "title:" c)))))

;;; Tokenizer (plan-09)

(ert-deftest synaxis-filter-test-tokenize-bare-words ()
  (should (equal '("a" "b" "c")
                 (synaxis-filter--tokenize "a  b\tc"))))

(ert-deftest synaxis-filter-test-tokenize-quoted-value ()
  (should (equal '("feed:PubMed Trending")
                 (synaxis-filter--tokenize "feed:\"PubMed Trending\""))))

(ert-deftest synaxis-filter-test-tokenize-bare-quoted ()
  (should (equal '("exact phrase")
                 (synaxis-filter--tokenize "\"exact phrase\""))))

(ert-deftest synaxis-filter-test-tokenize-mixed ()
  (should (equal '("tag:hardware"
                   "feed:PubMed Trending"
                   "-tag:promo"
                   "exact phrase")
                 (synaxis-filter--tokenize
                  "tag:hardware feed:\"PubMed Trending\" -tag:promo \"exact phrase\""))))

(ert-deftest synaxis-filter-test-tokenize-empty-quoted-dropped ()
  (should (equal '("a" "b")
                 (synaxis-filter--tokenize "a \"\" b"))))

(ert-deftest synaxis-filter-test-tokenize-unmatched-quote-forgiving ()
  (should (equal '("feed:PubMed Trending")
                 (synaxis-filter--tokenize "feed:\"PubMed Trending"))))

(ert-deftest synaxis-filter-test-tokenize-escaped-quote ()
  (should (equal '("title:He said \"hi\"")
                 (synaxis-filter--tokenize
                  "title:\"He said \\\"hi\\\"\""))))

;;; Quoted-value parse + compile

(ert-deftest synaxis-filter-test-parse-quoted-feed ()
  (should (equal '((feed . "PubMed Trending"))
                 (synaxis-filter-parse "feed:\"PubMed Trending\""))))

(ert-deftest synaxis-filter-test-parse-quoted-not-tag ()
  (should (equal '((not-tag . "slow read"))
                 (synaxis-filter-parse "-tag:\"slow read\""))))

(ert-deftest synaxis-filter-test-compile-quoted-feed ()
  (let* ((tokens (synaxis-filter-parse "feed:\"PubMed Trending\""))
         (c (synaxis-filter-compile tokens)))
    (should (string-match-p "f\\.title LIKE \\?" (plist-get c :where)))
    (should (equal '("%PubMed Trending%") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-not-tag-quoted ()
  (let* ((tokens (synaxis-filter-parse "-tag:\"slow read\""))
         (c (synaxis-filter-compile tokens)))
    (should (equal '("slow read") (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-bare-quoted-text ()
  (let* ((tokens (synaxis-filter-parse "\"exact phrase\""))
         (c (synaxis-filter-compile tokens)))
    (should (equal '("%exact phrase%" "%exact phrase%")
                   (plist-get c :params)))))

;;; Completion quoting

(ert-deftest synaxis-filter-test-quote-if-needed ()
  (should (equal "plain" (synaxis-filter--quote-if-needed "plain")))
  (should (equal "\"two words\""
                 (synaxis-filter--quote-if-needed "two words"))))

(ert-deftest synaxis-filter-test-prefix-each-quotes-spaced-values ()
  (let ((out (synaxis-filter--prefix-each
              "feed:" '("Hackaday" "PubMed Trending"))))
    (should (member "feed:Hackaday" out))
    (should (member "feed:\"PubMed Trending\"" out))))

;;; Tag-rules end-to-end with quoted filter

(ert-deftest synaxis-filter-test-tag-rule-quoted-feed-tags-entry ()
  "A tag rule with a quoted multi-word feed title tags matching entries."
  (synaxis-tests--with-tmp
   (require 'synaxis)
   (synaxis-db-add-feed "https://example.com/pm" '(:title "PubMed Trending"))
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/pm" :source-id "1"
                          :title "A study" :date "1970-01-01T00:01:40Z"))))
     (let ((synaxis-tag-rules
            '((:filter "feed:\"PubMed Trending\"" :add ("medicine")))))
       (synaxis-tag-rules-apply-entry id))
     (should (member "medicine" (synaxis-db-get-tags id))))))

;;; now-form date parsing

(ert-deftest synaxis-filter-test-date-spec-now-bare ()
  "`now' yields a point spec equal to current epoch (within 1s tolerance)."
  (let* ((before (float-time))
         (spec (synaxis-filter-parse-date-spec "now"))
         (after (float-time)))
    (should spec)
    (should (= (plist-get spec :from) (plist-get spec :to)))
    (should (<= before (plist-get spec :from) after))))

(ert-deftest synaxis-filter-test-date-spec-now-minus-7d ()
  "`now-7d' yields a point spec offset by exactly 7 days."
  (let* ((before (float-time))
         (spec (synaxis-filter-parse-date-spec "now-7d"))
         (after (float-time))
         (t-val (plist-get spec :from)))
    (should spec)
    (should (= t-val (plist-get spec :to)))
    (should (<= (- before (* 7 86400)) t-val (- after (* 7 86400))))))

(ert-deftest synaxis-filter-test-date-spec-now-plus-1h ()
  "`now+1h' resolves to a positive offset."
  (let* ((before (float-time))
         (spec (synaxis-filter-parse-date-spec "now+1h"))
         (t-val (plist-get spec :from)))
    (should (>= t-val (+ before 3599)))))

(ert-deftest synaxis-filter-test-date-spec-now-bogus-unit-rejected ()
  "`now-7x' is not a valid spec."
  (should (null (synaxis-filter-parse-date-spec "now-7x"))))

(ert-deftest synaxis-filter-test-date-spec-now-no-sign-no-offset-rejected ()
  "`now7d' (no sign) is not a valid spec."
  (should (null (synaxis-filter-parse-date-spec "now7d"))))

;;; date comparison-operator token parsing

(ert-deftest synaxis-filter-test-parse-date-cmp-ge-now-7d ()
  "`date:>=now-7d' yields one date-cmp token with op `>=' and time = now-7d."
  (let* ((toks (synaxis-filter-parse "date:>=now-7d")))
    (should (= 1 (length toks)))
    (should (eq 'date-cmp (car (car toks))))
    (should (equal ">=" (plist-get (cdr (car toks)) :op)))
    (should (numberp (plist-get (cdr (car toks)) :time)))))

(ert-deftest synaxis-filter-test-parse-date-cmp-lt-iso ()
  "`date:<2024-01-01' resolves :time to start of Jan 1 2024."
  (let* ((toks (synaxis-filter-parse "date:<2024-01-01"))
         (cmp (cdr (car toks)))
         (expected (float-time (encode-time 0 0 0 1 1 2024))))
    (should (equal "<" (plist-get cmp :op)))
    (should (= expected (plist-get cmp :time)))))

(ert-deftest synaxis-filter-test-parse-date-cmp-gt-span-uses-to ()
  "`date:>2024-03' resolves :time to start of Apr 2024 (the span's :to)."
  (let* ((toks (synaxis-filter-parse "date:>2024-03"))
         (cmp (cdr (car toks)))
         (expected (float-time (encode-time 0 0 0 1 4 2024))))
    (should (equal ">" (plist-get cmp :op)))
    (should (= expected (plist-get cmp :time)))))

(ert-deftest synaxis-filter-test-parse-date-cmp-le-span-uses-to ()
  "`date:<=2024-03' resolves :time to start of Apr 2024."
  (let* ((toks (synaxis-filter-parse "date:<=2024-03"))
         (cmp (cdr (car toks)))
         (expected (float-time (encode-time 0 0 0 1 4 2024))))
    (should (equal "<=" (plist-get cmp :op)))
    (should (= expected (plist-get cmp :time)))))

(ert-deftest synaxis-filter-test-parse-date-cmp-ge-span-uses-from ()
  "`date:>=2024-03' resolves :time to start of Mar 2024 (the span's :from)."
  (let* ((toks (synaxis-filter-parse "date:>=2024-03"))
         (cmp (cdr (car toks)))
         (expected (float-time (encode-time 0 0 0 1 3 2024))))
    (should (= expected (plist-get cmp :time)))))

(ert-deftest synaxis-filter-test-parse-date-without-operator-falls-back-to-range ()
  "`date:2024-03-15' still yields a `date' token (not date-cmp)."
  (let ((toks (synaxis-filter-parse "date:2024-03-15")))
    (should (eq 'date (car (car toks))))))

(ert-deftest synaxis-filter-test-parse-date-cmp-bogus-value-dropped ()
  "`date:>=floopy' has an unparseable value; token is dropped."
  (should (null (synaxis-filter-parse "date:>=floopy"))))

(ert-deftest synaxis-filter-test-parse-negated-date-cmp-dropped ()
  "`-date:>=now-7d' is meaningless; token is dropped (consistent with `-date:')."
  (should (null (synaxis-filter-parse "-date:>=now-7d"))))

;;; date-cmp compilation

(ert-deftest synaxis-filter-test-compile-date-cmp-ge-emits-single-clause ()
  "`date:>=now-7d' compiles to exactly one `e.date >= ?' clause."
  (let* ((toks (synaxis-filter-parse "date:>=now-7d"))
         (c (synaxis-filter-compile toks)))
    (should (string-match-p "\\`e\\.date >= \\?\\'" (plist-get c :where)))
    (should (= 1 (length (plist-get c :params))))))

(ert-deftest synaxis-filter-test-compile-date-cmp-gt-maps-to-ge ()
  "`date:>2024-03' compiles to `e.date >= ?' with start of Apr 2024 as param."
  (let* ((toks (synaxis-filter-parse "date:>2024-03"))
         (c (synaxis-filter-compile toks))
         (expected (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                       (encode-time 0 0 0 1 4 2024) t)))
    (should (string-match-p "e\\.date >= \\?" (plist-get c :where)))
    (should (equal (list expected) (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-cmp-lt-emits-strict-lower ()
  "`date:<2024-01-01' compiles to `e.date < ?' with start of Jan 1 as param."
  (let* ((toks (synaxis-filter-parse "date:<2024-01-01"))
         (c (synaxis-filter-compile toks))
         (expected (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                       (encode-time 0 0 0 1 1 2024) t)))
    (should (string-match-p "e\\.date < \\?" (plist-get c :where)))
    (should (equal (list expected) (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-cmp-le-emits-strict-upper ()
  "`date:<=2024-03' compiles to `e.date < ?' with start of Apr 2024."
  (let* ((toks (synaxis-filter-parse "date:<=2024-03"))
         (c (synaxis-filter-compile toks))
         (expected (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                       (encode-time 0 0 0 1 4 2024) t)))
    (should (string-match-p "e\\.date < \\?" (plist-get c :where)))
    (should (equal (list expected) (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-cmp-conjoins-two ()
  "Two date-cmp tokens AND into two clauses."
  (let* ((toks (synaxis-filter-parse "date:>=now-30d date:<now-7d"))
         (c (synaxis-filter-compile toks))
         (w (plist-get c :where)))
    (should (string-match-p "e\\.date >= \\?" w))
    (should (string-match-p "e\\.date < \\?" w))
    (should (string-match-p " AND " w))
    (should (= 2 (length (plist-get c :params))))))

(ert-deftest synaxis-filter-test-compile-date-cmp-mixes-with-tag ()
  "date-cmp composes with tag tokens."
  (let* ((toks (synaxis-filter-parse "tag:emacs date:>=now-7d"))
         (c (synaxis-filter-compile toks)))
    (should (string-match-p "EXISTS" (plist-get c :where)))
    (should (string-match-p "e\\.date >= \\?" (plist-get c :where)))
    (should (= 2 (length (plist-get c :params))))))

;;; bare now-form intervals (not comparison ops)

(ert-deftest synaxis-filter-test-compile-bare-now-not-impossible ()
  "`date:now' compiles open-ended from now: single `e.date >= ?', no upper bound."
  (let* ((toks (synaxis-filter-parse "date:now"))
         (spec (cdr (car toks)))
         (c (synaxis-filter-compile toks))
         (params (plist-get c :params))
         (where (plist-get c :where)))
    (should (eq 'date (car (car toks))))
    (should (null (plist-get spec :to)))
    (should (numberp (plist-get spec :from)))
    (should (string-match-p "e\\.date >= \\?" where))
    (should-not (string-match-p "e\\.date < " where))
    (should (= 1 (length params)))))

(ert-deftest synaxis-filter-test-compile-bare-now-minus-7d-last-week ()
  "`date:now-7d' compiles like last 7 days: >= now-7d and < now."
  (let* ((before (float-time))
         (toks (synaxis-filter-parse "date:now-7d"))
         (after (float-time))
         (c (synaxis-filter-compile toks))
         (params (plist-get c :params))
         (where (plist-get c :where))
         (from-iso (nth 0 params))
         (to-iso (nth 1 params))
         (from (float-time (date-to-time from-iso)))
         (to (float-time (date-to-time to-iso))))
    (should (eq 'date (car (car toks))))
    (should (string-match-p "e\\.date >= \\?" where))
    (should (string-match-p "e\\.date < \\?" where))
    (should (= 2 (length params)))
    (should (< from to))
    ;; ISO formatting is second-resolution; allow 1s truncation slack.
    (should (<= (- before (* 7 86400) 1) from (+ (- after (* 7 86400)) 1)))
    (should (<= (- before 1) to (+ after 1)))))

(ert-deftest synaxis-filter-test-parse-date-cmp-now-still-point ()
  "Comparison forms still resolve `now-7d' as a single point boundary."
  (let* ((toks (synaxis-filter-parse "date:>=now-7d"))
         (c (synaxis-filter-compile toks)))
    (should (eq 'date-cmp (car (car toks))))
    (should (string-match-p "\\`e\\.date >= \\?\\'" (plist-get c :where)))
    (should (= 1 (length (plist-get c :params))))))

;;; synaxis-filter-explain

(defun synaxis-filter-test--explain-body (filter)
  "Run `synaxis-filter-explain' on FILTER and return the buffer contents."
  (synaxis-filter-explain filter)
  (with-current-buffer "*Synaxis Filter Explain*"
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest synaxis-filter-test-explain-renders-where-and-params ()
  "Explain output shows the WHERE fragment and each parameter on a line."
  (let ((body (synaxis-filter-test--explain-body "tag:emacs")))
    (should (string-match-p "Filter: tag:emacs" body))
    (should (string-match-p "WHERE" body))
    (should (string-match-p "EXISTS" body))
    (should (string-match-p "Parameters" body))
    (should (string-match-p "\"emacs\"" body))))

(ert-deftest synaxis-filter-test-explain-renders-tokens ()
  "Explain output shows parsed token cells."
  (let ((body (synaxis-filter-test--explain-body "tag:emacs")))
    (should (string-match-p "Tokens" body))
    (should (string-match-p "(tag . \"emacs\")" body))))

(ert-deftest synaxis-filter-test-explain-shows-explicit-limit ()
  "An explicit `limit:50' appears in the Limit line."
  (let ((body (synaxis-filter-test--explain-body "tag:emacs limit:50")))
    (should (string-match-p "Limit: 50" body))))

(ert-deftest synaxis-filter-test-explain-handles-empty-filter ()
  "An empty filter produces a `1=1' WHERE and no parameters."
  (let ((body (synaxis-filter-test--explain-body "")))
    (should (string-match-p "1=1" body))))

(ert-deftest synaxis-filter-test-explain-handles-date-cmp ()
  "Explain on a date-cmp filter shows the e.date clause and an ISO param."
  (let ((body (synaxis-filter-test--explain-body "date:>=now-7d")))
    (should (string-match-p "e\\.date >= \\?" body))
    (should (string-match-p
             "\"[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" body))))

(provide 'synaxis-filter-tests)
;;; synaxis-filter-tests.el ends here
