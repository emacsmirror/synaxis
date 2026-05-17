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
    (should (equal '(1000.0 2000.0) (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-from-only ()
  (let* ((spec '(:from 1000.0 :to nil))
         (c (synaxis-filter-compile (list (cons 'date spec)))))
    (should (string-match-p "e\\.date >= \\?" (plist-get c :where)))
    (should-not (string-match-p "e\\.date < " (plist-get c :where)))
    (should (equal '(1000.0) (plist-get c :params)))))

(ert-deftest synaxis-filter-test-compile-date-to-only ()
  (let* ((spec '(:from nil :to 2000.0))
         (c (synaxis-filter-compile (list (cons 'date spec)))))
    (should-not (string-match-p "e\\.date >= " (plist-get c :where)))
    (should (string-match-p "e\\.date < \\?" (plist-get c :where)))
    (should (equal '(2000.0) (plist-get c :params)))))

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
                          :title "T" :date 1.0))))
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

(ert-deftest synaxis-filter-test-completions-include-entry-titles ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/x")
   (synaxis-db-upsert-entry '(:feed-url "https://example.com/x" :source-id "1"
                                        :title "Recent Post" :date 100.0))
   (let ((c (synaxis-filter-completions)))
     ;; Multi-word titles are emitted quoted so they round-trip
     ;; through `synaxis-filter-parse'.
     (should (member "title:\"Recent Post\"" c)))))

(ert-deftest synaxis-filter-test-completions-respect-entry-title-limit ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/x")
   (dotimes (i 50)
     (synaxis-db-upsert-entry
      `(:feed-url "https://example.com/x" :source-id ,(format "%d" i)
                  :title ,(format "Entry %03d" i) :date ,(float i))))
   (let* ((synaxis-filter-title-completion-limit 10)
          (c (synaxis-filter-completions))
          (titles (seq-filter (lambda (s)
                                (and (string-prefix-p "title:" s)
                                     (> (length s) 6)))
                              c)))
     (should (= 10 (length titles))))))

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
                          :title "A study" :date 100.0))))
     (let ((synaxis-tag-rules
            '((:filter "feed:\"PubMed Trending\"" :add ("medicine")))))
       (synaxis-tag-rules-apply-entry id))
     (should (member "medicine" (synaxis-db-get-tags id))))))

;;; Regex token parsing

(ert-deftest synaxis-filter-test-parse-title-regex ()
  "`title:/^Re:/' yields a regex-title token with the pattern stripped of slashes."
  (should (equal '((regex-title . "^Re:"))
                 (synaxis-filter-parse "title:/^Re:/"))))

(ert-deftest synaxis-filter-test-parse-not-title-regex ()
  "`-title:/foo/' yields a not-regex-title token."
  (should (equal '((not-regex-title . "foo"))
                 (synaxis-filter-parse "-title:/foo/"))))

(ert-deftest synaxis-filter-test-parse-content-regex ()
  (should (equal '((regex-content . "bar"))
                 (synaxis-filter-parse "content:/bar/"))))

(ert-deftest synaxis-filter-test-parse-not-content-regex ()
  (should (equal '((not-regex-content . "bar"))
                 (synaxis-filter-parse "-content:/bar/"))))

(ert-deftest synaxis-filter-test-parse-feed-regex ()
  (should (equal '((regex-feed . "baz"))
                 (synaxis-filter-parse "feed:/baz/"))))

(ert-deftest synaxis-filter-test-parse-not-feed-regex ()
  (should (equal '((not-regex-feed . "baz"))
                 (synaxis-filter-parse "-feed:/baz/"))))

(ert-deftest synaxis-filter-test-parse-bare-regex ()
  "A bare `/foo|bar/' token yields a regex-text token."
  (should (equal '((regex-text . "foo|bar"))
                 (synaxis-filter-parse "/foo|bar/"))))

(ert-deftest synaxis-filter-test-parse-bare-not-regex ()
  "`-/foo/' yields a not-regex-text token."
  (should (equal '((not-regex-text . "foo"))
                 (synaxis-filter-parse "-/foo/"))))

(ert-deftest synaxis-filter-test-parse-prefix-without-closing-slash-is-literal-like ()
  "`title:/foo' (no closing slash) stays a literal LIKE token."
  (should (equal '((title . "/foo"))
                 (synaxis-filter-parse "title:/foo"))))

(ert-deftest synaxis-filter-test-parse-quoted-regex-with-whitespace ()
  "`title:\"/foo bar/\"' preserves the whitespace inside the pattern."
  (should (equal '((regex-title . "foo bar"))
                 (synaxis-filter-parse "title:\"/foo bar/\""))))

(ert-deftest synaxis-filter-test-parse-invalid-regex-signals-user-error ()
  "An unclosed character class raises `user-error' at parse time."
  (should-error (synaxis-filter-parse "title:/[/") :type 'user-error))

(ert-deftest synaxis-filter-test-parse-empty-slashes-not-regex ()
  "`title://' is not a regex (empty pattern); falls through to literal LIKE."
  (should (equal '((title . "//"))
                 (synaxis-filter-parse "title://"))))

;;; Regex compilation

(ert-deftest synaxis-filter-test-compile-no-regex-omits-post-filter ()
  "A compile of pure LIKE tokens yields nil :post-filter."
  (let ((spec (synaxis-filter-compile '((title . "foo")))))
    (should (null (plist-get spec :post-filter)))))

(ert-deftest synaxis-filter-test-compile-regex-produces-post-filter ()
  "A compile that includes a regex token yields a callable :post-filter."
  (let ((spec (synaxis-filter-compile '((regex-title . "^A")))))
    (should (functionp (plist-get spec :post-filter)))))

(ert-deftest synaxis-filter-test-post-filter-matches-title-regex ()
  "The compiled predicate matches a title against the pattern."
  (let* ((spec (synaxis-filter-compile '((regex-title . "^A"))))
         (pred (plist-get spec :post-filter)))
    (should     (funcall pred (list :title "Alpha" :content "")))
    (should-not (funcall pred (list :title "Bravo" :content "")))))

(ert-deftest synaxis-filter-test-post-filter-matches-case-insensitively ()
  "Regex matching is always case-insensitive."
  (let* ((spec (synaxis-filter-compile '((regex-title . "rust"))))
         (pred (plist-get spec :post-filter)))
    (should (funcall pred (list :title "RUST is fun" :content "")))
    (should (funcall pred (list :title "RuSt"        :content "")))))

(ert-deftest synaxis-filter-test-post-filter-handles-negation ()
  "not-regex-title inverts the match."
  (let* ((spec (synaxis-filter-compile '((not-regex-title . "^A"))))
         (pred (plist-get spec :post-filter)))
    (should-not (funcall pred (list :title "Alpha" :content "")))
    (should     (funcall pred (list :title "Bravo" :content "")))))

(ert-deftest synaxis-filter-test-post-filter-content-regex ()
  (let* ((spec (synaxis-filter-compile '((regex-content . "world"))))
         (pred (plist-get spec :post-filter)))
    (should     (funcall pred (list :title "x" :content "hello world")))
    (should-not (funcall pred (list :title "x" :content "hello")))))

(ert-deftest synaxis-filter-test-post-filter-feed-regex ()
  (let* ((spec (synaxis-filter-compile '((regex-feed . "Daily"))))
         (pred (plist-get spec :post-filter)))
    (should     (funcall pred (list :feed-title "The Daily News")))
    (should-not (funcall pred (list :feed-title "Weekly Digest")))))

(ert-deftest synaxis-filter-test-post-filter-text-regex-matches-title-or-content ()
  (let* ((spec (synaxis-filter-compile '((regex-text . "foo"))))
         (pred (plist-get spec :post-filter)))
    (should     (funcall pred (list :title "foo bar" :content "x")))
    (should     (funcall pred (list :title "x"       :content "foo bar")))
    (should-not (funcall pred (list :title "x"       :content "y")))))

(ert-deftest synaxis-filter-test-post-filter-conjoins-multiple-regex-tokens ()
  "Two regex tokens in the same spec are AND-ed."
  (let* ((spec (synaxis-filter-compile
                '((regex-title . "^A") (regex-content . "world"))))
         (pred (plist-get spec :post-filter)))
    (should     (funcall pred (list :title "Alpha" :content "hello world")))
    (should-not (funcall pred (list :title "Alpha" :content "hello")))
    (should-not (funcall pred (list :title "Bravo" :content "hello world")))))

(ert-deftest synaxis-filter-test-post-filter-nil-strings-do-not-error ()
  "A nil :title or :content does not blow up the predicate."
  (let* ((spec (synaxis-filter-compile '((regex-text . "foo"))))
         (pred (plist-get spec :post-filter)))
    (should-not (funcall pred (list :title nil :content nil)))))

(provide 'synaxis-filter-tests)
;;; synaxis-filter-tests.el ends here
