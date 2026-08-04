;;; synaxis-show-tests.el --- Tests for synaxis-show  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-show'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-show.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'synaxis-test-utils)


(ert-deftest synaxis-show-test-render-shr-inserts-title-and-content ()
  (with-temp-buffer
    (synaxis-show-render-shr
     '(:title "Hello"
              :feed-title "F"
              :date "2024-01-02T03:04:05Z"
              :content "<p>Body text.</p>"
              :content-type "html"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Hello" text))
      (should (string-match-p "Body text" text)))))

(ert-deftest synaxis-show-test-render-handles-text-content-type ()
  (with-temp-buffer
    (synaxis-show-render-shr
     '(:title "T"
              :feed-title "F"
              :date "1970-01-01T00:00:01Z"
              :content "plain content"
              :content-type "text"))
    (should (string-match-p "plain content"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest synaxis-show-test-render-handles-missing-content ()
  (with-temp-buffer
    (synaxis-show-render-shr
     '(:title "T" :feed-title "F" :date "1970-01-01T00:00:01Z" :content nil :content-type nil))
    (should (string-match-p "T"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest synaxis-show-test-show-entry-marks-read ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/x")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/x" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z" :content "<p>x</p>"
                          :content-type "html"))))
     (synaxis-db-add-tag id "unread")
     (should (member "unread" (synaxis-db-get-tags id)))
     (synaxis-show-entry id)
     (should-not (member "unread" (synaxis-db-get-tags id))))))

(ert-deftest synaxis-show-test-walk-next-moves-to-next-peer ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/n")
   (let ((ids (cl-loop for i below 3
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/n"
                                            :source-id ,(format "%d" i)
                                            :title ,(format "T%d" i)
                                            :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" i t)
                                            :content "x"
                                            :content-type "html")))))
     (synaxis-show-entry (nth 0 ids) ids)
     (with-current-buffer "*synaxis-show*"
       (synaxis-show--walk +1)
       (should (equal (nth 1 ids) synaxis-show--entry-id))
       (synaxis-show--walk +1)
       (should (equal (nth 2 ids) synaxis-show--entry-id))))))

(ert-deftest synaxis-show-test-walk-prev-from-first-errors ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/n")
   (let ((ids (cl-loop for i below 2
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/n"
                                            :source-id ,(format "%d" i)
                                            :title "T" :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" i t))))))
     (synaxis-show-entry (nth 0 ids) ids)
     (with-current-buffer "*synaxis-show*"
       (should-error (synaxis-show--walk -1) :type 'user-error)))))

(ert-deftest synaxis-show-test-walk-next-from-last-errors ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/n")
   (let ((ids (cl-loop for i below 2
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/n"
                                            :source-id ,(format "%d" i)
                                            :title "T" :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" i t))))))
     (synaxis-show-entry (nth 1 ids) ids)
     (with-current-buffer "*synaxis-show*"
       (should-error (synaxis-show--walk +1) :type 'user-error)))))

(ert-deftest synaxis-show-test-no-peers-errors ()
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/n")
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/n" :source-id "1"
                          :title "T" :date "1970-01-01T00:00:01Z"))))
     (synaxis-show-entry id)            ;; no peers passed
     (with-current-buffer "*synaxis-show*"
       (should-error (synaxis-show--walk +1) :type 'user-error)))))

(ert-deftest synaxis-show-test-walk-syncs-list-point-to-new-entry ()
  "After walk +1 from the show buffer, point in `*synaxis*' lands on the new row."
  (synaxis-tests--with-tmp
   (require 'synaxis-search)
   (synaxis-db-add-feed "https://example.com/n")
   (let ((ids (cl-loop for i below 3
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/n"
                                            :source-id ,(format "%d" i)
                                            :title ,(format "T%d" i)
                                            :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (- 10 i) t))))))
     (let ((buf (get-buffer-create "*synaxis*")))
       (with-current-buffer buf
         (synaxis-search-mode)
         (setq synaxis-search--filter "")
         (synaxis-search-refresh))
       (synaxis-show-entry (nth 0 ids) ids)
       (with-current-buffer "*synaxis-show*"
         (synaxis-show--walk +1))
       (with-current-buffer buf
         (should (equal (nth 1 ids) (tabulated-list-get-id))))))))

(ert-deftest synaxis-show-test-walk-noop-when-no-list-buffer ()
  "Walking when `*synaxis*' is absent still navigates, no error."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/n")
   (let ((ids (cl-loop for i below 2
                       collect (synaxis-db-upsert-entry
                                `(:feed-url "https://example.com/n"
                                            :source-id ,(format "%d" i)
                                            :title "T" :date ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" i t))))))
     (when-let* ((buf (get-buffer "*synaxis*"))) (kill-buffer buf))
     (synaxis-show-entry (nth 0 ids) ids)
     (with-current-buffer "*synaxis-show*"
       (synaxis-show--walk +1)
       (should (equal (nth 1 ids) synaxis-show--entry-id))))))

(ert-deftest synaxis-show-test-render-stores-current-link ()
  "Rendering an entry seeds `synaxis-show--current-link' from `:link'."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/b" '(:title "Brows"))
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/b" :source-id "1"
                          :title "T" :link "https://example.com/post"
                          :date "1970-01-01T00:00:01Z"))))
     (synaxis-show-entry id)
     (with-current-buffer "*synaxis-show*"
       (should (equal "https://example.com/post"
                      synaxis-show--current-link))))))

(ert-deftest synaxis-show-test-browse-entry-calls-browse-url ()
  "`synaxis-show-browse-entry' passes the stored link to `browse-url'."
  (synaxis-tests--with-tmp
   (synaxis-db-add-feed "https://example.com/b" '(:title "Brows"))
   (let ((id (synaxis-db-upsert-entry
              '(:feed-url "https://example.com/b" :source-id "1"
                          :title "T" :link "https://example.com/post"
                          :date "1970-01-01T00:00:01Z")))
         (browsed nil))
     (synaxis-show-entry id)
     (with-current-buffer "*synaxis-show*"
       (cl-letf (((symbol-function 'browse-url)
                  (lambda (u &rest _) (setq browsed u))))
         (synaxis-show-browse-entry))
       (should (equal "https://example.com/post" browsed))))))

(ert-deftest synaxis-show-test-browse-entry-errors-when-no-link ()
  "Browse errors with `user-error' when no link is recorded."
  (with-temp-buffer
    (synaxis-show-mode)
    (should-error (synaxis-show-browse-entry) :type 'user-error)))

(ert-deftest synaxis-show-test-render-tolerates-bad-date ()
  "Bad ISO dates must not signal when rendering."
  (with-temp-buffer
    (synaxis-show-render-shr
     '(:title "T" :feed-title "F" :date "not-a-date" :content "hi" :content-type "text"))
    (goto-char (point-min))
    (should (search-forward "not-a-date" nil t))
    (should (search-forward "hi" nil t))))

(provide 'synaxis-show-tests)
;;; synaxis-show-tests.el ends here
