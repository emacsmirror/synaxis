;;; synaxis-test-utils.el --- Shared test fixtures and helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Common scaffolding used across the synaxis ERT files: a universal
;; temp-DB macro, fixture loaders, and a synthetic HTTP-response
;; constructor.  This file also `require's every synaxis module so
;; that `cl-letf' on a cross-module function in a test does not unbind
;; it during unwind when only one test file is loaded standalone.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Force-load every public synaxis module.  Without this, a test that
;; `cl-letf's a function from a not-yet-required module will have
;; `cl-letf' record a void slot for restoration, and the unwind step
;; `fmakunbound's the symbol -- a class of test flakiness that bites
;; only when files are loaded individually.
(require 'synaxis)
(require 'synaxis-edit)
(require 'synaxis-scrape)
(require 'synaxis-ol)

;;; Paths

(defconst synaxis-tests--fixtures-dir
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding test fixture files (XML, HTML, JSON).")

(defun synaxis-tests--load-fixture (name)
  "Read fixture NAME from `synaxis-tests--fixtures-dir' and return it as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name synaxis-tests--fixtures-dir))
    (buffer-string)))

;;; Temp-DB fixture

(defmacro synaxis-tests--with-tmp (&rest body)
  "Run BODY in a fresh temp DB with sensible test defaults bound.

Binds a temporary directory and `synaxis-db-file', marks
`synaxis-testing' non-nil, clears the cached connection, nils the
new-entry hooks and the autoupdate timer, and routes
`display-buffer' to a no-window action so tests do not pop windows.
Cleans up the DB connection and temp directory on unwind."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "synaxis-test" t))
          (synaxis-db-file (expand-file-name "test.db" dir))
          (synaxis-testing t)
          (synaxis-db--connection nil)
          (synaxis-new-entry-parse-hook nil)
          (synaxis-new-entry-hook nil)
          (synaxis-update-interval nil)
          (synaxis--update-timer nil)
          (display-buffer-alist '((".*" display-buffer-no-window))))
     (unwind-protect
         (progn ,@body)
       (dolist (bn '("*synaxis*" "*synaxis-show*" "*synaxis-scrape-test*"))
         (when (get-buffer bn) (kill-buffer bn)))
       (synaxis-db-close)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

;;; Seeding helpers

(defun synaxis-tests--seed-entry (feed-url source-id title date &optional unread)
  "Insert a feed (titled \"F\") plus one entry under FEED-URL.
SOURCE-ID, TITLE, and DATE go into the entry.  When UNREAD is
non-nil the entry is tagged `unread'.  Returns the new entry id."
  (synaxis-db-add-feed feed-url '(:title "F"))
  (let ((id (synaxis-db-upsert-entry
             (list :feed-url feed-url :source-id source-id
                   :title title :date date))))
    (when unread (synaxis-db-add-tag id "unread"))
    id))

;;; HTTP synth

(defun synaxis-tests--http-response (status headers body)
  "Return a buffer containing a synthetic HTTP response.
STATUS is the status line (e.g. \"200 OK\"), HEADERS is an alist
of (NAME . VALUE) pairs, BODY is the response body string."
  (let ((buf (generate-new-buffer " *synaxis-test-http*")))
    (with-current-buffer buf
      (insert (format "HTTP/1.1 %s\r\n" status))
      (dolist (h headers)
        (insert (format "%s: %s\r\n" (car h) (cdr h))))
      (insert "\r\n" body))
    buf))

(provide 'synaxis-test-utils)
;;; synaxis-test-utils.el ends here
