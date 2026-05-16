;;; synaxis-ol-tests.el --- Tests for synaxis-ol  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-ol'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ol)

(load (expand-file-name "../lisp/synaxis-ol.el"
                        (file-name-directory (or load-file-name buffer-file-name))))
(require 'synaxis-test-utils)


(defun synaxis-ol-tests--seed-entry ()
  "Insert one feed + entry; return the entry id."
  (synaxis-db-add-feed "https://example.com/feed" '(:title "Example"))
  (synaxis-db-upsert-entry
   '(:feed-url "https://example.com/feed"
               :source-id "1"
               :title "Hello"
               :link "https://example.com/post-1"
               :date 1.0
               :content "<p>Body.</p>"
               :content-type "html")))

;;; Helpers

(ert-deftest synaxis-ol-test-description-uses-feed-and-title ()
  (should (string= "Example: Hello"
                   (synaxis-ol--description
                    '(:feed-title "Example" :title "Hello")))))

(ert-deftest synaxis-ol-test-description-falls-back-when-fields-missing ()
  (should (string= "synaxis: (untitled)"
                   (synaxis-ol--description nil))))

;;; Store-link

(ert-deftest synaxis-ol-test-store-link-from-show-mode ()
  (synaxis-tests--with-tmp
   (require 'synaxis-show)
   (let ((id (synaxis-ol-tests--seed-entry))
         (org-store-link-plist nil))
     (synaxis-show-entry id)
     (with-current-buffer "*synaxis-show*"
       (synaxis-ol-store-link)
       (should (string= "synaxis:https://example.com/post-1"
                        (plist-get org-store-link-plist :link)))
       (should (string= "Example: Hello"
                        (plist-get org-store-link-plist :description)))))))

(ert-deftest synaxis-ol-test-store-link-ignores-unrelated-modes ()
  (let ((org-store-link-plist nil))
    (with-temp-buffer
      (text-mode)
      (should-not (synaxis-ol-store-link))
      (should-not org-store-link-plist))))

(ert-deftest synaxis-ol-test-store-link-from-search-mode ()
  (synaxis-tests--with-tmp
   (require 'synaxis-search)
   (let ((id (synaxis-ol-tests--seed-entry))
         (org-store-link-plist nil))
     (with-temp-buffer
       (synaxis-search-mode)
       (setq tabulated-list-entries `((,id [" " " " "Example" "" "Hello"])))
       (tabulated-list-print)
       (goto-char (point-min))
       (synaxis-ol-store-link)
       (should (string= "synaxis:https://example.com/post-1"
                        (plist-get org-store-link-plist :link)))))))

;;; Follow

(ert-deftest synaxis-ol-test-follow-known-link-opens-show ()
  (require 'synaxis-show)
  (synaxis-tests--with-tmp
   (let* ((id (synaxis-ol-tests--seed-entry))
          (called-with nil))
     (cl-letf (((symbol-function 'synaxis-show-entry)
                (lambda (entry-id &optional _peers)
                  (setq called-with entry-id))))
       (synaxis-ol-follow "https://example.com/post-1" nil)
       (should (equal id called-with))))))

(ert-deftest synaxis-ol-test-follow-unknown-link-falls-back-to-browse-url ()
  (require 'synaxis-show)
  (synaxis-tests--with-tmp
   (synaxis-ol-tests--seed-entry)
   (let ((browsed nil))
     (cl-letf (((symbol-function 'browse-url)
                (lambda (url &rest _) (setq browsed url))))
       (synaxis-ol-follow "https://elsewhere.example/x" nil)
       (should (string= "https://elsewhere.example/x" browsed))))))

;;; Export

(ert-deftest synaxis-ol-test-export-html ()
  (should (string= "<a href=\"https://example.com/x\">label</a>"
                   (synaxis-ol-export "https://example.com/x"
                                      "label" 'html nil))))

(ert-deftest synaxis-ol-test-export-markdown ()
  (should (string= "[label](https://example.com/x)"
                   (synaxis-ol-export "https://example.com/x"
                                      "label" 'md nil))))

(ert-deftest synaxis-ol-test-export-latex ()
  (should (string= "\\href{https://example.com/x}{label}"
                   (synaxis-ol-export "https://example.com/x"
                                      "label" 'latex nil))))

(ert-deftest synaxis-ol-test-export-falls-back-to-desc-when-no-backend-match ()
  (should (string= "label"
                   (synaxis-ol-export "u" "label" 'unknown-backend nil))))

(ert-deftest synaxis-ol-test-export-uses-path-when-desc-nil ()
  (should (string= "[https://x](https://x)"
                   (synaxis-ol-export "https://x" nil 'md nil))))

;;; Registration

(ert-deftest synaxis-ol-test-registered-in-org-link-parameters ()
  (require 'ol)
  (let ((params (org-link-get-parameter "synaxis" :follow)))
    (should (eq params #'synaxis-ol-follow))))

(provide 'synaxis-ol-tests)
;;; synaxis-ol-tests.el ends here
