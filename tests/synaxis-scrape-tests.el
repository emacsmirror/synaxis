;;; synaxis-scrape-tests.el --- Tests for synaxis-scrape  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-scrape'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-scrape.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(provide 'synaxis-scrape-tests)
;;; synaxis-scrape-tests.el ends here
