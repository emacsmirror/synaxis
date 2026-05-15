;;; synaxis-search-tests.el --- Tests for synaxis-search  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-search'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-search.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(provide 'synaxis-search-tests)
;;; synaxis-search-tests.el ends here
