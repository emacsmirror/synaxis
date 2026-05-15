;;; synaxis-db-tests.el --- Tests for synaxis-db  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `synaxis-db'.

;;; Code:

(require 'ert)

(load (expand-file-name "../lisp/synaxis-db.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(provide 'synaxis-db-tests)
;;; synaxis-db-tests.el ends here
