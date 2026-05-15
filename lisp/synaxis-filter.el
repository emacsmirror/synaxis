;;; synaxis-filter.el --- Filter mini-language  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: news, hypermedia, rss, atom
;; URL: https://codeberg.org/thanosapollo/synaxis

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Filter mini-language parser, modeled on elfeed's syntax
;; (`+tag', `-tag', `@duration', `=feed-re', `#limit', free-text
;; search) but compiled to a SQL `WHERE' clause and parameter list
;; rather than a byte-compiled predicate.

;;; Code:

(provide 'synaxis-filter)
;;; synaxis-filter.el ends here
