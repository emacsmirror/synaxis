;;; synaxis-scrape.el --- HTML scraping to synthetic feeds  -*- lexical-binding: t; -*-

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

;; Generate synthetic feeds from arbitrary HTML pages using a small
;; CSS-selector subset (tag, `.class', `#id', descendant combinator).
;; Scheduled for v0.2.

;;; Code:

(require 'dom)

(provide 'synaxis-scrape)
;;; synaxis-scrape.el ends here
