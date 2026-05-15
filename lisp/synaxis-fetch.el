;;; synaxis-fetch.el --- Feed fetching  -*- lexical-binding: t; -*-

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

;; Async feed retrieval via `url-queue-retrieve', with conditional GET
;; (If-None-Match / If-Modified-Since) and dispatch to the parser and
;; database layers.

;;; Code:

(require 'url-queue)

(provide 'synaxis-fetch)
;;; synaxis-fetch.el ends here
