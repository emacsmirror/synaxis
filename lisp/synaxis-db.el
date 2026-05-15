;;; synaxis-db.el --- SQLite storage layer  -*- lexical-binding: t; -*-

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

;; SQLite connection, schema, migrations, and CRUD primitives for
;; feeds, entries, tags, and scrape rules.  All persistent state lives
;; here; other modules go through this API.

;;; Code:

(require 'sqlite)

(provide 'synaxis-db)
;;; synaxis-db.el ends here
