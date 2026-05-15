;;; synaxis-parse.el --- Feed parsing  -*- lexical-binding: t; -*-

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

;; Generic-parser core plus per-format adapters (Atom 1.0, RSS 2.0,
;; RSS 1.0/RDF, JSON Feed).  Pure: bytes or DOM in, entry plists out.
;; No SQL, no network.

;;; Code:

(require 'dom)

(provide 'synaxis-parse)
;;; synaxis-parse.el ends here
