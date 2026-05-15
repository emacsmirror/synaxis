;;; synaxis-search.el --- Entry list buffer  -*- lexical-binding: t; -*-

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

;; The entry list buffer.  Pure renderer: holds no derived state.
;; Every refresh runs a fresh `SELECT' against the database and
;; replays the ewoc nodes.  Keymaps are defined via `keymap-popup'.

;;; Code:

(require 'ewoc)

(provide 'synaxis-search)
;;; synaxis-search.el ends here
