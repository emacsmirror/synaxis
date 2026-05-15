;;; synaxis.el --- Feed reader with SQLite storage  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: news, hypermedia, rss, atom
;; URL: https://codeberg.org/thanosapollo/synaxis
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (keymap-popup "0.2.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; synaxis (Greek σύναξις, "gathering") is a small feed reader for
;; Emacs.  It reads RSS, Atom, and JSON feeds, and generates synthetic
;; feeds from arbitrary HTML pages via CSS selectors.  SQLite is the
;; single source of truth for all stored state.

;;; Code:

(defgroup synaxis nil
  "Feed reader with SQLite storage."
  :group 'applications
  :prefix "synaxis-"
  :link '(url-link "https://codeberg.org/thanosapollo/synaxis"))

(defvar synaxis-testing nil
  "Non-nil when running unit tests.
Code that registers global side effects (timers, kill hooks,
auto-save) should honour this flag.")

(provide 'synaxis)
;;; synaxis.el ends here
