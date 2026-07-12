;;; synaxis-parse.el --- Feed parsing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Maintainer: Thanos Apollo <public@thanosapollo.org>
;; Keywords: news, hypermedia, rss, atom
;; URL: https://git.thanosapollo.org/synaxis

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

;; Per-format feed adapters built on a small set of shared helpers.
;; All functions are pure: bytes or DOM in, entry plists out.  No SQL,
;; no network.
;;
;; Entry plist shape consumed by `synaxis-db-upsert-entry':
;;
;;   (:source-id   STRING        ; non-nil; synthesised from hash if absent
;;    :title       STRING        ; "" if missing
;;    :link        STRING-OR-NIL
;;    :date        STRING        ; ISO 8601, UTC, "YYYY-MM-DDTHH:MM:SSZ"
;;    :content     STRING-OR-NIL
;;    :content-type "html" | "text" | nil
;;    :meta        PLIST-OR-NIL)
;;
;; Feed plist returned by `synaxis-parse-string':
;;
;;   (:type SYMBOL :title STRING :entries (PLIST...))

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'parse-time)
(require 'url-parse)
(require 'url-expand)

;;; Date helpers

(defun synaxis-parse--time-to-iso (time)
  "Format Emacs TIME value as canonical UTC ISO 8601 string.
Result is `YYYY-MM-DDTHH:MM:SSZ' (literal Z, second precision)."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun synaxis-parse--decode-iso8601 (s)
  "Parse S as ISO 8601 datetime; return canonical ISO string or nil."
  (and (stringp s)
       (not (string-empty-p s))
       (condition-case nil
           (synaxis-parse--time-to-iso (parse-iso8601-time-string s))
         (error nil))))

(defun synaxis-parse--decode-rfc822 (s)
  "Parse S as RFC 822/2822 datetime; return canonical ISO string or nil."
  (and (stringp s)
       (not (string-empty-p s))
       (condition-case nil
           (let ((decoded (parse-time-string s)))
             (and (decoded-time-year decoded)
                  (synaxis-parse--time-to-iso (encode-time decoded))))
         (error nil))))

(defun synaxis-parse--decode-date (s)
  "Best-effort parse of date string S, returning a canonical ISO string.
Tries ISO 8601 then RFC 822; falls back to the current time on
failure, so callers never need to handle a nil date."
  (or (synaxis-parse--decode-iso8601 s)
      (synaxis-parse--decode-rfc822 s)
      (synaxis-parse--time-to-iso (current-time))))

;;; Charset decoding

(defconst synaxis-parse--charset-aliases
  '(("latin1" . iso-8859-1) ("latin-1" . iso-8859-1) ("utf8" . utf-8))
  "Charset names that do not intern directly to a coding system.")

(defun synaxis-parse--charset->coding (name)
  "Return a coding-system symbol for charset NAME, or nil if unknown."
  (and (stringp name)
       (let* ((key (downcase (string-trim name)))
              (sym (or (cdr (assoc key synaxis-parse--charset-aliases))
                       (intern key))))
         (and (coding-system-p sym) sym))))

(defun synaxis-parse--content-type-charset (content-type)
  "Return the charset named in the CONTENT-TYPE header value, or nil."
  (and (stringp content-type)
       (string-match "charset=[ \t]*\"?\\([^\";[:space:]]+\\)" content-type)
       (match-string 1 content-type)))

(defun synaxis-parse--sniff-coding (bytes)
  "Return a coding system declared in BYTES, or nil.
Looks for an XML `encoding' pseudo-attribute or an HTML `<meta
charset>' near the start of the raw response BYTES."
  (let ((head (substring bytes 0 (min 1024 (length bytes)))))
    (or (and (string-match "<\\?xml[^>]*encoding=[\"']\\([^\"']+\\)[\"']" head)
             (synaxis-parse--charset->coding (match-string 1 head)))
        (and (string-match "<meta[^>]*charset=[\"']?\\([^\"'>[:space:]]+\\)" head)
             (synaxis-parse--charset->coding (match-string 1 head))))))

(defun synaxis-parse--decode-bytes (bytes &optional content-type)
  "Decode raw response BYTES to a multibyte string.
Coding is taken from CONTENT-TYPE's charset, else a declaration
sniffed from BYTES (XML/HTML), else UTF-8.  A multibyte BYTES is
returned unchanged, since it has already been decoded upstream."
  (if (multibyte-string-p bytes) bytes
    (decode-coding-string
     bytes
     (or (synaxis-parse--charset->coding
          (synaxis-parse--content-type-charset content-type))
         (synaxis-parse--sniff-coding bytes)
         'utf-8))))

;;; URL resolution

(defun synaxis-parse--resolve-url (base path)
  "Resolve PATH against BASE URL into an absolute URL string.
Handles absolute, page-relative, root-relative, protocol-relative,
and fragment-only paths.  Returns nil for nil or empty PATH.  When
BASE is nil, returns PATH unchanged (callers can fold across
optional xml:base chains)."
  (cond
   ((or (null path) (string-empty-p path)) nil)
   ((string-prefix-p "http://"  path) path)
   ((string-prefix-p "https://" path) path)
   ((null base) path)
   ((string-prefix-p "//" path)
    (let ((scheme (url-type (url-generic-parse-url base))))
      (concat scheme ":" path)))
   ((string-prefix-p "#" path)
    (concat (replace-regexp-in-string "#.*\\'" "" base) path))
   (t (url-expand-file-name path base))))

;;; DOM helpers

(defun synaxis-parse--text-of (node)
  "Return all text content of NODE concatenated recursively.
Local helper avoiding `dom-text' / `dom-texts' / `dom-inner-text', which
have diverged across Emacs versions."
  (cond ((null node) "")
        ((stringp node) node)
        ((listp node)
         (mapconcat #'synaxis-parse--text-of (dom-children node) ""))
        (t "")))

(defun synaxis-parse--text-node (node)
  "Return concatenated text content of NODE, trimmed.  Nil if NODE is nil."
  (and node (string-trim (synaxis-parse--text-of node))))

(defun synaxis-parse--dom-children-as-html (node)
  "Serialise the children of NODE as an HTML string."
  (if (and node (listp node))
      (with-temp-buffer
        (dolist (child (dom-children node))
          (cond ((stringp child) (insert child))
                ((listp child) (dom-print child))))
        (string-trim (buffer-string)))
    ""))

(defun synaxis-parse--atom-text-container (node)
  "Extract content from an Atom text-container NODE (`content' or `summary').
Returns a plist with `:content' and `:content-type'."
  (let ((type (or (dom-attr node 'type) "text")))
    (pcase type
      ("xhtml"
       (let ((div (dom-child-by-tag node 'div)))
         (list :content (synaxis-parse--dom-children-as-html div)
               :content-type "html")))
      ("text"
       (list :content (synaxis-parse--text-node node)
             :content-type "text"))
      (_
       (list :content (synaxis-parse--text-node node)
             :content-type "html")))))

;;; Source-id synthesis

(defun synaxis-parse--source-id (entry &optional feed-url)
  "Return a stable source-id for ENTRY, synthesising from content if absent.
FEED-URL is mixed into the hash so two feeds publishing the same
content do not collide."
  (or (plist-get entry :source-id)
      (concat "synaxis:sha1:"
              (secure-hash
               'sha1
               (format "%s|%s|%s|%s"
                       (or feed-url "")
                       (or (plist-get entry :link) "")
                       (or (plist-get entry :title) "")
                       (or (plist-get entry :date) ""))))))

(defun synaxis-parse--ensure-source-ids (entries &optional feed-url)
  "Return ENTRIES with every plist's `:source-id' set.
FEED-URL, when given, is mixed into the synthetic ID when an entry
lacks both `:source-id' and `:link'."
  (mapcar (lambda (e)
            (plist-put e :source-id (synaxis-parse--source-id e feed-url)))
          entries))

;;; Format detection

(defun synaxis-parse--detect-format-string (s)
  "Return one of `atom', `rss', `rss1', `json' for feed bytes S."
  (with-temp-buffer
    (insert s)
    (synaxis-parse--detect-format-buffer)))

(defun synaxis-parse--detect-format-buffer ()
  "Return one of `atom', `rss', `rss1', `json' for the current buffer."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n\r\f")
    (cond
     ((eq (char-after) ?\{) 'json)
     ((re-search-forward
       "<\\(?:\\?xml[^>]*?\\?>[ \t\n\r]*\\)?<?\\([a-zA-Z]+:\\)?\\([a-zA-Z]+\\)"
       nil t)
      (let ((tag (downcase (match-string 2))))
        (pcase tag
          ("feed" 'atom)
          ("rss"  'rss)
          ("rdf"  'rss1)
          (_ (user-error "Unknown synaxis feed root element %S" tag)))))
     (t (user-error "Cannot detect synaxis feed format")))))

;;; Atom adapter

(defun synaxis-parse--atom-link (item)
  "Return the alternate link href of Atom ITEM, or nil."
  (let ((links (dom-by-tag item 'link)))
    (or (cl-loop for l in links
                 for rel = (dom-attr l 'rel)
                 when (or (null rel) (string= rel "alternate"))
                 return (dom-attr l 'href))
        (and links (dom-attr (car links) 'href)))))

(defun synaxis-parse--atom-entry (item base)
  "Convert Atom entry ITEM (DOM node) to an entry plist.
BASE is the absolute base URL against which a relative entry link
is resolved (nil leaves the raw href in place)."
  (let* ((title (synaxis-parse--text-node (dom-child-by-tag item 'title)))
         (id (synaxis-parse--text-node (dom-child-by-tag item 'id)))
         (raw (synaxis-parse--atom-link item))
         (link (or (synaxis-parse--resolve-url base raw) raw))
         (date (synaxis-parse--decode-date
                (or (synaxis-parse--text-node (dom-child-by-tag item 'updated))
                    (synaxis-parse--text-node (dom-child-by-tag item 'published)))))
         (cnode (or (dom-child-by-tag item 'content)
                    (dom-child-by-tag item 'summary)))
         (cplist (and cnode (synaxis-parse--atom-text-container cnode))))
    (append (list :title (or title "")
                  :source-id id
                  :link link
                  :date date)
            cplist)))

(defun synaxis-parse--xml-base (node parent-base)
  "Return NODE's effective xml:base resolved against PARENT-BASE.
Falls back to PARENT-BASE when the attribute is absent.  Note
that `libxml-parse-xml-region' strips the implicit `xml:'
namespace prefix, so the attribute lands on the DOM under the
bare symbol `base'."
  (let ((attr (dom-attr node 'base)))
    (or (synaxis-parse--resolve-url parent-base attr)
        parent-base)))

(defun synaxis-parse--from-atom (dom &optional feed-url)
  "Parse DOM as an Atom 1.0 feed.
FEED-URL is the absolute URL of the feed itself; entry links are
resolved against it.  A feed-level or entry-level `xml:base'
attribute, if present, layers on top per RFC 4287 section 4."
  (let ((feed-base (synaxis-parse--xml-base dom feed-url)))
    (list :type 'atom
          :title (synaxis-parse--text-node (dom-child-by-tag dom 'title))
          :entries (synaxis-parse--ensure-source-ids
                    (cl-loop for entry in (dom-by-tag dom 'entry)
                             for base = (synaxis-parse--xml-base entry feed-base)
                             collect (synaxis-parse--atom-entry entry base))
                    feed-url))))

;;; RSS 2.0 adapter

(defun synaxis-parse--rss-item (item base)
  "Convert RSS 2.0 ITEM (DOM node) to an entry plist.
BASE is the absolute base URL against which a relative link is
resolved (nil leaves the raw link in place)."
  (let* ((title (synaxis-parse--text-node (dom-child-by-tag item 'title)))
         (raw (synaxis-parse--text-node (dom-child-by-tag item 'link)))
         (link (or (synaxis-parse--resolve-url base raw) raw))
         (guid (synaxis-parse--text-node (dom-child-by-tag item 'guid)))
         (date (synaxis-parse--decode-date
                (synaxis-parse--text-node (dom-child-by-tag item 'pubDate))))
         (encoded (dom-child-by-tag item 'encoded))
         (desc (dom-child-by-tag item 'description))
         (cnode (or encoded desc))
         (content (and cnode (synaxis-parse--text-node cnode))))
    (list :title (or title "")
          :source-id (or guid link)
          :link link
          :date date
          :content content
          :content-type (and content "html"))))

(defun synaxis-parse--from-rss (dom &optional feed-url)
  "Parse DOM as an RSS 2.0 feed.
FEED-URL resolves protocol- or page-relative item links."
  (let* ((channel (dom-child-by-tag dom 'channel))
         (title (and channel
                     (synaxis-parse--text-node
                      (dom-child-by-tag channel 'title)))))
    (list :type 'rss
          :title title
          :entries (synaxis-parse--ensure-source-ids
                    (cl-loop for item in (dom-by-tag dom 'item)
                             collect (synaxis-parse--rss-item item feed-url))
                    feed-url))))

;;; RSS 1.0 / RDF adapter

(defun synaxis-parse--rss1-item (item base)
  "Convert RSS 1.0 ITEM (DOM node) to an entry plist.
BASE is the absolute base URL against which a relative link is
resolved (nil leaves the raw link in place)."
  (let* ((title (synaxis-parse--text-node (dom-child-by-tag item 'title)))
         (raw (synaxis-parse--text-node (dom-child-by-tag item 'link)))
         (link (or (synaxis-parse--resolve-url base raw) raw))
         (about (dom-attr item 'rdf:about))
         (date (synaxis-parse--decode-date
                (or (synaxis-parse--text-node (dom-child-by-tag item 'date))
                    (synaxis-parse--text-node (dom-child-by-tag item 'pubDate)))))
         (desc (synaxis-parse--text-node (dom-child-by-tag item 'description))))
    (list :title (or title "")
          :source-id (or about link)
          :link link
          :date date
          :content desc
          :content-type (and desc "html"))))

(defun synaxis-parse--from-rss1 (dom &optional feed-url)
  "Parse DOM as an RSS 1.0 / RDF feed.
FEED-URL resolves protocol- or page-relative item links."
  (let* ((channel (dom-child-by-tag dom 'channel))
         (title (and channel
                     (synaxis-parse--text-node
                      (dom-child-by-tag channel 'title)))))
    (list :type 'rss1
          :title title
          :entries (synaxis-parse--ensure-source-ids
                    (cl-loop for item in (dom-by-tag dom 'item)
                             collect (synaxis-parse--rss1-item item feed-url))
                    feed-url))))

;;; JSON Feed adapter

(defun synaxis-parse--json-item (item base)
  "Convert a JSON Feed ITEM plist to a synaxis entry plist.
BASE is the absolute base URL against which a relative item URL is
resolved (nil leaves the URL unchanged)."
  (let* ((id (plist-get item :id))
         (raw (plist-get item :url))
         (url (or (synaxis-parse--resolve-url base raw) raw))
         (title (plist-get item :title))
         (date (synaxis-parse--decode-date
                (plist-get item :date_published)))
         (html (plist-get item :content_html))
         (text (plist-get item :content_text)))
    (list :title (or title "")
          :source-id (and id (format "%s" id))
          :link url
          :date date
          :content (or html text)
          :content-type (cond (html "html") (text "text")))))

(defun synaxis-parse--from-json (object &optional feed-url)
  "Parse OBJECT (a JSON Feed top-level plist) into a feed plist.
FEED-URL resolves protocol- or page-relative item URLs."
  (list :type 'json
        :title (plist-get object :title)
        :entries (synaxis-parse--ensure-source-ids
                  (cl-loop for item in (plist-get object :items)
                           collect (synaxis-parse--json-item item feed-url))
                  feed-url)))

;;; Public entry points

(defun synaxis-parse-buffer (&optional feed-url)
  "Parse the current buffer as a feed.  Return a feed plist.
FEED-URL, when non-nil, is used to resolve protocol- and
page-relative entry links and to seed `:source-id' synthesis."
  (let ((type (synaxis-parse--detect-format-buffer)))
    (pcase type
      ('json
       (save-excursion
         (goto-char (point-min))
         (synaxis-parse--from-json
          (json-parse-buffer :object-type 'plist :array-type 'list)
          feed-url)))
      (_
       (let ((dom (libxml-parse-xml-region (point-min) (point-max))))
         (pcase-exhaustive type
           ('atom (synaxis-parse--from-atom dom feed-url))
           ('rss  (synaxis-parse--from-rss  dom feed-url))
           ('rss1 (synaxis-parse--from-rss1 dom feed-url))))))))

(defun synaxis-parse-string (s &optional feed-url)
  "Parse string S as a feed.  Return a feed plist.
FEED-URL is forwarded to `synaxis-parse-buffer'."
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert s)
    (synaxis-parse-buffer feed-url)))

(provide 'synaxis-parse)
;;; synaxis-parse.el ends here
