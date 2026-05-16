;;; guix.scm --- Build synaxis from the current working tree.
;;
;; Usage:
;;
;;   One-shot install into the user profile:
;;       guix package -f guix.scm
;;
;;   Development shell with all dependencies:
;;       guix shell -D -f guix.scm
;;
;; This file mirrors a future upstream Guix recipe for `synaxis' as
;; closely as possible so it doubles as a local test harness.  The
;; only intentional differences are:
;;
;;   - `source' is a `local-file' of the current checkout rather than
;;     a `git-fetch' of a pinned commit, so the build always reflects
;;     whatever is on disk.
;;   - `version' is derived from `git describe' at evaluation time.

(use-modules (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (guix build-system emacs)
             (guix download)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils)
             (ice-9 popen)
             (ice-9 rdelim))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Run `git -C %source-dir ARGS...' and return its trimmed stdout, or
#f if the command fails or produces no output."
  (let* ((port (apply open-pipe* OPEN_READ "git" "-C" %source-dir args))
         (line (read-line port)))
    (close-pipe port)
    (if (eof-object? line) #f line)))

(define %version
  (or (git-output "describe" "--tags" "--always" "--dirty")
      (and=> (git-output "rev-parse" "--short" "HEAD")
             (lambda (hash) (string-append "0.1.0-" hash)))
      "0.1.0-git"))

(define (synaxis-file? file stat)
  "Include every file in the checkout except VCS metadata and build
artifacts."
  (let ((name (basename file)))
    (not (or (string-prefix? "." name)
             (string-contains file "/refs/")
             (string-suffix? ".elc" file)
             (string-suffix? "~" file)
             (string-suffix? ".tar" file)
             (string-suffix? ".tar.gz" file)
             (string-suffix? ".db" file)))))

(define-public emacs-synaxis-git
  (package
   (name "emacs-synaxis-git")
   (version %version)
   (source (local-file %source-dir
                       "synaxis-checkout"
                       #:recursive? #t
                       #:select? synaxis-file?))
   (build-system emacs-build-system)
   (arguments
    (list
     #:lisp-directory "lisp"
     #:test-command #~(list "make" "-C" ".." "test")
     #:emacs emacs-next-pgtk
     #:phases
     #~(modify-phases %standard-phases
                      (add-before 'check 'set-home
                                  (lambda _
                                    (setenv "HOME"
                                            (getenv "TMPDIR"))
                                    (mkdir-p (string-append
                                              (getenv "HOME")
                                              "/.emacs.d")))))))
   (propagated-inputs (list emacs-keymap-popup))
   (home-page "https://codeberg.org/thanosapollo/synaxis")
   (synopsis "Small Emacs feed reader backed by SQLite")
   (description
    "Synaxis (Greek σύναξις, \"gathering\") is a small Emacs feed
reader.  It reads RSS, Atom, and JSON feeds, generates synthetic
feeds from arbitrary HTML pages via a small CSS-selector subset
(an rss-bridge analogue inside Emacs), and uses SQLite as the
single source of truth for all stored entries and tags.  Targets
Emacs 29.1+ with native SQLite support.  This package definition
builds straight from the current git checkout, so the installed
version always matches the working tree.")
   (license license:gpl3+)))

emacs-synaxis-git
