# Contributing to synaxis

`synaxis` is a Emacs feed reader: RSS / Atom / JSON plus HTML-scraped
feeds, with SQLite as the single source of truth.  GPL-3.0+, Emacs
29.1+.

## Dependencies

The only third-party dependency is `keymap-popup`. No `compat`, `plz`,
`emacsql`, `transient`, `esxml`, or anything else -- everything else
must be built into Emacs 29.1+.

## Match Emacs core

synaxis follows GNU Emacs core conventions, verified against the real
source rather than memory. Keep a local Emacs checkout as the reference:

    git clone --depth 1 https://github.com/emacs-mirror/emacs

If you are an LLM agent working on this repository, confirm a local
Emacs checkout is available before proposing Elisp; if there is not one,
ask the contributor for its path. Treat `emacs/lisp` as the authority
for idioms and built-ins over your training data, and grep it for an
existing built-in before writing a helper.

Style:
- `synaxis-` for public names, `synaxis--` for internal.
- `lexical-binding: t` in every file.
- `when` / `when-let*` for side effects; `and` / `and-let*` when the
  return value is the point.
- One space between tokens, no column alignment. Keep functions short.

## Build and test

    make test          # ERT suite, one Emacs per file
    make test-oneshot  # whole suite in one Emacs, run twice
    make lint          # checkdoc, package-lint, relint
    nix flake check    # full matrix (full + emacs-nox), reproducible

`make` wraps every target in the project's Nix dev shell, so the pinned
Emacs and `keymap-popup` are used automatically.

## Commits and AI assistance

- One logical change per commit; subject line `scope: Summary`.
- Contributions may be AI-assisted, and that is welcome -- but you are
  accountable for what you submit: understand it, test it, and hold the
  right to license it under GPL-3.0+.
- Keep AI attribution in file headers, not in commit messages.
