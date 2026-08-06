# AGENTS.md

Public guidance for contributors and coding agents working on `synaxis`.

## Project

`synaxis` is an Emacs feed reader: RSS / Atom / JSON plus HTML-scraped
synthetic feeds (CSS-subset selectors), with local SQLite as the single
source of truth. Sources live in `lisp/`, ERT tests in `tests/`. GPL-3.0+,
Emacs 29.1+ (native SQLite).

Only third-party dependency: `keymap-popup`. No `compat`, `plz`,
`esxml-query`, `emacsql`, `transient`, or other external packages.
`Package-Requires: ((emacs "29.1") (keymap-popup "0.2.1"))`.

## Architecture

- One concern per module under `lisp/`. Entry hub `synaxis.el`; storage
  `synaxis-db.el`; parse `synaxis-parse.el`; async fetch
  `synaxis-fetch.el`; filter language `synaxis-filter.el`; list UI
  `synaxis-search.el`; entry UI `synaxis-show.el`; scrape
  `synaxis-scrape.el`; fast tabulated-list helpers `synaxis-tl.el`; feed
  editor `synaxis-edit.el`; Org link `synaxis-ol.el`.
- SQLite is source of truth for display. Views never block on network.
  Fetch only syncs: `url-queue-retrieve` → parse → upsert →
  `synaxis-new-entry-hook`. List buffer is a pure renderer: re-SELECT and
  reprint (or `synaxis-tl-replace-entry` for one row). No Elisp cache of
  filtered results.
- Tags are rows in `entry_tags`. `unread` is a tag, not a column. Feed and
  entry `meta TEXT` stores freeform plists via `synaxis-db--encode-meta` /
  `synaxis-db--decode-meta`. Production feed key today: `:autotags`
  (applied on ingest); do not invent other SQL/`json_extract` meta keys.
- Filter mini-language compiles to SQL `WHERE`, not byte-compiled Elisp
  predicates. Prefer one SELECT / JOIN / `IN (...)` over N+1 queries.
- `synaxis-tl.el` is vendored tabulated-list speed code also used
  elsewhere under other names; keep changes portable and focused.

## Data and fetch safety

- Honour `synaxis-testing` in any code that registers global side effects
  (hooks, timers, kill-emacs behavior).
- HTTP via built-in `url-queue-retrieve` with conditional GET
  (etag / last-modified). No curl shell-out; no external HTTP client.
- XML via `libxml-parse-xml-region` + `dom.el`. JSON via
  `json-parse-buffer` / `json-parse-string` with `:object-type 'plist` and
  `:array-type 'list`. Entry HTML via `shr` through
  `synaxis-show-display-function`.
- Interactive commands: confirm destructive bulk work with `y-or-n-p`,
  show counts before bulk ops, and use `user-error` for expected
  failures.

## Emacs Lisp conventions

- Lexical binding in every file. Public: `synaxis-` / `synaxis-MODULE-`.
  Internal: `synaxis--` / `synaxis-MODULE--`. Faces: `synaxis-*-face`.
- `defcustom` always has `:type` and `:group`. This package's `defface`
  forms also set `:group 'synaxis`. Prefer `keymap-popup-define` for
  interactive keymaps.
- `when` / `when-let*` for side effects; `and` / `and-let*` when the
  value matters. `,@(and ...)` not `,@(when ...)` in backquotes.
- Prefer pure helpers and plain plists/alists. Effects (SQL, network,
  buffer) stay at boundaries. No EIEIO; `cl-defstruct` only when it
  clearly simplifies. Prefer `cl-lib` over full `cl`.
- One space between tokens; no column alignment. Keep functions short
  and reviewable. Prefer `declare-function` over hard `require` only to
  break real load cycles.
- Match GNU Emacs core idioms (`sqlite-*`, `url-queue-retrieve`, `dom`,
  `shr`) over training-data habits. Grep Emacs core for an existing
  built-in before inventing a helper.

## Verification

Makefile targets enter the project Nix dev shell when available, so the
pinned Emacs and `keymap-popup` are used automatically.

```sh
make test          # ERT suite, one Emacs per file
make test-oneshot  # whole suite in one Emacs
make lint          # checkdoc, package-lint, relint
make compile       # byte-compile lisp/synaxis*.el
nix flake check    # full matrix, reproducible
```

After changing a module: byte-compile it and run its ERT file before
moving on. Prefer temporary databases in tests; never point batch tests
at a live user DB. Run `git diff --check` before commit.
