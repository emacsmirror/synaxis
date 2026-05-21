.POSIX:

EMACS         ?= emacs
KEYMAP_POPUP  ?= $(HOME)/Dev/emacs-lisp/keymap-popup
LISP          := $(wildcard lisp/synaxis*.el)
TESTS         := $(wildcard tests/synaxis*-tests.el)

ifndef EMACS_CMD
GUIX := $(shell command -v guix 2>/dev/null)
GUIX_FLAGS ?=
ifdef GUIX
GUIX_SHELL := guix shell $(GUIX_FLAGS) --pure -D -f guix.scm emacs-next --
EMACS_CMD  := $(GUIX_SHELL) $(EMACS)
LOAD       := -L lisp
else
GUIX_SHELL :=
EMACS_CMD  := $(EMACS)
LOAD       := -L lisp -L $(KEYMAP_POPUP)
endif
endif

GUIX_WRAP = $(if $(GUIX_SHELL),$(GUIX_SHELL) $(MAKE) --no-print-directory EMACS_CMD=$(EMACS) LOAD="-L lisp",$(MAKE) --no-print-directory)

BATCH = $(EMACS_CMD) -Q --batch $(LOAD)

.PHONY: all compile do-compile test do-test lint do-lint clean autoloads load

all: compile test

compile:
	@$(GUIX_WRAP) do-compile

do-compile:
	$(BATCH) -f batch-byte-compile $(LISP)

test:
	@$(GUIX_WRAP) do-test

do-test:
	$(BATCH) -L tests -l ert \
	    $(addprefix -l ,$(TESTS)) \
	    -f ert-run-tests-batch-and-exit

lint:
	@$(GUIX_WRAP) do-lint

do-lint:
	@for f in $(LISP); do \
	  echo "Checking $$f..."; \
	  $(BATCH) --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

autoloads:
	$(EMACS_CMD) -Q --batch \
	    --eval "(setq make-backup-files nil)" \
	    --eval "(require 'loaddefs-gen)" \
	    --eval "(loaddefs-generate \"lisp\" \"lisp/synaxis-autoloads.el\")"

clean:
	rm -f lisp/*.elc tests/*.elc lisp/synaxis-autoloads.el

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	  (dolist (sym '(synaxis-search-mode-map synaxis-show-mode-map synaxis-edit-map)) \
	    (when (boundp sym) (makunbound sym))))" > /dev/null
	@for f in $(LISP); do \
	  emacsclient --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m $$f\n"; \
	done
	@emacsclient --eval "(dolist (buf (buffer-list)) \
	  (with-current-buffer buf \
	    (cond ((derived-mode-p 'synaxis-search-mode) \
	           (use-local-map synaxis-search-mode-map)) \
	          ((derived-mode-p 'synaxis-show-mode) \
	           (use-local-map synaxis-show-mode-map)))))" > /dev/null
	@printf "\033[32mLoaded synaxis into running Emacs\033[0m\n"
