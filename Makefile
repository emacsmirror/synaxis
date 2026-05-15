EMACS         ?= emacs
KEYMAP_POPUP  ?= $(HOME)/Dev/emacs-lisp/keymap-popup
LISP          := $(wildcard lisp/synaxis*.el)
TESTS         := $(wildcard tests/synaxis-*-tests.el)

LOAD := -L lisp -L $(KEYMAP_POPUP)

.PHONY: all compile test clean autoloads load

all: compile test

compile:
	$(EMACS) -Q --batch $(LOAD) -f batch-byte-compile $(LISP)

test:
	$(EMACS) -Q --batch $(LOAD) -L tests -l ert \
	    $(addprefix -l ,$(TESTS)) \
	    -f ert-run-tests-batch-and-exit

autoloads:
	$(EMACS) -Q --batch \
	    --eval "(setq make-backup-files nil)" \
	    --eval "(require 'loaddefs-gen)" \
	    --eval "(loaddefs-generate \"lisp\" \"lisp/synaxis-autoloads.el\")"

clean:
	rm -f lisp/*.elc tests/*.elc lisp/synaxis-autoloads.el

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	  (dolist (sym '(synaxis-search-mode-map synaxis-show-mode-map)) \
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
