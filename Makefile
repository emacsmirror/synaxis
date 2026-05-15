EMACS         ?= emacs
KEYMAP_POPUP  ?= $(HOME)/Dev/emacs-lisp/keymap-popup
LISP          := $(wildcard lisp/synaxis*.el)
TESTS         := $(wildcard tests/synaxis-*-tests.el)

LOAD := -L lisp -L $(KEYMAP_POPUP)

.PHONY: all compile test clean autoloads

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
