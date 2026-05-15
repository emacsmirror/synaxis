EMACS         ?= emacs
KEYMAP_POPUP  ?= $(HOME)/Dev/emacs-lisp/keymap-popup
LISP          := $(wildcard lisp/synaxis*.el)
TESTS         := $(wildcard tests/synaxis-*-tests.el)

LOAD := -L lisp -L $(KEYMAP_POPUP)

.PHONY: all compile test clean

all: compile test

compile:
	$(EMACS) -Q --batch $(LOAD) -f batch-byte-compile $(LISP)

test:
	$(EMACS) -Q --batch $(LOAD) -L tests -l ert \
	    $(addprefix -l ,$(TESTS)) \
	    -f ert-run-tests-batch-and-exit

clean:
	rm -f lisp/*.elc tests/*.elc
