EMACS ?= emacs
LISP  := $(wildcard lisp/synaxis*.el)
TESTS := $(wildcard tests/synaxis-*-tests.el)

.PHONY: all compile test clean

all: compile test

compile:
	$(EMACS) -Q --batch -L lisp -f batch-byte-compile $(LISP)

test:
	$(EMACS) -Q --batch -L lisp -L tests -l ert \
	    $(addprefix -l ,$(TESTS)) \
	    -f ert-run-tests-batch-and-exit

clean:
	rm -f lisp/*.elc tests/*.elc
