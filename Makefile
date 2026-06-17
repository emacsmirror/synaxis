.PHONY: all compile do-compile native-comp do-native-comp test do-test \
        do-test-summary test-oneshot do-test-oneshot lint do-lint \
        lint-checkdoc lint-package-lint lint-relint autoloads load clean

NIX := $(shell command -v nix 2>/dev/null)

# Re-enter the Nix dev shell once so every target runs against the
# pinned Emacs + keymap-popup.  Inside the shell (SYNAXIS_ENV_WRAPPED)
# or with no Nix available, run the work directly.
ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(SYNAXIS_ENV_WRAPPED),)
ifneq ($(NIX),)
ENV_MAKE = nix develop path:$(CURDIR) --command env SYNAXIS_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif

EMACS_CMD  ?= emacs
EMACS_OPTS ?= -Q --batch

# Outside the Nix shell with no Nix at all, fall back to a local
# keymap-popup checkout so -L can find it.
KEYMAP_POPUP ?= $(HOME)/Projects/emacs-lisp/keymap-popup
LOAD := -L lisp
ifeq ($(SYNAXIS_ENV_WRAPPED),)
ifeq ($(NIX),)
LOAD += -L $(KEYMAP_POPUP)
endif
endif

JOBS         ?= $(shell nproc 2>/dev/null || echo 4)
TEST_RESULTS := .test-results

LISP  := $(wildcard lisp/synaxis*.el)
TESTS := $(wildcard tests/synaxis*-tests.el)
TEST_STAMPS := $(patsubst tests/%.el,$(TEST_RESULTS)/%.stamp,$(TESTS))

BATCH = $(EMACS_CMD) $(EMACS_OPTS) $(LOAD)

all: compile test

compile:
	@$(ENV_MAKE) do-compile

do-compile:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
	    -f batch-byte-compile $(LISP)

native-comp:
	@$(ENV_MAKE) do-native-comp

# Native-compile each file and fail on "not known to be defined" --
# catches missing `require's (a macro used without its library compiles
# under byte-comp by transitive load order, but native-comp flags it).
do-native-comp:
	@fails=0; \
	for f in $(LISP); do \
	  output=$$($(BATCH) --eval "(native-compile \"$$f\")" 2>&1); \
	  matched=$$(echo "$$output" | grep "is not known to be defined" || true); \
	  if [ -n "$$matched" ]; then \
	    printf "\033[31m%s\033[0m\n" "$$f"; \
	    echo "$$matched"; \
	    fails=1; \
	  fi; \
	done; \
	if [ $$fails -eq 0 ]; then \
	  printf "\033[32mnative-comp clean\033[0m\n"; \
	else exit 1; fi

test:
	@$(ENV_MAKE) -j$(JOBS) -Otarget do-test

do-test:
	@rm -rf $(TEST_RESULTS)
	@mkdir -p $(TEST_RESULTS)
	@$(MAKE) --no-print-directory -j$(JOBS) -Otarget do-test-summary

$(TEST_RESULTS)/%.stamp: tests/%.el
	@output=$$($(BATCH) -L tests -l ert -l $< \
	  -f ert-run-tests-batch-and-exit 2>&1); \
	rc=$$?; \
	n=$$(echo "$$output" | grep -o 'Ran [0-9]*' | grep -o '[0-9]*'); \
	if [ $$rc -ne 0 ]; then \
	  printf "\033[31mFAIL\033[0m $< ($${n:-0} tests)\n"; \
	  echo "$$output" | grep '  FAILED'; \
	  printf "FAIL %s\n" "$${n:-1}" > $@; \
	else \
	  printf "\033[32m  OK\033[0m $< ($$n tests)\n"; \
	  printf "OK %s\n" "$$n" > $@; \
	fi

do-test-summary: $(TEST_STAMPS)
	@total=0; passed=0; failed=0; failed_files=""; \
	for f in $(TEST_STAMPS); do \
	  read status n < $$f; \
	  total=$$((total + n)); \
	  if [ "$$status" = "FAIL" ]; then \
	    failed=$$((failed + n)); \
	    base=$$(basename $$f .stamp); \
	    failed_files="$$failed_files tests/$$base.el"; \
	  else \
	    passed=$$((passed + n)); \
	  fi; \
	done; \
	echo ""; \
	if [ $$failed -eq 0 ]; then \
	  printf "\033[32m$$total tests, $$passed passed, 0 failed\033[0m\n"; \
	  rm -rf $(TEST_RESULTS); \
	else \
	  printf "\033[31m$$total tests, $$passed passed, $$failed failed\033[0m\n"; \
	  for f in $$failed_files; do echo "  $$f"; done; \
	  printf "\nStamps preserved in $(TEST_RESULTS)/ for debugging.\n"; \
	fi; \
	[ $$failed -eq 0 ]

test-oneshot:
	@$(ENV_MAKE) do-test-oneshot

# Load every test file into one Emacs and run the whole suite twice.
# Surfaces cross-test state pollution that the per-file `do-test' runs
# (one Emacs per file) cannot see.
do-test-oneshot:
	$(BATCH) -L tests -l ert \
	    $(addprefix -l ,$(TESTS)) \
	    --eval="(let ((bad 0)) \
	              (dotimes (_ 2) \
	                (setq bad (+ bad (ert-stats-completed-unexpected \
	                                  (ert-run-tests-batch t))))) \
	              (kill-emacs (if (zerop bad) 0 1)))"

lint:
	@$(ENV_MAKE) do-lint

do-lint: lint-checkdoc lint-package-lint lint-relint

lint-checkdoc:
	@for f in $(LISP); do \
	  echo "checkdoc $$f"; \
	  $(BATCH) --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

lint-package-lint:
	$(BATCH) --eval "(require 'package-lint)" \
	    --eval "(setq package-lint-main-file \"lisp/synaxis.el\")" \
	    -f package-lint-batch-and-exit $(LISP)

lint-relint:
	$(BATCH) --eval "(require 'relint)" -f relint-batch lisp

autoloads:
	$(EMACS_CMD) $(EMACS_OPTS) \
	    --eval "(setq make-backup-files nil)" \
	    --eval "(require 'loaddefs-gen)" \
	    --eval "(loaddefs-generate \"lisp\" \"lisp/synaxis-autoloads.el\")"

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

clean:
	rm -f lisp/*.elc tests/*.elc lisp/synaxis-autoloads.el
	rm -rf $(TEST_RESULTS)
