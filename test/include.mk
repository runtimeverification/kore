ifeq ($(origin TOP), undefined)
	TOP = $(shell git rev-parse --show-toplevel)
endif

include $(TOP)/include.mk

DEF_DIR ?= .
TEST_DIR ?= .

DEF ?= test
EXT ?= $(DEF)
KPROVE_MODULE ?= VERIFICATION

IGNORE_EXIT = true

DIFF = diff -u
FAILED = ( mv $@ $@.fail && false )
FAILED_STORE_PROOFS = ( mv $(STORE_PROOFS) $(STORE_PROOFS).fail && mv $@ $@.fail && false )

KOMPILED := $(TEST_DIR)/$(DEF)-kompiled
export KOMPILED
DEF_KORE_DEFAULT = $(KOMPILED)/definition.kore
DEF_KORE ?= $(DEF_KORE_DEFAULT)
TEST_DEPS = $(K) $(DEF_KORE)

TESTS = \
	$(wildcard $(DEF_DIR)/*.verify) \
	$(wildcard $(TEST_DIR)/*.$(EXT)) \
	$(wildcard $(TEST_DIR)/*-spec.k) \
	$(wildcard $(TEST_DIR)/*-spec.stderr) \
	$(wildcard $(TEST_DIR)/test-*.sh)

OUTS += $(foreach TEST, $(TESTS), $(TEST).out)
GOLDEN += $(foreach OUT, $(OUTS), $(OUT).golden)

KOMPILE_OPTS += --output-definition $(KOMPILED) -Wno unused-var -Wno missing-syntax-module

ifeq "$(shell uname)" "Darwin"
	KOMPILE_OPTS += --no-haskell-binary
endif

KRUN_OPTS += --definition $(KOMPILED)
KORE_EXEC_OPTS += \
	$(if $(STORE_PROOFS),\
		--save-proofs $(STORE_PROOFS),\
		$(if $(RECALL_PROOFS),\
			--save-proofs $(@:.out=.save-proofs.kore)\
		)\
	)
KPROVE_SPEC = $<
KPROVE_SPEC_OPTS = -Wno unused-var -Wno missing-syntax-module

$(DEF_KORE_DEFAULT): $(DEF_DIR)/$(DEF).k $(K)
	@echo ">>>" $(CURDIR) "kompile" $<
	rm -fr $(KOMPILED)
	env -u KORE_EXEC_OPTS $(KOMPILE) $(KOMPILE_OPTS) $<

# From make 3.82 news: http://cvs.savannah.gnu.org/viewvc/*checkout*/make/make/NEWS?revision=2.120
# * WARNING: Backward-incompatibility!
#   The pattern-specific variables and pattern rules are now applied in the
#   shortest stem first order instead of the definition order (variables
#   and rules with the same stem length are still applied in the definition
#   order). This produces the usually-desired behavior where more specific
#   patterns are preferred. To detect this feature search for 'shortest-stem'
#   in the .FEATURES special variable.

%.golden: DIFF = true
%.golden: %
	cp $< $@

### RUN

%.$(EXT).out: $(TEST_DIR)/%.$(EXT) $(TEST_DEPS)
	@echo ">>>" $(CURDIR) "krun" $<
	@echo "KORE_EXEC_OPTS =" $(KORE_EXEC_OPTS)
	rm -f $@
	$(KRUN) $(KRUN_OPTS) $< --output-file $@
	$(DIFF) $@.golden $@ || $(FAILED)

%.verify.out: $(DEF_KORE_DEFAULT)
	$(KORE_PARSER) $(DEF_KORE_DEFAULT) --verify >/dev/null 2>$@ || true
	$(DIFF) $@.golden $@ || $(FAILED)

### SEARCH

%.search.final.$(EXT).out: KRUN_OPTS += --search-final

%.search.star.$(EXT).out: KRUN_OPTS += --search-all

%.search.one.$(EXT).out: KRUN_OPTS += --search-one-step

%.search.plus.$(EXT).out: KRUN_OPTS += --search-one-or-more-steps

PATTERN_OPTS = --pattern "$$(cat $*.k)"

%.search-pattern.final.$(EXT).out: KRUN_OPTS += --search-final $(PATTERN_OPTS)

%.search-pattern.star.$(EXT).out: KRUN_OPTS += --search-all $(PATTERN_OPTS)

%.search-pattern.one.$(EXT).out: KRUN_OPTS += --search-one-step $(PATTERN_OPTS)

%.search-pattern.plus.$(EXT).out: \
	KRUN_OPTS += --search-one-or-more-steps $(PATTERN_OPTS)

### PROVE

%-spec.k.out: $(TEST_DIR)/%-spec.k $(TEST_DEPS)
	@echo ">>>" $(CURDIR) "kprove" $<
	@echo "KORE_EXEC_OPTS =" $(KORE_EXEC_OPTS)
	rm -f $@
	$(if $(STORE_PROOFS),rm -f $(STORE_PROOFS),$(if $(RECALL_PROOFS),cp $(RECALL_PROOFS) $(@:.out=.save-proofs.kore)))
	$(KPROVE) $(KPROVE_OPTS) $(KPROVE_SPEC_OPTS) $(KPROVE_SPEC) >$@ || true
	$(DIFF) $@.golden $@ || $(FAILED)
	$(if $(STORE_PROOFS),$(DIFF) $(STORE_PROOFS).golden $(STORE_PROOFS) || $(FAILED_STORE_PROOFS))

%-save-proofs-spec.k.out: STORE_PROOFS = $(@:.out=.save-proofs.kore)

%.save-proofs.kore: %.out
	[ -f $@ ]

%-spec.k.repl: $(TEST_DIR)/%-spec.k $(TEST_DEPS)
	$(KPROVE) $(KPROVE_REPL_OPTS) $<

%-spec.k.run-repl-script: $(TEST_DIR)/%-spec.k $(TEST_DEPS)
	$(KPROVE) $(KPROVE_REPL_OPTS) $<
%-spec.k.run-repl-script: KORE_REPL_OPTS= -r --repl-script $@

%-repl-spec.k.out: KPROVE_OPTS = $(KPROVE_REPL_OPTS)

%-repl-script-spec.k.out: %-repl-script-spec.k.repl
%-repl-script-spec.k.out: KORE_REPL_OPTS = -r --save-run-output --repl-script $<.repl
%-repl-script-spec.k.out: KPROVE_OPTS = $(KPROVE_REPL_OPTS)

### SCRIPTS

test-%.sh.out: $(TEST_DIR)/test-%.sh
	@echo ">>>" $(CURDIR) $(@:.out=)
	rm -f $@
	$(TEST_DIR)/$(@:.out=) > $@ || $(IGNORE_EXIT)
	$(DIFF) $@.golden $@ || $(FAILED)

### TARGETS

test: test-k

test-simplifierx: test-k-simplifierx

test-k-simplifierx: KORE_EXEC_OPTS += --simplifierx
test-k-simplifierx: KORE_REPL_OPTS += --simplifierx
test-k-simplifierx: KORE_MATCH_DISJUNCTION_OPTS += --simplifierx
test-k-simplifierx: KORE_CHECK_FUNCTIONS_OPTS += --simplifierx
test-k-simplifierx: $(OUTS)

test-k: $(OUTS)

build-test: $(TEST_DEPS)

golden: $(GOLDEN)

clean: clean-execution
	rm -fr $(KOMPILED)

clean-execution:
	rm -fr $(TEST_DIR)/*.out $(TEST_DIR)/*.save-proofs.kore

.PHONY: test-k test-k-simplifierx test-simplifierx test golden clean clean-execution
