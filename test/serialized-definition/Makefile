DEF_KORE = $(KORE_FILES)

include $(CURDIR)/../include.mk

KORE_FILES=$(wildcard [a-z]*definition.kore)

ifeq "$(shell uname)" "Darwin"
.PHONY: test
test:
	@echo "Skipping serialization test on Mac OS"
else
test: $(addprefix test-,$(basename $(KORE_FILES)))
endif

MODULE=UNDEFINED-MODULE

# lame solution: specify the --module $MODULE individually instead of grep|tail|sed
test-a-to-f-definition: MODULE=TEST
test-test-totalSupply-definition: MODULE=VERIFICATION

test-%: %.kore
	@echo "Serializing definition $<"
	$(KORE_EXEC) $< --output serialized.bin --module $(MODULE) --serialize
	@echo "Loading serialized definition (to hit an error afterwards)"
	sh -c "$(KORE_EXEC) serialized.bin \
		--module $(MODULE) \
		--pattern serialized.bin \
		--debug-rewrite FAIL-ME-PLEASE 2>&1 | \
		grep FAIL-ME-PLEASE"
	@rm serialized.bin

.PHONY: clean
clean:
	rm -f serialized.bin

golden:
