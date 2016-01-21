#!/usr/bin/make -f

RUSTC ?= rustc
CRUST_HOME ?= ..
RBMC ?= $(CRUST_HOME)/bin/rbmc
PREPROCESS ?= $(CRUST_HOME)/bin/Preprocess
CRUST_NATIVE ?= $(CRUST_HOME)/bin/crust.native

TEST_HOME ?= $(CRUST_HOME)/stdlib_tests
SRC ?= $(TEST_HOME)/src
FILTERS ?= $(TEST_HOME)/filters
ZEALOT ?= python $(TEST_HOME)/bin/zealot.py
SCRIPT_BIN ?= $(TEST_HOME)/bin


STDLIBS = core libc alloc unicode collections __crust2
STDLIB_RLIBS = $(patsubst %,lib/lib%.rlib,$(STDLIBS))
STDLIB_IRS = $(patsubst %,ir/lib%.ir,$(STDLIBS))

TARGET = $(TEST_HOME)/x86_64-custom-linux-gnu.json


lib/lib%.rlib: $(SRC)/lib%/lib.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<

lib/lib%.rlib: $(CRUST_HOME)/tests/%.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<

lib/lib%.rlib: $(CRUST_HOME)/tests/driver/%.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<

lib/lib%.rlib: $(CRUST_HOME)/tests/custom/%.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<

lib/lib__crust.rlib: $(CRUST_HOME)/src/crust-stubs.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<

lib/lib__crust2.rlib: $(CRUST_HOME)/src/crust.rs lib/lib__crust.rlib
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<


ir/lib%.ir: $(SRC)/lib%/lib.rs $(STDLIB_RLIBS)
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@

ir/lib%.ir: $(CRUST_HOME)/tests/%.rs $(STDLIB_RLIBS)
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@

ir/lib%.ir: $(CRUST_HOME)/tests/driver/%.rs $(STDLIB_RLIBS)
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@

ir/lib%.ir: $(CRUST_HOME)/tests/custom/%.rs $(STDLIB_RLIBS)
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@

ir/lib__crust2.ir: $(CRUST_HOME)/src/crust.rs $(STDLIB_RLIBS)
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@


# NB: avoid overlap with other (prefix)%.ir rules
ir/scrubbed.%.ir: ir/%.ir
	$(PREPROCESS) --passes move-break,scrub <$< >$@.tmp
	mv -v $@.tmp $@

ir/prep.%.ir: ir/%.ir
	$(PREPROCESS) --passes hl-prepare-libs <$< >$@.tmp
	mv -v $@.tmp $@

ir/stubs.%.ir: ir/prep.%.ir
	$(PREPROCESS) --passes stubify <$< >$@.tmp
	mv -v $@.tmp $@

ir/stdlibs.ir: $(STDLIB_IRS)
	cat $^ >$@.tmp
	mv -v $@.tmp $@

ir/stdlibs_lib%.ir: ir/lib%.ir ir/stdlibs.ir ir/lib__crust2.ir
	cat $^ >$@.tmp
	mv -v $@.tmp $@

ir/core_lib%.ir: ir/lib%.ir ir/libcore.ir ir/lib__crust2.ir
	cat $^ >$@.tmp
	mv -v $@.tmp $@

ir/alloc_lib%.ir: ir/lib%.ir ir/libcore.ir ir/liblibc.ir ir/liballoc.ir ir/lib__crust2.ir
	cat $^ >$@.tmp
	mv -v $@.tmp $@

.SECONDEXPANSION:

driver/%.lib.ir: ir/$$(shell $(TEST_HOME)/bin/filter_helper.sh $(FILTERS)/$$*.filter).ir \
		$(FILTERS)/%.filter
	cp -v $< $@

driver/%.lib-prep.ir: ir/prep.$$(shell $(TEST_HOME)/bin/filter_helper.sh $(FILTERS)/$$*.filter).ir \
		$(FILTERS)/%.filter
	cp -v $< $@

driver/%.lib-stubs.ir: ir/stubs.$$(shell $(TEST_HOME)/bin/filter_helper.sh $(FILTERS)/$$*.filter).ir \
		$(FILTERS)/%.filter
	cp -v $< $@

driver/%.drv0: driver/%.lib.ir
	cat $< | $(PREPROCESS) \
		--passes hl-generate-drivers \
		--merged-filter $(FILTERS)/$*.filter \
		>$@.tmp
	mv -v $@.tmp $@

driver/%.drv: driver/%.drv0
	cat $< | $(PREPROCESS) \
		--passes hl-clean-drivers \
		>$@.tmp
	mv -v $@.tmp $@

test/%.rs: driver/%.drv
	cat $< | $(CRUST_NATIVE) -driver-gen -test-case-prefix test/$*
	mv -v test/$*_0.rs $@

test/%.drv.ir: test/%.rs lib/lib__crust.rlib
	$(ZEALOT) $(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@

test/%.drv-prep.ir: driver/%.lib-stubs.ir test/%.drv.ir
	cat $^ | $(PREPROCESS) --passes hl-prepare-drivers >$@.tmp
	mv -v $@.tmp $@

#test/%.drv-split.stamp: test/%.drv.ir
#	rm -fv test/$*.drv-split-*.ir
#	$(SCRIPT_BIN)/split-drivers.sh $< test/$*.drv-split
#	touch $@

#test/%.drv-prep.stamp: driver/%.lib-stubs.ir test/%.drv-split.stamp
#	rm -fv test/$*.drv-prep-*.ir
#	ls test/$*.drv-split-*.ir | parallel \
#		$(SCRIPT_BIN)/prepare-driver.sh $(PREPROCESS) $< test/$*.drv-prep
#	cat $^ | $(PREPROCESS) --passes hl-prepare-drivers >$@.tmp
#	touch $@

test/%.drv-fin.ir: driver/%.lib-prep.ir test/%.drv-prep.ir
	cat $^ | $(PREPROCESS) --passes hl-finish-drivers >$@.tmp
	mv -v $@.tmp $@

test/%.c: test/%.drv-fin.ir
	$(CRUST_NATIVE) $< >$@.tmp
	mv -v $@.tmp $@

.SECONDARY:
