#!/usr/bin/make -f

RUSTC ?= rustc
CRUST_HOME ?= ..
RBMC ?= $(CRUST_HOME)/bin/rbmc
PREPROCESS ?= $(CRUST_HOME)/bin/Preprocess
CRUST_NATIVE ?= $(CRUST_HOME)/bin/crust.native

TEST_HOME ?= .
SRC ?= $(TEST_HOME)/src
FILTERS ?= $(TEST_HOME)/filters


STDLIBS = core libc alloc unicode collections
STDLIB_RLIBS = $(patsubst %,lib/lib%.rlib,$(STDLIBS))
STDLIB_IRS = $(patsubst %,ir/lib%.ir,$(STDLIBS))

TARGET = x86_64-custom-linux-gnu.json


lib/lib%.rlib: $(SRC)/lib%/lib.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<

lib/lib%.rlib: ../tests/%.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<

lib/lib%.rlib: ../tests/driver/%.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<

lib/lib__crust.rlib: ../src/crust-stubs.rs
	$(RUSTC) -L lib --out-dir=lib --target=$(TARGET) $<


ir/lib%.ir: $(SRC)/lib%/lib.rs $(STDLIB_RLIBS)
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@

ir/lib%.ir: ../tests/%.rs $(STDLIB_RLIBS)
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@

ir/lib%.ir: ../tests/driver/%.rs $(STDLIB_RLIBS)
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@


ir/stdlibs.ir: $(STDLIB_IRS)
	cat $^ >$@.tmp
	mv -v $@.tmp $@

ir/stdlibs_lib%.ir: ir/lib%.ir ir/stdlibs.ir
	cat $^ >$@.tmp
	mv -v $@.tmp $@

ir/core_lib%.ir: ir/lib%.ir ir/libcore.ir
	cat $^ >$@.tmp
	mv -v $@.tmp $@

ir/alloc_lib%.ir: ir/lib%.ir ir/libcore.ir ir/liblibc.ir ir/liballoc.ir
	cat $^ >$@.tmp
	mv -v $@.tmp $@

ir/%.scrubbed.ir: ir/%.ir
	$(PREPROCESS) --passes move-break,scrub <$< >$@.tmp
	mv -v $@.tmp $@

.SECONDEXPANSION:

driver/%.ir: ir/$$(shell bin/filter_helper.sh $(FILTERS)/$$*.filter).ir \
		$(FILTERS)/%.filter
	cp -v $< $@

driver/%.drv0: driver/%.ir
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

test/%_0.rs: driver/%.drv
	cat $< | $(CRUST_NATIVE) -driver-gen -test-case-prefix test/$*

test/%_0.drv.ir: test/%_0.rs lib/lib__crust.rlib
	$(RBMC) -L lib --target=$(TARGET) $< >$@.tmp
	mv -v $@.tmp $@

test/%_0.drv-pp.ir: test/%_0.drv.ir driver/%.drv0
	cat $^ | $(PREPROCESS) \
		--passes hl-compile-drivers >$@.tmp
	mv -v $@.tmp $@

test/%_0.c: test/%_0.drv-pp.ir
	$(CRUST_NATIVE) $< >$@.tmp
	mv -v $@.tmp $@

.SECONDARY:
