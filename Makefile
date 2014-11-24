RUSTC=rustc
RUST_FLAGS=-A warnings -C rpath
.PHONY: all crust clean
MAKE=make

all: rbmc crust

rbmc: main.rs trans.rs
	$(RUSTC) $(RUST_FLAGS) -C rpath main.rs

crust:
	$(MAKE) -C crust

clean:
	rm -f rbmc
	$(MAKE) -C crust clean
