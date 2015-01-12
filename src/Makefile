RUSTC=rustc
RUST_FLAGS=-A warnings -C rpath
GHC=ghc
ALEX=alex
.PHONY: all crust clean archive
MAKE=make
BINARIES=rbmc crust Preprocess

all: $(BINARIES)

rbmc: main.rs trans.rs
	$(RUSTC) $(RUST_FLAGS) main.rs

crust:
	$(MAKE) -C crust

Preprocess: Preprocess.hs Lexer.hs Parser.hs
	$(GHC) --make Preprocess.hs

Lexer.hs: Lexer.x
	$(ALEX) Lexer.x

clean:
	rm -f rbmc
	bash -c "rm -f Lexer.hs {Lexer,Parser,Preprocess}.{o,hi} Preprocess"
	$(MAKE) -C crust clean