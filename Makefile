RUSTC=rustc
RUST_FLAGS=-A warnings -C rpath
GHC=ghc
ALEX=alex
.PHONY: all crust clean
MAKE=make

all: rbmc crust Preprocess

rbmc: main.rs trans.rs
	$(RUSTC) $(RUST_FLAGS) -C rpath main.rs

crust:
	$(MAKE) -C crust

Preprocess: Preprocess.hs Lexer.hs Parser.hs
	$(GHC) --make Preprocess.hs

Lexer.hs: Lexer.x
	$(ALEX) Lexer.x

clean:
	rm -f rbmc
	rm -f Lexer.hs {Lexer,Parser,Preprocess}.{o,hi} Preprocess
	$(MAKE) -C crust clean
