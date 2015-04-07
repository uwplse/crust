RUSTC=rustc -O
RUST_FLAGS=-A warnings -C rpath
GHC=ghc -O3
ALEX=alex
.PHONY: all crust clean archive
MAKE=make
BINARIES=rbmc crust Preprocess

all: $(BINARIES)

rbmc: main.rs trans.rs
	$(RUSTC) $(RUST_FLAGS) main.rs

crust:
	$(MAKE) -C crust

Preprocess: $(wildcard *.hs) Lexer.hs
	$(GHC) --make Preprocess.hs

Lexer.hs: Lexer.x
	$(ALEX) Lexer.x

clean:
	rm -f rbmc
	bash -c "rm -f Lexer.hs {DropGlue,Lexer,Parser,Preprocess,TempLift,Index,Builder,Pprint,Rename}.{o,hi} Preprocess"
	$(MAKE) -C crust clean
