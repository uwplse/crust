.PHONY: all rust_lib crust
MAKE=make

all: crust lib/libcore-4e7c5e5c.rlib

crust:
	$(MAKE) -C src
	cp --preserve=timestamps ./src/rbmc ./bin
	cp --preserve=timestamps ./src/Preprocess ./bin
	cp --preserve=timestamps ./src/crust/_build/crust.native ./bin

lib/libcore-4e7c5e5c.rlib: 
	mkdir lib/ > /dev/null
	bash ./bin/find_core.sh

clean:
	rm -rf ./lib
	rm -rf ./bin/rbmc ./bin/crust.native ./bin/Preprocess
	make -C src clean
