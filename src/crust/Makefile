SPACE :=
SPACE +=
COMMA=,

OCAMLBUILD=ocamlbuild
BUILD_PACKAGES=str
OB_PACKAGE_STR=$(subst $(SPACE),$(COMMA),$(BUILD_PACKAGES))

OCAMLBUILD_FLAGS=-cflag -g -lflag -g -use-ocamlfind -package $(OB_PACKAGE_STR) -tag debug
.PHONY: clean

crust.native: *.ml *.mli
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $@

clean:
	$(OCAMLBUILD) -clean

