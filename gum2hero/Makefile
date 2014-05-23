LIB=xml-light.cma
INCLUDE=-I /usr/lib/ocaml/xml-light -I ../herodotos/diff

all: gum2hero

gum2hero:
	ocamlc -dtypes $(INCLUDE) $(LIB) -o $@ $@.ml
