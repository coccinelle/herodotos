TARGET = diff

INTFS=ast_diff.mli
SOURCES =  gumtree.ml gnudiff.ml diff.ml churn.ml
INCLUDES = -I ../commons -I ../exists -I ../org

LIBNAMES=xml-light

# Automatic computation
-include ../Makefile.config
OCAMLFIND=ocamlfind

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
LIBDIR=$(shell $(OCAMLFIND) query $(LIBNAMES))
FLAGS=$(LIBDIR:%=-I %)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(FLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(FLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) #$(INCLUDES)

all: $(LIB)
all.opt: $(OPTLIB)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PHONY: depend clean

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

$(OBJS) $(OPTOBJS): $(INTFS:.mli=.cmi)

$(LIB): $(OBJS)
	$(OCAMLC) -a -o $(LIB) $(OBJS)

$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

clean::
	rm -f $(TARGET)
	rm -f *.cm[aiox] *.cmxa *.a *.o *.annot
	rm -f *~ .*~

distclean: clean
	rm -f .depend

.depend depend: #$(GENERATED)
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
