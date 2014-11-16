TARGET = commons

SOURCES = misc.ml global.ml versions.ml debug.ml
INTFS = ast.mli
TOPLIB =

LIBNAMES=bolt
P4=bolt

# Automatic computation
OCAMLFIND=ocamlfind

-include ../Makefile.config

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

LIBDIR=$(shell $(OCAMLFIND) query $(LIBNAMES))
P4DIR=$(shell $(OCAMLFIND) query $(P4))
P4FLAG=-pp 'camlp4o $(P4DIR)/bolt_pp.cmo'
INCLUDES=$(LIBDIR:%=-I %) $(TOPLIB:%=-I ../%)

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes $(FLAGS)
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES) $(P4FLAG)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES) $(P4FLAG)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES) $(P4FLAG)

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

.depend depend: $(INTFS) $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
