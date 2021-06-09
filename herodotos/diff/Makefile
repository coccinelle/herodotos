TARGET = diff

INTFS=ast_diff.mli diff_type.mli
SOURCES =  gumtree.ml gnudiff.ml hybrid.ml diff.ml churn.ml
INCLUDEDIRS = commons exists org hcl
P4=volt

LIBNAMES=parmap volt #xml-light

# Automatic computation
-include ../Makefile.config
OCAMLFIND=ocamlfind

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
LIBDIR=$(shell $(OCAMLFIND) query $(LIBNAMES))
P4DIR=$(shell $(OCAMLFIND) query $(P4))
INCLUDES=$(LIBDIR:%=-I %) $(INCLUDEDIRS:%=-I ../%)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)
P4FLAG=-pp 'camlp4o $(P4DIR)/bolt_pp.cmo'

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES) $(P4FLAG)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES) $(P4FLAG)
OCAMLDEP = ocamldep$(OPTBIN) $(P4FLAG) #$(INCLUDES)

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
