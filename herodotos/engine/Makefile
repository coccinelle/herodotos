TARGET = engine

SOURCES = compute_size_and_date.ml \
	occ_correl.ml   \
	org2bug.ml      \
	correl.ml       \
	cfghelper.ml    \
	cfgmode.ml      \
	cfginit.ml      \
	cfgpreinit.ml   \
	cfgcorrel.ml    \
	cfgstat.ml      \
	cfgerase.ml     \
	cfgscm.ml       \
	test.ml
INTFS =
TOPLIB=commons hcl exists org diff graph html scm database

LIBNAMES=postgresql parmap bolt
P4=bolt
# Automatic computation
OCAMLFIND=ocamlfind

-include ../Makefile.config

LIBDIR=$(shell $(OCAMLFIND) query $(LIBNAMES))
P4DIR=$(shell $(OCAMLFIND) query $(P4))
P4FLAG=-pp 'camlp4o $(P4DIR)/bolt_pp.cmo'
INCLUDES=$(LIBDIR:%=-I %) $(TOPLIB:%=-I ../%)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes $(FLAGS)
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

.depend depend: # $(INTFS) $(SOURCES)
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
