TARGET = engine

SOURCES = occ_correl.ml \
	org2bug.ml      \
	correl.ml       \
	cfghelper.ml    \
	cfgmode.ml      \
	cfginit.ml      \
	cfgpreinit.ml   \
	cfgcorrel.ml    \
	cfgstat.ml      \
	cfgerase.ml     \
	test.ml
INTFS =
INCLUDES = -I ../commons -I ../hcl -I ../exists -I ../org -I ../diff -I ../graph -I ../html -I ../scm

# Automatic computation
-include ../Makefile.config

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes $(FLAGS)
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)

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
