TARGET = database

SOURCES = database.ml sql.ml sql_update.ml
INTFS =
INCLUDES = -I ../commons -I ../org -I ../hcl

OCAMLFIND=ocamlfind

LIBNAMES=unix postgresql
NATIVELIBS=unix.cma threads.cma postgresql.cma

# Automatic computation
-include ../Makefile.config

LIBDIR=$(shell $(OCAMLFIND) query $(LIBNAMES))
FLAGS=$(LIBDIR:%=-I %) $(INCLUDES)
CFLAGS=-g -thread $(FLAGS)
#LFLAGS=-thread $(FLAGS) -custom -ccopt  $(NATIVELIBS) $(LIBS)
LFLAGS=-g -thread $(FLAGS)  $(NATIVELIBS) $(LIBS)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

# The Caml compilers.
OCAMLCFLAGS ?= -g -dtypes $(FLAGS)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)
ifeq ("$(OPTBIN)",".opt")
OCAMLC=$(OCAMLFIND) ocamlc -predicates native
OCAMLOPT=$(OCAMLFIND) ocamlopt -predicates native
else
OCAMLC=$(OCAMLFIND) ocamlc
OCAMLOPT=$(OCAMLFIND) ocamlopt
endif

all: $(LIB)
all.opt: $(OPTLIB)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PHONY: depend clean

.ml.cmo:
	$(OCAMLC) -c $@ $(CFLAGS) $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $@ $(CFLAGS) $<

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
