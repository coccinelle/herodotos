-include ../Makefile.config

OCAMLFIND=ocamlfind

SRC=database.ml
TARGET=database

OBJS=$(SRC:%.ml=%.cmo)
OPTOBJS=$(SRC:%.ml=%.cmx)
LIBNAMES=unix postgresql
NATIVELIBS=unix.cma threads.cma postgresql.cma

#LIBS=$(shell $(OCAMLFIND) query -a-format $(LIBNAMES))
LIBS=
LIBDIR=$(shell $(OCAMLFIND) query $(LIBNAMES))
FLAGS=$(LIBDIR:%=-I %) -I ../config
CFLAGS=-g -thread $(FLAGS)
#LFLAGS=-thread $(FLAGS) -custom -ccopt  $(NATIVELIBS) $(LIBS)
LFLAGS=-g -thread $(FLAGS)  $(NATIVELIBS) $(LIBS)

LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

all: $(LIB)
all.opt: $(OPTLIB)

ifeq ("$(OPTBIN)",".opt")
OCAMLC=$(OCAMLFIND) ocamlc -predicates native
OCAMLOPT=$(OCAMLFIND) ocamlopt -predicates native
else
OCAMLC=$(OCAMLFIND) ocamlc
OCAMLOPT=$(OCAMLFIND) ocamlopt
endif

.SUFFIXES: .ml .cmo .cmi .cmx
.PHONY: all depend

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmo:
	$(OCAMLC) -c $@ $(CFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $@ $(CFLAGS) $<

$(LIB): $(OBJS)
	$(OCAMLC) -a -o $(LIB) $(OBJS)

$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

main: $(LIBS) $(OBJS)
	$(OCAMLC) -o $(TARGET) $(LFLAGS) $^

distclean clean:
	rm -rf $(OBJS) .depend *~
	rm -rf *.cm[aiox] *.cmxa *.a *.o *.annot

depend:
	ocamldep $(SRC) > .depend

-include .depend
