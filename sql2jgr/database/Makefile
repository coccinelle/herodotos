OCAMLFIND=ocamlfind

SRC=database.ml
TARGET=database

OBJS=$(SRC:%.ml=%.cmo)
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


OPT=1
ifeq ($(OPT),1)
OCAMLC=ocamlfind ocamlc -predicates native
else
OCAMLC=ocamlfind ocamlc
endif

.SUFFIXES: .ml .cmo
.PHONY: all depend

.ml.cmo:
	$(OCAMLC) -c $@ $(CFLAGS) $^


$(LIB): $(OBJS)
	$(OCAMLC) -a -o $(LIB) $(OBJS)

$(OPTLIB): $(OPTOBJS)
	$(OCAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)

main: $(LIBS) $(OBJS)
	$(OCAMLC) -o $(TARGET) $(LFLAGS) $^

clean:
	rm -rf $(OBJS) *~
	rm -rf *.cm[aiox] *.cmxa *.a *.o *.annot

depend:
	ocamldep $(SRC) > .depend

-include .depend
