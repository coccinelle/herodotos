OCAMLFIND=ocamlfind
SUBDIRS=commons config database scm graph html
EXTLIBS=postgresql

SRC=engine.ml sql2jgr.ml
TARGET=sql2jgr

OBJS=$(SRC:%.ml=%.cmo)
LIBS=$(SUBDIRS:%=%.cma)

OCAMLLIBS=unix.cma threads.cma str.cma $(EXTLIBS:%=%.cma)

LIBDIR=$(shell $(OCAMLFIND) query $(EXTLIBS))
ALLLIBS=$(OCAMLLIBS) $(LIBS)

DEP = $(SUBDIRS)

INCLUDES=$(SUBDIRS:%=-I %) $(LIBDIR:%=-I %)
FLAGS=$(INCLUDES)
CFLAGS=-thread $(FLAGS)
LFLAGS=-g -thread $(FLAGS) -custom -ccopt $(LIBDIR:%=-L%)

OPT=1
ifeq ($(OPT),1)
OCAMLC=ocamlfind ocamlc -predicates native
else
OCAMLC=ocamlfind ocamlc
endif

all: $(TARGET)

.SUFFIXES: .ml .cmo
.PHONY: all depend clean distclean $(DEP)

$(LIBS): $(LIBS:%.cma=%)
$(DEP):
	$(MAKE) -C $@

.ml.cmo:
	$(OCAMLC) -c $@ $(CFLAGS) $<

$(TARGET): $(DEP) $(OBJS)
	$(OCAMLC) -o $(TARGET) $(LFLAGS) $(ALLLIBS) $(OBJS)

distclean clean:
	rm -rf $(TARGET)
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	rm -rf *.cm[aiox] *.a *.o *~

depend:
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	#ocamldep $(SRC) > .depend
	ocamldep $(SUBDIRS:%=-I %) $(SRC) > .depend
	#ocamldep $(SRC) > .depend

-include .depend
