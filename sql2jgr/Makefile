
-include Makefile.config

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

NOBJS=$(OBJS:%.cmo=%.cmx)
NLIBS=$(LIBS:%.cma=%.cmxa)
ALLNLIBS=$(ALLLIBS:%.cma=%.cmxa)

EXEC = $(TARGET)
DEP = $(SUBDIRS)

INCLUDES=$(SUBDIRS:%=-I %) $(LIBDIR:%=-I %)
FLAGS=$(INCLUDES)
CFLAGS=-g -thread $(FLAGS)
LFLAGS=-g -thread $(FLAGS) -ccopt $(LIBDIR:%=-L%)
BLFLAGS=-custom

ifeq ("$(OPTBIN)",".opt")
OCAMLC=$(OCAMLFIND) ocamlc -predicates native
OCAMLOPT=$(OCAMLFIND) ocamlopt -predicates native
else
OCAMLC=$(OCAMLFIND) ocamlc
OCAMLOPT=$(OCAMLFIND) ocamlopt
endif

.PHONY:: all all.opt clean distclean $(DEP)
.PHONY:: depend install uninstall
.PHONY:: test

all: init bytecode
opt: all.opt
all.opt: init native-noopt
world: init bytecode native

init: depend
bytecode: $(EXEC)
native: $(EXEC).opt
native-noopt: native
	mv -f $(EXEC).opt $(EXEC)

#
# To compile bytecode version
#

$(LIBS): $(LIBS:%.cma=%)
$(DEP):
	$(MAKE) -C $@

$(EXEC): $(LIBS) $(OBJS)
	$(OCAMLC) -o $@ $(BLFLAGS) $(LFLAGS) $(ALLLIBS) $(OBJS)

#
# To compile native version
#

$(NLIBS): $(NLIBS:%.cmxa=%.opt)
$(DEP:%=%.opt):
	$(MAKE) -C $(@:%.opt=%) all.opt

$(EXEC).opt: $(NLIBS) $(NOBJS)
	$(OCAMLOPT) -o $@ $(LFLAGS) $(ALLNLIBS) $(NOBJS)

test:
	$(MAKE) -C test

distclean clean:
	rm -rf $(TARGET)
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	rm -rf *.cm[aiox] *.a *.o *~
	rm -rf .depend Makefile.config

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmo:
	$(OCAMLC) -c $@ $(CFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $@ $(CFLAGS) $<

depend:
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	#ocamldep $(SRC) > .depend
	ocamldep $(SUBDIRS:%=-I %) $(SRC) > .depend
	#ocamldep $(SRC) > .depend

-include .depend

Makefile.config:
	@echo "\n\tRun ./configure first\n"

install:
	if [ -f $(EXEC) ]; then $(INSTALL_PROGRAM) $(EXEC) $(DESTDIR)$(BINDIR); fi
	if [ -f $(EXEC).opt ]; then $(INSTALL_PROGRAM) $(EXEC).opt $(DESTDIR)$(BINDIR); fi

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(EXEC)
	rm -f $(DESTDIR)$(BINDIR)/$(EXEC).opt

-include Makefile.dev
