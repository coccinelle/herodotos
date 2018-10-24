
-include Makefile.config

TARGET=herodotos

SOURCES =
SUBDIRS = commons exists hcl org diff scm graph html database engine
MENHIR_PATH=../menhirLib
DEPDIRS = $(MENHIR_PATH)
EXTLIBS=postgresql parmap bolt # xml-light
P4=bolt

OCAMLFIND=ocamlfind

OCAMLLIBS=str.cma unix.cma threads.cma bigarray.cma dynlink.cma $(EXTLIBS:%=%.cma)
FLAG=

# ocamlmktop -custom -thread  -o threadedocaml unix.cma threads.cma -cclib -lthreads
# Automatic computation
OCAMLFIND=ocamlfind

LIBDIR=$(shell $(OCAMLFIND) query $(EXTLIBS))
P4DIR=$(shell $(OCAMLFIND) query $(P4))
P4FLAG=-pp 'camlp4o $(P4DIR)/bolt_pp.cmo'
DEPDIRS = $(MENHIR_PATH)
INCLUDES=$(SUBDIRS:%=-I %) $(LIBDIR:%=-I %)

OBJS = $(SOURCES:.ml=.cmo)
ROOTLIB = $(TARGET).cmo
FLAGS= $(INCLUDES) $(FLAG) $(P4FLAG)
LIBS = $(SUBDIRS:%=%.cma)
ALLLIBS = $(LIBS) $(ROOTLIB)

OPTOBJS = $(OBJS:.cmo=.cmx)
OPTROOTLIB = $(ROOTLIB:.cmo=.cmx)
OPTLIBS = $(LIBS:.cma=.cmxa)
OPTALLLIBS = $(OPTLIBS) $(OPTROOTLIB)
OPTOCAMLLIBS=$(OCAMLLIBS:.cma=.cmxa)

EXEC = $(TARGET)
DEP = $(SUBDIRS) $(DEPDIRS)

MENHIR=$(MENHIR_PATH)/menhirLib.cmo
MENHIRO=$(MENHIR_PATH)/menhirLib.cmx

# The Caml compilers.
CFLAGS ?= -g -dtypes -thread $(FLAGS)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES) $(P4FLAG)
ifeq ("$(OPTBIN)",".opt")
OCAMLC=$(OCAMLFIND) ocamlc -predicates native
OCAMLOPT=$(OCAMLFIND) ocamlopt -predicates native
else
OCAMLC=$(OCAMLFIND) ocamlc
OCAMLOPT=$(OCAMLFIND) ocamlopt
endif

.PHONY:: all all.opt clean distclean debclean $(DEP)
.PHONY:: .depend depend install sudo_install deploy
.PHONY:: test debian debians

all: init bytecode
opt: all.opt
all.opt: init native-noopt
world: init bytecode native

init: check-config .depend scripts/Makefile.inc
bytecode: $(EXEC)
native: $(EXEC).opt
native-noopt: native
	mv -f $(EXEC).opt $(EXEC)

test/Makefile:
	@echo "Generating basic Makefile"
	@echo "BOLT_CONFIG=debug.config" > $@
	@echo "-include $(DESTDIR)$(SHAREDIR)/Makefile.inc" >> $@

test/debug.config:
	@echo "Generating basic debug/trace config for Bolt"
	cp scripts/debug.config $@

test: test/Makefile test/debug.config
	$(MAKE) -C test preinit
	cd test ; ./fooldir.sh
	$(MAKE) -C test init
	tar xjvf test/orig-org.tbz2 -C test
	tar xjvf test/correl.tbz2 -C test
	tar xjvf test/fp-checked.tbz2 -C test
	tar xjvf test/patchset.tbz2 -C test
	$(MAKE) -C test correl
	tar xjvf test/exist.tbz2 -C test
	$(MAKE) -C test web

view:
	if [ "$(PDFVIEWER)" = "okular" ]; then \
		find test/gr -name "*.pdf" | xargs $(PDFVIEWER) ; \
	else \
	 	for f in `find test/gr -name "*.pdf"`; do \
			$(PDFVIEWER) $$f & \
		done \
	fi

#########################
## Dependencies        ##
#########################
exists: $(MENHIR_PATH) commons
bugs: $(MENHIR_PATH) commons
orgs: $(MENHIR_PATH) commons diff
diff: commons
hcl: commons
engine: commons diff exists orgs graph database scm
#########################
exists.opt: $(MENHIR_PATH).opt commons.opt
bugs.opt: $(MENHIR_PATH).opt commons.opt
orgs.opt: $(MENHIR_PATH).opt commons.opt diff.opt
diff.opt: commons.opt
hcl.opt: commons.opt
engine.opt: commons.opt diff.opt exists.opt orgs.opt
#########################

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma

.ml.cmo:
	$(OCAMLC) -c $@ $(CFLAGS) $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $@ $(CFLAGS) $<

#
# To compile bytecode version
#

#$(ROOTLIB): $(OBJS)
#	$(OCAMLC) -a -o $@ $(OBJS)

$(EXEC): $(ALLLIBS) $(TARGET).cmo
	$(OCAMLC) $(CFLAGS) -o $@ $(OCAMLLIBS) $(MENHIR) $^

$(LIBS): $(LIBS:%.cma=%)
$(DEP):
	$(MAKE) -C $@

#
# To compile native version
#

#$(OPTROOTLIB): $(OPTOBJS)
#	$(OCAMLOPT) -a -o $@ $(OPTOBJS)

$(EXEC).opt: $(OPTALLLIBS)  $(TARGET).cmx
	$(OCAMLOPT) $(CFLAGS) -o $@ $(OPTOCAMLLIBS) $(MENHIRO) $^

$(OPTLIBS): $(LIBS:%.cma=%.opt)
$(DEP:%=%.opt):
	$(MAKE) -C $(@:%.opt=%) all.opt

##############################################################################
# Install
##############################################################################

# don't remove DESTDIR, it can be set by package build system like ebuild
# for staged installation.

scripts/Makefile.inc: scripts/Makefile.inc.in Makefile.config
	sed "s|SHAREDIR|$(SHAREDIR)|" $@.in > $@

install:
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) scripts/Makefile.inc $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) scripts/Makefile.images $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DATA) scripts/Makefile.prj $(DESTDIR)$(SHAREDIR)
	if [ -f $(TARGET) ]; then $(INSTALL_PROGRAM) $(TARGET) $(DESTDIR)$(BINDIR); fi
	if [ -f $(TARGET).opt ]; then $(INSTALL_PROGRAM) $(TARGET).opt $(DESTDIR)$(BINDIR); fi

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(TARGET) $(DESTDIR)$(BINDIR)/$(TARGET).opt
	rm -f $(DESTDIR)$(SHAREDIR)/Makefile.*
	rmdir -p --ignore-fail-on-non-empty $(DESTDIR)$(SHAREDIR)

localclean:
	rm -f scripts/Makefile.inc
	rm -f $(EXEC) $(EXEC).opt
	rm -f *.cm[aiox] *.a *.o *.annot
	rm -f *~ .*~

clean: localclean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	$(MAKE) -C $(MENHIR_PATH) $@
	if [ -f test/Makefile ] ; then $(MAKE) -C test $@ ;fi

distclean: localclean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	-$(MAKE) -C $(MENHIR_PATH) $@
	$(MAKE) -C demo $@
	$(MAKE) $@-test
	rm -rf .depend Makefile.config
	find -name "*~" -delete
	find -name "\.#*" -delete

distclean-test:
	for f in `cat test/.gitignore`; do rm -rf test/$$f; done
	rm -rf test/*.patchset

debclean:
	rm -rf build-stamp install-stamp my-very-own-patch-stamp

depend: .depend
.depend:
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	$(OCAMLDEP) *.mli *.ml > $@

-include .depend

check-config:
	@if [ ! -f Makefile.config ] ; then \
		echo "\n\tRun ./configure first\n" \
		exit 1 ; fi

TOLICENSIFY=$(SUBDIRS) scripts #test

#
# Automatically called by pack.
#
licensify:
	ocaml str.cma ../tools/licensify.ml
	set -e; for i in $(TOLICENSIFY); do cd $$i; \
		ocaml str.cma ../../tools/licensify.ml; cd -; done

ocamlbuild-byte:
	ocamlbuild -use-ocamlfind -cflags -thread -lflags -thread herodotos.byte

ocamlbuild-native:
	ocamlbuild -use-ocamlfind -cflags -thread -lflags -thread herodotos.native
