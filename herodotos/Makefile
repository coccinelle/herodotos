
-include Makefile.config

TARGET=herodotos

SOURCES =
SUBDIRS = commons exists hcl org diff scm graph html database engine
MENHIR_PATH=../menhirLib
DEPDIRS = $(MENHIR_PATH)

OCAMLLIBS=str.cma unix.cma
FLAG=

# ocamlmktop -custom -thread  -o threadedocaml unix.cma threads.cma -cclib -lthreads
# Automatic computation
INCLUDES=$(SUBDIRS:%=-I %)

OBJS = $(SOURCES:.ml=.cmo)
ROOTLIB = $(TARGET).cmo
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
OCAMLCFLAGS ?= -g -dtypes $(FLAGS)
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT = ocamlopt$(OPTBIN) $(FLAGS) $(INCLUDES)
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)

.PHONY:: all all.opt clean distclean debclean $(DEP)
.PHONY:: depend install sudo_install deploy
.PHONY:: test debian debians

all: init bytecode
opt: all.opt
all.opt: init native-noopt
world: init bytecode native

init: check-config depend scripts/Makefile.inc
bytecode: $(EXEC)
native: $(EXEC).opt
native-noopt: native
	mv -f $(EXEC).opt $(EXEC)

test:
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
engine: commons diff exists orgs
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
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

#
# To compile bytecode version
#

#$(ROOTLIB): $(OBJS)
#	$(OCAMLC) -a -o $@ $(OBJS)

$(EXEC): $(ALLLIBS) $(TARGET).cmo
	$(OCAMLC) -o $@ $(OCAMLLIBS) $(MENHIR) $^

$(LIBS): $(LIBS:%.cma=%)
$(DEP):
	$(MAKE) -C $@

#
# To compile native version
#

#$(OPTROOTLIB): $(OPTOBJS)
#	$(OCAMLOPT) -a -o $@ $(OPTOBJS)

$(EXEC).opt: $(OPTALLLIBS)  $(TARGET).cmx
	$(OCAMLOPT) -o $@ $(OPTOCAMLLIBS) $(MENHIRO) $^

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
	if [ -d test ] ; \
		then $(MAKE) -C test $@ ;fi
	rm -rf .depend Makefile.config
	find -name "*~" -delete
	find -name "\.#*" -delete

debclean:
	rm -rf build-stamp install-stamp my-very-own-patch-stamp

depend:
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	$(OCAMLDEP) *.mli *.ml > .depend

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
