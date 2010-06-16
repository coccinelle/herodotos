
-include Makefile.config

TARGET=org2sql
DEPLOY=palace bossa

SOURCES =
SUBDIRS = commons org database
MENHIR_PATH=$(shell ocamlfind query menhirLib)
DEPDIRS = $(MENHIR_PATH)

OCAMLLIBS=str.cma unix.cma
FLAGS=

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
.PHONY:: docs test debian ubuntu

all: init bytecode
opt: all.opt
all.opt: init native-noopt
world: init bytecode native

init: depend
bytecode: $(EXEC)
native: $(EXEC).opt
native-noopt: native
	mv -f $(EXEC).opt $(EXEC)

test:
	ln -sf test.org test_patt.orig.org 
	ln -sf test.org test_runall.orig.org 
	ln -sf test.org test_runall1.orig.org 
	ln -sf test.org test_patt.new.org 
	./$(TARGET) --notes test_patt.orig.org --prefix /var/linuxes/ $(TFLAGS)
	./$(TARGET) --notes test_runall.orig.org --prefix /var/linuxes/ $(TFLAGS)
	./$(TARGET) --notes test_runall1.orig.org --prefix /var/linuxes/ $(TFLAGS)
#	./$(TARGET) test_patt.new.org --prefix /var/linuxes/ $(TFLAGS)

ubuntu debian:
	$(MAKE) -C debian

ubuntus debians:
	$(MAKE) -C debian signed

#########################
## Dependencies        ##
#########################
orgs: $(MENHIR_PATH) commons diff
orgs.opt: $(MENHIR_PATH).opt commons.opt diff.opt
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

sudo_install: update
	@echo "Installing on $(shell uname -n)"
	sudo $(MAKE) install

install:
	mkdir -p $(DESTDIR)$(BINDIR)
	if [ -f $(TARGET) ]; then $(INSTALL_PROGRAM) $(TARGET) $(DESTDIR)$(BINDIR); fi
	if [ -f $(TARGET).opt ]; then $(INSTALL_PROGRAM) $(TARGET).opt $(DESTDIR)$(BINDIR); fi

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(TARGET) $(DESTDIR)$(BINDIR)/$(TARGET).opt

localclean:
	rm -f $(EXEC) $(EXEC).opt
	rm -f test_patt.new.org test_patt.orig.org
	rm -f *.cm[aiox] *.a *.o *.annot
	rm -f *~ .*~

clean: localclean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

distclean: localclean
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	rm -rf .depend Makefile.config
	find -name "*~" -delete
	find -name "\.#*" -delete

debclean:
	rm -rf build-stamp install-stamp my-very-own-patch-stamp

depend:
	for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend

Makefile.config:
	@echo "\n\tRun ./configure first\n"


##############################################################################
# Git Developer rules
#
#	git cvsimport -d :ext:topps:/var/cvs/cocci -C herodotos-git gengraph
#
##############################################################################
gitupdate:
	git cvsimport -p x -d :ext:topps:/var/cvs/cocci gengraph

update:
	cvs up && make world
##############################################################################
# Package
#
#	Calling "pack" should be enough. Source package will be build
#	in .. directory
#
##############################################################################

-include /etc/lsb-release

VERSION=$(shell cat commons/global.ml |grep "version\s*=" |perl -p -e 's/.*"(.*)".*/$$1/;')
PKGVERSION=$(shell dpkg-parsechangelog -ldebian/changelog.$(DISTRIB_CODENAME) \
	 | sed -n 's/^Version: \(.*\)/\1/p')
TMP=/tmp
PACKAGE=$(TARGET)-$(VERSION)

version:
	@echo "Org2SQL version $(VERSION)"
	@echo "Org2SQL $(DISTRIB_ID) version $(PKGVERSION)"

TOLICENSIFY=$(SUBDIRS) #test

#
# Automatically called by pack.
#
licensify:
	ocaml str.cma licensify.ml
	set -e; for i in $(TOLICENSIFY); do cd $$i; ocaml str.cma ../licensify.ml; cd -; done

#
# Pre-generate parsers with menhir
# Note: This allows to remove the menhir dependency.
#
# This target is called by pack
#
menhir:
	for i in `find -name "*.mly"` ; do \
		make -C `dirname $$i` `basename $$i` ; \
	done

#
# This target build a source tarball to distribute
#
EXCL_SYNC=--exclude ".git"          \
	--exclude ".gitignore"      \
	--exclude "test"      \
	--exclude "TODO"
EXCLUDE=--exclude $(PACKAGE)/debian

prepack:
	rsync -a $(EXCL_SYNC) . $(TMP)/$(PACKAGE)
	cp ../tools/licensify.ml $(TMP)/$(PACKAGE)
	$(MAKE) -C $(TMP)/$(PACKAGE) licensify
	rm -rf $(TMP)/$(PACKAGE)/licensify.ml

packsrc: prepack
	$(MAKE) -C $(TMP)/$(PACKAGE)/debian lucid
	$(MAKE) -C $(TMP)/$(PACKAGE)/debian karmic
	$(MAKE) push
	rm -rf  $(TMP)/$(PACKAGE)/

push:
	cd $(TMP)/ && for p in `ls $(TARGET)_$(VERSION)~*_source.changes`; do dput $(TARGET) $$p ; done
	rm -rf $(TMP)/$(TARGET)_$(VERSION)~*_source.changes
	rm -rf $(TMP)/$(TARGET)_$(VERSION)~*_source.$(TARGET).upload
	rm -rf $(TMP)/$(TARGET)_$(VERSION)~*.dsc
	rm -rf $(TMP)/$(TARGET)_$(VERSION)~*.tar.gz

packbin: prepack
	$(MAKE) -C $(TMP)/$(PACKAGE)/debian binary
	rm -rf  $(TMP)/$(PACKAGE)/
	rm -rf $(TMP)/$(TARGET)_$(VERSION)~*_source.build

pack: prepack
	cd $(TMP)/$(PACKAGE) && ./configure
	$(MAKE) -C $(TMP)/$(PACKAGE) depend
	$(MAKE) -C $(TMP)/$(PACKAGE) distclean
	$(MAKE) -C $(TMP)/$(PACKAGE) menhir
#	tar cjvf ../$(PACKAGE)-`date +%F`.tbz2 -C $(TMP) \
#		$(EXCL_SRC) --exclude-vcs $(PACKAGE)
	tar cjvf $(TMP)/$(PACKAGE).tbz2 -C $(TMP) \
		$(EXCLUDE) --exclude-vcs $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)/
