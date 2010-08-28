TARGET=herodotos
PREFIX?=/usr/local
SUBDIRS=herodotos org2sql sql2jgr

.PHONY:: all opt world depend clean distclean push

all opt world depend distclean clean install uninstall:
	for d in $(SUBDIRS); do    \
		$(MAKE) -C $$d $@; \
		$(MAKE) $@-l;      \
	done

##############################################################################
# Top level targets
##############################################################################

-include herodotos/Makefile.config

all-l opt-l world-l depend-l:

install-l: check-config
	mkdir -p $(DESTDIR)$(MANDIR)/man1
	$(INSTALL_DATA) docs/herodotos.1 $(DESTDIR)$(MANDIR)/man1/
	$(INSTALL_DATA) docs/org2sql.1 $(DESTDIR)$(MANDIR)/man1/
	$(INSTALL_DATA) docs/sql2jgr.1 $(DESTDIR)$(MANDIR)/man1/

uninstall-l: check-config
	rm -f $(DESTDIR)$(MANDIR)/man1/herodotos.1
	rm -f $(DESTDIR)$(MANDIR)/man1/org2sql.1
	rm -f $(DESTDIR)$(MANDIR)/man1/sql2jgr.1
	rmdir -p --ignore-fail-on-non-empty $(DESTDIR)$(MANDIR)/man1

distclean-l clean-l:
	rm -f *~

docs:
	$(MAKE) -C docs/manual pdf

check-config:
	@if [ ! -f herodotos/Makefile.config ] ; then \
		echo "\n\tRun ./configure first\n" \
		exit 1 ; fi

##############################################################################
# Package
#
#	Calling "pack" should be enough. Source package will be build
#	in .. directory
#
##############################################################################

-include /etc/lsb-release

VERSION=$(shell cat herodotos/commons/global.ml |grep "version\s*=" |perl -p -e 's/.*"(.*)".*/$$1/;')
PKGVERSION=$(shell dpkg-parsechangelog -ldebian/changelog.$(DISTRIB_CODENAME) \
	 | sed -n 's/^Version: \(.*\)/\1/p')
TMP=/tmp
PACKAGE=$(TARGET)-$(VERSION)

version:
	@echo "Herodotos version $(VERSION)"
	@echo "Herodotos $(DISTRIB_ID) version $(PKGVERSION)"

#
# Automatically called by prepack.
#
licensify:
	for i in $(SUBDIRS); do make -C $$i licensify ; done

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
	--exclude "TODO"      \
	--exclude "Makefile.dev"
EXCLUDE=--exclude "tools"           \
	--exclude $(PACKAGE)/debian

prepack:
	rsync -a $(EXCL_SYNC) . $(TMP)/$(PACKAGE)
	$(MAKE) -C $(TMP)/$(PACKAGE) distclean
	$(MAKE) -C $(TMP)/$(PACKAGE) licensify
	echo "-include Makefile.local" > $(TMP)/$(PACKAGE)/herodotos/demo/Makefile
	rm -rf $(TMP)/$(PACKAGE)/herodotos/tools
	rm -rf $(TMP)/$(PACKAGE)/tools

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
	$(MAKE) -C $(TMP)/$(PACKAGE) configure
	$(MAKE) -C $(TMP)/$(PACKAGE) depend
	$(MAKE) -C $(TMP)/$(PACKAGE) distclean
	$(MAKE) -C $(TMP)/$(PACKAGE) menhir
#	tar cjvf ../$(PACKAGE)-`date +%F`.tbz2 -C $(TMP) \
#		$(EXCL_SRC) --exclude-vcs $(PACKAGE)
	tar cjvf $(TMP)/$(PACKAGE).tbz2 -C $(TMP) \
		$(EXCLUDE) --exclude-vcs $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)/

-include Makefile.dev
