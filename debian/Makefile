include /etc/lsb-release

all:
	debuild -S

karmic lucid:
	ln -f changelog.$@ changelog
	$(MAKE)
	rm -f changelog

binary:
	ln -f changelog.$(DISTRIB_CODENAME) changelog
	debuild -uc -us -d
	rm -f changelog
