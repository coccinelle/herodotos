SUBDIRS=herodotos org2sql sql2jgr

.PHONY: all world clean distclean push

all world distclean clean install uninstall:
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d $@; \
	done

configure:
	for d in $(SUBDIRS); do \
		cd $$d ; ./$@; cd - 2> /dev/null;\
	done

push:
	git push
	git push forge
