SUBDIRS=herodotos sql2jgr

.PHONY: all world clean distclean push

all world distclean clean:
	for d in $(SUBDIRS); do \
		$(MAKE) -C $$d $@; \
	done

push:
	git push
	git push forge
