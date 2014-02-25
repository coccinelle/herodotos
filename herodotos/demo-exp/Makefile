CONF=demo.hc
HERODOTOS=../herodotos


-include Makefile.local

ifeq ($(D),1)
FLAGS=--debug -v
else ifeq ($(D),2)
FLAGS=--debug -vv
else ifeq ($(D),3)
FLAGS=--debug -vvv
else
FLAGS?=
endif

.PHONY:: distclean

PWD=$(shell pwd)
HOST=$(shell uname -n | cut -f1 -d"." | tr '-' '_')

setup: $(CONF)

$(CONF): $(CONF).base
	cpp -P -undef -D$(HOST) $(@:%=%.base) > $@
	sed -i -e "s|projects=\"\(.*\)\"$$|projects=\"$(PWD)/\1\"|" $@
	@echo "Run 'make init' now"

fix-orig fix-correl fix-new:
	sed -i "s|\./test|`pwd`/test|g" results/test/error.$(@:fix-%=%).org

unfix-orig unfix-correl unfix-new:
	sed -i "s|`pwd`/test|./test|g" results/test/error.$(@:unfix-%=%).org

distclean:
	rm -rf results gr website .depend demo.hc

diff:
	diff -u tests/$(PRJ)/ver0/test.c tests/$(PRJ)/ver1/test.c

gumtree:
	gumtree --output xml tests/$(PRJ)/ver0/test.c tests/$(PRJ)/ver1/test.c

-include ../scripts/Makefile.inc
