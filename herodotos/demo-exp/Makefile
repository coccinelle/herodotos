CONF=demo.hc
HERODOTOS=../herodotos
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

setup: $(CONF)

all:: ./results/test/Test_Notes2.new.org

./results/test/Test_Notes2.new.org:
	echo "ver0;8;\nver1;10;\nver2;8;" > $@

$(CONF): $(CONF).base
	sed "s|projects=\".*\"$$|projects=\"$(PWD)\"|" demo.hc.base > demo.hc
	@echo "Run 'make init' now"

fix-orig fix-correl fix-new:
	sed -i "s|\./test|`pwd`/test|g" results/test/error.$(@:fix-%=%).org

unfix-orig unfix-correl unfix-new:
	sed -i "s|`pwd`/test|./test|g" results/test/error.$(@:unfix-%=%).org

distclean:
	rm -rf results gr website .depend demo.hc

-include ../scripts/Makefile.inc
