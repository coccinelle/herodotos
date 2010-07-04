CONF=test.s2j
SUBCONF=count-per-dir.s2j    \
	rate-per-dir.s2j     \
	subdir.s2j           \
	rate-4-x86.s2j       \
	density.s2j          \
	cmp-rate-per-dir.s2j

SQL2JGR=sql2jgr.opt
#FLAGS?=-vvv --debug --profile
GRAPHS=$(shell ../$(SQL2JGR) -c $(CONF) --graphs | xargs)

.PHONY: all clean graphs $(GRAPHS)

all: $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS)

graphs: $(GRAPHS)

gr/db-orig.pdf: $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS) $(@:%.pdf=%.jgr)

$(GRAPHS): $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS) $@

$(CONF): $(CONF).base $(SUBCONF)
	 cpp -P -undef -w $(@:%=%.base) > $@

clean:
	rm -f *~
