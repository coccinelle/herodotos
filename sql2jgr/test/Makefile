CONF=test.s2j
SUBCONF=count-per-dir.s2j    \
	rcu.s2j              \
	subdir.s2j           \
	rate-4-x86.s2j       \
	density.s2j          \
	cmp-rate-per-dir.s2j \
	across.s2j           \
	avg-ages.s2j         \
	annexes.s2j          \
	lifecycle.s2j
#	rate-per-dir.s2j     \
#	count-4-x86.s2j      \

SQL2JGR=sql2jgr.opt
#FLAGS?=-vvv --debug --profile
GRAPHS=$(shell ../$(SQL2JGR) -c $(CONF) --graphs 2> /dev/null | xargs)

.PHONY: all clean graphs $(GRAPHS)

all: $(CONF)
	../$(SQL2JGR) -c $^ --jgr $(FLAGS)

graphs: $(GRAPHS)

list:
	../$(SQL2JGR) -c $(CONF) --graphs

gr/db-orig.pdf: $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS) $(@:%.pdf=%.jgr)

$(GRAPHS): $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS) --jgr $@

$(CONF): $(CONF).base $(SUBCONF)
	 cpp -P -undef -w $(@:%=%.base) > $@

clean:
	rm -f *~
