CONF=test.s2j
SQL2JGR=sql2jgr.opt
#FLAGS?=-vvv --debug --profile

all: $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS)

gr/db-orig.pdf: $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS) $(@:%.pdf=%.jgr)

$(CONF): $(CONF).base count-per-dir.s2j rate-per-dir.s2j subdir.s2j rate-4-x86.s2j density.s2j
	 cpp -P -undef -w $(@:%=%.base) > $@

clean:
	rm -f *~

