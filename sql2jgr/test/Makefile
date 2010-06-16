CONF=test.s2j
SQL2JGR=sql2jgr.opt
#FLAGS?=-vvv --debug --profile

all: $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS)

gr/db-orig.pdf: $(CONF)
	../$(SQL2JGR) -c $^ $(FLAGS) $(@:%.pdf=%.jgr)

$(CONF): $(CONF).base
	 cpp -P -U linux $(@:%=%.base) > $@

clean:
	rm -f *~
