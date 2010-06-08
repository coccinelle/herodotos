CONF=test.s2j
#FLAGS?=-vvv --debug --profile
FLAGS?=-v
#FLAGS=

all: $(CONF)
	../sql2jgr -c $^ $(FLAGS)

gr/db-orig.pdf: $(CONF)
	../sql2jgr -c $^ $(FLAGS) $(@:%.pdf=%.jgr)

$(CONF): $(CONF).base
	 cpp -P -U linux $(@:%=%.base) > $@

clean:
	rm -f *~
