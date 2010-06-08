CONF=test.s2j
#FLAGS?=-vvv --debug --profile
#FLAGS?=-v
FLAGS=

all: $(CONF)
	../sql2jgr -c $^ $(FLAGS)

$(CONF): $(CONF).base
	 cpp -P $(@:%=%.base) | sed "s|/1 |/linux|" > $@

clean:
	rm -f *~
