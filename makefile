HAPPY=$(shell which happy)

# better flags to optimize
# -c -fglasgow-exts
HAPPYFLAGS=-a -i -g

PARSERS = src/Language/Javascript/ES1/Parser.hs

OBJS= $(PARSERS)

src/Language/Javascript/ES1/Parser.hs: parsers/javascript_es1.y
	$(HAPPY) $(HAPPYFLAGS) $< -o $@

parsers: $(PARSERS)

tests:
	stack test

.PHONY: all
all: parsers tests

clean:
	rm -rf $(OBJS)
