
PAGES=$(patsubst %.sgml,%.1,$(wildcard *.sgml))

all: $(PAGES)

%.1: %.sgml
	docbook2man -w all $<

clean:
	rm -rf $(PAGES) manpage.links manpage.refs

.PHONY: all clean
