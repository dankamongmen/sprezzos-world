DOCS := users-and-groups.html users-and-groups.txt

all: $(DOCS)

clean:
	rm -f $(DOCS)

%.html: %.sgml
	sgmltools -b onehtml $<

%.txt: %.sgml
	sgmltools -b txt $<
