all: aqhbci-tool4.1 hbcixml3.1 aqbanking-config.1

aqhbci-tool4.1.xml: aqhbci-tool4.1.txt aqhbci-tool4.1.generated.txt
	asciidoc -d manpage -b docbook aqhbci-tool4.1.txt

hbcixml3.1.xml: hbcixml3.1.txt hbcixml3.1.generated.txt
	asciidoc -d manpage -b docbook hbcixml3.1.txt

aqbanking-config.1.xml: aqbanking-config.1.txt aqbanking-config.1.generated.txt
	asciidoc -d manpage -b docbook aqbanking-config.1.txt

hbcixml3.1.generated.txt: ../../src/plugins/backends/aqhbci/tools/hbcixml/hbcixml3
	../../src/plugins/backends/aqhbci/tools/hbcixml/hbcixml3 --help 2>&1 \
		| sed -e '1,/^ General Options:/d;s/^ -\([a-z-][^-]*\)\s\+-/-\1::\n    /;s/\s*::/::/;s/^ \(Special Options.*\):/=== \1/;/^\s*RDH/s/RDH/* RDH/;/^\s*DDV/s/DDV/* DDV/;/^\(The simplest usage.*\)/,$$d' \
		> hbcixml3.1.generated.txt

aqhbci-tool4.1.generated.txt: ../../src/plugins/backends/aqhbci/tools/aqhbci-tool/aqhbci-tool4
	../../src/plugins/backends/aqhbci/tools/aqhbci-tool/aqhbci-tool4 --help 2>&1 \
		| sed -e 's/^Usage:.*//;s/^Global Options:.*//;s/:$$/::/g;s/^\(\S\+.*\)::$$/== \1/g;s/^  \(\S\+\)::/\1::/g;s/^ \[\(.*\)\]/\1::/g' \
		> aqhbci-tool4.1.generated.txt

aqbanking-config.1.generated.txt: ../../aqbanking-config
	../../aqbanking-config --help \
		| sed -e 's/^Usage:.*//;s/^.*aqbanking-config --\(\w\+\)/--\1::\n/;' \
		> aqbanking-config.1.generated.txt

%.xml:	%.txt
	asciidoc -d manpage -b docbook $*.txt

%: %.xml
	xmlto man $*.xml

clean:
	rm -f *.generated.txt *.xml *.1
