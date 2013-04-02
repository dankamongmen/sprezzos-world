all: gwenhywfar-config.1 gct-tool.1 xmlmerge.1

gwenhywfar-config.1.xml: gwenhywfar-config.1.txt gwenhywfar-config.1.generated.txt
	asciidoc -d manpage -b docbook gwenhywfar-config.1.txt

gwenhywfar-config.1.generated.txt: ../../gwenhywfar-config
	../../gwenhywfar-config --help \
		| sed -e 's/^Usage:.*//;s/^.*gwenhywfar-config --\(\w\+\)/--\1::\n/;' \
		> gwenhywfar-config.1.generated.txt

gct-tool.1.xml: gct-tool.1.txt gct-tool.1.generated.txt
	asciidoc -d manpage -b docbook gct-tool.1.txt

gct-tool.1.generated.txt: ../../tools/gcttool/gct-tool
	LANG=C ../../tools/gcttool/gct-tool --help 2>&1 \
		| sed -e 's/^Usage:.*//;s/^Global Options:.*//;s/:$$/::/g;s/^\(\S\+.*\)::$$/== \1/g;s/^  \(\S\+\)::/\1::/g;s/^ \[\(.*\)\]/\1::/g' \
		> gct-tool.1.generated.txt

%.xml:	%.txt
	asciidoc -d manpage -b docbook $*.txt

%: %.xml
	xmlto man $*.xml

clean:
	rm -f *.generated.txt *.xml *.1
