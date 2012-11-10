XML_FILES = $(wildcard *.xml)
MAN_FILES = $(XML_FILES:.xml=)

XSL = http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl
XSL_PARAMS = --param man.charmap.use.subset 0

.PHONY: all
all: $(MAN_FILES)

%: %.xml
	xmllint --valid --noout $(<)
	xsltproc $(XSL_PARAMS) $(XSL) $(<)

.PHONY: clean
clean:
	rm $(MAN_FILES)

# vim:ts=4 sw=4 noet
