# -*- makefile -*-

# Notes:
# 1. you must have package libxml2-python2.3 installed
# 2. your document must have a titleabbrev in the refentryinfo
# 3. your document must have a refmiscinfo with role=infodirsection

XTR=python -c 'import sys, libxml2; \
print libxml2.parseFile(sys.argv[1]).xpathEval(sys.argv[2])[0].content'

all: $(patsubst %.dbk,%.info,$(wildcard *.dbk))

%.info: %.dbk
	docbook2x-texi --info --to-stdout $< | \
	  awk -v base=`basename $< .dbk` \
	    -v sect="`${XTR} $< '//refmiscinfo[@role="infodirsection"]'`" \
	    -v title="`${XTR} $< '/refentry/refentryinfo/titleabbrev'`" \
	    'BEGIN { p=1 } \
	    /^START-INFO-DIR-ENTRY/ { print \
	        "INFO-DIR-SECTION " sect "\n" $$0;  p=0 } \
	    /^END-INFO-DIR-ENTRY/ { p=1 } \
	    /^\* / { if (p==0) { gsub(/.*\./, ""); \
	        print "* " title ": (" base ")." $$0 } } \
	    /^File: stdout,/ { if (p==1) { \
	        gsub(/File: stdout/, "File: " base ".info"); p=2 } } \
	    { if (p) { print } }' > $@

clean::
	rm -f *.info *~
