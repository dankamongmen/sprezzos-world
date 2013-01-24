SUBDIRS = 
EXTRA_DIST = fixman

LC = pl

PO4A_TRANSLATED = aptitude.xml manpage.xml

include $(top_srcdir)/buildlib/doc.mk
include $(top_srcdir)/buildlib/po4a.mk

DOCBOOK_MANS = aptitude.8
DOCBOOK_TARGETS = docbook-man

include $(top_srcdir)/buildlib/docbook.mk

all-local: docbook-stamp

man_MANS = aptitude.8
#notrans_man_MANS = aptitude-create-state-bundle.1 aptitude-run-state-bundle.1
