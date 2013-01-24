SUBDIRS = images
EXTRA_DIST = fixman

LC = fr

include $(top_srcdir)/buildlib/doc.mk
include $(top_srcdir)/buildlib/po4a.mk

README = README.fr
README_encoding = utf-8

IMAGES := $(wildcard $(srcdir)/images/*.png) $(PO4A_IMAGES)

DOCBOOK_TARGETS = docbook-html docbook-man docbook-readme
DOCBOOK_MANS = aptitude.8

include $(top_srcdir)/buildlib/docbook.mk

all-local: docbook-stamp

man_MANS = aptitude.8
#notrans_man_MANS = aptitude-create-state-bundle.1 aptitude-run-state-bundle.1
