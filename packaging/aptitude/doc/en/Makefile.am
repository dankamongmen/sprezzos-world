SUBDIRS = images
EXTRA_DIST = aptitude.xml manpage.xml

LC = en

include $(top_srcdir)/buildlib/doc.mk

README = README
README_encoding = iso-8859-1

IMAGES := $(wildcard $(srcdir)/images/*.png) images/safety-cost-level-diagram.svg

DOCBOOK_TARGETS = docbook-man docbook-html docbook-readme

include $(top_srcdir)/buildlib/docbook.mk

all-local: docbook-stamp

man_MANS = aptitude.8
notrans_man_MANS = aptitude-create-state-bundle.1 aptitude-run-state-bundle.1
