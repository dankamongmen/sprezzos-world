SUBDIRS = images
EXTRA_DIST = aptitude.xml

LC = cs

include $(top_srcdir)/buildlib/doc.mk

README = README.cs
README_encoding = utf-8

IMAGES := $(wildcard $(srcdir)/images/*.png)

DOCBOOK_XML = aptitude.xml
DOCBOOK_TARGETS = docbook-man docbook-html docbook-readme

include $(top_srcdir)/buildlib/docbook.mk

all-local: docbook-stamp

man_MANS = aptitude.8
#notrans_man_MANS = aptitude-create-state-bundle.1 aptitude-run-state-bundle.1
