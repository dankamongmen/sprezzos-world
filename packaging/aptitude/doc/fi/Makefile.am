SUBDIRS = images
EXTRA_DIST = aptitude.xml manpage.xml fixman

LC = fi

include $(top_srcdir)/buildlib/doc.mk

README = README.fi
README_encoding = utf-8

IMAGES := $(wildcard $(srcdir)/images/*.png)

DOCBOOK_TARGETS = docbook-man docbook-html docbook-readme

DOCBOOK_MANS = aptitude.8

include $(top_srcdir)/buildlib/docbook.mk

all-local: docbook-stamp

man_MANS = aptitude.8
