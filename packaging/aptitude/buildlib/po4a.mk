# Makefile rules for targets from po4a translations
#

PO4A_TRANSLATED ?= $(shell cat $(top_srcdir)/doc/po4a/TRANSLATED)
PO4A_IMAGES := $(filter images/%,$(PO4A_TRANSLATED))

PO4AFLAGS = --srcdir=$(top_srcdir)/doc --destdir=$(top_builddir)/doc \
	$(top_srcdir)/doc/po4a/po4a.cfg

clean-local: clean-po4a

clean-po4a:
	-rm -fr $(PO4A_TRANSLATED) $(PO4A_IMAGES)
	-rm -fr $(addprefix "$(srcdir)/",$(PO4A_IMAGES))

vpath %.po $(top_srcdir)/doc/po4a/po

if HAVE_PO4A

$(PO4A_TRANSLATED): $(LC).po $(top_srcdir)/doc/en/$@
	$(PO4A) --translate-only=$(LC)/$@ $(PO4AFLAGS)

endif
