# vim: ft=make:ts=4

# The Makefile is included from languages/{american,british,english}/Makefile
# and adds two special values for LANGUAGES setting into English languages:
# 
#  + `debian-none' for HASHFILES, what causes none *.hash file to be build
#     or installed; only the generated munched word lists are installed instead
#     into $(DESTDIR)/usr/share/ispell by the `install' target;
# 
#  + `debian-auto' for EXTRADICT, what causes appropriate extra dictionary file
#    (like american-small or american-huge, etc) from /usr/share/dict to be choosen
#    when the munched list are generated.
# 
# Also it overwrites install target for Debian

ISHAREDIR    := $(DESTDIR)/usr/share/ispell

ifeq (debian-none,$(HASHFILES))

override INSTALL_TARGET := install-orig

install: override HASHFILES :=

install: install-orig
	@. $(CONFIG); \
	set -ex; \
	for dict in $(MASTERDICTS); do \
	    [ -d $(ISHAREDIR) ] || mkdir -p -m 0755 $(ISHAREDIR) ; \
 		rm -f $(ISHAREDIR)/$$dict.mwl $(ISHAREDIR)/$$dict.mwl.gz; \
		cp -a $$dict $(ISHAREDIR)/$$dict.mwl ; \
		gzip -9  $(ISHAREDIR)/$$dict.mwl     ; \
	done

endif



ifeq (debian-auto,$(EXTRADICT))
ifneq (,$(VARIANTS))

EXTRDBASE    := /usr/share/dict/$(VARIANTS)-english

%.med+: $(EXTRDBASE)        
%.med+: override EXTRADICT := $(EXTRDBASE)

%.lrg+: $(EXTRDBASE)-large
%.lrg+: override EXTRADICT := $(EXTRDBASE)-large

%.xlg+: $(EXTRDBASE)-huge
%.xlg+: override EXTRADICT := $(EXTRDBASE)-huge

%.sml+: $(EXTRDBASE)-small 
%.sml+: override EXTRADICT := $(EXTRDBASE)-small

endif
endif

.PHONY: debian-auto debian-none all install install-orig

# Pass -T utf8 to munchlist to avoid `illegal character' warnings given
# on UTF8-encoded files from /usr/share/dict
override MUNCHLIST := $(MUNCHLIST) -T utf8

