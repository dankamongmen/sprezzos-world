
SCRIPT = $(shell find src -type f -name '*.scm' | sed 's,^src/,,')
SCRIPT_SRCDIR = src

include ../common-script.mk

update-from-svn:
	rm -rf src
	svn export https://gimpfx-foundry.svn.sourceforge.net/svnroot/gimpfx-foundry/gimp-2.6/stable/ src
	rm -f version
	svn info  https://gimpfx-foundry.svn.sourceforge.net/svnroot/gimpfx-foundry/gimp-2.6/stable/ | grep '^Revision' | awk '{print "r"$$2}' > version

copyright: copyright.in
	rm -f $@
	cp copyright.in $@
	grep -Ei '^;.*copyright' src/*.scm | sed 's,:;,:,;s,^src/,,;s,^,    ,' >> $@

.PHONY: update-from-svn
