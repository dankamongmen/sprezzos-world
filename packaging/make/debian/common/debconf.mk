############################ -*- Mode: Makefile -*- ###########################
## debconf.mk --- 
## Author           : Manoj Srivastava ( srivasta@glaurung.internal.golden-gryphon.com ) 
## Created On       : Fri Mar 12 11:11:31 2004
## Created On Node  : glaurung.internal.golden-gryphon.com
## Last Modified By : Manoj Srivastava
## Last Modified On : Mon Apr 11 13:19:10 2005
## Last Machine Used: glaurung.internal.golden-gryphon.com
## Update Count     : 20
## Status           : Unknown, Use with caution!
## HISTORY          : 
## Description      : helps with using debconf
## 
## arch-tag: 32b933a9-05ad-4c03-97a8-8644745b832a
##
###############################################################################

# The idea behind this scheme is that the maintainer (or whoever's
# building the package for upload to unstable) has to build on a
# machine with po-debconf installed, but nobody else does.

# Also, make sure that debian/control has ${debconf-depends} in the
# appropriate Depends: line., and use the following in the binary
# target:
#  dpkg-gencontrol -V'debconf-depends=debconf (>= $(MINDEBCONFVER))'
#

# WARNING!! You need to create the debian/templates file before this
# all works.

# Run debconf-updatepo whenever the template file changes.
# the tool podebconf-report-po is also a great friend to have in such
# circumstances 
define CHECKPO
	@for i in debian/po/*.po; do                         \
	  if [ -f $$i ]; then                        \
	    echo \"Checking: $$i\";                  \
	    msgmerge -U $$i debian/po/templates.pot;        \
	    msgfmt -o /dev/null -c --statistics $$i; \
	  fi;                                        \
	done
endef
