## Process this file with automake to produce Makefile.in
# Copyright © 2003-2005 Scott James Remnant <scott@netsplit.com>
# Copyright © 2002-2003 Colin Walters <walters@debian.org>

pkgdata_DATA = \
	list \
	essential-packages-list

EXTRA_DIST = \
	list
	list2depends \
	make-esslist.sh \
	essential-packages-list-*

essential-packages-list: essential-packages-list-@DEB_HOST_ARCH@
	ln -s $< $@

essential-packages-list-@DEB_HOST_ARCH@:
	echo "No essential packages list is available" > $@

clean-local:
	rm -f essential-packages-list
