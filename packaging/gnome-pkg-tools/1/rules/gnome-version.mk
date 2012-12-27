# makefile snippet for generating ${gnome:Version} and
# ${gnome:NextVersion} substitution variables

DEB_GNOME_VERSION := $(shell echo $(DEB_VERSION) | sed -r 's/^([0-9]+:)?([0-9]\.[0-9]+)\..*$$/\1\2/')
DEB_GNOME_NEXTVERSION := $(shell echo $(DEB_GNOME_VERSION) | awk -F. '{ printf("%s.%i\n",$$1,$$2+1) }')

$(patsubst %,binary-install/%,$(DEB_PACKAGES)) :: binary-install/%:
	echo gnome:Version=$(DEB_GNOME_VERSION) >> debian/$(cdbs_curpkg).substvars
	echo gnome:NextVersion=$(DEB_GNOME_NEXTVERSION) >> debian/$(cdbs_curpkg).substvars
