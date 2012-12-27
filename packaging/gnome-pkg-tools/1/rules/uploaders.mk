# =========================================================================
# Debian GNOME Packaging Tools
# GNU Make class for auto-updating the Uploaders field
#
# $Id: uploaders.mk 22417 2009-11-21 10:51:59Z joss $
# =========================================================================

# Members list of Debian GNOME Maintainers
TEAM_LIST ?= /usr/share/gnome-pkg-tools/pkg-gnome.team
# Uploaders which should always be listed in UPLOADERS; the Maintainer is still
# excluded
ALWAYS_UPLOADS ?= Debian GNOME Maintainers <pkg-gnome-maintainers@lists.alioth.debian.org>
# Shell command to get the maintainer from the control file
GET_MAINTAINER_CMD ?= sed -n 's/^Maintainer: //p' debian/control.in
# Number of uploads to be considered recent for the list of recent uploaders
RECENT_UPLOADS ?= 10
# Shell command to get list of all recent uploaders
GET_RECENT_UPLOADERS_CMD ?= sed -n 's/  .*//; s/^ -- //p' debian/changelog | head -n $(RECENT_UPLOADS) | sort -u
# Create uploaders as intersection of GNOME uploaders and recent uploaders, but
# without the Maintainer
UPLOADERS ?= $(shell (echo "$(ALWAYS_UPLOADS)"; sed 's/,//' $(TEAM_LIST); $(GET_RECENT_UPLOADERS_CMD)) | grep -vF "`$(GET_MAINTAINER_CMD)`" | sort | uniq -d | sed '$$,$$! s/$$/,/')
# backward compatibility
uploaders := $(UPLOADERS)
# Header for debian/control (warning to not modify it directly)
CONTROL_HEADER ?= /usr/share/gnome-pkg-tools/control.header

ifeq ($(DISABLE_UPDATE_UPLOADERS),)
clean::
	{ cat $(CONTROL_HEADER) ; \
	  sed "s/@GNOME_TEAM@/$(UPLOADERS)/" debian/control.in ; } \
	> debian/control
endif
