# -*- mode: makefile; coding: utf-8 -*-
# Copyright © 2006 Sylvain Le Gall <gildor#debian.org>
# Description: Rules to manage translation made through po4a.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
# MA 02110-1301, USA. 

_cdbs_scripts_path ?= /usr/lib/cdbs
_cdbs_rules_path ?= /usr/share/cdbs/1/rules
_cdbs_class_path ?= /usr/share/cdbs/1/class

ifndef _cdbs_rules_po4a
_cdbs_rules_po4a = 1

# needed by debian/control:: rule below
include $(_cdbs_rules_path)/buildcore.mk$(_cdbs_makefile_suffix)

# po4a files 
PO4A_SOURCES :=

# po4a program to use
PO4A := po4a

# flags for po4a in clean target
PO4A_CLEAN_FLAGS := --no-translations --rm-backups 

# flags for po4a in build target
PO4A_BUILD_FLAGS:= --rm-backups

# file containing the list of generated files
PO4A_LIST := $(CURDIR)/debian/po4a-list

# add required build dependency for used tools 
CDBS_BUILD_DEPENDS := $(CDBS_BUILD_DEPENDS), po4a

# generate the translations (rules follow recommendation of po4a author)
%.po4a-build:: %
	$(PO4A) $(PO4A_BUILD_FLAGS) $<
	echo $@ >> $(PO4A_LIST) 
	touch $@

# call po4a when it is required
# TODO: find a better solution than recursive call to debian/rules
po4a: po4a-stamp
po4a-stamp::
	if test "x$(strip $(PO4A_SOURCES))" != "x"; then \
	  $(CURDIR)/debian/rules \
	    $(addsuffix .po4a-build,\
	      $(PO4A_SOURCES)); \
	fi
	touch $@

build: po4a

# update the POT and PO (rules follow recommendation of po4a author)
clean::
	if test -f $(PO4A_LIST); then \
	  for i in `grep ".po4a-build" $(PO4A_LIST)`; do \
	    $(PO4A) $(PO4A_CLEAN_FLAGS) $${i%%.po4a-build}; \
	  done; \
	fi
	-if test -f $(PO4A_LIST); then \
	  $(RM) `cat $(PO4A_LIST)`; \
	  $(RM) $(PO4A_LIST); \
	fi
	-$(RM) po4a-stamp

endif
