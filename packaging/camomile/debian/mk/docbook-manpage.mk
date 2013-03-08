# -*- mode: makefile; coding: utf-8 -*-
# Copyright © 2006 Sylvain Le Gall <gildor#debian.org>
# Description: Rules to manage manpages written in XML.
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

ifndef _cdbs_rules_docbookxml
_cdbs_rules_docbook_manpage = 1

# needed by debian/control:: rule below
include $(_cdbs_rules_path)/buildcore.mk$(_cdbs_makefile_suffix)

# needed for po4a-stamp
# TODO: if released in CDBS, change above
include $(CURDIR)/debian/mk/po4a.mk$(_cdbs_makefile_suffix)

# docbook manpages to build
DOCBOOK_MANPAGE_SOURCES := 

# extra docbook sources to look for (docbook extract to be included in 
# other docbook manpage)
DOCBOOK_MANPAGE_EXTRA_SOURCES :=

# version of docbook sources to use (must follow the scheme " (>= version)")
DOCBOOK_MANPAGE_VERSION := 

# add required build dependency for used tools 
CDBS_BUILD_DEPENDS := $(CDBS_BUILD_DEPENDS), docbook-xml@docbook_version@, docbook-xsl, libxml2-utils, xsltproc

# xmllint program to use
DOCBOOK_MANPAGE_XMLLINT := xmllint

# flags for xmllint
DOCBOOK_MANPAGE_XMLLINT_FLAGS := --nonet --noout --postvalid --xinclude

# xsltproc program to use
DOCBOOK_MANPAGE_XSLTPROC := xsltproc

# flags for xsltproc
DOCBOOK_MANPAGE_XSLTPROC_FLAGS := --nonet --xinclude --param man.charmap.use.subset 0

# XSL stylesheet to transform docbook to manpage
DOCBOOK_MANPAGE_XSLTPROC_XSL := /usr/share/xml/docbook/stylesheet/nwalsh/manpages/docbook.xsl

# file containing the list of generated files
DOCBOOK_MANPAGE_LIST := $(CURDIR)/debian/docbook-manpage-list

# compute higher docbook version, if required (no version provided and sources defined)
# and replace it in generated control file.
ifneq ($(DEB_AUTO_UPDATE_DEBIAN_CONTROL),)
debian/control::
	if test -f $(CURDIR)/debian/control && test -f $(CURDIR)/debian/control.in; then \
	  DOCBOOK_MANPAGE_VERSION=$(strip $(DOCBOOK_MANPAGE_VERSION)); \
	  if test "x$$DOCBOOK_MANPAGE_VERSION" == "x" && \
	     test "x$(strip $(DOCBOOK_MANPAGE_SOURCES))" != "x"; then \
	    DOCBOOK_ALL_SOURCES="$(DOCBOOK_MANPAGE_SOURCES) $(DOCBOOK_MANPAGE_EXTRA_SOURCES)"; \
	    DOCBOOK_MANPAGE_VERSION=`grep -s -e "-//OASIS//DTD DocBook XML V[0-9\.]*//EN" $$DOCBOOK_ALL_SOURCES | \
		                     sed "s,.*-//OASIS//DTD DocBook XML V\\([0-9\.]*\\)//EN.*,\\1," | \
		                     sort -r | head -n 1`; \
	    if test "x$$DOCBOOK_MANPAGE_VERSION" != "x"; then \
	      DOCBOOK_MANPAGE_VERSION=" (>= $$DOCBOOK_MANPAGE_VERSION)"; \
	    fi; \
	  fi; \
	  sed -i "s,@docbook_version@,$$DOCBOOK_MANPAGE_VERSION," $(CURDIR)/debian/control; \
	fi
endif

# check docbook sources for any problem
%.docbook-manpage-check:: % 
	$(DOCBOOK_MANPAGE_XMLLINT) $(DOCBOOK_MANPAGE_XMLLINT_FLAGS) $<
	echo $@ >> $(DOCBOOK_MANPAGE_LIST)
	touch $@

# build manpage
%.docbook-manpage-build:: % $(DOCBOOK_MANPAGE_XSLTPROC_XSL)
	$(DOCBOOK_MANPAGE_XSLTPROC) \
	  $(DOCBOOK_MANPAGE_XSLTPROC_FLAGS) \
	  -o $(dir $<) \
	  $(DOCBOOK_MANPAGE_XSLTPROC_XSL) \
	  $<
	echo $@ >> $(DOCBOOK_MANPAGE_LIST)
	touch $@


# check all docbook sources and build manpage.
# TODO: find a better solution than recursive call to debian/rules
docbook-manpage: docbook-manpage-stamp
docbook-manpage-stamp:: po4a-stamp
	if test "x$(strip $(DOCBOOK_MANPAGE_SOURCES))" != "x" && \
	   test "x$(strip $(DOCBOOK_MANPAGE_EXTRA_SOURCES))" != "x"; then \
	   $(CURDIR)/debian/rules \
	    $(addsuffix .docbook-manpage-check,\
	      $(DOCBOOK_MANPAGE_SOURCES) \
	      $(DOCBOOK_MANPAGE_EXTRA_SOURCES)) \
	    $(addsuffix .docbook-manpage-build, \
	      $(DOCBOOK_MANPAGE_SOURCES)); \
	fi
	touch $@

build build-arch: docbook-manpage

clean::
	-if test -f $(DOCBOOK_MANPAGE_LIST); then \
	  $(RM) `cat $(DOCBOOK_MANPAGE_LIST)`; \
	  $(RM) $(DOCBOOK_MANPAGE_LIST); \
	fi   
	-$(RM) docbook-manpage-stamp
	
endif
