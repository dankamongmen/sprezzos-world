# -*- mode: makefile; coding: utf-8 -*-

# Copyright (c) 2008-2009 Canonical Ltd.
# Author(s): Alexander Sack <asac@ubuntu.com>
#            Fabien Tassin <fta@sofaraway.org>
#            Benjamin Drung <bdrung@debian.org>
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
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#
# Usage: include this file in your CDBS debian/rules file and define the
#        following variables:
#
#        MOZ_EXTENSION_PKG (OPTIONAL):
#                if defined the given binary package name is used to ship
#                this xpi; otherwise the first binary package listed in
#                debian/control is used
#
#        MOZ_XPI_FILE (OPTIONAL):
#                if defined the given .xpi file is used; otherwise we try to
#                guess one using wildcard (*.xpi)
#
#        MOZ_XPI_BUILD_COMMAND (OPTIONAL):
#                if defined the given command will be run _before_ the extension
#                gets packaged up the standard .xpi way. Thus, the build command
#                should produce an .xpi in top level directory. Note: If this
#                command is specified, MOZ_XPI_CLEAN_COMMAND (see below) will be
#                run during |clean|. If the .xpi file will not be build from
#                source, you have to set this variable to nothing.
#
#        MOZ_XPI_CLEAN_COMMAND (OPTIONAL):
#                only has an effect if MOZ_XPI_BUILD_COMMAND (see above) is set.
#                It defaults to `rm -f *.xpi`.
#
#        MOZ_XPI_MOZILLA_EXTRA_DIRS (OPTIONAL):
#                defines extra directories to link the extension in. Usually
#                xpi.mk creates the right links based on targetApplication
#                parsed in install.rdf; if you need more directories, use this.
#
#        MOZ_XPI_DOCUMENTED_LICENSE_FILES (OPTIONAL):
#                defines extra license files which need to be excluded during
#                the installation of the XPI file to the packaging tree. When
#                using parameter, be sure that you documented _all_ excluded
#                license files in debian/copyright appropriately. If not defined
#                the common license file names are guessed.
#
#        MOZ_XPI_EXT_NAME (OPTIONAL):
#                defines the name of the extension (without any prefixes like
#                mozilla- or xul-ext-). If not defined MOZ_EXTENSION_PKG with
#                stripped prefixes is used. This value is used to determine
#                xpi:Provides.
#
#        MOZ_XPI_PRESERVE_PERMISSIONS (OPTIONAL):
#                if defined (set to 1), the permission of the installed files
#                will not be changed. If not defined or set to $(null), the
#                permission of the files will be set to 644 and the permissions
#                of scripts (files containing a shebang) will be set to 755.
#
#        MOZ_XPI_INSTALL_DIRECTORY (OPTIONAL):
#                The xpi file will be installed in the specified directory.
#                This directory must be an absolute path. Use this parameter
#                with care.
#
#        MOZ_XPI_DISABLE_SYSTEM_PREFS (OPTIONAL):
#                if defined (set to 1), no system preference file will be
#                created in /etc.
#
#        Unused variables (can be removed):
#
#        MOZ_XPI_EMID (OPTIONAL):
#                if defined the given id is used to determine the link name
#                in the Firefox extensions directory. if not defined we try
#                our best to extract the em:id from the install.rdf file shipped
#                by any xpi
#                '''Note''': this variable is not used any more

MOZ_EXTENSION_PKG ?= $(strip $(shell grep ^Package: debian/control | head -n 1 | sed "s/^Package://"))

MOZ_XPI_BUILD_COMMAND ?= xpi-pack $(CURDIR) $(MOZ_EXTENSION_PKG).xpi
MOZ_XPI_CLEAN_COMMAND ?= rm -f *.xpi

ifneq (,$(MOZ_XPI_FILE))
xpi_file = $(wildcard $(MOZ_XPI_FILE))
else
xpi_file = $(wildcard *.xpi)
endif

ifneq (,$(MOZ_XPI_PRESERVE_PERMISSIONS))
install_xpi_extra_parameter += --preserve-permissions
endif

ifneq (,$(MOZ_XPI_DISABLE_SYSTEM_PREFS))
install_xpi_extra_parameter += --disable-system-prefs
endif

ifneq (,$(MOZ_XPI_INSTALL_DIRECTORY))
install_xpi_extra_parameter += -i $(MOZ_XPI_INSTALL_DIRECTORY)
endif

ifeq ($(origin MOZ_XPI_DOCUMENTED_LICENSE_FILES),undefined)
install_xpi_extra_parameter += --remove-license-files
else
install_xpi_extra_parameter += $(foreach exclude,$(MOZ_XPI_DOCUMENTED_LICENSE_FILES),-x $(exclude))
endif

install_xpi_extra_parameter += $(foreach dir,$(MOZ_XPI_MOZILLA_EXTRA_DIRS),-l $(dir))

# ### cdbs hooks
# build xpi using MOZ_XPI_BUILD_COMMAND if defined
build/$(MOZ_EXTENSION_PKG)::
ifneq (,$(MOZ_XPI_BUILD_COMMAND))
	$(MOZ_XPI_BUILD_COMMAND)
endif

install/$(MOZ_EXTENSION_PKG):: xpi-install

xpi-install:
	install-xpi -p$(MOZ_EXTENSION_PKG) $(xpi_file) $(install_xpi_extra_parameter)
	dh_xul-ext -p$(MOZ_EXTENSION_PKG)

# clean build and remove all .xpi in top-level if a MOZ_XPI_BUILD_COMMAND is defined
ifneq (,$(MOZ_XPI_BUILD_COMMAND))
clean::
	dh_testdir
	dh_clean
	$(MOZ_XPI_CLEAN_COMMAND)
endif

.PHONY: clean xpi-install
