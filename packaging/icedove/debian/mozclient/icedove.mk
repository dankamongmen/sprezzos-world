# -*- mode: makefile; coding: utf-8 -*-

# Copyright (c) 2009 Guido Guenther <agx@sigxcpu.org>
# Description: Project Icedove 3.0
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

BRANDING_DIR = mail/branding/icedove/

MOZCLIENT_PROJECTNAME := icedove
include debian/mozclient/thunderbird.mk

makebuilddir/$(DEB_MOZ_APPLICATION):: debian/stamp-icedove-branding
debian/stamp-icedove-branding:
	cp -af debian/icedove-branding $(BRANDING_DIR)
	cp debian/app-icons/icedove16.png $(BRANDING_DIR)/mailicon16.png
	cp debian/app-icons/icedove22.png $(BRANDING_DIR)/mailicon22.png
	cp debian/app-icons/icedove24.png $(BRANDING_DIR)/mailicon24.png
	cp debian/app-icons/icedove32.png $(BRANDING_DIR)/mailicon32.png
	cp debian/app-icons/icedove48.png $(BRANDING_DIR)/mailicon48.png
	cp debian/app-icons/icedove256.png $(BRANDING_DIR)/mailicon256.png
	cp debian/app-icons/icedove48.png $(BRANDING_DIR)/content/icon48.png
	cp debian/app-icons/icedove64.png $(BRANDING_DIR)/content/icon64.png
	cp debian/preview.png mail/themes/gnomestripe/mail/preview.png
	touch $@

