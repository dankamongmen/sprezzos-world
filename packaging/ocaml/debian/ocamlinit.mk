#
# Description: Useful Makefile rules for OCaml related packages
#
# Copyright © 2009 Stéphane Glondu <steph@glondu.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA.
#

_ocaml_share_path ?= /usr/share/ocaml

ifndef _ocaml_share_ocamlinit
_ocaml_share_ocamlinit = 1

include $(_ocaml_share_path)/ocamlvars.mk

# list of .in files contained (non-recursively) in debian/ that requires
# pre-build filling.
# debian/rules writers might need to add stuff to this list:
#  e.g.: OCAML_IN_FILES += debian/patches/foo	# (no .in extension)
OCAML_IN_FILES ?= $(filter-out debian/control,$(patsubst %.in,%,$(wildcard debian/*.in)))

# WARNING: there are currently duplications with ocamlvars.mk and
# ocaml.mk, but hopefully they will be removed at some point in the
# future

OCAMLINIT_SED := \
  -e 's%@OCamlABI@%$(OCAML_ABI)%g' \
  -e 's%@OCamlStdlibDir@%$(OCAML_STDLIB_DIR)%g' \
  -e 's%@OCamlDllDir@%$(OCAML_DLL_DIR)%g'

ifeq ($(OCAML_HAVE_OCAMLOPT),yes)
  OCAMLINIT_SED += -e 's/^OPT: //' -e '/^BYTE: /d'
else
  OCAMLINIT_SED += -e '/^OPT: /d' -e 's/^BYTE: //'
endif

ifeq ($(OCAML_NATDYNLINK),yes)
  OCAMLINIT_SED += -e 's/^DYN: //'
else
  OCAMLINIT_SED += -e '/^DYN: /d'
endif

ocamlinit: ocamlinit-stamp
ocamlinit-stamp:
	for t in $(OCAML_IN_FILES); do \
	  sed $(OCAMLINIT_SED) $$t.in > $$t; \
	done
	sed -i 's@\./@@' debian/ocaml-nox.lintian-overrides
	touch $@

ocamlinit-clean:
	rm -f ocamlinit-stamp $(OCAML_IN_FILES)

.PHONY: ocamlinit ocamlinit-clean

endif
