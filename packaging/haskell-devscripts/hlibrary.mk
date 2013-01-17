# -*- mode: makefile -*-
# Copyright 2008 Kari Pahula <kaol@debian.org>
# Description: A class for Haskell library packages
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
# 02111-1307 USA.

export GREP_OPTIONS :=

# Set a dummy HOME variable upon build. Some build daemons do not set HOME, but
# cabal expects it to be available.
export HOME = /homedoesnotexistatbuildtime


DEB_CABAL_PACKAGE ?= $(shell cat *.cabal |\
 perl -ne \
 'if (/^name\s*:\s*(.*?)\s*$$/i) {$$_ = $$1; tr/A-Z/a-z/; print; exit 0;}')
CABAL_PACKAGE=$(DEB_CABAL_PACKAGE)
CABAL_VERSION=$(shell cat *.cabal | egrep -i '^\s*version:' | head -n1 | sed -r 's,^\s*version:\s*,,i'| sed -r 's,\s*$$,,i')

DEB_ENABLE_TESTS ?= no

ENABLE_PROFILING = $(shell egrep -qe '^Package: libghc-.*-prof$$' debian/control && echo --enable-library-profiling; exit 0)

NO_GHCI_FLAG = $(shell test -e /usr/bin/ghci || echo --ghc-option=-DDEBIAN_NO_GHCI; exit 0)

DEB_COMPRESS_EXCLUDE += .haddock .hs .txt

# TODO:
# - some of this would probably be useful for generic Haskell programs,
#   not just libraries
# - provide more hooks
# - get this included in the cdbs package once this gets mature enough (maybe?)

DEB_SETUP_BIN_NAME ?= debian/hlibrary.setup
DEB_HADDOCK_HTML_DIR ?= /usr/share/doc/libghc-$(CABAL_PACKAGE)-doc/html/

# most likely you don't need to touch this one
GHC6_VERSION = $(shell ghc --numeric-version)
GHC_VERSION = $(shell ghc --numeric-version)
DEB_HADDOCK_DIR ?= /usr/lib/ghc-doc/haddock/$(CABAL_PACKAGE)-$(CABAL_VERSION)/

DEB_HADDOCK_OPTS += --html --hoogle
ifndef DEB_NO_IMPLICIT_HADDOCK_HYPERLINK
DEB_HADDOCK_OPTS += --hyperlink-source
endif

DEB_HOOGLE_TXT_DIR = /usr/lib/ghc-doc/hoogle/

MAKEFILE := debian/hlibrary.Makefile

#ifneq (,$(filter parallel=%,$(DEB_BUILD_OPTIONS)))
#    NUMJOBS = $(patsubst parallel=%,%,$(filter parallel=%,$(DEB_BUILD_OPTIONS)))
#    MAKEFLAGS := -j$(NUMJOBS)
#    BUILD_GHC := $(DEB_SETUP_BIN_NAME) makefile -f $(MAKEFILE) && $(MAKE) $(MAKEFLAGS) -f $(MAKEFILE) && $(BUILD_GHC)
#endif

ifneq (,$(findstring noopt,$(DEB_BUILD_OPTIONS)))
   OPTIMIZATION = --disable-optimization
endif

ifeq ($(DEB_ENABLE_TESTS),yes)
ifeq (,$(filter nocheck,$(DEB_BUILD_OPTIONS)))
   TESTS = --enable-tests
endif
endif

DEB_BUILD_DEPENDENCIES = build-arch

clean::
	[ ! -x "$(DEB_SETUP_BIN_NAME)" ] || $(DEB_SETUP_BIN_NAME) clean
	rm -rf dist dist-ghc dist-hugs $(DEB_SETUP_BIN_NAME) Setup.hi Setup.ho Setup.o .*config*
	rm -f build-ghc-stamp build-hugs-stamp build-haddock-stamp
	rm -rf debian/tmp-inst-ghc
	rm -f debian/extra-depends
	rm -f debian/libghc-$(CABAL_PACKAGE)-doc.links
	rm -f $(MAKEFILE)
	rm -rf debian/dh_haskell_shlibdeps

$(DEB_SETUP_BIN_NAME):
	if test ! -e Setup.lhs -a ! -e Setup.hs; then echo "No setup script found!"; exit 1; fi
	for setup in Setup.lhs Setup.hs; do if test -e $$setup; then ghc --make $$setup -o $(DEB_SETUP_BIN_NAME); exit 0; fi; done

dist-ghc: $(DEB_SETUP_BIN_NAME)
	$(DEB_SETUP_BIN_NAME) configure --ghc -v2 \
		--prefix=/usr --libdir=/usr/lib/haskell-packages/ghc/lib \
		--builddir=dist-ghc \
		--haddockdir=$(DEB_HADDOCK_DIR) \
		--htmldir=$(DEB_HADDOCK_HTML_DIR) $(ENABLE_PROFILING) $(NO_GHCI_FLAG) \
		$(DEB_SETUP_GHC6_CONFIGURE_ARGS) $(DEB_SETUP_GHC_CONFIGURE_ARGS) $(OPTIMIZATION) $(TESTS)

build-ghc-stamp: dist-ghc
	$(DEB_SETUP_BIN_NAME) build --builddir=dist-ghc
	touch $@

ifeq ($(DEB_ENABLE_TESTS),yes)
ifeq (,$(filter nocheck,$(DEB_BUILD_OPTIONS)))
check-ghc-stamp: build-ghc-stamp
	$(DEB_SETUP_BIN_NAME) test --builddir=dist-ghc
	touch $@
else
check-ghc-stamp: build-ghc-stamp
	@echo DEB_BUILD_OPTIONS contains nocheck, not running checks
	touch $@
endif
else
check-ghc-stamp: build-ghc-stamp
	@echo DEB_ENABLE_TESTS not set to yes, not running any tests.
	touch $@
endif

build/libghc-$(CABAL_PACKAGE)-prof build/libghc-$(CABAL_PACKAGE)-dev:: build-ghc-stamp check-ghc-stamp

build-haddock-stamp:
	[ ! -x /usr/bin/haddock ] || $(DEB_SETUP_BIN_NAME) haddock --builddir=dist-ghc $(DEB_HADDOCK_OPTS)
	touch build-haddock-stamp

build/haskell-$(CABAL_PACKAGE)-doc build/libghc-$(CABAL_PACKAGE)-doc:: dist-ghc build-haddock-stamp

dist-hugs: $(DEB_SETUP_BIN_NAME)
	$(DEB_SETUP_BIN_NAME) configure --hugs --prefix=/usr -v2 --builddir=dist-hugs $(DEB_SETUP_HUGS_CONFIGURE_ARGS)

build/libhugs-$(CABAL_PACKAGE):: dist-hugs
	$(DEB_SETUP_BIN_NAME) build --builddir=dist-hugs

debian/tmp-inst-ghc: $(DEB_SETUP_BIN_NAME) dist-ghc
	$(DEB_SETUP_BIN_NAME) copy --builddir=dist-ghc --destdir=debian/tmp-inst-ghc

debian/extra-depends: debian/tmp-inst-ghc
	pkg_config=`$(DEB_SETUP_BIN_NAME) register --builddir=dist-ghc --gen-pkg-config | tr -d ' \n' | sed -r 's,^.*:,,'` ; \
		dh_haskell_extra_depends $$pkg_config ; \
		rm $$pkg_config

install/libghc-$(CABAL_PACKAGE)-dev:: debian/tmp-inst-ghc debian/extra-depends
	cd debian/tmp-inst-ghc ; find usr/lib/haskell-packages/ghc/lib/ \
		\( ! -name "*_p.a" ! -name "*.p_hi" \) \
		-exec install -Dm 644 '{}' ../$(notdir $@)/'{}' ';'
	pkg_config=`$(DEB_SETUP_BIN_NAME) register --builddir=dist-ghc --gen-pkg-config | tr -d ' \n' | sed -r 's,^.*:,,'`; \
		$(if $(HASKELL_HIDE_PACKAGES),sed -i 's/^exposed: True$$/exposed: False/' $$pkg_config;) \
		install -Dm 644 $$pkg_config debian/$(notdir $@)/var/lib/ghc/package.conf.d/$$pkg_config; \
		rm -f $$pkg_config
	if [ 'z$(DEB_GHC_EXTRA_PACKAGES)' != 'z' ] ; then \
		echo '$(DEB_GHC_EXTRA_PACKAGES)' > debian/$(notdir $@)/usr/lib/haskell-packages/ghc/lib/$(CABAL_PACKAGE)-$(CABAL_VERSION)/extra-packages ; \
	fi
	dh_haskell_provides -p$(notdir $@)
	dh_haskell_depends -p$(notdir $@)
	dh_haskell_shlibdeps -p$(notdir $@)

install/libghc-$(CABAL_PACKAGE)-prof:: debian/tmp-inst-ghc install/libghc-$(CABAL_PACKAGE)-dev debian/extra-depends
	cd debian/tmp-inst-ghc ; find usr/lib/haskell-packages/ghc/lib/ \
		! \( ! -name "*_p.a" ! -name "*.p_hi" \) \
		-exec install -Dm 644 '{}' ../$(notdir $@)/'{}' ';'
	dh_haskell_provides -p$(notdir $@)
	dh_haskell_depends -p$(notdir $@)

install/haskell-$(CABAL_PACKAGE)-doc install/libghc-$(CABAL_PACKAGE)-doc:: debian/tmp-inst-ghc debian/extra-depends
	mkdir -p debian/$(notdir $@)/$(DEB_HADDOCK_HTML_DIR)
	cd debian/tmp-inst-ghc/ ; find ./$(DEB_HADDOCK_HTML_DIR)/ \
		! -name "*.haddock" -exec install -Dm 644 '{}' \
		../$(notdir $@)/'{}' ';'
	mkdir -p debian/$(notdir $@)/$(DEB_HADDOCK_DIR)
	[ 0 = `ls debian/tmp-inst-ghc/$(DEB_HADDOCK_DIR)/ 2>/dev/null | wc -l` ] || \
		cp -r debian/tmp-inst-ghc/$(DEB_HADDOCK_DIR)/*.haddock \
		debian/$(notdir $@)/$(DEB_HADDOCK_DIR)
	find debian/$(notdir $@)/$(DEB_HADDOCK_HTML_DIR) -name "*.txt" \
		-printf "%p $(DEB_HOOGLE_TXT_DIR)/%f\n" >> debian/libghc-$(CABAL_PACKAGE)-doc.links
	sed -i s,^debian/libghc-$(CABAL_PACKAGE)-doc,, debian/libghc-$(CABAL_PACKAGE)-doc.links
	dh_haskell_depends -p$(notdir $@)

install/libhugs-$(CABAL_PACKAGE):: $(DEB_SETUP_BIN_NAME) dist-hugs debian/extra-depends
	$(DEB_SETUP_BIN_NAME) copy --destdir=debian/libhugs-$(CABAL_PACKAGE) --builddir=dist-hugs
	rm -rf debian/libhugs-$(CABAL_PACKAGE)/usr/share/doc/*
	dh_haskell_depends -p$(notdir $@)

