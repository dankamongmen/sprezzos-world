#!/usr/bin/make -f

# Debian X Strike Force Build System (XSFBS): Make portion

# Copyright 1996 Stephen Early
# Copyright 1997 Mark Eichin
# Copyright 1998-2005, 2007 Branden Robinson
# Copyright 2005 David Nusinow
#
# Licensed under the GNU General Public License, version 2.  See the file
# /usr/share/common-licenses/GPL or <http://www.gnu.org/copyleft/gpl.txt>.

# Originally by Stephen Early <sde1000@debian.org>
# Modified by Mark W. Eichin <eichin@kitten.gen.ma.us>
# Modified by Adam Heath <doogie@debian.org>
# Modified by Branden Robinson <branden@debian.org>
# Modified by Fabio Massimo Di Nitto <fabbione@fabbione.net>
# Modified by David Nusinow <dnusinow@debian.org>
# Acknowledgements to Manoj Srivastava.

# Pass $(DH_OPTIONS) into the environment for debhelper's benefit.
export DH_OPTIONS

# force quilt to not use ~/.quiltrc and to use debian/patches
QUILT = QUILT_PATCHES=debian/patches quilt --quiltrc /dev/null

# Set up parameters for the upstream build environment.

# Determine (source) package name from Debian changelog.
SOURCE_NAME:=$(shell dpkg-parsechangelog -ldebian/changelog \
                        | grep '^Source:' | awk '{print $$2}')

# Determine package version from Debian changelog.
SOURCE_VERSION:=$(shell dpkg-parsechangelog -ldebian/changelog \
                        | grep '^Version:' | awk '{print $$2}')

# Determine upstream version number.
UPSTREAM_VERSION:=$(shell echo $(SOURCE_VERSION) | sed 's/-.*//')

# Determine the source version without the epoch for make-orig-tar-gz
NO_EPOCH_VER:=$(shell echo $(UPSTREAM_VERSION) | sed 's/^.://')

# Figure out who's building this package.
BUILDER:=$(shell echo $${DEBEMAIL:-$${EMAIL:-$$(echo $$LOGNAME@$$(cat /etc/mailname 2>/dev/null))}})

# Find out if this is an official build; an official build has nothing but
# digits, dots, and/or the codename of a release in the Debian part of the
# version number.  Anything else indicates an unofficial build.
OFFICIAL_BUILD:=$(shell VERSION=$(SOURCE_VERSION); if ! expr "$$(echo $${VERSION\#\#*-} | sed 's/\(woody\|sarge\|etch\|lenny\)//g')" : ".*[^0-9.].*" >/dev/null 2>&1; then echo yes; fi)

# Set up parameters for the Debian build environment.

# Determine our architecture.
BUILD_ARCH:=$(shell dpkg-architecture -qDEB_BUILD_ARCH)
# Work around some old-time dpkg braindamage.
BUILD_ARCH:=$(subst i486,i386,$(BUILD_ARCH))
# The DEB_HOST_ARCH variable may be set per the Debian cross-compilation policy.
ifdef DEB_HOST_ARCH
 ARCH:=$(DEB_HOST_ARCH)
else
 # dpkg-cross sets the ARCH environment variable; if set, use it.
 ifdef ARCH
  ARCH:=$(ARCH)
 else
  ARCH:=$(BUILD_ARCH)
 endif
endif

# $(STAMP_DIR) houses stamp files for complex targets.
STAMP_DIR:=stampdir

# $(DEBTREEDIR) is where all install rules are told (via $(DESTDIR)) to place
# their files.
DEBTREEDIR:=$(CURDIR)/debian/tmp

# All "important" targets have four lines:
#   1) A target name that is invoked by a package-building tool or the user.
#      This consists of a dependency on a "$(STAMP_DIR)/"-prefixed counterpart.
#   2) A line delcaring 1) as a phony target (".PHONY:").
#   3) A "$(STAMP_DIR)/"-prefixed target which does the actual work, and may
#   depend on other targets.
#   4) A line declaring 3) as a member of the $(stampdir_targets) variable; the
#   "$(STAMP_DIR)/" prefix is omitted.
#
# This indirection is needed so that the "stamp" files that signify when a rule
# is done can be located in a separate "stampdir".  Recall that make has no way
# to know when a goal has been met for a phony target (like "build" or
# "install").
#
# At the end of each "$(STAMP_DIR)/" target, be sure to run the command ">$@"
# so that the target will not be run again.  Removing the file will make Make
# run the target over.

# All phony targets should be declared as dependencies of .PHONY, even if they
# do not have "($STAMP_DIR)/"-prefixed counterparts.

# Define a harmless default rule to keep things from going nuts by accident.
.PHONY: default
default:

# Set up the $(STAMP_DIR) directory.
.PHONY: stampdir
stampdir_targets+=stampdir
stampdir: $(STAMP_DIR)/stampdir
$(STAMP_DIR)/stampdir:
	mkdir $(STAMP_DIR)
	>$@

# Set up the package build directory as quilt expects to find it.
.PHONY: prepare
stampdir_targets+=prepare
prepare: $(STAMP_DIR)/prepare
$(STAMP_DIR)/prepare: $(STAMP_DIR)/logdir $(STAMP_DIR)/genscripts
	>$@

.PHONY: logdir
stampdir_targets+=logdir
logdir: $(STAMP_DIR)/logdir
$(STAMP_DIR)/logdir: $(STAMP_DIR)/stampdir
	mkdir -p $(STAMP_DIR)/log
	>$@

# Apply all patches to the upstream source.
.PHONY: patch
stampdir_targets+=patch
patch: $(STAMP_DIR)/patch
$(STAMP_DIR)/patch: $(STAMP_DIR)/prepare
	if ! [ `which quilt` ]; then \
		echo "Couldn't find quilt. Please install it or add it to the build-depends for this package."; \
		exit 1; \
	fi; \
	if $(QUILT) next >/dev/null 2>&1; then \
	  echo -n "Applying patches..."; \
	  if $(QUILT) push -a -v >$(STAMP_DIR)/log/patch 2>&1; then \
	    cat $(STAMP_DIR)/log/patch; \
	    echo "successful."; \
	  else \
	    cat $(STAMP_DIR)/log/patch; \
	    echo "failed! (check $(STAMP_DIR)/log/patch for details)"; \
	    exit 1; \
	  fi; \
	else \
	  echo "No patches to apply"; \
	fi; \
	>$@

# Revert all patches to the upstream source.
.PHONY: unpatch
unpatch: $(STAMP_DIR)/logdir
	rm -f $(STAMP_DIR)/patch
	@echo -n "Unapplying patches..."; \
	if $(QUILT) applied >/dev/null 2>/dev/null; then \
	  if $(QUILT) pop -a -v >$(STAMP_DIR)/log/unpatch 2>&1; then \
	    cat $(STAMP_DIR)/log/unpatch; \
	    echo "successful."; \
	  else \
	    cat $(STAMP_DIR)/log/unpatch; \
	    echo "failed! (check $(STAMP_DIR)/log/unpatch for details)"; \
	    exit 1; \
	  fi; \
	else \
	  echo "nothing to do."; \
	fi

# Clean the generated maintainer scripts.
.PHONY: cleanscripts
cleanscripts:
	rm -f $(STAMP_DIR)/genscripts
	rm -f debian/*.config \
	      debian/*.postinst \
	      debian/*.postrm \
	      debian/*.preinst \
	      debian/*.prerm

# Clean the package build tree.
.PHONY: xsfclean
xsfclean: cleanscripts unpatch
	dh_testdir
	rm -rf .pc
	rm -rf $(STAMP_DIR)
	dh_clean

# Remove files from the upstream source tree that we don't need, or which have
# licensing problems.  It must be run before creating the .orig.tar.gz.
#
# Note: This rule is for Debian package maintainers' convenience, and is not
# needed for conventional build scenarios.
.PHONY: prune-upstream-tree
prune-upstream-tree:
	# Ensure we're in the correct directory.
	dh_testdir
	grep -rvh '^#' debian/prune/ | xargs --no-run-if-empty rm -rf

# Verify that there are no offsets or fuzz in the patches we apply.
#
# Note: This rule is for Debian package maintainers' convenience, and is not
# needed for conventional build scenarios.
.PHONY: patch-audit
patch-audit: prepare unpatch
	@echo -n "Auditing patches..."; \
	>$(STAMP_DIR)/log/patch; \
	FUZZY=; \
	while [ -n "$$($(QUILT) next)" ]; do \
	  RESULT=$$($(QUILT) push -v | tee -a $(STAMP_DIR)/log/patch | grep ^Hunk | sed 's/^Hunk.*\(succeeded\|FAILED\).*/\1/');\
	  case "$$RESULT" in \
	    succeeded) \
	      echo "fuzzy patch: $$($(QUILT) top)" \
	        | tee -a $(STAMP_DIR)/log/$$($(QUILT) top); \
	      FUZZY=yes; \
	      ;; \
	    FAILED) \
	      echo "broken patch: $$($(QUILT) next)" \
	        | tee -a $(STAMP_DIR)/log/$$($(QUILT) next); \
	      exit 1; \
	      ;; \
	  esac; \
	done; \
	if [ -n "$$FUZZY" ]; then \
	  echo "there were fuzzy patches; please fix."; \
	  exit 1; \
	else \
	  echo "done."; \
	fi

# Generate the maintainer scripts.
.PHONY: genscripts
stampdir_targets+=genscripts
genscripts: $(STAMP_DIR)/genscripts
$(STAMP_DIR)/genscripts: $(STAMP_DIR)/stampdir
	for FILE in debian/*.config.in \
	            debian/*.postinst.in \
	            debian/*.postrm.in \
	            debian/*.preinst.in \
	            debian/*.prerm.in; do \
	  if [ -e "$$FILE" ]; then \
	    MAINTSCRIPT=$$(echo $$FILE | sed 's/.in$$//'); \
	    sed -n '1,/^#INCLUDE_SHELL_LIB#$$/p' <$$FILE \
	      | sed -e '/^#INCLUDE_SHELL_LIB#$$/d' >$$MAINTSCRIPT.tmp; \
	    cat debian/xsfbs/xsfbs.sh >>$$MAINTSCRIPT.tmp; \
	    sed -n '/^#INCLUDE_SHELL_LIB#$$/,$$p' <$$FILE \
	      | sed -e '/^#INCLUDE_SHELL_LIB#$$/d' >>$$MAINTSCRIPT.tmp; \
	    sed -e 's/@SOURCE_VERSION@/$(SOURCE_VERSION)/' \
	        -e 's/@OFFICIAL_BUILD@/$(OFFICIAL_BUILD)/' \
	      <$$MAINTSCRIPT.tmp >$$MAINTSCRIPT; \
	    rm $$MAINTSCRIPT.tmp; \
	  fi; \
	done
	# Validate syntax of generated shell scripts.
	#sh debian/scripts/validate-posix-sh debian/*.config \
	#                                    debian/*.postinst \
	#                                    debian/*.postrm \
	#                                    debian/*.preinst \
	#                                    debian/*.prerm
	>$@

SERVERMINVERS = $(shell cat /usr/share/xserver-xorg/serverminver 2>/dev/null)
VIDEOABI = $(shell cat /usr/share/xserver-xorg/videoabiver 2>/dev/null)
INPUTABI = $(shell cat /usr/share/xserver-xorg/inputabiver 2>/dev/null)
SERVER_DEPENDS = xserver-xorg-core (>= $(SERVERMINVERS))
VIDDRIVER_PROVIDES = xserver-xorg-video-$(VIDEOABI)
INPDRIVER_PROVIDES = xserver-xorg-input-$(INPUTABI)
ifeq ($(PACKAGE),)
PACKAGE=$(shell awk '/^Package:/ { print $$2; exit }' < debian/control)
endif

.PHONY: serverabi
serverabi: install
ifeq ($(SERVERMINVERS),)
	@echo error: xserver-xorg-dev needs to be installed
	@exit 1
else
	echo "xserver:Depends=$(SERVER_DEPENDS)" >> debian/$(PACKAGE).substvars
	echo "xviddriver:Provides=$(VIDDRIVER_PROVIDES)" >> debian/$(PACKAGE).substvars
	echo "xinpdriver:Provides=$(INPDRIVER_PROVIDES)" >> debian/$(PACKAGE).substvars
endif

# vim:set noet ai sts=8 sw=8 tw=0:
