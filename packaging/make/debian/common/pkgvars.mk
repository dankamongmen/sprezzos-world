############################ -*- Mode: Makefile -*- ###########################
## pkgvars.mk --- 
## Author           : Manoj Srivastava ( srivasta@glaurung.green-gryphon.com ) 
## Created On       : Sat Nov 15 02:56:30 2003
## Created On Node  : glaurung.green-gryphon.com
## Last Modified By : Manoj Srivastava
## Last Modified On : Thu Jun 15 12:05:46 2006
## Last Machine Used: glaurung.internal.golden-gryphon.com
## Update Count     : 11
## Status           : Unknown, Use with caution!
## HISTORY          : 
## Description      : This is what allows us toseparate out the top level
##                    targets, by determining which packages needto be built.
## 
## arch-tag: 75fcc720-7389-4eaa-a7ac-c556d3eac331
## 
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
###############################################################################

# The maintainer information.
maintainer := $(shell LC_ALL=C dpkg-parsechangelog | grep ^Maintainer: | \
                sed 's/^Maintainer: *//')
email := srivasta@debian.org

# Priority of this version (or urgency, as dchanges would call it)
urgency := $(shell LC_ALL=C dpkg-parsechangelog | grep ^Urgency: | \
             sed 's/^Urgency: *//')

# Common useful variables
DEB_SOURCE_PACKAGE := $(strip $(shell egrep '^Source: ' debian/control      |       \
                                      cut -f 2 -d ':'))
DEB_VERSION        := $(strip $(shell LC_ALL=C dpkg-parsechangelog          |       \
                                      egrep '^Version:' | cut -f 2 -d ' '))
DEB_ISNATIVE       := $(strip $(shell LC_ALL=C dpkg-parsechangelog          |       \
                       perl -ne 'print if (m/^Version:/g && ! m/^Version:.*\-/);'))
DEB_DISTRIBUTION  := $(strip $(shell LC_ALL=C dpkg-parsechangelog          |        \
                                      egrep '^Distribution:' | cut -f 2 -d ' '))

DEB_PACKAGES := $(shell perl -e '                                                    \
                  $$/="";                                                            \
                  while(<>){                                                         \
                     $$p=$$1 if m/^Package:\s*(\S+)/;                                \
                     die "duplicate package $$p" if $$seen{$$p};                     \
                     $$seen{$$p}++; print "$$p " if $$p;                             \
                  }' debian/control )

DEB_INDEP_PACKAGES := $(shell perl -e '                                              \
                         $$/="";                                                     \
                         while(<>){                                                  \
                            $$p=$$1 if m/^Package:\s*(\S+)/;                         \
                            die "duplicate package $$p" if $$seen{$$p};              \
                            $$seen{$$p}++;                                           \
                            $$a=$$1 if m/^Architecture:\s*(\S+)/m;                   \
                            next unless ($$a eq "all");                              \
                            print "$$p " if $$p;                                     \
                         }' debian/control )

DEB_ARCH_PACKAGES := $(shell perl -e '                                               \
                         $$/="";                                                     \
                         while(<>){                                                  \
                            $$p=$$1 if m/^Package:\s*(\S+)/;                         \
                            die "duplicate package $$p" if $$seen{$$p};              \
                            $$seen{$$p}++;                                           \
                            $$c="";                                                  \
	                    if (/^Architecture:\s*(.*?)\s*$$/sm) {                   \
                              @a = split /\s+/, $$1 };                               \
	                      for my $$b (@a) {                                      \
                                next unless ($$b eq "$(DEB_HOST_ARCH)" ||            \
                                             $$b eq "any");                          \
                                $$c="$$p";                                           \
                            }                                                        \
                            print "$$c " if $$c;                                     \
                         }' debian/control )

# This package is what we get after removing the psuedo dirs we use in rules
package   = $(notdir $@)
DEBIANDIR = $(dir $(firstword $(MAKEFILE_LIST)))

ifeq  (,$(filter parallel=%,$(FAILS_PARALLEL_BUILD)))
  ifneq (,$(filter parallel=%,$(DEB_BUILD_OPTIONS)))
    NUMJOBS = $(patsubst parallel=%,-j%,$(filter parallel=%,$(DEB_BUILD_OPTIONS)))
  endif
endif

# Define canned sequences used to strip executables and libraries,
# keeping in mind the directives in DEB_BUILD_OPTIONS
ifeq (,$(findstring nostrip,$(DEB_BUILD_OPTIONS)))
define strip-exec
find $(TMPTOP) -type f | while read i; do                                    \
   if file -b $$i | egrep -q "^ELF.*executable"; then                        \
     strip --strip-all --remove-section=.comment --remove-section=.note $$i; \
   fi;                                                                       \
 done
endef

define strip-lib
find $(TMPTOP) -type f | while read i; do                                         \
   if file -b $$i | egrep -q "^ELF.*shared object"; then                          \
     strip --strip-unneeded --remove-section=.comment --remove-section=.note $$i; \
   fi;                                                                            \
done
endef
else
define strip-exec
@echo Not strippping executables as asked
endef

define strip-lib
@echo Not strippping libraries as asked
endef

endif

# this canned command specifies how to run dpkg-shlibs to add things
# to debian/substvars by scanning executables and libraries; this
# should suffice for the common case. Some rules files might need some
# changes to the command sequence, though
define get-shlib-deps
k=`find $(TMPTOP) -type f | ( while read i; do          \
    if file -b $$i |                                    \
      egrep -q "^ELF.*(executable.*dynamically linked|shared object)"; then   \
        j="$$j $$i";                                     \
    fi;                                                  \
done; echo $$j; )`; if [ -n "$$k" ]; then dpkg-shlibdeps $$k; fi
endef

# This canned sequence checks to see if all the libraries we link to
# actually provide some symbols needed by some executable ot library
# in the package itself.
ifeq (,$(strip $(filter nocheck,$(DEB_BUILD_OPTIONS))))
  ifeq ($(DEB_BUILD_GNU_TYPE),$(DEB_HOST_GNU_TYPE))
define check-libraries
echo Checking libs
xtra=$$($(SHELL) debian/common/checklibs); \
if [ -n "$$extra" ]; then                  \
  echo "Extra libraries: $$extra";         \
  exit 1;                                  \
fi
endef
  else
define check-libraries
echo Not checking libs
endef
  endif
else
define check-libraries
echo Not checking libs
endef
endif


#Local variables:
#mode: makefile
#End:
