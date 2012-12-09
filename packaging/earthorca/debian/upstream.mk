# Generic rules to help download sources from ftp.mozilla.org.
# Define the following variables before including this file:
# PRODUCT - product codename (e.g. browser)
# PRODUCT_NAME - name of the product (e.g. firefox)

# The VERSION_FILTER transforms upstream version patterns to versions
# used in debian/changelog. Versions are to be transformed as follows:
# 4.0      -> 4.0
# 4.0a1    -> 4.0~a1
# 4.0b5    -> 4.0~b5
# That should ensure the proper ordering
VERSION_FILTER := sed 's/\([0-9]\)\([ab]\)/\1~\2/g'
$(call lazy,UPSTREAM_VERSION,$$(shell cat $(PRODUCT)/config/version.txt))
GRE_SRCDIR := $(strip $(foreach dir,. mozilla,$(if $(wildcard $(dir)/config/milestone.pl),$(dir))))
ifndef GRE_SRCDIR
$(error Could not determine the top directory for GRE codebase)
endif
GRE_MILESTONE := $(shell $(GRE_SRCDIR)/config/milestone.pl --topsrcdir $(GRE_SRCDIR) | $(VERSION_FILTER))

# Construct GRE_VERSION from the first two digits in GRE_MILESTONE
GRE_VERSION := $(subst ~, ,$(subst ., ,$(GRE_MILESTONE)))
export JS_SO_VERSION := $(firstword $(GRE_VERSION))d
export GRE_VERSION := $(firstword $(GRE_VERSION)).$(word 2,$(GRE_VERSION))

# Last version in debian/changelog
DEBIAN_SRC_VERSION := $(shell dpkg-parsechangelog | sed -n 's/^\(Source\|Version\): *// p')
DEBIAN_SOURCE := $(firstword $(DEBIAN_SRC_VERSION))
DEBIAN_VERSION := $(word 2, $(DEBIAN_SRC_VERSION))
# Debian part of the above version (anything after the last dash)
DEBIAN_RELEASE := $(lastword $(subst -, ,$(DEBIAN_VERSION)))
# Upstream part of the debian/changelog version (anything before the last dash)
UPSTREAM_RELEASE := $(DEBIAN_VERSION:%-$(DEBIAN_RELEASE)=%)
# Aurora builds have the build id in the upstream part of the debian/changelog version
export MOZ_BUILD_DATE := $(word 2,$(subst +, ,$(UPSTREAM_RELEASE)))
UPSTREAM_RELEASE := $(firstword $(subst +, ,$(UPSTREAM_RELEASE)))

# Check if the version in debian/changelog matches actual upstream version
# as VERSION_FILTER transforms it.
FILTERED_UPSTREAM_VERSION := $(shell echo $(UPSTREAM_VERSION) | $(VERSION_FILTER))
ifneq ($(FILTERED_UPSTREAM_VERSION),$(subst esr,,$(firstword $(subst ~b, ,$(UPSTREAM_RELEASE)))))
$(error Upstream version in debian/changelog ($(UPSTREAM_RELEASE)) does not match actual upstream version ($(FILTERED_UPSTREAM_VERSION)))
endif

VERSION = $(UPSTREAM_RELEASE)
SOURCE_TARBALL = $(DEBIAN_SOURCE)_$(VERSION)$(SOURCE_BUILD_DATE:%=+%).orig.tar.bz2
SOURCE_TARBALL_LOCATION = ..

SOURCE_VERSION = $(subst ~,,$(VERSION))

# Find the right channel corresponding to the version number
ifneq (,$(filter suite mail calendar,$(PRODUCT)))
REPO_PREFIX = comm
else
REPO_PREFIX = mozilla
endif
ifneq (,$(findstring esr, $(VERSION)))
SOURCE_TYPE := releases
SOURCE_CHANNEL := $(REPO_PREFIX)-esr$(firstword $(subst ., ,$(VERSION)))
L10N_CHANNEL := $(REPO_PREFIX)-release
else
ifneq (,$(findstring ~b, $(VERSION)))
# Betas are under releases/
SOURCE_TYPE := releases
SOURCE_CHANNEL := $(REPO_PREFIX)-beta
else
ifneq (,$(filter %~a2, $(VERSION)))
# Aurora
SOURCE_TYPE := nightly
SOURCE_CHANNEL := $(REPO_PREFIX)-aurora
else
ifneq (,$(filter %~a1, $(VERSION)))
# Nightly
SOURCE_TYPE := nightly
SOURCE_CHANNEL := $(REPO_PREFIX)-central
L10N_REPO := http://hg.mozilla.org/l10n-central
else
# Release
SOURCE_TYPE := releases
SOURCE_CHANNEL := $(REPO_PREFIX)-release
endif
endif
endif
endif
ifndef L10N_CHANNEL
L10N_CHANNEL := $(SOURCE_CHANNEL)
endif

BASE_URL = ftp://ftp.mozilla.org/pub/mozilla.org/$(PRODUCT_NAME)/$(SOURCE_TYPE)

L10N_FILTER = awk '(NF == 1 || /linux/) && $$1 != "en-US" { print $$1 }'
$(call lazy,L10N_LANGS,$$(shell $$(L10N_FILTER) $(PRODUCT)/locales/shipped-locales))
ifeq ($(SOURCE_TYPE),releases)
SOURCE_URL = $(BASE_URL)/$(SOURCE_VERSION)/source/$(PRODUCT_NAME)-$(SOURCE_VERSION).source.tar.bz2
SOURCE_REV = $(call uc,$(PRODUCT_NAME))_$(subst .,_,$(SOURCE_VERSION))_RELEASE
L10N_REV = $(SOURCE_REV)
SOURCE_REPO = http://hg.mozilla.org/releases/$(SOURCE_CHANNEL)
else
ifeq ($(SOURCE_TYPE),nightly)
$(call lazy,LATEST_NIGHTLY,$$(shell $$(PYTHON) debian/latest_nightly.py $(BASE_URL)/latest-$(SOURCE_CHANNEL)))
SOURCE_BUILD_DATE = $(firstword $(LATEST_NIGHTLY))
SOURCE_URL = $(subst /rev/,/archive/,$(word 2, $(LATEST_NIGHTLY))).tar.bz2
SOURCE_REV = $(patsubst %.tar.bz2,%,$(notdir $(SOURCE_URL)))
L10N_REV = tip
SOURCE_REPO = $(patsubst %/,%,$(dir $(patsubst %/,%,$(dir $(SOURCE_URL)))))
endif
endif

ifndef L10N_REPO
L10N_REPO := $(subst $(SOURCE_CHANNEL),l10n/$(L10N_CHANNEL:$(REPO_PREFIX)-%=mozilla-%),$(SOURCE_REPO))
endif

ifneq (,$(filter import download,$(MAKECMDGOALS)))
ifneq (,$(filter-out $(VERSION),$(UPSTREAM_RELEASE))$(filter $(SOURCE_CHANNEL),$(REPO_PREFIX)-aurora $(REPO_PREFIX)-central))
$(call lazy,L10N_LANGS,$$(shell curl -s $(SOURCE_REPO)/raw-file/$(SOURCE_REV)/$(PRODUCT)/locales/shipped-locales | $$(L10N_FILTER)))
endif
L10N_TARBALLS = $(foreach lang,$(L10N_LANGS),$(SOURCE_TARBALL_LOCATION)/$(SOURCE_TARBALL:%.orig.tar.bz2=%.orig-l10n-$(lang).tar.bz2))

ALL_TARBALLS = $(SOURCE_TARBALL_LOCATION)/$(SOURCE_TARBALL) $(L10N_TARBALLS) $(SOURCE_TARBALL_LOCATION)/$(SOURCE_TARBALL:%.orig.tar.bz2=%.orig-compare-locales.tar.bz2)

download: $(ALL_TARBALLS)

import: $(ALL_TARBALLS)
	$(PYTHON) debian/import-tar.py $(addprefix -H ,$(BRANCH)) $< | git fast-import

$(SOURCE_TARBALL_LOCATION)/$(SOURCE_TARBALL): debian/source.filter
	$(PYTHON) debian/repack.py -o $@ $(SOURCE_URL)

$(L10N_TARBALLS): $(SOURCE_TARBALL_LOCATION)/$(SOURCE_TARBALL:%.orig.tar.bz2=%.orig-l10n-%.tar.bz2): debian/l10n.filter
	$(PYTHON) debian/repack.py -o $@ -t $* -f debian/l10n.filter $(L10N_REPO)/$*/archive/$(L10N_REV).tar.bz2

$(SOURCE_TARBALL_LOCATION)/$(SOURCE_TARBALL:%.orig.tar.bz2=%.orig-compare-locales.tar.bz2): debian/l10n.filter
	$(PYTHON) debian/repack.py -o $@ -t compare-locales -f debian/l10n.filter http://hg.mozilla.org/build/compare-locales/archive/$(L10N_REV).tar.bz2 > $@

endif
.PHONY: download
