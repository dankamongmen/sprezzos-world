# In the default configuration, including this Makefile will allow uploads to
# experimental, prevent upload to unstable, and warn in other cases

# target distribution
DEB_DISTRIBUTION ?= $(shell LC_ALL=C dpkg-parsechangelog | sed -n 's/^Distribution: //p')

ALLOWED_DISTS ?= experimental
DISALLOWED_DISTS ?= unstable

ifeq ($(findstring $(DEB_DISTRIBUTION),$(ALLOWED_DISTS)),$(DEB_DISTRIBUTION))
# distribution is listed in ALLOWED_DISTS; skip
$(info Allowed distribution: $(DEB_DISTRIBUTION))
else
ifeq ($(findstring $(DEB_DISTRIBUTION),$(DISALLOWED_DISTS)),$(DEB_DISTRIBUTION))
# distribution is listed in DISALLOWED_DISTS; fail
$(error Disallowed distribution: $(DEB_DISTRIBUTION))
else
# unknown distribution; warn
$(warning Unknown distribution: $(DEB_DISTRIBUTION))
endif
endif

