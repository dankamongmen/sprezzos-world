# copied from CDBS' buildvars
DEB_SOURCE_PACKAGE ?= $(strip $(shell egrep '^Source: ' debian/control | cut -f 2 -d ':'))
DEB_VERSION ?= $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')
DEB_NOEPOCH_VERSION ?= $(shell echo $(DEB_VERSION) | cut -d: -f2-)
DEB_UPSTREAM_VERSION ?= $(shell echo $(DEB_NOEPOCH_VERSION) | sed 's/-[^-]*$$//')

DEB_SOURCE_PACKAGE_INITIAL ?= $(shell echo $(DEB_SOURCE_PACKAGE) | cut -c 1)

# where to store the resulting .orig tarball
DEB_TARBALL_DOWNLOAD_DIR ?= .

UBUNTU_COMPONENT ?= main
# defaults to non-native; you don't need to set this as both are tried
UBUNTU_NATIVE ?=
UBUNTU_DOWNLOAD_URL ?= http://archive.ubuntu.com/ubuntu/pool/$(UBUNTU_COMPONENT)/$(DEB_SOURCE_PACKAGE_INITIAL)/$(DEB_SOURCE_PACKAGE)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION)$(if $(filter 1 true yes,$(UBUNTU_NATIVE)),,.orig).tar.gz

UBUNTU_DOWNLOAD_COMMAND ?= wget -N -T10 -t1 -O $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz $(UBUNTU_DOWNLOAD_URL)

get-orig-source:
	dh_testdir
	@@echo 'Source package:                      $(DEB_SOURCE_PACKAGE)'
	@@echo 'Source package:                      $(DEB_VERSION)'
	@@echo 'Source version without epoch:        $(DEB_NOEPOCH_VERSION)'
	@@echo 'Upstream version (for orig tarball): $(DEB_UPSTREAM_VERSION)'
	mkdir -p $(DEB_TARBALL_DOWNLOAD_DIR)
	# try to download non-native, then native, or fail
	$(foreach UBUNTU_NATIVE,no yes,$(UBUNTU_DOWNLOAD_COMMAND) ||) false

.PHONY: get-orig-source
