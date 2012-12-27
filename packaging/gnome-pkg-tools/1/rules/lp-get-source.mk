# copied from CDBS' buildvars
DEB_SOURCE_PACKAGE ?= $(strip $(shell egrep '^Source: ' debian/control | cut -f 2 -d ':'))
DEB_VERSION ?= $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')
DEB_NOEPOCH_VERSION ?= $(shell echo $(DEB_VERSION) | cut -d: -f2-)
DEB_UPSTREAM_VERSION ?= $(shell echo $(DEB_NOEPOCH_VERSION) | sed 's/-[^-]*$$//')

# where to store the resulting .orig tarball
DEB_TARBALL_DOWNLOAD_DIR ?= .
TARBALL_EXT ?= tar.gz

LP_DOWNLOAD_PAGE ?= https://launchpad.net/$(DEB_SOURCE_PACKAGE)/+download
LP_DOWNLOAD_URL ?= $(shell wget -q -O - $(LP_DOWNLOAD_PAGE) | grep -o 'href="http://launchpad.net/$(DEB_SOURCE_PACKAGE)/.*/+download/$(DEB_SOURCE_PACKAGE)-$(DEB_UPSTREAM_VERSION)\.$(TARBALL_EXT)"' | cut -d'"' -f2)

LP_DOWNLOAD_COMMAND ?= wget -T15 -t1 -O $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) $(LP_DOWNLOAD_URL)

get-orig-source:
	dh_testdir
	@@echo 'Source package:                      $(DEB_SOURCE_PACKAGE)'
	@@echo 'Upstream version:                    $(DEB_UPSTREAM_VERSION)'
	@@echo 'Download page:                       $(LP_DOWNLOAD_PAGE)'
	@@echo 'Download url:                        $(LP_DOWNLOAD_URL)'
	mkdir -p $(DEB_TARBALL_DOWNLOAD_DIR)
	$(LP_DOWNLOAD_COMMAND)

.PHONY: get-orig-source
