# copied from CDBS' buildvars
DEB_SOURCE_PACKAGE ?= $(strip $(shell egrep '^Source: ' debian/control | cut -f 2 -d ':'))
DEB_VERSION ?= $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')
DEB_NOEPOCH_VERSION ?= $(shell echo $(DEB_VERSION) | cut -d: -f2-)
DEB_UPSTREAM_VERSION ?= $(shell echo $(DEB_NOEPOCH_VERSION) | sed 's/-[^-]*$$//')

# where to store the resulting .orig tarball
DEB_TARBALL_DOWNLOAD_DIR ?= .
TARBALL_EXT ?= tar.gz

SF_PROJECT ?= $(DEB_SOURCE_PACKAGE)
SF_MODULE ?= $(DEB_SOURCE_PACKAGE)
SF_TARBALL ?= $(SF_MODULE)-$(DEB_UPSTREAM_VERSION).$(TARBALL_EXT)
SF_DOWNLOAD_URL ?= http://$$sf_mirror.dl.sourceforge.net/$(SF_PROJECT)/$(SF_TARBALL)
SF_MIRRORS ?= aarnet cdnetworks-kr-1 cdnetworks-kr-2 cdnetworks-us-1 citylan dfn freefr garr heanet hivelocity ignum internode iweb jaist kent nchc ncu netcologne softlayer space sunet superb-dca2 superb-sea2 switch ufpr voxel waix
SF_DOWNLOAD_COMMAND ?= for sf_mirror in $(SF_MIRRORS); do wget -nc -T10 -t1 -O $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) $(SF_DOWNLOAD_URL) && break; done

get-orig-source:
	dh_testdir
	@@echo 'Source package:                      $(DEB_SOURCE_PACKAGE)'
	@@echo 'Source package:                      $(DEB_VERSION)'
	@@echo 'Source version without epoch:        $(DEB_NOEPOCH_VERSION)'
	@@echo 'Upstream version (for orig tarball): $(DEB_UPSTREAM_VERSION)'
	mkdir -p $(DEB_TARBALL_DOWNLOAD_DIR)
	$(SF_DOWNLOAD_COMMAND)
	case "$(TARBALL_EXT)" in \
	tar.bz2|tbz) \
	  bzcat $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) | \
	  gzip -9 >$(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz ; \
	  rm -f $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) \
	  ;;\
	esac

.PHONY: get-orig-source
