# copied from CDBS' buildvars
DEB_SOURCE_PACKAGE ?= $(strip $(shell egrep '^Source: ' debian/control | cut -f 2 -d ':'))
DEB_VERSION ?= $(shell dpkg-parsechangelog | egrep '^Version:' | cut -f 2 -d ' ')
DEB_NOEPOCH_VERSION ?= $(shell echo $(DEB_VERSION) | cut -d: -f2-)
DEB_UPSTREAM_VERSION ?= $(shell echo $(DEB_NOEPOCH_VERSION) | sed 's/-[^-]*$$//')

# compute the major.minor part of the upstream version
DEB_UPSTREAM_VERSION_MAJOR_MINOR := $(shell echo $(DEB_UPSTREAM_VERSION) | sed -r -n 's/^([0-9]+\.[0-9]+).*/\1/p')

# search for a GIT revision in the version of the changelog
# accepted formats: foo+git20090430.42ad43 (or ~ instead of +)
DEB_UPSTREAM_GIT_REV ?= $(shell echo $(DEB_UPSTREAM_VERSION) | sed -rn 's/^.*[\.~+\d]+git[0-9]+\.([0-9a-f]+)$$/\1/p')

# where to store the resulting .orig tarball
DEB_TARBALL_DOWNLOAD_DIR ?= .

# whether to generate a tarball in tarball (dbs format); default is to use the
# upstream tarball, set to non-empty to change this behavior
DEB_USE_DBS_TARBALL_LAYOUT ?=

DEB_SOURCE_FORMAT := $(shell cat debian/source/format 2>/dev/null || echo 1.0)

ifeq ($(DEB_SOURCE_FORMAT),3.0 (quilt))
  TARBALL_EXT ?= tar.xz
else
  TARBALL_EXT ?= tar.gz
endif

GNOME_MODULE ?= $(DEB_SOURCE_PACKAGE)
GNOME_TARBALL ?= $(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION).$(TARBALL_EXT)
GNOME_DOWNLOAD_URL ?= http://ftp.gnome.org/pub/GNOME/sources/$(GNOME_MODULE)/$(DEB_UPSTREAM_VERSION_MAJOR_MINOR)/$(GNOME_TARBALL)
GNOME_DOWNLOAD_COMMAND ?= wget -T10 -t3 -O $(if $(DEB_USE_DBS_TARBALL_LAYOUT),$(DEB_TARBALL_DOWNLOAD_DIR)/$(GNOME_TARBALL),$(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT)) $(GNOME_DOWNLOAD_URL)

GNOME_GIT_URL ?= git://git.gnome.org/$(GNOME_MODULE)

get-orig-source:
	dh_testdir
	@@echo 'Source package:                      $(DEB_SOURCE_PACKAGE)'
	@@echo 'Source version:                      $(DEB_VERSION)'
	@@echo 'Source version without epoch:        $(DEB_NOEPOCH_VERSION)'
	@@echo 'Upstream version (for orig tarball): $(DEB_UPSTREAM_VERSION)'
	# create tarball dir
	mkdir -p $(DEB_TARBALL_DOWNLOAD_DIR)
ifeq ($(DEB_UPSTREAM_GIT_REV),)
	$(GNOME_DOWNLOAD_COMMAND)
	@case "$(TARBALL_EXT)" in \
	tar.bz2|tbz) \
	  if [ "$(DEB_SOURCE_FORMAT)" != "3.0 (quilt)" ]; then \
	    echo "$(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) -> $(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz ...";\
	    bzcat $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) | \
	    gzip -9 >$(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz ; \
	    rm -f $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT); \
	  fi \
	  ;;\
	tar.xz) \
	  if [ "$(DEB_SOURCE_FORMAT)" != "3.0 (quilt)" ]; then \
	    echo "$(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) -> $(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz ...";\
	    xzcat $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) | \
	    gzip -9 >$(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz ; \
	    rm -f $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT); \
	  fi \
	  ;;\
	zip) \
	  echo "$(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) -> $(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz ...";\
	  unzip -q $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) \
	    -d $(DEB_TARBALL_DOWNLOAD_DIR)/$(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION) ;\
	    (cd $(DEB_TARBALL_DOWNLOAD_DIR);\
	      if [ -d $(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION)/$(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION) ]; then \
	        cd $(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION); \
	        tar cfz ../$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz $(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION);\
	      else \
	        tar cfz $(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz $(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION); \
	      fi \
	     );\
	  rm -rf $(DEB_TARBALL_DOWNLOAD_DIR)/$(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.$(TARBALL_EXT) \
	    $(DEB_TARBALL_DOWNLOAD_DIR)/$(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION);\
	  ;;\
	esac
else
	@@echo 'Upstream GIT rev:                    $(DEB_UPSTREAM_GIT_REV)'
	@@echo 'Upstream GIT repo:                   $(GNOME_GIT_URL)'
	@@git clone $(GNOME_GIT_URL) \
	    $(DEB_TARBALL_DOWNLOAD_DIR)/$(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION)
	@@cd $(DEB_TARBALL_DOWNLOAD_DIR)/$(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION); \
	    git checkout $(DEB_UPSTREAM_GIT_REV); \
	    $(RM) -rf .git
	@@(cd $(DEB_TARBALL_DOWNLOAD_DIR);\
	    tar cfz $(DEB_SOURCE_PACKAGE)_$(DEB_UPSTREAM_VERSION).orig.tar.gz \
	    $(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION) \
	  )
	@@$(RM) -rf $(DEB_TARBALL_DOWNLOAD_DIR)/$(GNOME_MODULE)-$(DEB_UPSTREAM_VERSION)
endif

.PHONY: get-orig-source
