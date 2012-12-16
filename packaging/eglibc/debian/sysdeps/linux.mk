EGLIBC_OVERLAYS ?= $(shell ls glibc-linuxthreads* glibc-ports* glibc-libidn*)
MIN_KERNEL_SUPPORTED := 2.6.32
libc = libc6

# NPTL Config
threads = yes
libc_add-ons = nptl $(add-ons)

ifeq ($(DEB_BUILD_PROFILE),bootstrap)
  libc_extra_config_options = $(extra_config_options)
else
  libc_extra_config_options = --with-selinux $(extra_config_options)
endif

ifndef LINUX_SOURCE
  ifeq ($(DEB_HOST_GNU_TYPE),$(DEB_BUILD_GNU_TYPE))
    LINUX_HEADERS := /usr/include
  else
    LINUX_HEADERS := /usr/$(DEB_HOST_GNU_TYPE)/include
  endif
  LINUX_ARCH_HEADERS := /usr/include/$(DEB_HOST_MULTIARCH)
else
  LINUX_HEADERS := $(LINUX_SOURCE)/include
endif

# Minimum Kernel supported
with_headers = --with-headers=$(shell pwd)/debian/include --enable-kernel=$(call xx,MIN_KERNEL_SUPPORTED)

KERNEL_HEADER_DIR = $(stamp)mkincludedir
$(stamp)mkincludedir:
	rm -rf debian/include
	mkdir debian/include

	# Kernel headers
	if [ -d "$(LINUX_ARCH_HEADERS)/asm" ]; then \
		ln -s $(LINUX_ARCH_HEADERS)/asm debian/include ; \
	else \
		ln -s $(LINUX_HEADERS)/asm debian/include ; \
	fi
	ln -s $(LINUX_HEADERS)/asm-generic debian/include
	ln -s $(LINUX_HEADERS)/linux debian/include

	# Library headers
	for h in libaudit.h selinux sys/capability.h ; do \
	    mkdir -p debian/include/$$(dirname $$h) ; \
	    if [ -d "/usr/include/$(DEB_HOST_MULTIARCH)/$$h" ]; then \
	        ln -s /usr/include/$(DEB_HOST_MULTIARCH)/$$h debian/include/$$h ; \
	    else \
		ln -s /usr/include/$$h debian/include/$$h ; \
	    fi ; \
	done

	# To make configure happy if libc6-dev is not installed.
	touch debian/include/assert.h

	touch $@

# Also to make configure happy.
export CPPFLAGS = -isystem $(shell pwd)/debian/include

# This round of ugliness decomposes the Linux kernel version number
# into an integer so it can be easily compared and then does so.
CURRENT_KERNEL_VERSION=$(shell uname -r)
define kernel_check
(minimum=$$((`echo $(1) | sed 's/^\([0-9]*\.[0-9]*\)\([^.0-9]\|$$\)/\1.0\2/; s/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\1 \* 10000 + \2 \* 100 + \3/'`)); \
current=$$((`echo $(CURRENT_KERNEL_VERSION) | sed 's/^\([0-9]*\.[0-9]*\)\([^.0-9]\|$$\)/\1.0\2/; s/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\1 \* 10000 + \2 \* 100 + \3/'`)); \
if [ $$current -lt $$minimum ]; then \
  false; \
fi)
endef
