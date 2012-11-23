# Makefile -- For the nvidia module

#
# By default, the build is done against the running linux kernel source.
# To build against a different kernel source tree, set LINUXDIR:
#
#    make LINUXDIR=/path/to/kernel/source

SHELL=/bin/sh

.SUFFIXES:

ifndef LINUXDIR
RUNNING_REL := $(shell uname -r)

LINUXDIR := $(shell if [ -e /lib/modules/$(RUNNING_REL)/source ]; then \
		 echo /lib/modules/$(RUNNING_REL)/source; \
		 else echo /lib/modules/$(RUNNING_REL)/build; fi)
endif

ifndef O
O := $(shell if [ -e /lib/modules/$(RUNNING_REL)/build ]; then \
		 echo /lib/modules/$(RUNNING_REL)/build; \
		 else echo ""; fi)
#O := $(LINUXDIR)
endif

ifdef ARCH
MACHINE := $(ARCH)
else
MACHINE := $(shell uname -m)
endif

# Modules for all architectures
MODULE_LIST := nvidia.o

CLEANFILES = *.o *.ko .depend .*.flags .*.d .*.cmd *.mod.c .tmp_versions

# VERSION is not defined from the initial invocation.  It is defined when
# this Makefile is invoked from the kernel's root Makefile.

ifndef VERSION

ifdef RUNNING_REL

# SuSE has the version.h and autoconf.h headers for the current kernel
# in /boot as /boot/vmlinuz.version.h and /boot/vmlinuz.autoconf.h.
# Check these first to see if they match the running kernel.

BOOTVERSION_PREFIX = /boot/vmlinuz.

VV := $(shell if [ -f $(BOOTVERSION_PREFIX)version.h ]; then \
	grep UTS_RELEASE $(BOOTVERSION_PREFIX)version.h | \
	cut -d' ' -f3; fi)

ifeq ($(VV),"$(RUNNING_REL)")
HEADERFROMBOOT := 1
GETCONFIG := MAKEFILES=$(shell /bin/pwd)/.config
HAVECONFIG := y
endif

# On Red Hat we need to check if there is a .config file in the kernel
# source directory.  If there isn't, we need to check if there's a
# matching file in the configs subdirectory.

ifneq ($(HAVECONFIG),y)
HAVECONFIG := $(shell if [ -e $(LINUXDIR)/.config ]; then echo y; fi)
endif

ifneq ($(HAVECONFIG),y)
REL_BASE := $(shell echo $(RUNNING_REL) | sed 's/-.*//')
REL_TYPE := $(shell echo $(RUNNING_REL) | sed 's/[0-9.-]//g')
ifeq ($(REL_TYPE),)
RHCONFIG := configs/kernel-$(REL_BASE)-$(MACHINE).config
else
RHCONFIG := configs/kernel-$(REL_BASE)-$(MACHINE)-$(REL_TYPE).config
endif
HAVECONFIG := $(shell if [ -e $(LINUXDIR)/$(RHCONFIG) ]; then echo y; fi)
ifneq ($(HAVECONFIG),y)
RHCONFIG :=
endif
endif

ifneq ($(HAVECONFIG),y)
ifneq ($(0),$(LINUXDIR))
GETCONFIG += O=$(O)
endif
HAVECONFIG := $(shell if [ -e $(O)/.config ]; then echo y; fi)
endif

ifneq ($(HAVECONFIG),y)
$(error Cannot find a kernel config file)
endif

endif

CLEANCONFIG := $(shell if cmp -s $(LINUXDIR)/.config .config; then echo y; fi)
ifeq ($(CLEANCONFIG),y)
CLEANFILES += $(LINUXDIR)/.config .config $(LINUXDIR)/tmp_include_depends
endif

all: modules

modules: includes
	+make -C $(LINUXDIR) $(GETCONFIG) M=`/bin/pwd` modules

ifeq ($(HEADERFROMBOOT),1)

BOOTHEADERS = version.h autoconf.h
BOOTCONFIG = .config

CLEANFILES += $(BOOTHEADERS) $(BOOTCONFIG)

includes:: $(BOOTHEADERS) $(BOOTCONFIG)

version.h: $(BOOTVERSION_PREFIX)version.h
	rm -f $@
	ln -s $< $@

autoconf.h: $(BOOTVERSION_PREFIX)autoconf.h
	rm -f $@
	ln -s $< $@

.config: $(BOOTVERSION_PREFIX)config
	rm -f $@
	ln -s $< $@
endif

# This prepares an unused Red Hat kernel tree for the build.
ifneq ($(RHCONFIG),)
includes:: $(LINUXDIR)/.config $(LINUXDIR)/tmp_include_depends .config

$(LINUXDIR)/.config: $(LINUXDIR)/$(RHCONFIG)
	rm -f $@
	ln -s $< $@

.config: $(LINUXDIR)/$(RHCONFIG)
	rm -f $@
	ln -s $< $@

$(LINUXDIR)/tmp_include_depends:
	echo all: > $@
endif

clean cleandir:
	rm -rf $(CLEANFILES)

$(MODULE_LIST)::
	make modules

install:
	make -C $(LINUXDIR) $(GETCONFIG) M=`/bin/pwd` modules_install

else

# Check for kernel versions that we don't support.

BELOW26 := $(shell if [ $(VERSION) -lt 2 -o $(PATCHLEVEL) -lt 6 ]; then \
		echo y; fi)

ifeq ($(BELOW26),y)
$(error Only 2.6.x and later kernels are supported \
	($(VERSION).$(PATCHLEVEL).$(SUBLEVEL)))
endif

# This needs to go before all other include paths.
CC += -I$(M)

ifeq ($(CONFIG_X86_64),y)
NVARCH = .x86_64
else
NVARCH = .i386
endif

include $(M)/Makefile.kbuild

endif

.PHONY: includes
