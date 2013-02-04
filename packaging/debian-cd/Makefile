#!/usr/bin/make -f

# Main Makefile for debian-cd
#
# Copyright 1999 Rapha?l Hertzog <hertzog@debian.org>
# See the README file for the license
#
# Significantly modified 2005-2006 Steve McIntyre <93sam@debian.org>
# for multi-arch and mixed bin/src discs
#
# The environment variables must have been set
# before. For this you can source the CONF.sh 
# file in your shell


## DEFAULT VALUES
ifndef VERBOSE_MAKE
Q=@
endif
ifndef TASK
TASK=Debian-generic
endif
ifndef MKISOFS
export MKISOFS=$(shell which genisoimage mkisofs | head -1)
endif
ifndef MKISOFS_OPTS
#For normal users
MKISOFS_OPTS=-r
#For symlink farmers
#MKISOFS_OPTS=-r -F .
endif
ifndef HOOK
HOOK=$(BASEDIR)/tools/$(CODENAME).hook
endif

export BUILD_DATE=$(shell date -u +%Y%m%d-%H:%M)
export ARCHES_NOSRC=$(shell echo $(ARCHES) | sed 's/source//')
ifeq ($(ARCHES),source)
	export SOURCEONLY=yes
endif
ifeq ($(shell echo $(ARCHES) | sed 's/.*source.*/1/'),1)
	export INC_SOURCE=yes
endif

UNDER_ARCHES=$(shell echo $(ARCHES) | sed 's/\ /_/g')
ARCH_MKISOFS = ${${UNDER_ARCHES}_MKISOFS}
ARCH_MKISOFS_OPTS = ${${UNDER_ARCHES}_MKISOFS_OPTS}
ifneq (${ARCH_MKISOFS},)
    MKISOFS = ${ARCH_MKISOFS}
endif
ifneq (${ARCH_MKISOFS_OPTS},)
    MKISOFS_OPTS = ${ARCH_MKISOFS_OPTS}
endif

## Internal variables  
apt=$(BASEDIR)/tools/apt-selection
sort_deps=$(BASEDIR)/tools/sort_deps
md5sum=md5sum
jigdo_cleanup=$(BASEDIR)/tools/jigdo_cleanup
grab_md5=$(BASEDIR)/tools/grab_md5
make_image=$(BASEDIR)/tools/make_image
merge_package_lists=$(BASEDIR)/tools/merge_package_lists
update_popcon=$(BASEDIR)/tools/update_popcon
update_tasks=$(BASEDIR)/tools/update_tasks
grab_source_list=$(BASEDIR)/tools/grab_source_list
which_deb=$(BASEDIR)/tools/which_deb

BDIR=$(TDIR)/$(CODENAME)
TASKDIR=$(BDIR)/tasks
ADIR=$(APTTMP)
DB_DIR=$(BDIR)/debootstrap

export DEBOOTSTRAP_DIR := $(DB_DIR)/usr/lib/debootstrap
export PATH := $(DB_DIR)/usr/sbin:$(PATH)
export BDIR
export TASKDIR

## DEBUG STUFF ##

default:
	@echo "Please refer to the README file for more information"
	@echo "about the different targets available."

## CHECKS ##

# Basic checks in order to avoid problems
ok:
ifdef FORCE_FAIL
	@echo Debug variable FORCE_FAIL defined -- abort now; false
endif
ifndef TDIR
	@echo TDIR undefined -- set up CONF.sh; false
endif
ifndef BASEDIR
	@echo BASEDIR undefined -- set up CONF.sh; false
endif
ifndef MIRROR
	@echo MIRROR undefined -- set up CONF.sh; false
endif
ifndef ARCHES
	@echo ARCHES undefined -- set up CONF.sh; false
endif
ifndef CODENAME
	@echo CODENAME undefined -- set up CONF.sh; false
endif
ifndef OUT
	@echo OUT undefined -- set up CONF.sh; false
endif
ifdef NONFREE
ifdef EXTRANONFREE
	@echo Never use NONFREE and EXTRANONFREE at the same time; false
endif
endif
	@if [ $(DISKTYPE) = "NETINST" -o $(DISKTYPE) = "BC" ] ; then \
		if [ "$(INC_SOURCE)"x = "yes"x ] ; then \
			echo "Including source is not supported on a netinst/bc CD"; \
			false; \
		fi; \
	fi

## INITIALIZATION ##

# Creation of the directories needed
init: ok $(OUT) $(TDIR) $(BDIR) $(ADIR) $(TASKDIR) $(BDIR)/DATE $(DB_DIR) unstable-map
$(OUT):
	$(Q)mkdir -p $(OUT)
$(TDIR):
	$(Q)mkdir -p $(TDIR)
$(BDIR):
	$(Q)mkdir -p $(BDIR)
$(ADIR):
	$(Q)mkdir -p $(ADIR)
$(TASKDIR):
ifneq ($(ARCHES),source)
	$(Q)echo "Updating task files..."
	$(Q)mkdir -p $(TASKDIR)
	$(Q)echo "- copying task files from 'tasks/$(DI_CODENAME)/'"
	$(Q)cp -r $(BASEDIR)/tasks/$(CODENAME)/* $(TASKDIR)
	$(Q)echo "- generating dynamic task files"
	$(Q)set -e; cd $(TASKDIR); \
		$(BASEDIR)/tools/update_tasks; \
		$(BASEDIR)/tools/generate_di_list; \
		$(BASEDIR)/tools/generate_di+k_list
ifeq ($(FORCE_FIRMWARE),1)
	# Generate firmware task file using the contents of the archive
	$(Q)$(BASEDIR)/tools/generate_firmware_task "$(ARCHES)" $(TASKDIR)/firmware
endif
endif
$(BDIR)/DATE:
	$(Q)date -u '+%Y%m%d' > $(BDIR)/DATE
	$(Q)date -u '+%Y%m%dT%H%M%SZ' > $(BDIR)/DATE-zulu

ifdef MIRROR
LATEST_DB := $(MIRROR)/$(shell $(which_deb) $(MIRROR) $(CODENAME) debootstrap)
$(DB_DIR): $(LATEST_DB)
	@rm -rf $(DB_DIR)
	$(Q)dpkg -x $(LATEST_DB) $(DB_DIR)
	$(Q)if [ ! -e $(DEBOOTSTRAP_DIR) ] ; then \
		ln -sf share $(DB_DIR)/usr/lib ; \
	fi
endif

# Make sure unstable/sid points to testing/wheezy, as there is no build
# rule for unstable/sid.
unstable-map:
	$(Q)if [ ! -d $(BASEDIR)/data/sid ] ; then \
		ln -s wheezy $(BASEDIR)/data/sid ; \
	fi
	$(Q)if [ ! -d $(BASEDIR)/tools/boot/sid ] ; then \
		ln -s wheezy $(BASEDIR)/tools/boot/sid ; \
	fi

#################
## CLEAN RULES ##
#################

# Cleans the current arch tree (but not packages selection info)
clean: ok dir-clean
dir-clean:
	$(Q)rm -rf $(BDIR)/CD[1234567890]*
	$(Q)rm -rf $(TASKDIR)
	$(Q)rm -f $(BDIR)/*.filelist*
	$(Q)rm -f  $(BDIR)/packages-stamp $(BDIR)/upgrade-stamp $(BDIR)/md5-check

# Completely cleans the current arch tree
realclean: distclean
distclean: ok clean
	$(Q)echo "Cleaning the build directory"
	$(Q)rm -rf $(ADIR)
	$(Q)rm -rf $(TDIR)

####################
## STATUS and APT ##
####################

$(CODENAME)_status: ok init
	$(Q)for ARCH in $(ARCHES_NOSRC); do \
		echo "Using the provided status file for $(CODENAME)-$$ARCH ..."; \
		cp $(BASEDIR)/data/$(CODENAME)/status.$$ARCH $(ADIR)/$(CODENAME)-$$ARCH/status 2>/dev/null || $(MAKE) status || $(MAKE) correctstatus ; \
	done

# Regenerate the status file with only packages that
# are of priority standard or higher
status: init $(ADIR)/status
$(ADIR)/status:
	@echo "Generating a fake status file for apt-get and apt-cache..."
	$(Q)for ARCH in $(ARCHES); do \
		mkdir -p $(ADIR)/$(CODENAME)-$$ARCH/apt/preferences.d; \
		if [ $$ARCH = "source" -o "$(INSTALLER_CD)" = "1" -o "$(INSTALLER_CD)" = "2" -o "$(INSTALLER_CD)" = "C" ];then \
			:> $(ADIR)/$(CODENAME)-$$ARCH/status ; \
		else \
			zcat $(MIRROR)/dists/$(CODENAME)/main/binary-$$ARCH/Packages.gz | \
			perl -000 -ne 's/^(Package: .*)$$/$$1\nStatus: install ok installed/m; print if (/^Priority: (required|important|standard)/m or /^Section: base/m);' \
			>> $(ADIR)/$(CODENAME)-$$ARCH/status ; \
		fi; \
	done;
	:> $(ADIR)/status
    # Updating the apt database
	$(Q)for ARCH in $(ARCHES); do \
		export ARCH=$$ARCH; \
		$(apt) update; \
	done
    #
    # Checking the consistency of the standard system
    # If this does fail, then launch make correctstatus
    #
	$(Q)for ARCH in $(ARCHES); do \
		export ARCH=$$ARCH; \
		$(apt) check || $(MAKE) correctstatus; \
	done

# Only useful if the standard system is broken
# It tries to build a better status file with apt-get -f install
correctstatus: status apt-update
    # You may need to launch correctstatus more than one time
    # in order to correct all dependencies
    #
    # Removing packages from the system :
	$(Q)set -e; \
	if [ "$(ARCHES)" != "source" ] ; then \
		for ARCH in $(ARCHES_NOSRC); do \
			export ARCH=$$ARCH; \
			for i in `$(apt) deselected -f install`; do \
				echo $$ARCH:$$i; \
				perl -i -000 -ne "print unless /^Package: \Q$$i\E/m" \
				$(ADIR)/$(CODENAME)-$$ARCH/status; \
			done; \
		done; \
    fi
    #
    # Adding packages to the system :
	$(Q)set -e; \
	if [ "$(ARCHES)" != "source" ] ; then \
		for ARCH in $(ARCHES_NOSRC); do \
			export ARCH=$$ARCH; \
			for i in `$(apt) selected -f install`; do \
				echo $$ARCH:$$i; \
				$(apt) cache dumpavail | perl -000 -ne \
				"s/^(Package: .*)\$$/\$$1\nStatus: install ok installed/m; \
				print if /^Package: \Q$$i\E\s*\$$/m;" \
				>> $(ADIR)/$(CODENAME)-$$ARCH/status; \
			done; \
		done; \
    fi
    #
    # Showing the output of apt-get check :
	$(Q)for ARCH in $(ARCHES_NOSRC); do \
		ARCH=$$ARCH $(apt) check; \
	done

apt-update: status
	$(Q)if [ "$(ARCHES)" != "source" ] ; then \
		for ARCH in $(ARCHES); do \
			echo "Apt-get is updating its files ..."; \
			ARCH=$$ARCH $(apt) update; \
		done; \
    fi

## GENERATING LISTS ##

# Deleting the list only
deletelist: ok
	$(Q)-rm $(BDIR)/rawlist
	$(Q)-rm $(BDIR)/list

packagelists: ok apt-update genlist

# Build the raw list (cpp output) with doubles and spaces
$(BDIR)/rawlist:
# Dirty workaround for saving space, we add some hints to break ties.
# This is just a temporal solution, sort_deps should be a little bit less
# silly so that this is not needed. For more info have a look at
# http://lists.debian.org/debian-cd/2004/debian-cd-200404/msg00093.html
	$(Q)if [ "$(SOURCEONLY)"x != "yes"x ] ; then \
		if [ "$(INSTALLER_CD)"x = "1"x ] ; then \
			: ; \
		elif [ "$(INSTALLER_CD)"x = "2"x -o "$(INSTALLER_CD)"x = "C"x ] ; then \
			echo "mawk" >>$(BDIR)/rawlist; \
		else \
			echo "mawk" >>$(BDIR)/rawlist; \
			echo "exim4-daemon-light" >>$(BDIR)/rawlist; \
		fi; \
	fi

	$(Q)if [ "$(SOURCEONLY)"x != "yes"x ] ; then \
		if [ _$(INSTALLER_CD) != _1 ]; then \
			for ARCH in $(ARCHES_NOSRC); do \
				BINCLUDE=`[ -n "$(BASE_INCLUDE)" ] && cat $(BASE_INCLUDE) | tr "\n" "," | sed 's!,$$!!g'`; \
				[ -z "$$BINCLUDE" ] || BINCLUDE="--include=$$BINCLUDE"; \
				BEXCLUDE=`[ -n "$(BASE_EXCLUDE)" ] && cat $(BASE_EXCLUDE) | tr "\n" "," | sed 's!,$$!!g'`; \
				[ -z "$$BEXCLUDE" ] || BEXCLUDE="--exclude=$$BEXCLUDE"; \
				debootstrap --arch $$ARCH \
				            --print-debs \
				            $$BINCLUDE $$BEXCLUDE \
				            $(CODENAME) \
				            $(TDIR)/debootstrap.tmp \
				            file:$(MIRROR) \
			                $(DEBOOTSTRAP_SCRIPT) 2>/dev/null \
				    | tr ' ' '\n' > $(BDIR)/debootstrap-list; \
				cat $(BDIR)/debootstrap-list >>$(BDIR)/rawlist; \
				rm -rf $(TDIR)/debootstrap.tmp; \
			done; \
		fi; \
	fi

	$(Q)for ARCH in $(ARCHES_NOSRC); do \
		ARCHDEFS="$$ARCHDEFS -D ARCH_`echo $$ARCH | sed 's/-/_/'`"; \
		ARCHUNDEFS="$$ARCHUNDEFS -U $$ARCH"; \
	done; \
	for VARIANT in $(VARIANTS); do \
		VARIANTDEFS="$$VARIANTDEFS -D VARIANT_$$VARIANT"; \
	done; \
	if [ "$(FORCE_FIRMWARE)"x = "1"x ] ; then \
		ARCHDEFS="$$ARCHDEFS -DFORCE_FIRMWARE"; \
	fi; \
	if [ "$(EXCLUDE_486_KERNEL)"x = "1"x ] ; then \
		ARCHDEFS="$$ARCHDEFS -DARCH_i386_EXCLUDE_486_KERNEL"; \
	fi; \
	if [ "$(SOURCEONLY)"x != "yes"x ] ; then \
		cat $(TASKDIR)/$(TASK) | \
		cpp -nostdinc -nostdinc++ -P -undef $$ARCHDEFS $$VARIANTDEFS\
	   		$$ARCHUNDEFS -U i386 -U linux -U unix \
		    -DFORCENONUSONCD1=0 \
		    -I $(TASKDIR) - - >> $(BDIR)/rawlist; \
	fi

    # If we're *only* doing source, then we need to build a list of all the
    # available source packages. Deliberately ignore the tasks too.
	$(Q)if [ "$(SOURCEONLY)"x = "yes"x ] ; then \
		awk '/^Package:/ {print $$2}' $(ADIR)/$(CODENAME)-source/apt-state/lists/*Sources | \
			sort -u > $(BDIR)/rawlist; \
	fi
#	ls -al $(BDIR)/rawlist

# Generate the complete listing of packages from the task
# Build a nice list without doubles and without spaces
genlist: ok $(BDIR)/list
$(BDIR)/list: $(BDIR)/rawlist
	@echo "Generating the complete list of packages to be included in $(BDIR)/list..."
	$(Q)perl -ne 'chomp; next if /^\s*$$/; \
	          print "$$_\n" if not $$seen{$$_}; $$seen{$$_}++;' \
		  $(BDIR)/rawlist \
		  > $(BDIR)/list

## IMAGE BUILDING ##

image-trees: ok genlist
    # Use sort_deps to do the dependency sorting
	$(Q)for ARCH in $(ARCHES_NOSRC); do \
		ARCH=$$ARCH $(sort_deps) $(BDIR)/list; \
	done
	$(Q)if [ "$(SOURCEONLY)"x = "yes"x ] ; then \
		$(grab_source_list) $(BDIR) $(ADIR) $(BDIR)/list $(BDIR)/packages; \
	else \
		$(merge_package_lists) $(BDIR) $(ADIR) "$(ARCHES)" $(BDIR)/packages; \
	fi
	$(Q)if [ "$(INC_SOURCE)"x = "yes"x ] ; then \
		grep ^source $(BDIR)/packages > $(BDIR)/packages.source; \
	fi
	$(Q)$(BASEDIR)/tools/make_disc_trees.pl $(BASEDIR) $(MIRROR) $(TDIR) $(CODENAME) "$(ARCHES)" "$(MKISOFS)" "$(MKISOFS_OPTS) $(JIGDO_OPTS)"

images: ok $(OUT) $(BDIR)/md5-check
	$(Q)$(make_image) "$(BDIR)" "$(ARCHES)" "$(OUT)" "$(DEBVERSION)" "$(MIRROR)" "$(MKISOFS)" "$(MKISOFS_OPTS)" "$(JIGDO_OPTS)" "$(jigdo_cleanup)"

check-number-given:
	@test -n "$(CD)" || (echo "Give me a CD=<num> parameter !" && false)

# Generate only one image number $(CD)
image: check-number-given images

# Calculate the md5sums for the images (if available), or get from templates
imagesums:
	$(Q)$(BASEDIR)/tools/imagesums $(OUT) $(SUMS_EXTENSION)

## MISC TARGETS ##

$(BDIR)/md5-check: mirrorcheck	

mirrorcheck: ok
	$(Q)$(grab_md5) $(MIRROR) "$(ARCHES)" $(CODENAME) $(DI_CODENAME) $(BDIR)/md5-check
	$(Q)for ARCH in $(ARCHES); do \
		if [ -e $(BASEDIR)/data/$(CODENAME)/$$ARCH/extra-sources ]; then \
			echo "Extra dedicated source added; need to grab source MD5 info too"; \
			$(grab_md5) $(MIRROR) source $(CODENAME) $(DI_CODENAME) $(BDIR)/md5-check; \
		fi; \
	done

update-popcon:
	$(update_popcon) tasks/$(CODENAME)/popularity-contest

# Little trick to simplify things
official_images: ok init packagelists image-trees images
