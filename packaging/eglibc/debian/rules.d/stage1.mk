## This reuses various macros from the debian/rules.d/build.mk file

ifeq ($(DEB_STAGE),stage1)

override EGLIBC_PASSES = libc
override DEB_ARCH_REGULAR_PACKAGES = $(libc)-dev
override DEB_INDEP_REGULAR_PACKAGES =
override DEB_UDEB_PACKAGES =

## Development libraries we need to fake
stage1_libfake.so :=		\
	libc.so

stage1_libfake.a :=		\
	libanl.a		\
	libBrokenLocale.a	\
	libbsd-compat.a		\
	libc.a			\
	libc_nonshared.a	\
	libcrypt.a		\
	libdl.a			\
	libg.a			\
	libieee.a		\
	libm.a			\
	libmcheck.a		\
	libnsl.a		\
	libpthread.a		\
	libpthread_nonshared.a	\
	libresolv.a		\
	librpcsvc.a		\
	librt.a			\
	libutil.a		\

$(stamp)build_libc: $(stamp)configure_libc
	@echo Building $(curpass)
	@## Build the crtX.o init routines
	$(call logme, -a $(log_build), $(MAKE) -C $(DEB_BUILDDIR) $(NJOBS) csu/subdir_lib)
	$(call logme, -a $(log_build), $(AR) qcs $(DEB_BUILDDIR)/libfake.a)
	$(call logme, -a $(log_build), $(CC) -nostdlib -nostartfiles -shared \
				-o $(DEB_BUILDDIR)/libfake.so $(DEB_BUILDDIR)/libfake.a)
	$(call logme, -a $(log_build), echo "---------------" ; echo -n "Build ended: " ; date --rfc-2822)
	touch $@

$(stamp)check_libc: $(stamp)build_libc
	@echo Nothing to test for $(curpass)
	touch $@

$(stamp)install_libc: DESTDIR=$(CURDIR)/debian/tmp-$(curpass)
$(stamp)install_libc: $(stamp)check_libc
	@echo Installing $(curpass)
	rm -rf $(CURDIR)/debian/tmp-$(curpass)
	## These libc/ld-linux binaries are total garbage, but they allow
	## a subsequent stage2 GCC build to succeed.
	install -d $(DESTDIR)/usr/lib/$(DEB_HOST_MULTIARCH)
	for lib_a in $(stage1_libfake.a); do \
		install -T $(DEB_BUILDDIR)/libfake.a $(DESTDIR)/usr/lib/$(DEB_HOST_MULTIARCH)/$$lib_a; \
	done
	for lib_so in $(stage1_libfake.so); do \
		install -T $(DEB_BUILDDIR)/libfake.so $(DESTDIR)/usr/lib/$(DEB_HOST_MULTIARCH)/$$lib_so; \
	done
	$(MAKE) -C $(DEB_BUILDDIR) install_root=$(DESTDIR) install-bootstrap-headers=yes \
		csu/subdir_install install-headers
	mkdir -p $(DESTDIR)/usr/include/$(DEB_HOST_MULTIARCH)
	mv $(DESTDIR)/usr/include/bits          $(DESTDIR)/usr/include/$(DEB_HOST_MULTIARCH)
	mv $(DESTDIR)/usr/include/gnu           $(DESTDIR)/usr/include/$(DEB_HOST_MULTIARCH)
	mv $(DESTDIR)/usr/include/sys           $(DESTDIR)/usr/include/$(DEB_HOST_MULTIARCH)
	mv $(DESTDIR)/usr/include/fpu_control.h $(DESTDIR)/usr/include/$(DEB_HOST_MULTIARCH)
	mv $(DESTDIR)/usr/include/a.out.h	$(DESTDIR)/usr/include/$(DEB_HOST_MULTIARCH)
	mv $(DESTDIR)/usr/include/ieee754.h	$(DESTDIR)/usr/include/$(DEB_HOST_MULTIARCH)
	$(call xx,extra_install)
	touch $@

endif
