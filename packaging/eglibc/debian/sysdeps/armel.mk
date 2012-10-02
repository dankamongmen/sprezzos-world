libc_add-ons = ports nptl $(add-ons)

#EGLIBC_PASSES += armhf
#DEB_ARCH_REGULAR_PACKAGES += libc6-armhf libc6-dev-armhf
#armhf_add-ons = ports nptl $(add-ons)
#armhf_configure_target = arm-linux-gnueabihf
#armhf_CC = $(CC) -mfloat-abi=hard
#armhf_CXX = $(CXX) -mfloat-abi=hard
#armhf_slibdir = /lib/arm-linux-gnueabihf
#armhf_libdir = /usr/lib/arm-linux-gnueabihf
#
#define libc6-dev-armhf_extra_pkg_install
#mkdir -p debian/libc6-dev-armhf/usr/include
#cp -a debian/tmp-armhf/usr/include/bits \
#	debian/libc6-dev-armhf/usr/include/
#cp -a debian/tmp-armhf/usr/include/gnu \
#	debian/libc6-dev-armhf/usr/include/
#cp -a debian/tmp-armhf/usr/include/sys \
#	debian/libc6-dev-armhf/usr/include/
#cp debian/tmp-armhf/usr/include/fpu_control.h \
#	debian/libc6-dev-armhf/usr/include/
#endef
#
#define libc6-armhf_extra_pkg_install
#mkdir -p debian/libc6-armhf$(armhf_slibdir)
#ln -sf $(armhf_slibdir)/ld-linux-armhf.so.3 debian/libc6-armhf/lib
#ln -sf ld-linux-armhf.so.3 debian/libc6-armhf$(armhf_slibdir)/ld-linux.so.3 
#endef
