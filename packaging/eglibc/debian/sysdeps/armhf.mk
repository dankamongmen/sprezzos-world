libc_add-ons = ports nptl $(add-ons)

# Install a compat symlink so old binaries keep working:
define libc6_extra_pkg_install
mkdir -p debian/libc6/lib/arm-linux-gnueabihf
ln -sf ld-linux-armhf.so.3 debian/libc6/lib/arm-linux-gnueabihf/ld-linux.so.3
endef

define libc6-udeb_extra_pkg_install
mkdir -p debian/libc6-udeb/lib/arm-linux-gnueabihf
ln -sf /lib/ld-linux-armhf.so.3 debian/libc6-udeb/lib/arm-linux-gnueabihf/ld-linux.so.3
endef

#EGLIBC_PASSES += armel
#DEB_ARCH_REGULAR_PACKAGES += libc6-armel libc6-dev-armel
#armel_add-ons = ports nptl $(add-ons)
#armel_configure_target = arm-linux-gnueabi
#armel_CC = $(CC) -mfloat-abi=softfp
#armel_CXX = $(CXX) -mfloat-abi=softfp
#armel_slibdir = /lib/arm-linux-gnueabi
#armel_libdir = /usr/lib/arm-linux-gnueabi
#
#define libc6-dev-armel_extra_pkg_install
#mkdir -p debian/libc6-dev-armel/usr/include
#cp -a debian/tmp-armel/usr/include/bits \
#	debian/libc6-dev-armel/usr/include/
#cp -a debian/tmp-armel/usr/include/gnu \
#	debian/libc6-dev-armel/usr/include/
#cp -a debian/tmp-armel/usr/include/sys \
#	debian/libc6-dev-armel/usr/include/
#cp debian/tmp-armel/usr/include/fpu_control.h \
#	debian/libc6-dev-armel/usr/include/
#endef
#
#define libc6-armel_extra_pkg_install
#mkdir -p debian/libc6-armel/lib
#ln -sf $(armel_slibdir)/ld-linux.so.3 debian/libc6-armel/lib
#endef
