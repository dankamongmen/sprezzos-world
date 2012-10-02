extra_config_options = --enable-multi-arch

# We use -march=i686 and glibc's i686 routines use cmov, so require it.
# A Debian-local glibc patch adds cmov to the search path.
# The optimized libraries also use NPTL!
EGLIBC_PASSES += i686
DEB_ARCH_REGULAR_PACKAGES += libc6-i686
i686_add-ons = nptl $(add-ons)
i686_configure_target=i686-linux-gnu
i686_extra_cflags = -march=i686 -mtune=generic
i686_slibdir = /lib/$(DEB_HOST_MULTIARCH)/i686/cmov
i686_extra_config_options = $(extra_config_options) --disable-profile

# We use -mno-tls-direct-seg-refs to not wrap-around segments, as it
# greatly increase the speed when running under the 32bit Xen hypervisor.
EGLIBC_PASSES += xen
DEB_ARCH_REGULAR_PACKAGES += libc6-xen
xen_add-ons = nptl $(add-ons)
xen_configure_target=i686-linux-gnu
xen_extra_cflags = -march=i686 -mtune=generic -mno-tls-direct-seg-refs
xen_slibdir = /lib/$(DEB_HOST_MULTIARCH)/i686/nosegneg
xen_extra_config_options = $(extra_config_options) --disable-profile

define libc6-xen_extra_pkg_install
mkdir -p debian/libc6-xen/etc/ld.so.conf.d
echo '# This directive teaches ldconfig to search in nosegneg subdirectories' >  debian/libc6-xen/etc/ld.so.conf.d/libc6-xen.conf
echo '# and cache the DSOs there with extra bit 1 set in their hwcap match'   >> debian/libc6-xen/etc/ld.so.conf.d/libc6-xen.conf
echo '# fields. In Xen guest kernels, the vDSO tells the dynamic linker to'   >> debian/libc6-xen/etc/ld.so.conf.d/libc6-xen.conf
echo '# search in nosegneg subdirectories and to match this extra hwcap bit'  >> debian/libc6-xen/etc/ld.so.conf.d/libc6-xen.conf
echo '# in the ld.so.cache file.'                                             >> debian/libc6-xen/etc/ld.so.conf.d/libc6-xen.conf
echo 'hwcap 1 nosegneg'                                                       >> debian/libc6-xen/etc/ld.so.conf.d/libc6-xen.conf
endef

# build 64-bit (amd64) alternative library
EGLIBC_PASSES += amd64
DEB_ARCH_REGULAR_PACKAGES += libc6-amd64 libc6-dev-amd64
libc6-amd64_shlib_dep = libc6-amd64 (>= $(shlib_dep_ver))
amd64_add-ons = nptl $(add-ons)
amd64_configure_target = x86_64-linux-gnu
# __x86_64__ is defined here because Makeconfig uses -undef and the
# /usr/include/asm wrappers need that symbol.
amd64_CC = $(CC) -m64 -D__x86_64__
amd64_CXX = $(CXX) -m64 -D__x86_64__
amd64_extra_config_options = $(extra_config_options) --disable-profile
amd64_rtlddir = /lib64
amd64_slibdir = /lib64
amd64_libdir = /usr/lib64

define amd64_extra_install
cp debian/tmp-amd64/usr/bin/ldd \
	debian/tmp-libc/usr/bin
endef

define libc6-dev_extra_pkg_install
mkdir -p debian/libc6-dev/$(libdir)/xen
cp -af debian/tmp-xen/$(libdir)/*.a \
	debian/libc6-dev/$(libdir)/xen
endef

define libc6-dev-amd64_extra_pkg_install
mkdir -p debian/libc6-dev-amd64/usr/include
cp -a debian/tmp-amd64/usr/include/bits \
	debian/libc6-dev-amd64/usr/include/
cp -a debian/tmp-amd64/usr/include/gnu \
	debian/libc6-dev-amd64/usr/include/
cp -a debian/tmp-amd64/usr/include/sys \
	debian/libc6-dev-amd64/usr/include/
cp debian/tmp-amd64/usr/include/fpu_control.h \
	debian/libc6-dev-amd64/usr/include/
endef
