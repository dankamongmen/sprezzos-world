extra_config_options = --enable-multi-arch

# build 64-bit (ppc64) alternative library
EGLIBC_PASSES += ppc64
DEB_ARCH_REGULAR_PACKAGES += libc6-ppc64 libc6-dev-ppc64
ppc64_add-ons = nptl $(add-ons)
ppc64_configure_target = powerpc64-linux-gnu
ppc64_CC = $(CC) -m64
ppc64_CXX = $(CXX) -m64
libc6-ppc64_shlib_dep = libc6-ppc64 (>= $(shlib_dep_ver))
ppc64_extra_config_options := $(extra_config_options) --disable-profile
ppc64_rtlddir = /lib64
ppc64_slibdir = /lib64
ppc64_libdir = /usr/lib64

define libc6-dev-ppc64_extra_pkg_install

mkdir -p debian/libc6-dev-ppc64/usr/include
ln -s powerpc-linux-gnu/bits debian/libc6-dev-ppc64/usr/include/
ln -s powerpc-linux-gnu/gnu debian/libc6-dev-ppc64/usr/include/
ln -s powerpc-linux-gnu/fpu_control.h debian/libc6-dev-ppc64/usr/include/

mkdir -p debian/libc6-dev-ppc64/usr/include/powerpc-linux-gnu/gnu
cp -a debian/tmp-ppc64/usr/include/gnu/stubs-64.h \
        debian/libc6-dev-ppc64/usr/include/powerpc-linux-gnu/gnu

mkdir -p debian/libc6-dev-ppc64/usr/include/sys
for i in `ls debian/tmp-libc/usr/include/powerpc-linux-gnu/sys` ; do \
        ln -s ../powerpc-linux-gnu/sys/$$i debian/libc6-dev-ppc64/usr/include/sys/$$i ; \
done

endef

