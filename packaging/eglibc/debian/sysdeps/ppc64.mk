libc_rtlddir = /lib64
extra_config_options = --enable-multi-arch

# build 32-bit (powerpc) alternative library
EGLIBC_PASSES += powerpc
DEB_ARCH_REGULAR_PACKAGES += libc6-powerpc libc6-dev-powerpc
libc6-powerpc_shlib_dep = libc6-powerpc (>= $(shlib_dep_ver))
powerpc_add-ons = nptl $(add-ons)
powerpc_configure_target = powerpc-linux-gnu
powerpc_CC = $(CC) -m32
powerpc_CXX = $(CXX) -m32
powerpc_extra_config_options := $(extra_config_options) --disable-profile
powerpc_slibdir = /lib32
powerpc_libdir = /usr/lib32

define libc6-dev-powerpc_extra_pkg_install

mkdir -p debian/libc6-dev-powerpc/usr/include
ln -s powerpc64-linux-gnu/bits debian/libc6-dev-powerpc/usr/include/
ln -s powerpc64-linux-gnu/gnu debian/libc6-dev-powerpc/usr/include/
ln -s powerpc64-linux-gnu/fpu_control.h debian/libc6-dev-powerpc/usr/include/

mkdir -p debian/libc6-dev-powerpc/usr/include/powerpc64-linux-gnu/gnu
cp -a debian/tmp-powerpc/usr/include/gnu/stubs-32.h \
        debian/libc6-dev-powerpc/usr/include/powerpc64-linux-gnu/gnu

mkdir -p debian/libc6-dev-powerpc/usr/include/sys
for i in `ls debian/tmp-libc/usr/include/powerpc64-linux-gnu/sys` ; do \
        ln -s ../powerpc64-linux-gnu/sys/$$i debian/libc6-dev-powerpc/usr/include/sys/$$i ; \
done

endef

# create a symlink for the 32 bit dynamic linker in /lib
define libc6-powerpc_extra_pkg_install
mkdir -p debian/$(curpass)/lib
ln -s /lib32/ld.so.1 debian/$(curpass)/lib
endef
