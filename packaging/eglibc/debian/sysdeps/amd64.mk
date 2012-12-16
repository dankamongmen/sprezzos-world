libc_rtlddir = /lib64
extra_config_options = --enable-multi-arch

# build 32-bit (i386) alternative library
EGLIBC_PASSES += i386
DEB_ARCH_REGULAR_PACKAGES += libc6-i386 libc6-dev-i386
libc6-i386_shlib_dep = libc6-i386 (>= $(shlib_dep_ver))
i386_add-ons = nptl $(add-ons)
i386_configure_target = i686-linux-gnu
i386_CC = $(CC) -m32
i386_CXX = $(CXX) -m32
i386_extra_cflags = -march=pentium4 -mtune=generic
i386_extra_config_options = $(extra_config_options) --disable-profile
i386_slibdir = /lib32
i386_libdir = /usr/lib32

define libc6-dev-i386_extra_pkg_install

mkdir -p debian/libc6-dev-i386/usr/include
ln -sf x86_64-linux-gnu/bits debian/libc6-dev-i386/usr/include/
ln -sf x86_64-linux-gnu/gnu debian/libc6-dev-i386/usr/include/
ln -sf x86_64-linux-gnu/fpu_control.h debian/libc6-dev-i386/usr/include/

mkdir -p debian/libc6-dev-i386/usr/include/x86_64-linux-gnu/gnu
cp -a debian/tmp-i386/usr/include/gnu/stubs-32.h \
        debian/libc6-dev-i386/usr/include/x86_64-linux-gnu/gnu

mkdir -p debian/libc6-dev-i386/usr/include/sys
for i in `ls debian/tmp-libc/usr/include/x86_64-linux-gnu/sys` ; do \
	ln -sf ../x86_64-linux-gnu/sys/$$i debian/libc6-dev-i386/usr/include/sys/$$i ; \
done

endef

define libc6-i386_extra_pkg_install
mkdir -p debian/libc6-i386/lib
ln -sf /lib32/ld-linux.so.2 debian/libc6-i386/lib
endef

# build x32 ABI alternative library
EGLIBC_PASSES += x32
DEB_ARCH_REGULAR_PACKAGES += libc6-x32 libc6-dev-x32
libc6-x32_shlib_dep = libc6-x32 (>= $(shlib_dep_ver))
x32_add-ons = nptl $(add-ons)
x32_configure_target = x86_64-linux-gnux32
x32_CC = $(DEB_HOST_GNU_TYPE)-$(BASE_CC)-4.7 -mx32
x32_CXX = $(DEB_HOST_GNU_TYPE)-$(BASE_CXX)-4.7 -mx32
x32_extra_config_options = $(extra_config_options) --disable-profile
x32_slibdir = /libx32
x32_libdir = /usr/libx32

define libc6-dev-x32_extra_pkg_install

mkdir -p debian/libc6-dev-x32/usr/include/x86_64-linux-gnu/gnu
cp -a debian/tmp-x32/usr/include/gnu/stubs-x32.h \
	debian/libc6-dev-x32/usr/include/x86_64-linux-gnu/gnu/

endef

