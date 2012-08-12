# build 64-bit (s390x) alternative library
EGLIBC_PASSES += s390x
DEB_ARCH_REGULAR_PACKAGES += libc6-s390x libc6-dev-s390x
s390x_add-ons = nptl $(add-ons)
s390x_configure_target = s390x-linux-gnu
s390x_CC = $(CC) -m64
s390x_CXX = $(CXX) -m64
libc6-s390x_shlib_dep = libc6-s390x (>= $(shlib_dep_ver))
s390x_extra_config_options := $(extra_config_options) --disable-profile
s390x_rtlddir = /lib64
s390x_slibdir = /lib64
s390x_libdir = /usr/lib64

define libc6-dev-s390x_extra_pkg_install

mkdir -p debian/libc6-dev-s390x/usr/include
ln -s s390-linux-gnu/bits debian/libc6-dev-s390x/usr/include/
ln -s s390-linux-gnu/gnu debian/libc6-dev-s390x/usr/include/
ln -s s390-linux-gnu/fpu_control.h debian/libc6-dev-s390x/usr/include/

mkdir -p debian/libc6-dev-s390x/usr/include/s390-linux-gnu/gnu
cp -a debian/tmp-s390x/usr/include/gnu/stubs-64.h \
        debian/libc6-dev-s390x/usr/include/s390-linux-gnu/gnu

mkdir -p debian/libc6-dev-s390x/usr/include/sys
for i in `ls debian/tmp-libc/usr/include/s390-linux-gnu/sys` ; do \
        ln -s ../s390-linux-gnu/sys/$$i debian/libc6-dev-s390x/usr/include/sys/$$i ; \
done

endef

define libc6-s390x_extra_pkg_install
mkdir -p debian/$(curpass)/lib
ln -s /lib64/ld64.so.1 debian/$(curpass)/lib
endef
