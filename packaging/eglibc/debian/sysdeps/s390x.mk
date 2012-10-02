libc_rtlddir = /lib

# build 32-bit (s390) alternative library
EGLIBC_PASSES += s390
DEB_ARCH_REGULAR_PACKAGES += libc6-s390 libc6-dev-s390
s390_add-ons = nptl $(add-ons)
s390_configure_target = s390-linux-gnu
s390_CC = $(CC) -m31
s390_CXX = $(CXX) -m31
s390_extra_config_options := $(extra_config_options) --disable-profile
s390_slibdir = /lib32
s390_libdir = /usr/lib32

define libc6-dev-s390_extra_pkg_install

mkdir -p debian/libc6-dev-s390/usr/include
ln -s s390x-linux-gnu/bits debian/libc6-dev-s390/usr/include/
ln -s s390x-linux-gnu/gnu debian/libc6-dev-s390/usr/include/
ln -s s390x-linux-gnu/fpu_control.h debian/libc6-dev-s390/usr/include/

mkdir -p debian/libc6-dev-s390/usr/include/s390x-linux-gnu/gnu
cp -a debian/tmp-s390/usr/include/gnu/stubs-32.h \
        debian/libc6-dev-s390/usr/include/s390x-linux-gnu/gnu

mkdir -p debian/libc6-dev-s390/usr/include/sys
for i in `ls debian/tmp-libc/usr/include/s390x-linux-gnu/sys` ; do \
        ln -s ../s390x-linux-gnu/sys/$$i debian/libc6-dev-s390/usr/include/sys/$$i ; \
done

endef

define libc6-s390_extra_pkg_install
mkdir -p debian/$(curpass)/lib
ln -s /lib32/ld.so.1 debian/$(curpass)/lib
endef
