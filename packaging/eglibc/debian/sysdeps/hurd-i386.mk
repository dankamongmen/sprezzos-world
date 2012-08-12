# We use -march=i686 and glibc's i686 routines use cmov, so require it.
# A Debian-local glibc patch adds cmov to the search path.
EGLIBC_PASSES += i686
DEB_ARCH_REGULAR_PACKAGES += libc0.3-i686
i686_add-ons = $(libc_add-ons)
i686_configure_target=i686-gnu
i686_extra_cflags = -march=i686 -mtune=generic
i686_slibdir = /lib/$(DEB_HOST_MULTIARCH)/i686/cmov
i686_extra_config_options = $(extra_config_options) --disable-profile --disable-compatible-utmp

# We use -mno-tls-direct-seg-refs to not wrap-around segments, as it
# greatly increase the speed when running under the 32bit Xen hypervisor.
EGLIBC_PASSES += xen
DEB_ARCH_REGULAR_PACKAGES += libc0.3-xen
xen_add-ons = $(libc_add-ons)
xen_configure_target=i686-gnu
xen_extra_cflags = -march=i686 -mtune=generic -mno-tls-direct-seg-refs
xen_slibdir = /lib/$(DEB_HOST_MULTIARCH)/i686/nosegneg
xen_extra_config_options = $(extra_config_options) --disable-profile

define libc0.3-dev_extra_pkg_install
mkdir -p debian/libc0.3-dev/$(libdir)/xen
cp -af debian/tmp-xen/$(libdir)/*.a \
	debian/libc0.3-dev/$(libdir)/xen
endef

