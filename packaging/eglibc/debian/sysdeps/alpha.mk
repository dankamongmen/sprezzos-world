libc_add-ons = ports nptl $(add-ons)

libc = libc6.1

# build an ev67 optimized library
EGLIBC_PASSES += alphaev67
DEB_ARCH_REGULAR_PACKAGES += libc6.1-alphaev67
alphaev67_add-ons = ports nptl $(add-ons)
alphaev67_configure_target = alphaev67-linux-gnu
alphaev67_extra_cflags = -mcpu=ev67 -mtune=ev67 -O2
alphaev67_extra_config_options = $(extra_config_options) --disable-profile
alphaev67_slibdir = /lib/$(DEB_HOST_MULTIARCH)/ev67
