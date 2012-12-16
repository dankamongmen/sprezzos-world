libc_add-ons = ports nptl $(add-ons)

# Passing --disable-profile until arm64 has profiling support
extra_config_options = --enable-multi-arch --disable-profile

# There is no gcc-4.6 for arm64, so use 4.7
CC = $(DEB_HOST_GNU_TYPE)-$(BASE_CC)-4.7
CXX = $(DEB_HOST_GNU_TYPE)-$(BASE_CXX)-4.7
