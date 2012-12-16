indep_binaries := $(indep_binaries) gcc-source

ifeq ($(BACKPORT),true)
  p_source = gcc$(pkg_ver)-$(GCC_VERSION)-source
else
  p_source = gcc$(pkg_ver)-source
endif
d_source= debian/$(p_source)

$(binary_stamp)-gcc-source:
	dh_testdir
	dh_testroot

	dh_installdocs -p$(p_source)
	dh_installchangelogs -p$(p_source)

	dh_install -p$(p_source) $(gcc_tarball) usr/src/gcc$(pkg_ver)
#	dh_install -p$(p_source) $(gcj_tarball) usr/src/gcc$(pkg_ver)
	tar cf - $$(find './debian' -mindepth 1 \( \
		-name .svn -prune -o \
		-path './debian/gcc-*' -type d -prune -o \
		-path './debian/cpp-*' -type d -prune -o \
		-path './debian/*fortran*' -type d -prune -o \
		-path './debian/lib*' -type d -prune -o \
		-path './debian/patches/*' -prune -o \
		-path './debian/tmp*' -prune -o \
		-path './debian/files' -prune -o \
		-path './debian/rules.d/*' -prune -o \
		-path './debian/soname-cache' -prune -o \
		-path './debian/*substvars*' -prune -o \
		-path './debian/gcc-snapshot*' -prune -o \
		-path './debian/*[0-9]*.p*' -prune -o \
		-path './debian/*$(pkg_ver)[.-]*' -prune -o \
		-print \) ) \
	  | tar -x -C $(d_source)/usr/src/gcc$(pkg_ver)  -f -
	# FIXME: Remove generated files
	find $(d_source)/usr/src/gcc$(pkg_ver) -name '*.debhelper.log' -o -name .svn | xargs rm -rf

	dh_link -p$(p_source) \
		/usr/src/gcc$(pkg_ver)/debian/patches /usr/src/gcc$(pkg_ver)/patches

	mkdir -p $(d_source)/usr/share/lintian/overrides
	cp -p debian/$(p_source).overrides \
		$(d_source)/usr/share/lintian/overrides/$(p_source)

	dh_fixperms -p$(p_source)
	dh_compress -p$(p_source)
	dh_gencontrol -p$(p_source) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_source)
	dh_md5sums -p$(p_source)
	dh_builddeb -p$(p_source)

	touch $@
