arch_binaries  := $(arch_binaries) softfloat

p_softfloat  = gcc$(pkg_ver)-soft-float
d_softfloat  = debian/$(p_softfloat)

dirs_softfloat = \
	$(PFL)/$(libdir) \
	$(gcc_lib_dir)

files_softfloat = \
	$(PFL)/$(libdir)/soft-float \
	$(gcc_lib_dir)/soft-float

# ----------------------------------------------------------------------
$(binary_stamp)-softfloat: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_softfloat)
	dh_installdirs -p$(p_softfloat) $(dirs_softfloat)
	DH_COMPAT=2 dh_movefiles -p$(p_softfloat) $(files_softfloat)
	rm -rf $(d_softfloat)/$(PFL)/$(libdir)/soft-float/libssp.so*
	mv $(d_softfloat)/$(PFL)/$(libdir)/soft-float/libssp.a \
		$(d_softfloat)/$(PFL)/$(libdir)/soft-float/libssp_nonshared.a
	debian/dh_doclink -p$(p_softfloat) $(p_base)
	dh_strip -p$(p_softfloat) -Xlibgcj.a
	dh_compress -p$(p_softfloat)
	dh_fixperms -p$(p_softfloat)
	dh_shlibdeps -p$(p_softfloat)
	dh_gencontrol -p$(p_softfloat) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_softfloat)
	dh_md5sums -p$(p_softfloat)
	dh_builddeb -p$(p_softfloat)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
