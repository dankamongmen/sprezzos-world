ifneq (,$(filter yes, $(biarch64) $(biarch32) $(biarchn32) $(biarchx32) $(biarchhf) $(biarchsf)))
  arch_binaries  := $(arch_binaries) objc-multi
endif
arch_binaries := $(arch_binaries) objc

p_objc	= gobjc$(pkg_ver)$(cross_bin_arch)
d_objc	= debian/$(p_objc)

p_objc_m= gobjc$(pkg_ver)-multilib$(cross_bin_arch)
d_objc_m= debian/$(p_objc_m)

dirs_objc = \
	$(docdir)/$(p_xbase)/ObjC \
	$(gcc_lexec_dir)

files_objc = \
	$(gcc_lexec_dir)/cc1obj

$(binary_stamp)-objc: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_objc)
	dh_installdirs -p$(p_objc) $(dirs_objc)
	DH_COMPAT=2 dh_movefiles -p$(p_objc) $(files_objc)

	cp -p $(srcdir)/libobjc/{README*,THREADS*} \
		$(d_objc)/$(docdir)/$(p_xbase)/ObjC/.

	cp -p $(srcdir)/libobjc/ChangeLog \
		$(d_objc)/$(docdir)/$(p_xbase)/ObjC/changelog.libobjc

	debian/dh_doclink -p$(p_objc) $(p_xbase)

	debian/dh_rmemptydirs -p$(p_objc)

	dh_strip -p$(p_objc)
	dh_compress -p$(p_objc)

	dh_fixperms -p$(p_objc)
	dh_shlibdeps -p$(p_objc)
	dh_gencontrol -p$(p_objc) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_objc)
	dh_md5sums -p$(p_objc)
	dh_builddeb -p$(p_objc)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

$(binary_stamp)-objc-multi: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_objc_m)
	dh_installdirs -p$(p_objc_m) $(docdir)

	debian/dh_doclink -p$(p_objc_m) $(p_xbase)

	dh_strip -p$(p_objc_m)
	dh_compress -p$(p_objc_m)

	dh_fixperms -p$(p_objc_m)
	dh_shlibdeps -p$(p_objc_m)
	dh_gencontrol -p$(p_objc_m) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_objc_m)
	dh_md5sums -p$(p_objc_m)
	dh_builddeb -p$(p_objc_m)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
