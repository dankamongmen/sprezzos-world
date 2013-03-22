ifneq (,$(filter yes, $(biarch64) $(biarch32) $(biarchn32) $(biarchx32) $(biarchhf) $(biarchsf)))
  arch_binaries  := $(arch_binaries) objcxx-multi
endif
arch_binaries := $(arch_binaries) objcxx

p_objcx		= gobjc++$(pkg_ver)$(cross_bin_arch)
d_objcx		= debian/$(p_objcx)

p_objcx_m	= gobjc++$(pkg_ver)-multilib$(cross_bin_arch)
d_objcx_m	= debian/$(p_objcx_m)

dirs_objcx = \
	$(docdir)/$(p_xbase)/Obj-C++ \
	$(gcc_lexec_dir)

files_objcx = \
	$(gcc_lexec_dir)/cc1objplus

$(binary_stamp)-objcxx: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_objcx)
	dh_installdirs -p$(p_objcx) $(dirs_objcx)
	DH_COMPAT=2 dh_movefiles -p$(p_objcx) $(files_objcx)

	debian/dh_doclink -p$(p_objcx) $(p_xbase)
	cp -p $(srcdir)/gcc/objcp/ChangeLog \
		$(d_objcx)/$(docdir)/$(p_xbase)/Obj-C++/changelog

	debian/dh_rmemptydirs -p$(p_objcx)

	dh_strip -p$(p_objcx)
	dh_compress -p$(p_objcx)

	dh_fixperms -p$(p_objcx)
	dh_shlibdeps -p$(p_objcx)
	dh_gencontrol -p$(p_objcx) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_objcx)
	dh_md5sums -p$(p_objcx)
	dh_builddeb -p$(p_objcx)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

$(binary_stamp)-objcxx-multi: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_objcx_m)

	debian/dh_doclink -p$(p_objcx_m) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_objcx_m)
	dh_strip -p$(p_objcx_m)
	dh_compress -p$(p_objcx_m)

	dh_fixperms -p$(p_objcx_m)
	dh_shlibdeps -p$(p_objcx_m)
	dh_gencontrol -p$(p_objcx_m) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_objcx_m)
	dh_md5sums -p$(p_objcx_m)
	dh_builddeb -p$(p_objcx_m)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
