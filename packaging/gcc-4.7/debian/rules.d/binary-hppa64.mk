arch_binaries  := $(arch_binaries) hppa64

# ----------------------------------------------------------------------
$(binary_stamp)-hppa64: $(install_hppa64_stamp)
	dh_testdir
	dh_testroot

#	dh_installdirs -p$(p_hppa64)

	: # provide as and ld links
	dh_link -p $(p_hppa64) \
		/usr/bin/hppa64-linux-gnu-as \
		/$(hppa64libexecdir)/gcc/hppa64-linux-gnu/$(GCC_VERSION)/as \
		/usr/bin/hppa64-linux-gnu-ld \
		/$(hppa64libexecdir)/gcc/hppa64-linux-gnu/$(GCC_VERSION)/ld

	debian/dh_doclink -p$(p_hppa64) $(p_base)
	debian/dh_rmemptydirs -p$(p_hppa64)

	dh_strip -p$(p_hppa64) -X.o -Xlibgcc.a -Xlibgcov.a
	dh_compress -p$(p_hppa64)
	dh_fixperms -p$(p_hppa64)
	dh_shlibdeps -p$(p_hppa64)
	dh_gencontrol -p$(p_hppa64) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_hppa64)
	dh_md5sums -p$(p_hppa64)
	dh_builddeb -p$(p_hppa64)

	touch $@
