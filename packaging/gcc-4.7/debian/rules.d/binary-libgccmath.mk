ifeq ($(with_libgmath),yes)
  arch_binaries  := $(arch_binaries) libgmath
endif
#ifeq ($(with_libgmathdev),yes)
#  arch_binaries  := $(arch_binaries) libgmathdev
#endif
ifeq ($(with_lib64gmath),yes)
  arch_binaries  := $(arch_binaries) lib64gmath
endif
ifeq ($(with_lib32gmath),yes)
  arch_binaries	:= $(arch_binaries) lib32gmath
endif

p_gmath		= libgccmath$(GCCMATH_SONAME)
p_l32gmath	= lib32gccmath$(GCCMATH_SONAME)
p_l64gmath	= lib64gccmath$(GCCMATH_SONAME)
p_gmathd	= libgccmath$(GCCMATH_SONAME)-dev

d_gmath		= debian/$(p_gmath)
d_l32gmath	= debian/$(p_l32gmath)
d_l64gmath	= debian/$(p_l64gmath)
d_gmathd	= debian/$(p_gmathd)

dirs_gmath = \
	$(docdir) \
	$(PF)/$(libdir)
files_gmath = \
	$(PF)/$(libdir)/libgcc-math.so.*

dirs_gmathd = \
	$(docdir)/$(p_base)
ifeq ($(with_libgmath),yes)
files_gmathd = \
	$(PF)/$(libdir)/libgcc-math.{a,so}
endif

ifeq ($(with_lib32gmath),yes)
	dirs_gmathd  += $(lib32)
	files_gmathd += $(lib32)/libgcc-math.{a,so}
endif
ifeq ($(with_lib64gmath),yes)
	dirs_gmathd  += $(PF)/lib64
	files_gmathd += $(PF)/lib64/libgcc-math.{a,so}
endif

$(binary_stamp)-libgmath: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp
	rm -rf $(d_gmath)
	dh_installdirs -p$(p_gmath) $(dirs_gmath)
	DH_COMPAT=2 dh_movefiles -p$(p_gmath) $(files_gmath)
	debian/dh_doclink -p$(p_gmath) $(p_base)
	debian/dh_rmemptydirs -p$(p_gmath)
	dh_strip -p$(p_gmath)
	dh_compress -p$(p_gmath)
	dh_fixperms -p$(p_gmath)
	dh_makeshlibs -p$(p_gmath) -V '$(p_gmath) (>= $(DEB_GCCMATH_SOVERSION))'
	dh_shlibdeps -p$(p_gmath)
	dh_gencontrol -p$(p_gmath) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_gmath)
	dh_md5sums -p$(p_gmath)
	dh_builddeb -p$(p_gmath)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-libgmathdev: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_gmathd)
	dh_installdirs -p$(p_gmathd) $(dirs_gmathd)
	DH_COMPAT=2 dh_movefiles -p$(p_gmathd) $(files_gmathd)
	debian/dh_doclink -p$(p_gmathd) $(p_base)
	cp -p $(srcdir)/libgcc-math/ChangeLog \
		$(d_gmathd)/$(docdir)/$(p_base)/changelog.libgcc-math
	debian/dh_rmemptydirs -p$(p_gmathd)
	dh_strip -p$(p_gmathd)
	dh_compress -p$(p_gmathd)
	dh_fixperms -p$(p_gmathd)
	dh_shlibdeps -p$(p_gmathd)
	dh_gencontrol -p$(p_gmathd) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_gmathd)
	dh_md5sums -p$(p_gmathd)
	dh_builddeb -p$(p_gmathd)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-lib64gmath: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l64gmath)
	dh_installdirs -p$(p_l64gmath) \
		$(PF)/lib64
	DH_COMPAT=2 dh_movefiles -p$(p_l64gmath) \
		$(PF)/lib64/libgcc-math.so.*

	debian/dh_doclink -p$(p_l64gmath) $(p_base)

	dh_strip -p$(p_l64gmath)
	dh_compress -p$(p_l64gmath)
	dh_fixperms -p$(p_l64gmath)
	dh_makeshlibs -p$(p_l64gmath) -V '$(p_l64gmath) (>= $(DEB_GCCMATH_SOVERSION))'
#	dh_shlibdeps -p$(p_l64gmath)
	dh_gencontrol -p$(p_l64gmath) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_l64gmath)
	dh_md5sums -p$(p_l64gmath)
	dh_builddeb -p$(p_l64gmath)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-lib32gmath: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l32gmath)
	dh_installdirs -p$(p_l32gmath) \
		$(lib32)
	DH_COMPAT=2 dh_movefiles -p$(p_l32gmath) \
		$(lib32)/libgcc-math.so.*

	debian/dh_doclink -p$(p_l32gmath) $(p_base)

	dh_strip -p$(p_l32gmath)
	dh_compress -p$(p_l32gmath)
	dh_fixperms -p$(p_l32gmath)
	dh_makeshlibs -p$(p_l32gmath) -V '$(p_l32gmath) (>= $(DEB_GCCMATH_SOVERSION))'
	dh_shlibdeps -p$(p_l32gmath)
	dh_gencontrol -p$(p_l32gmath) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_l32gmath)
	dh_md5sums -p$(p_l32gmath)
	dh_builddeb -p$(p_l32gmath)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
