ifeq ($(with_libgfortran),yes)
  $(lib_binaries) += libgfortran
endif
ifeq ($(with_fdev),yes)
  $(lib_binaries) += libgfortran-dev
endif
ifeq ($(with_lib64gfortran),yes)
  $(lib_binaries) += lib64fortran
endif
ifeq ($(with_lib64gfortrandev),yes)
  $(lib_binaries) += lib64gfortran-dev
endif
ifeq ($(with_lib32gfortran),yes)
  $(lib_binaries) += lib32fortran
endif
ifeq ($(with_lib32gfortrandev),yes)
  $(lib_binaries) += lib32gfortran-dev
endif
ifeq ($(with_libn32gfortran),yes)
  $(lib_binaries) += libn32fortran
endif
ifeq ($(with_libn32gfortrandev),yes)
  $(lib_binaries) += libn32gfortran-dev
endif
ifeq ($(with_libx32gfortran),yes)
  $(lib_binaries) += libx32fortran
endif
ifeq ($(with_libx32gfortrandev),yes)
  $(lib_binaries) += libx32gfortran-dev
endif
ifeq ($(with_libhfgfortran),yes)
  $(lib_binaries) += libhffortran
endif
ifeq ($(with_libhfgfortrandev),yes)
  $(lib_binaries) += libhfgfortran-dev
endif
ifeq ($(with_libsfgfortran),yes)
  $(lib_binaries) += libsffortran
endif
ifeq ($(with_libsfgfortrandev),yes)
  $(lib_binaries) += libsfgfortran-dev
endif

ifeq ($(with_fdev),yes)
  ifneq (,$(filter yes, $(biarch64) $(biarch32) $(biarchn32) $(biarchx32) $(biarchhf) $(biarchsf)))
    arch_binaries  := $(arch_binaries) fdev-multi
  endif
  arch_binaries  := $(arch_binaries) fdev
  ifneq ($(DEB_CROSS),yes)
    ifneq ($(GFDL_INVARIANT_FREE),yes)
      indep_binaries := $(indep_binaries) fortran-doc
    endif
  endif
endif

p_g95	= gfortran$(pkg_ver)$(cross_bin_arch)
p_g95_m	= gfortran$(pkg_ver)-multilib$(cross_bin_arch)
p_g95d	= gfortran$(pkg_ver)-doc
p_flib	= libgfortran$(FORTRAN_SONAME)$(cross_lib_arch)

d_g95	= debian/$(p_g95)
d_g95_m	= debian/$(p_g95_m)
d_g95d	= debian/$(p_g95d)

dirs_g95 = \
	$(docdir)/$(p_xbase)/fortran \
	$(PF)/bin \
	$(gcc_lexec_dir) \
	$(gcc_lib_dir) \
	$(PF)/include \
	$(PF)/share/man/man1
files_g95 = \
	$(PF)/bin/$(cmd_prefix)gfortran$(pkg_ver) \
	$(gcc_lib_dir)/finclude \
	$(gcc_lexec_dir)/f951 

ifneq ($(GFDL_INVARIANT_FREE),yes)
  files_g95 += \
	$(PF)/share/man/man1/$(cmd_prefix)gfortran$(pkg_ver).1
endif

# ----------------------------------------------------------------------
define __do_fortran
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l) $(d_d)
	dh_installdirs -p$(p_l) $(usr_lib$(2))
	DH_COMPAT=2 dh_movefiles -p$(p_l) $(usr_lib$(2))/libgfortran.so.*

	debian/dh_doclink -p$(p_l) $(p_base)
	debian/dh_doclink -p$(p_d) $(p_base)

	if [ -f debian/$(p_l).overrides ]; then \
		mkdir -p debian/$(p_l)/usr/share/lintian/overrides; \
		cp debian/$(p_l).overrides debian/$(p_l)/usr/share/lintian/overrides/$(p_l); \
	fi

	dh_strip -p$(p_l) --dbg-package=$(p_d)
	dh_compress -p$(p_l) -p$(p_d)
	dh_fixperms -p$(p_l) -p$(p_d)
	$(cross_makeshlibs) dh_makeshlibs -p$(p_l)
	$(call cross_mangle_shlibs,$(p_l))
	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_l) \
		$(call shlibdirs_to_search, \
			$(subst gfortran$(FORTRAN_SONAME),gcc$(GCC_SONAME),$(p_l)) \
			$(subst gfortran$(FORTRAN_SONAME),gcc$(QUADMATH_SONAME),$(p_l)) \
		,$(2))
	$(call cross_mangle_substvars,$(p_l))
	$(cross_gencontrol) dh_gencontrol -p$(p_l) -p$(p_d) \
		-- -v$(DEB_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(p_l))
	dh_installdeb -p$(p_l) -p$(p_d)
	dh_md5sums -p$(p_l) -p$(p_d)
	dh_builddeb -p$(p_l) -p$(p_d)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

do_fortran = $(call __do_fortran,lib$(1)gfortran$(FORTRAN_SONAME),$(1))


define __do_libgfortran_dev
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l)
	dh_installdirs -p$(1) $(gcc_lib_dir$(2))

	DH_COMPAT=2 dh_movefiles -p$(p_l) \
		$(gcc_lib_dir$(2))/libgfortranbegin.a \
		$(gcc_lib_dir$(2))/libcaf_single.a
	$(call install_gcc_lib,libgfortran,$(FORTRAN_SONAME),$(2),$(p_l))

	debian/dh_doclink -p$(p_l) $(p_base)
	debian/dh_rmemptydirs -p$(p_l)

	dh_strip -p$(p_l)
	dh_compress -p$(p_l)
	dh_fixperms -p$(p_l)
	$(cross_shlibdeps) dh_shlibdeps -p$(p_l)
	$(call cross_mangle_substvars,$(p_l))
	$(cross_gencontrol) dh_gencontrol -p$(p_l) -- -v$(DEB_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(p_l))
	dh_installdeb -p$(p_l)
	dh_md5sums -p$(p_l)
	dh_builddeb -p$(p_l)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef
# ----------------------------------------------------------------------

do_libgfortran_dev = $(call __do_libgfortran_dev,lib$(1)gfortran-$(BASE_VERSION)-dev,$(1))

$(binary_stamp)-libgfortran: $(install_stamp)
	$(call do_fortran,)

$(binary_stamp)-lib64fortran: $(install_stamp)
	$(call do_fortran,64)

$(binary_stamp)-lib32fortran: $(install_stamp)
	$(call do_fortran,32)

$(binary_stamp)-libn32fortran: $(install_stamp)
	$(call do_fortran,n32)

$(binary_stamp)-libx32fortran: $(install_stamp)
	$(call do_fortran,x32)

$(binary_stamp)-libhffortran: $(install_stamp)
	$(call do_fortran,hf)

$(binary_stamp)-libsffortran: $(install_stamp)
	$(call do_fortran,sf)

# ----------------------------------------------------------------------
$(binary_stamp)-fdev: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_g95)
	dh_installdirs -p$(p_g95) $(dirs_g95)

	DH_COMPAT=2 dh_movefiles -p$(p_g95) $(files_g95)

	mv $(d)/$(usr_lib)/libgfortran.spec $(d_g95)/$(gcc_lib_dir)/

ifneq ($(DEB_CROSS),yes)
	ln -sf gfortran$(pkg_ver) \
	    $(d_g95)/$(PF)/bin/$(DEB_TARGET_GNU_TYPE)-gfortran$(pkg_ver)
	ln -sf gfortran$(pkg_ver) \
	    $(d_g95)/$(PF)/bin/$(TARGET_ALIAS)-gfortran$(pkg_ver)
ifneq ($(GFDL_INVARIANT_FREE),yes)
	ln -sf gfortran$(pkg_ver).1 \
	    $(d_g95)/$(PF)/share/man/man1/$(DEB_TARGET_GNU_TYPE)-gfortran$(pkg_ver).1
	ln -sf gfortran$(pkg_ver).1 \
	    $(d_g95)/$(PF)/share/man/man1/$(TARGET_ALIAS)-gfortran$(pkg_ver).1
endif
endif

	debian/dh_doclink -p$(p_g95) $(p_xbase)

	cp -p $(srcdir)/gcc/fortran/ChangeLog \
		$(d_g95)/$(docdir)/$(p_xbase)/fortran/changelog
	debian/dh_rmemptydirs -p$(p_g95)

	dh_strip -p$(p_g95)
	dh_compress -p$(p_g95)
	dh_fixperms -p$(p_g95)
	dh_shlibdeps -p$(p_g95)
	dh_gencontrol -p$(p_g95) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_g95)
	dh_md5sums -p$(p_g95)
	dh_builddeb -p$(p_g95)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-fdev-multi: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_g95_m)
	dh_installdirs -p$(p_g95_m) $(docdir)

	debian/dh_doclink -p$(p_g95_m) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_g95_m)
	dh_strip -p$(p_g95_m)
	dh_compress -p$(p_g95_m)
	dh_fixperms -p$(p_g95_m)
	dh_shlibdeps -p$(p_g95_m)
	dh_gencontrol -p$(p_g95_m) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_g95_m)
	dh_md5sums -p$(p_g95_m)
	dh_builddeb -p$(p_g95_m)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-fortran-doc: $(build_html_stamp) $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_g95d)
	dh_installdirs -p$(p_g95d) \
		$(docdir)/$(p_xbase)/fortran \
		$(PF)/share/info
	DH_COMPAT=2 dh_movefiles -p$(p_g95d) \
		$(PF)/share/info/gfortran*

	debian/dh_doclink -p$(p_g95d) $(p_xbase)
ifneq ($(GFDL_INVARIANT_FREE),yes)
	dh_installdocs -p$(p_g95d)
	rm -f $(d_g95d)/$(docdir)/$(p_xbase)/copyright
	cp -p html/gfortran.html $(d_g95d)/$(docdir)/$(p_xbase)/fortran/
endif

	dh_compress -p$(p_g95d)
	dh_fixperms -p$(p_g95d)
	dh_installdeb -p$(p_g95d)
	dh_gencontrol -p$(p_g95d) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_g95d)
	dh_builddeb -p$(p_g95d)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

$(binary_stamp)-libgfortran-dev: $(install_stamp)
	$(call do_libgfortran_dev,)

$(binary_stamp)-lib64gfortran-dev: $(install_stamp)
	$(call do_libgfortran_dev,64)

$(binary_stamp)-lib32gfortran-dev: $(install_stamp)
	$(call do_libgfortran_dev,32)

$(binary_stamp)-libn32gfortran-dev: $(install_stamp)
	$(call do_libgfortran_dev,n32)

$(binary_stamp)-libx32gfortran-dev: $(install_stamp)
	$(call do_libgfortran_dev,x32)

$(binary_stamp)-libhfgfortran-dev: $(install_stamp)
	$(call do_libgfortran_dev,hf)

$(binary_stamp)-libsfgfortran-dev: $(install_stamp)
	$(call do_libgfortran_dev,sf)

