arch_binaries  := $(arch_binaries) spu spu-cxx spu-fortran

p_spugcc  = gcc$(pkg_ver)-spu
p_spucxx  = g++$(pkg_ver)-spu
p_spuf95  = gfortran$(pkg_ver)-spu

d_spugcc  = debian/$(p_spugcc)
d_spucxx  = debian/$(p_spucxx)
d_spuf95  = debian/$(p_spuf95)

dirs_spugcc = \
	$(PF)/bin \
	$(gcc_spu_lexec_dir) \
	$(gcc_spu_lib_dir) \

files_spugcc = \
	$(PF)/bin/spu-{cpp,gcc}$(pkg_ver) \
	$(PF)/bin/spu-{gcc-ar,gcc-ranlib,gcc-nm}$(pkg_ver) \
	$(gcc_spu_lexec_dir)/{cc1,collect2,lto1,lto-wrapper} \
	$(gcc_spu_lexec_dir)/liblto_plugin.so{,.0,.0.0.0} \
	$(gcc_spu_lib_dir)/{include,include-fixed} \
	$(gcc_spu_lib_dir)/{libgcc.a,libgcov.a,crtbegin.o,crtend.o} \

ifeq ($(with_spucache),yes)
    files_spugcc += \
	$(gcc_spu_lib_dir)/libgcc_cache*.a
  ifeq ($(with_spumea64),yes)
    files_spugcc += \
	$(gcc_spu_lib_dir)/mea64/libgcc_cache*.a
  endif
endif
ifeq ($(with_spumea64),yes)
    files_spugcc += \
        $(gcc_spu_lib_dir)/mea64/{libgcc.a,libgcov.a,crtbegin.o,crtend.o}
endif

ifneq ($(GFDL_INVARIANT_FREE),yes)
    files_spugcc += \
	$(PF)/share/man/man1/spu-{cpp,gcc}$(pkg_ver).1
endif

dirs_spucxx = \
	$(PF)/bin \
	$(gcc_spu_lexec_dir) \
	$(gcc_spu_lib_dir) \

files_spucxx = \
	$(PF)/bin/spu-g++$(pkg_ver) \
	$(gcc_spu_lexec_dir)/cc1plus \
	$(PF)/spu/include/c++ \
	$(gcc_spu_lib_dir)/lib{sup,std}c++.a

ifeq ($(with_spumea64),yes)
    files_spucxx += \
	$(gcc_spu_lib_dir)/mea64/lib{sup,std}c++.a
endif

ifneq ($(GFDL_INVARIANT_FREE),yes)
    files_spucxx += \
	$(PF)/share/man/man1/spu-g++$(pkg_ver).1
endif

dirs_spuf95 = \
	$(PF)/bin \
	$(gcc_spu_lexec_dir) \
	$(gcc_spu_lib_dir) \

files_spuf95 = \
	$(PF)/bin/spu-gfortran$(pkg_ver) \
	$(gcc_spu_lexec_dir)/f951 \
	$(gcc_spu_lib_dir)/finclude \
	$(gcc_spu_lib_dir)/libcaf_single.a \
	$(gcc_spu_lib_dir)/libgfortran.spec \
	$(gcc_spu_lib_dir)/libgfortran{,begin}.a

ifeq ($(with_spumea64),yes)
    files_spuf95 += \
	$(gcc_spu_lib_dir)/mea64/libcaf_single.a \
	$(gcc_spu_lib_dir)/mea64/libgfortranbegin.a \
	$(gcc_spu_lib_dir)/mea64/libgfortran{,begin}.a
endif

ifneq ($(GFDL_INVARIANT_FREE),yes)
    files_spuf95 += \
	$(PF)/share/man/man1/spu-gfortran$(pkg_ver).1
endif

# ----------------------------------------------------------------------
$(binary_stamp)-spu: $(install_spu_stamp)
	dh_testdir
	dh_testroot

	mv $(install_spu_stamp) $(install_spu_stamp)-tmp

	dh_installdirs -p$(p_spugcc) $(dirs_spugcc)
	DH_COMPAT=2 dh_movefiles --sourcedir=$(d_spu) -p$(p_spugcc) $(files_spugcc)

	debian/dh_doclink -p$(p_spugcc) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_spugcc)

	-dh_strip -p$(p_spugcc) -X.o -Xlibgcc.a -Xlibgcov.a
	dh_compress -p$(p_spugcc)
	dh_fixperms -p$(p_spugcc)
	dh_shlibdeps -p$(p_spugcc)
	dh_gencontrol -p$(p_spugcc) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_spugcc)
	dh_md5sums -p$(p_spugcc)
	dh_builddeb -p$(p_spugcc)

	trap '' 1 2 3 15; touch $@; mv $(install_spu_stamp)-tmp $(install_spu_stamp)

$(binary_stamp)-spu-cxx: $(install_spu_stamp)
	dh_testdir
	dh_testroot

	mv $(install_spu_stamp) $(install_spu_stamp)-tmp

	dh_installdirs -p$(p_spucxx) $(dirs_spucxx)
	DH_COMPAT=2 dh_movefiles --sourcedir=$(d_spu) -p$(p_spucxx) $(files_spucxx)

	ln -sf $(BASE_VERSION) $(d_spucxx)/usr/spu/include/c++/$(GCC_VERSION)

	debian/dh_doclink -p$(p_spucxx) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_spucxx)

	-dh_strip -p$(p_spucxx)
	dh_compress -p$(p_spucxx)
	dh_fixperms -p$(p_spucxx)
	dh_shlibdeps -p$(p_spucxx)
	dh_gencontrol -p$(p_spucxx) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_spucxx)
	dh_md5sums -p$(p_spucxx)
	dh_builddeb -p$(p_spucxx)

	trap '' 1 2 3 15; touch $@; mv $(install_spu_stamp)-tmp $(install_spu_stamp)

$(binary_stamp)-spu-fortran: $(install_spu_stamp)
	dh_testdir
	dh_testroot

	mv $(install_spu_stamp) $(install_spu_stamp)-tmp

	dh_installdirs -p$(p_spuf95) $(dirs_spuf95)
	mv $(d_spu)/$(PF)/spu/lib/libgfortran.spec $(d_spu)/$(gcc_spu_lib_dir)/

	DH_COMPAT=2 dh_movefiles --sourcedir=$(d_spu) -p$(p_spuf95) $(files_spuf95)

	debian/dh_doclink -p$(p_spuf95) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_spuf95)

	-dh_strip -p$(p_spuf95)
	dh_compress -p$(p_spuf95)
	dh_fixperms -p$(p_spuf95)
	dh_shlibdeps -p$(p_spuf95)
	dh_gencontrol -p$(p_spuf95) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_spuf95)
	dh_md5sums -p$(p_spuf95)
	dh_builddeb -p$(p_spuf95)

	@echo "Listing installed files not included in any package:"
	-find $(d_spu) ! -type d

	trap '' 1 2 3 15; touch $@; mv $(install_spu_stamp)-tmp $(install_spu_stamp)
	touch $@
