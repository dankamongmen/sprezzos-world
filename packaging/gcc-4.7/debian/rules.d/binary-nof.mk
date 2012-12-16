arch_binaries  := $(arch_binaries) nof

p_nof  = gcc$(pkg_ver)-nof
d_nof  = debian/$(p_nof)

dirs_nof = \
	$(docdir) \
	$(usr_lib)/nof
ifeq ($(with_cdev),yes)
  dirs_nof += \
	$(gcc_lib_dir)/nof
endif

ifeq ($(with_cdev),yes)
  files_nof = \
	$(libgcc_dir)/libgcc_s_nof.so.$(GCC_SONAME) \
	$(gcc_lib_dir)/libgcc_s_nof.so \
	$(usr_lib)/nof \
	$(gcc_lib_dir)/nof
else
  files_nof = \
	$(usr_lib)/libgcc_s_nof.so.$(GCC_SONAME)
endif

# ----------------------------------------------------------------------
$(binary_stamp)-nof: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	mv $(d)/$(usr_lib)/libgcc_s_nof.so.$(GCC_SONAME) $(d)/$(libgcc_dir)/.
	rm -f $(d)/$(usr_lib)/libgcc_s_nof.so
	ln -sf $(libgcc_dir)/libgcc_s_nof.so.$(GCC_SONAME) \
		$(d)/$(gcc_lib_dir)/libgcc_s_nof.so

	rm -rf $(d_nof)
	dh_installdirs -p$(p_nof) $(dirs_nof)
	DH_COMPAT=2 dh_movefiles -p$(p_nof) $(files_nof)
	debian/dh_doclink -p$(p_nof) $(p_base)
	dh_strip -p$(p_nof)
	dh_compress -p$(p_nof)
	dh_fixperms -p$(p_nof)
	dh_shlibdeps -p$(p_nof)

	dh_makeshlibs -p$(p_nof)
	: # Only keep the shlibs file for the libgcc_s_nof library
	fgrep libgcc_s_nof debian/$(p_nof)/DEBIAN/shlibs \
		> debian/$(p_nof)/DEBIAN/shlibs.tmp
	mv -f debian/$(p_nof)/DEBIAN/shlibs.tmp debian/$(p_nof)/DEBIAN/shlibs

	dh_gencontrol -p$(p_nof) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_nof)
	dh_md5sums -p$(p_nof)
	dh_builddeb -p$(p_nof)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
