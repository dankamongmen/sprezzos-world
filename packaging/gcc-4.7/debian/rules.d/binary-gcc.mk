ifneq (,$(filter yes, $(biarch64) $(biarch32) $(biarchn32) $(biarchx32) $(biarchhf) $(biarchsf)))
  arch_binaries  := $(arch_binaries) gcc-multi
endif
ifeq ($(with_plugins),yes)
  arch_binaries  := $(arch_binaries) gcc-plugindev
endif

arch_binaries  := $(arch_binaries) gcc

ifneq ($(DEB_CROSS),yes)
  ifneq ($(GFDL_INVARIANT_FREE),yes)
    indep_binaries := $(indep_binaries) gcc-doc
  endif
  ifeq ($(with_nls),yes)
    indep_binaries := $(indep_binaries) gcc-locales
  endif
endif

# gcc must be moved after g77 and g++
# not all files $(PF)/include/*.h are part of gcc,
# but it becomes difficult to name all these files ...

dirs_gcc = \
	$(docdir)/$(p_xbase)/{gcc,libssp,gomp,itm,quadmath} \
	$(PF)/bin \
	$(gcc_lexec_dir) \
	$(gcc_lib_dir)/{include,include-fixed} \
	$(PF)/share/man/man1 $(libgcc_dir)

# XXX: what about triarch mapping?
files_gcc = \
	$(PF)/bin/$(cmd_prefix){gcc,gcov}$(pkg_ver) \
	$(PF)/bin/$(cmd_prefix)gcc-{ar,ranlib,nm}$(pkg_ver) \
	$(PF)/share/man/man1/$(cmd_prefix)gcc-{ar,nm,ranlib}$(pkg_ver).1 \
	$(gcc_lexec_dir)/{collect2,lto1,lto-wrapper} \
	$(shell test -e $(d)/$(gcc_lib_dir)/SYSCALLS.c.X \
		&& echo $(gcc_lib_dir)/SYSCALLS.c.X)

ifneq ($(GFDL_INVARIANT_FREE),yes)
    files_gcc += \
	$(PF)/share/man/man1/$(cmd_prefix){gcc,gcov}$(pkg_ver).1
endif

usr_doc_files = debian/README.Bugs \
	$(shell test -f $(srcdir)/FAQ && echo $(srcdir)/FAQ) \
	$(shell test -f test-summary && echo test-summary)

p_loc	= gcc$(pkg_ver)-locales
d_loc	= debian/$(p_loc)

p_gcc_m	= gcc$(pkg_ver)-multilib$(cross_bin_arch)
d_gcc_m	= debian/$(p_gcc_m)

p_pld	= gcc$(pkg_ver)-plugin-dev$(cross_bin_arch)
d_pld	= debian/$(p_pld)

# ----------------------------------------------------------------------
$(binary_stamp)-gcc: $(install_dependencies)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_gcc)
	dh_installdirs -p$(p_gcc) $(dirs_gcc)

ifeq ($(with_linaro_branch),yes)
	if [ -f $(srcdir)/ChangeLog.linaro ]; then \
	  cp -p $(srcdir)/ChangeLog.linaro \
		$(d_gcc)/$(docdir)/$(p_xbase)/changelog.linaro; \
	fi
endif
ifeq ($(with_libssp),yes)
	cp -p $(srcdir)/libssp/ChangeLog \
		$(d_gcc)/$(docdir)/$(p_xbase)/libssp/changelog
endif
ifeq ($(with_gomp),yes)
	mv $(d)/$(usr_lib)/libgomp*.spec $(d_gcc)/$(gcc_lib_dir)/
	cp -p $(srcdir)/libgomp/ChangeLog \
		$(d_gcc)/$(docdir)/$(p_xbase)/gomp/changelog
endif
ifeq ($(with_itm),yes)
	mv $(d)/$(usr_lib)/libitm*.spec $(d_gcc)/$(gcc_lib_dir)/
	cp -p $(srcdir)/libitm/ChangeLog \
		$(d_gcc)/$(docdir)/$(p_xbase)/itm/changelog
endif
ifeq ($(with_qmath),yes)
	cp -p $(srcdir)/libquadmath/ChangeLog \
		$(d_gcc)/$(docdir)/$(p_xbase)/quadmath/changelog
endif

	DH_COMPAT=2 dh_movefiles -p$(p_gcc) $(files_gcc)

ifneq ($(DEB_CROSS),yes)
	ln -sf gcc$(pkg_ver) \
	    $(d_gcc)/$(PF)/bin/$(DEB_TARGET_GNU_TYPE)-gcc$(pkg_ver)
	ln -sf gcc$(pkg_ver) \
	    $(d_gcc)/$(PF)/bin/$(TARGET_ALIAS)-gcc$(pkg_ver)
	for i in ar ranlib nm; do \
	  ln -sf gcc-$$i$(pkg_ver) \
	    $(d_gcc)/$(PF)/bin/$(DEB_TARGET_GNU_TYPE)-gcc-$$i$(pkg_ver); \
	  ln -sf gcc-$$i$(pkg_ver) \
	    $(d_gcc)/$(PF)/bin/$(TARGET_ALIAS)-gcc-$$i$(pkg_ver); \
	done
ifneq ($(GFDL_INVARIANT_FREE),yes)
	ln -sf gcc$(pkg_ver).1 \
	    $(d_gcc)/$(PF)/share/man/man1/$(DEB_TARGET_GNU_TYPE)-gcc$(pkg_ver).1
	ln -sf gcc$(pkg_ver).1 \
	    $(d_gcc)/$(PF)/share/man/man1/$(TARGET_ALIAS)-gcc$(pkg_ver).1
endif
endif

#	dh_installdebconf
	debian/dh_doclink -p$(p_gcc) $(p_xbase)
	cp -p $(usr_doc_files) $(d_gcc)/$(docdir)/$(p_xbase)/.
	if [ -f testsuite-comparision ]; then \
	  cp -p testsuite-comparision $(d_gcc)/$(docdir)/$(p_xbase)/. ; \
	fi
	cp -p debian/README.ssp $(d_gcc)/$(docdir)/$(p_xbase)/
	cp -p debian/NEWS.gcc $(d_gcc)/$(docdir)/$(p_xbase)/NEWS
	cp -p debian/NEWS.html $(d_gcc)/$(docdir)/$(p_xbase)/NEWS.html
	cp -p $(srcdir)/ChangeLog $(d_gcc)/$(docdir)/$(p_xbase)/changelog
	cp -p $(srcdir)/gcc/ChangeLog \
		$(d_gcc)/$(docdir)/$(p_xbase)/gcc/changelog
	if [ -f $(builddir)/gcc/.bad_compare ]; then \
	  ( \
	    echo "The comparision of the stage2 and stage3 object files shows differences."; \
	    echo "The Debian package was modified to ignore these differences."; \
	    echo ""; \
	    echo "The following files differ:"; \
	    echo ""; \
	    cat $(builddir)/gcc/.bad_compare; \
	  ) > $(d_gcc)/$(docdir)/$(p_xbase)/BOOTSTRAP_COMPARISION_FAILURE; \
	else \
	  true; \
	fi
	debian/dh_rmemptydirs -p$(p_gcc)
	dh_strip -p$(p_gcc)
	dh_compress -p$(p_gcc) -X README.Bugs
	dh_fixperms -p$(p_gcc)
	dh_shlibdeps -p$(p_gcc)
	dh_gencontrol -p$(p_gcc) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_gcc)
	dh_md5sums -p$(p_gcc)
	dh_builddeb -p$(p_gcc)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

	: # remove empty directories, when all components are in place
	-find $(d) -type d -empty -delete

	@echo "Listing installed files not included in any package:"
	-find $(d) ! -type d

# ----------------------------------------------------------------------

$(binary_stamp)-gcc-multi: $(install_dependencies)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_gcc_m)
	dh_installdirs -p$(p_gcc_m) $(docdir)

	debian/dh_doclink -p$(p_gcc_m) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_gcc_m)

	dh_strip -p$(p_gcc_m)
	dh_compress -p$(p_gcc_m)
	dh_shlibdeps -p$(p_gcc_m)
	dh_fixperms -p$(p_gcc_m)
	dh_installdeb -p$(p_gcc_m)
	dh_gencontrol -p$(p_gcc_m) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_gcc_m)
	dh_builddeb -p$(p_gcc_m)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-gcc-plugindev: $(install_dependencies)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_pld)
	dh_installdirs -p$(p_pld) \
		$(docdir) \
		$(gcc_lib_dir)/plugin/include/config/arm
	DH_COMPAT=2 dh_movefiles -p$(p_pld) \
		$(gcc_lib_dir)/plugin

	: # FIXME: see #645018, this only works for the native build :-/
	if [ $(DEB_BUILD_ARCH) = $(DEB_HOST_ARCH) ] && [ $(DEB_HOST_ARCH) = $(DEB_TARGET_ARCH) ]; then \
	  cp $(builddir)/gcc/build/gengtype $(d_pld)/$(gcc_lib_dir)/; \
	  cp $(builddir)/gcc/gtype.state $(d_pld)/$(gcc_lib_dir)/; \
	fi

ifneq (,$(filter arm% mips% sh% sparc%,$(DEB_TARGET_GNU_CPU)))
	# see GCC #45078; vxworks-dummy.h is included for cpu_type in arm,
	# i386, mips, sh and sparc but only installed when it's i386; copy it
	# manually on the other arches where it's included
	cp $(srcdir)/gcc/config/vxworks-dummy.h \
		$(d_pld)/$(gcc_lib_dir)/plugin/include/config/
endif
	cp $(srcdir)/gcc/config/arm/arm-cores.def \
		$(d_pld)/$(gcc_lib_dir)/plugin/include/config/arm/

	debian/dh_doclink -p$(p_pld) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_pld)

	dh_strip -p$(p_pld)
	dh_compress -p$(p_pld)
	dh_shlibdeps -p$(p_pld)
	dh_fixperms -p$(p_pld)
	dh_installdeb -p$(p_pld)
	dh_gencontrol -p$(p_pld) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_pld)
	dh_builddeb -p$(p_pld)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-gcc-locales: $(install_dependencies)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_loc)
	dh_installdirs -p$(p_loc) \
		$(docdir)
	DH_COMPAT=2 dh_movefiles -p$(p_loc) \
		$(PF)/share/locale/*/*/cpplib*.* \
		$(PF)/share/locale/*/*/gcc*.*

	debian/dh_doclink -p$(p_loc) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_loc)

	dh_compress -p$(p_loc)
	dh_fixperms -p$(p_loc)
	dh_installdeb -p$(p_loc)
	dh_gencontrol -p$(p_loc) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_loc)
	dh_builddeb -p$(p_loc)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-gcc-doc: $(build_html_stamp) $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_doc)
	dh_installdirs -p$(p_doc) \
		$(docdir)/$(p_xbase) \
		$(PF)/share/info
	DH_COMPAT=2 dh_movefiles -p$(p_doc) \
		$(PF)/share/info/cpp{,internals}-* \
		$(PF)/share/info/gcc{,int}-* \
		$(PF)/share/info/lib{gomp,itm}-* \
		$(if $(with_qmath),$(PF)/share/info/libquadmath-*)
	rm -f $(d_doc)/$(PF)/share/info/gccinst*

ifeq ($(with_gomp),yes)
	$(MAKE) -C $(buildlibdir)/libgomp stamp-build-info
	cp -p $(buildlibdir)/libgomp/libgomp$(pkg_ver).info $(d_doc)/$(PF)/share/info/
endif
ifeq ($(with_itm),yes)
	-$(MAKE) -C $(buildlibdir)/libitm stamp-build-info
	if [ -f $(buildlibdir)/libitm/libitm$(pkg_ver).info ]; then \
	  cp -p $(buildlibdir)/libitm/libitm$(pkg_ver).info $(d_doc)/$(PF)/share/info/; \
	fi
endif

	debian/dh_doclink -p$(p_doc) $(p_xbase)
	dh_installdocs -p$(p_doc) html/gcc.html html/gccint.html
ifeq ($(with_gomp),yes)
	cp -p html/libgomp.html $(d_doc)/usr/share/doc/$(p_doc)/
endif
ifeq ($(with_itm),yes)
	cp -p html/libitm.html $(d_doc)/usr/share/doc/$(p_doc)/
endif
ifeq ($(with_qmath),yes)
	cp -p html/libquadmath.html $(d_doc)/usr/share/doc/$(p_doc)/
endif
	rm -f $(d_doc)/$(docdir)/$(p_xbase)/copyright
	debian/dh_rmemptydirs -p$(p_doc)

	dh_compress -p$(p_doc)
	dh_fixperms -p$(p_doc)
	dh_installdeb -p$(p_doc)
	dh_gencontrol -p$(p_doc) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_doc)
	dh_builddeb -p$(p_doc)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
