ifeq ($(with_libgo),yes)
  $(lib_binaries) += libgo
endif
ifeq ($(with_lib64go),yes)
  $(lib_binaries) += lib64go
endif
ifeq ($(with_lib32go),yes)
  $(lib_binaries) += lib32go
endif
ifeq ($(with_libn32go),yes)
  $(lib_binaries) += libn32go
endif
ifeq ($(with_libx32go),yes)
  $(lib_binaries) += libx32go
endif

arch_binaries  := $(arch_binaries) gccgo
ifneq (,$(filter yes, $(biarch64) $(biarch32) $(biarchn32) $(biarchx32)))
  arch_binaries  := $(arch_binaries) gccgo-multi
endif
ifneq ($(DEB_CROSS),yes)
  ifneq ($(GFDL_INVARIANT_FREE),yes)
    indep_binaries := $(indep_binaries) go-doc
  endif
endif

p_go	= gccgo$(pkg_ver)$(cross_bin_arch)
p_go_m	= gccgo$(pkg_ver)-multilib$(cross_bin_arch)
p_god	= gccgo$(pkg_ver)-doc
p_golib	= libgo$(GO_SONAME)$(cross_lib_arch)

d_go	= debian/$(p_go)
d_go_m	= debian/$(p_go_m)
d_god	= debian/$(p_god)
d_golib	= debian/$(p_golib)

dirs_go = \
	$(docdir)/$(p_xbase)/go \
	$(PF)/bin \
	$(gcc_lexec_dir) \
	$(gcc_lib_dir) \
	$(PF)/include \
	$(PF)/share/man/man1
files_go = \
	$(PF)/bin/$(cmd_prefix)gccgo$(pkg_ver) \
	$(gcc_lexec_dir)/go1

ifneq ($(GFDL_INVARIANT_FREE),yes)
  files_go += \
	$(PF)/share/man/man1/$(cmd_prefix)gccgo$(pkg_ver).1
endif

ifeq ($(with_standalone_go),yes)

  dirs_go += \
	$(gcc_lib_dir)/include \
	$(PF)/share/man/man1

# XXX: what about triarch mapping?
  files_go += \
	$(PF)/bin/{cpp,gcc,gcov}$(pkg_ver) \
	$(gcc_lexec_dir)/{collect2,lto1,lto-wrapper} \
	$(gcc_lexec_dir)/liblto_plugin.so{,.0,.0.0.0} \
	$(gcc_lib_dir)/{libgcc*,libgcov.a,*.o} \
	$(header_files) \
	$(shell test -e $(d)/$(gcc_lib_dir)/SYSCALLS.c.X \
		&& echo $(gcc_lib_dir)/SYSCALLS.c.X)

  ifneq ($(GFDL_INVARIANT_FREE),yes)
    files_go += \
	$(PF)/share/man/man1/{cpp,gcc,gcov}$(pkg_ver).1
  endif

  ifeq ($(biarch64),yes)
    files_go += $(gcc_lib_dir)/$(biarch64subdir)/{libgcc*,libgcov.a,*.o}
  endif
  ifeq ($(biarch32),yes)
    files_go += $(gcc_lib_dir)/$(biarch32subdir)/{libgcc*,*.o}
  endif
  ifeq ($(biarchn32),yes)
    files_go += $(gcc_lib_dir)/$(biarchn32subdir)/{libgcc*,libgcov.a,*.o}
  endif
  ifeq ($(biarchx32),yes)
    files_go += $(gcc_lib_dir)/$(biarchx32subdir)/{libgcc*,libgcov.a,*.o}
  endif
endif

# ----------------------------------------------------------------------
define __do_gccgo
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l) $(d_d)
	dh_installdirs -p$(p_l) $(usr_lib$(2))
	DH_COMPAT=2 dh_movefiles -p$(p_l) \
		$(usr_lib$(2))/libgo.so.* $(usr_lib$(2))/go

	debian/dh_doclink -p$(p_l) $(p_base)
	debian/dh_doclink -p$(p_d) $(p_base)

	dh_strip -p$(p_l) --dbg-package=$(p_d)
	dh_compress -p$(p_l) -p$(p_d)
	dh_fixperms -p$(p_l) -p$(p_d)
	$(cross_makeshlibs) dh_makeshlibs -p$(p_l)
	$(call cross_mangle_shlibs,$(p_l))
	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_l) \
		$(call shlibdirs_to_search,$(subst go$(GO_SONAME),gcc$(GCC_SONAME),$(p_l)),$(2))
	$(call cross_mangle_substvars,$(p_l))
	$(cross_gencontrol) dh_gencontrol -p$(p_l) -p$(p_d) \
		-- -v$(DEB_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(p_l))
	dh_installdeb -p$(p_l) -p$(p_d)
	dh_md5sums -p$(p_l) -p$(p_d)
	dh_builddeb -p$(p_l) -p$(p_d)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

do_gccgo = $(call __do_gccgo,lib$(1)go$(GO_SONAME),$(1))

define install_gccgo_lib
	mv $(d)/$(usr_lib$(3))/$(1).a debian/$(4)/$(gcc_lib_dir$(3))/
	rm -f $(d)/$(usr_lib$(3))/$(1)*.{la,so}
	dh_link -p$(4) \
	  /$(usr_lib$(3))/$(1).so.$(2) /$(gcc_lib_dir$(3))/$(1).so

endef

define do_go_dev
	dh_installdirs -p$(2) $(gcc_lib_dir$(1))
	DH_COMPAT=2 dh_movefiles -p$(2) \
		$(gcc_lib_dir$(1))/libgobegin.a
	$(call install_gccgo_lib,libgo,$(GO_SONAME),$(1),$(2))
endef
# ----------------------------------------------------------------------
$(binary_stamp)-libgo: $(install_stamp)
	$(call do_gccgo,)

$(binary_stamp)-lib64go: $(install_stamp)
	$(call do_gccgo,64)

$(binary_stamp)-lib32go: $(install_stamp)
	$(call do_gccgo,32)

$(binary_stamp)-libn32go: $(install_stamp)
	$(call do_gccgo,n32)

$(binary_stamp)-libx32go: $(install_stamp)
	$(call do_gccgo,x32)

# ----------------------------------------------------------------------
$(binary_stamp)-gccgo: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_go)
	dh_installdirs -p$(p_go) $(dirs_go)

	mv $(d)/$(usr_lib)/libgobegin.a \
		$(d)/$(gcc_lib_dir)/
	if [ -f $(d)/$(usr_lib64)/libgobegin.a ]; then \
	    mv $(d)/$(usr_lib64)/libgobegin.a \
		$(d)/$(gcc_lib_dir)/64/; \
	fi
	if [ -f $(d)/$(usr_lib32)/libgobegin.a ]; then \
	  if [ -d $(d)/$(gcc_lib_dir)/32 ]; then \
	    mv $(d)/$(usr_lib32)/libgobegin.a \
		$(d)/$(gcc_lib_dir)/32/; \
	  else \
	    mv $(d)/$(usr_lib32)/libgobegin.a \
		$(d)/$(gcc_lib_dir)/n32/; \
	  fi; \
	fi
	if [ -f $(d)/$(usr_libx32)/libgobegin.a ]; then \
	    mv $(d)/$(usr_libx32)/libgobegin.a \
		$(d)/$(gcc_lib_dir)/x32/; \
	fi

	DH_COMPAT=2 dh_movefiles -p$(p_go) $(files_go)

	$(call do_go_dev,,$(p_go))

ifneq ($(DEB_CROSS),yes)
	ln -sf gccgo$(pkg_ver) \
	    $(d_go)/$(PF)/bin/$(DEB_TARGET_GNU_TYPE)-gccgo$(pkg_ver)
	ln -sf gccgo$(pkg_ver) \
	    $(d_go)/$(PF)/bin/$(TARGET_ALIAS)-gccgo$(pkg_ver)
ifneq ($(GFDL_INVARIANT_FREE),yes)
	ln -sf gccgo$(pkg_ver).1 \
	    $(d_go)/$(PF)/share/man/man1/$(DEB_TARGET_GNU_TYPE)-gccgo$(pkg_ver).1
	ln -sf gccgo$(pkg_ver).1 \
	    $(d_go)/$(PF)/share/man/man1/$(TARGET_ALIAS)-gccgo$(pkg_ver).1
endif
endif

	debian/dh_doclink -p$(p_go) $(p_xbase)

#	cp -p $(srcdir)/gcc/go/ChangeLog \
#		$(d_go)/$(docdir)/$(p_base)/go/changelog
	debian/dh_rmemptydirs -p$(p_go)

	dh_strip -p$(p_go)
	dh_compress -p$(p_go)
	dh_fixperms -p$(p_go)
	dh_shlibdeps -p$(p_go)
	dh_gencontrol -p$(p_go) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_go)
	dh_md5sums -p$(p_go)
	dh_builddeb -p$(p_go)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-gccgo-multi: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_go_m)
	dh_installdirs -p$(p_go_m) $(docdir)

	$(foreach flavour,$(flavours), \
		$(call do_go_dev,$(flavour),$(p_go_m)))

	debian/dh_doclink -p$(p_go_m) $(p_xbase)
	debian/dh_rmemptydirs -p$(p_go_m)
	dh_strip -p$(p_go_m)
	dh_compress -p$(p_go_m)
	dh_fixperms -p$(p_go_m)
	dh_shlibdeps -p$(p_go_m)
	dh_gencontrol -p$(p_go_m) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_go_m)
	dh_md5sums -p$(p_go_m)
	dh_builddeb -p$(p_go_m)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-go-doc: $(build_html_stamp) $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_god)
	dh_installdirs -p$(p_god) \
		$(docdir)/$(p_xbase)/go \
		$(PF)/share/info
	DH_COMPAT=2 dh_movefiles -p$(p_god) \
		$(PF)/share/info/gccgo*

	debian/dh_doclink -p$(p_god) $(p_xbase)
	dh_installdocs -p$(p_god)
	rm -f $(d_god)/$(docdir)/$(p_xbase)/copyright
	cp -p html/gccgo.html $(d_god)/$(docdir)/$(p_xbase)/go/

	dh_compress -p$(p_god)
	dh_fixperms -p$(p_god)
	dh_installdeb -p$(p_god)
	dh_gencontrol -p$(p_god) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_god)
	dh_builddeb -p$(p_god)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
