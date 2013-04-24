ifeq ($(with_dev),yes)
  $(lib_binaries)  += libmudflapdev
endif

ifeq ($(with_libmudflap),yes)
  $(lib_binaries)  += libmudflap
  ifeq ($(with_lib64mudflap),yes)
    $(lib_binaries)  += lib64mudflap
  endif
  ifeq ($(with_lib32mudflap),yes)
    $(lib_binaries)	+= lib32mudflap
  endif
  ifeq ($(with_libn32mudflap),yes)
    $(lib_binaries)	+= libn32mudflap
  endif
  ifeq ($(with_libx32mudflap),yes)
    $(lib_binaries)	+= libx32mudflap
  endif
  ifeq ($(with_libhfmudflap),yes)
    $(lib_binaries)	+= libhfmudflap
  endif
  ifeq ($(with_libsfmudflap),yes)
    $(lib_binaries)	+= libsfmudflap
  endif
endif

p_mfd	= libmudflap$(MUDFLAP_SONAME)$(pkg_ver)-dev$(cross_lib_arch)
d_mfd	= debian/$(p_mfd)

define __do_mudflap
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l) $(d_d)
	dh_installdirs -p$(p_l) $(usr_lib$(2))
	DH_COMPAT=2 dh_movefiles -p$(p_l) $(usr_lib$(2))/libmudflap*.so.*

	debian/dh_doclink -p$(p_l) $(p_base)
	debian/dh_doclink -p$(p_d) $(p_base)

	debian/dh_rmemptydirs -p$(p_l)

	dh_strip -p$(p_l) --dbg-package=$(p_d)
	dh_compress -p$(p_l) -p$(p_d)
	dh_fixperms -p$(p_l) -p$(p_d)
	$(cross_makeshlibs) dh_makeshlibs -p$(p_l) -V '$(p_l) (>= $(DEB_SOVERSION))'
	$(call cross_mangle_shlibs,$(p_l))
	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_l) \
		$(call shlibdirs_to_search,,$(2))
	$(call cross_mangle_substvars,$(p_l))
	$(cross_gencontrol) dh_gencontrol -p$(p_l) -p$(p_d) \
		-- -v$(DEB_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(p_l))
	dh_installdeb -p$(p_l) -p$(p_d)
	dh_md5sums -p$(p_l) -p$(p_d)
	dh_builddeb -p$(p_l) -p$(p_d)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

do_mudflap = $(call __do_mudflap,lib$(1)mudflap$(MUDFLAP_SONAME),$(1))

define __do_mudflap_dev
	dh_installdirs -p$(p_mfd) $(1)
	DH_COMPAT=2 dh_movefiles -p$(p_mfd) $(1)/libmudflap*.a
	rm -f $(d)/$(2)/libmudflap*.so
	mv $(d)/$(2)/libmudflap*.a $(d_mfd)/$(1)/
	dh_link -p$(p_mfd) \
	  /$(2)/libmudflap.so.$(MUDFLAP_SONAME) /$(1)/libmudflap.so \
	  /$(2)/libmudflapth.so.$(MUDFLAP_SONAME) /$(1)/libmudflapth.so
endef

do_mudflap_dev = $(call __do_mudflap_dev,$(gcc_lib_dir$(1)),$(usr_lib$(1)))

# ----------------------------------------------------------------------

$(binary_stamp)-libmudflap: $(install_stamp)
	$(call do_mudflap,)

$(binary_stamp)-lib64mudflap: $(install_stamp)
	$(call do_mudflap,64)

$(binary_stamp)-lib32mudflap: $(install_stamp)
	$(call do_mudflap,32)

$(binary_stamp)-libn32mudflap: $(install_stamp)
	$(call do_mudflap,n32)

$(binary_stamp)-libx32mudflap: $(install_stamp)
	$(call do_mudflap,x32)

$(binary_stamp)-libhfmudflap: $(install_stamp)
	$(call do_mudflap,hf)

$(binary_stamp)-libsfmudflap: $(install_stamp)
	$(call do_mudflap,sf)

$(binary_stamp)-libmudflapdev: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_mfd)
	dh_installdirs -p$(p_mfd) $(docdir)/$(p_base)/mudflap

	DH_COMPAT=2 dh_movefiles -p$(p_mfd) $(gcc_lib_dir)/include/mf-runtime.h

	$(call do_mudflap_dev,)
ifeq ($(with_lib32mudflap),yes)
	$(call do_mudflap_dev,32)
endif
ifeq ($(with_lib64mudflap),yes)
	$(call do_mudflap_dev,64)
endif
ifeq ($(with_libn32mudflap),yes)
	$(call do_mudflap_dev,n32)
endif
ifeq ($(with_libx32mudflap),yes)
	$(call do_mudflap_dev,x32)
endif
ifeq ($(with_libhfmudflap),yes)
	$(call do_mudflap_dev,hf)
endif
ifeq ($(with_libsfmudflap),yes)
	$(call do_mudflap_dev,sf)
endif

	cp -p $(srcdir)/libmudflap/ChangeLog \
		$(d_mfd)/$(docdir)/$(p_base)/mudflap/changelog

	debian/dh_doclink -p$(p_mfd) $(p_base)
	debian/dh_rmemptydirs -p$(p_mfd)

	dh_strip -p$(p_mfd)
	dh_compress -p$(p_mfd)
	dh_fixperms -p$(p_mfd)
	$(cross_shlibdeps) dh_shlibdeps -p$(p_mfd)
	$(cross_gencontrol) dh_gencontrol -p$(p_mfd) \
		-- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_mfd)
	dh_md5sums -p$(p_mfd)
	dh_builddeb -p$(p_mfd)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
