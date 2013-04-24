$(lib_binaries)  += libasan
ifeq ($(with_lib64asan),yes)
  $(lib_binaries)  += lib64asan
endif
ifeq ($(with_lib32asan),yes)
  $(lib_binaries)	+= lib32asan
endif
ifeq ($(with_libn32asan),yes)
  $(lib_binaries)	+= libn32asan
endif
ifeq ($(with_libx32asan),yes)
  $(lib_binaries)	+= libx32asan
endif
ifeq ($(with_libhfasan),yes)
  $(lib_binaries)	+= libhfasan
endif
ifeq ($(with_libsfasan),yes)
  $(lib_binaries)	+= libsfasan
endif

define __do_asan
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l) $(d_d)
	dh_installdirs -p$(p_l) $(usr_lib$(2))
	DH_COMPAT=2 dh_movefiles -p$(p_l) $(usr_lib$(2))/libasan.so.*

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
			$(subst asan$(ASAN_SONAME),gcc$(GCC_SONAME),$(p_l)) \
			$(subst asan$(ASAN_SONAME),stdc++$(CXX_SONAME),$(p_l)) \
		,$(2))
	$(call cross_mangle_substvars,$(p_l))
	$(cross_gencontrol) dh_gencontrol -p$(p_l) -p$(p_d)	\
		-- -v$(DEB_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(p_l))
	dh_installdeb -p$(p_l) -p$(p_d)
	dh_md5sums -p$(p_l) -p$(p_d)
	dh_builddeb -p$(p_l) -p$(p_d)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

# ----------------------------------------------------------------------

do_asan = $(call __do_asan,lib$(1)asan$(ASAN_SONAME),$(1))

$(binary_stamp)-libasan: $(install_stamp)
	$(call do_asan,)

$(binary_stamp)-lib64asan: $(install_stamp)
	$(call do_asan,64)

$(binary_stamp)-lib32asan: $(install_stamp)
	$(call do_asan,32)

$(binary_stamp)-libn32asan: $(install_stamp)
	$(call do_asan,n32)

$(binary_stamp)-libx32asan: $(install_stamp)
	$(call do_asan,x32)

$(binary_stamp)-libhfasan: $(install_dependencies)
	$(call do_asan,hf)

$(binary_stamp)-libsfasan: $(install_dependencies)
	$(call do_asan,sf)
