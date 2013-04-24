$(lib_binaries)  += libtsan
ifeq (0,1)
ifeq ($(with_lib64tsan),yes)
  $(lib_binaries)  += lib64tsan
endif
ifeq ($(with_lib32tsan),yes)
  $(lib_binaries)	+= lib32tsan
endif
ifeq ($(with_libn32tsan),yes)
  $(lib_binaries)	+= libn32tsan
endif
ifeq ($(with_libx32tsan),yes)
  $(lib_binaries)	+= libx32tsan
endif
ifeq ($(with_libhftsan),yes)
  $(lib_binaries)	+= libhftsan
endif
ifeq ($(with_libsftsan),yes)
  $(lib_binaries)	+= libsftsan
endif
endif

define __do_tsan
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l) $(d_d)
	dh_installdirs -p$(p_l) $(usr_lib$(2))
	DH_COMPAT=2 dh_movefiles -p$(p_l) $(usr_lib$(2))/libtsan.so.*

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
			$(subst tsan$(TSAN_SONAME),gcc$(GCC_SONAME),$(p_l)) \
			$(subst tsan$(TSAN_SONAME),stdc++$(CXX_SONAME),$(p_l)) \
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

do_tsan = $(call __do_tsan,lib$(1)tsan$(TSAN_SONAME),$(1))

$(binary_stamp)-libtsan: $(install_stamp)
	$(call do_tsan,)

$(binary_stamp)-lib64tsan: $(install_stamp)
	$(call do_tsan,64)

$(binary_stamp)-lib32tsan: $(install_stamp)
	$(call do_tsan,32)

$(binary_stamp)-libn32tsan: $(install_stamp)
	$(call do_tsan,n32)

$(binary_stamp)-libx32tsan: $(install_stamp)
	$(call do_tsan,x32)

$(binary_stamp)-libhftsan: $(install_dependencies)
	$(call do_tsan,hf)

$(binary_stamp)-libsftsan: $(install_dependencies)
	$(call do_tsan,sf)
