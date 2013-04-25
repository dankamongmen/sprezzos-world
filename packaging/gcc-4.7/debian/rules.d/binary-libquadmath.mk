$(lib_binaries)  += libqmath
ifeq ($(with_lib64qmath),yes)
  $(lib_binaries)  += lib64qmath
endif
ifeq ($(with_lib32qmath),yes)
  $(lib_binaries)	+= lib32qmath
endif
ifeq ($(with_libn32qmath),yes)
  $(lib_binaries)	+= libn32qmath
endif
ifeq ($(with_libx32qmath),yes)
  $(lib_binaries)	+= libx32qmath
endif
ifeq ($(with_libhfqmath),yes)
  $(lib_binaries)	+= libhfqmath
endif
ifeq ($(with_libsfqmath),yes)
  $(lib_binaries)	+= libsfqmath
endif

define __do_qmath
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l) $(d_d)
	dh_installdirs -p$(p_l) $(usr_lib$(2))
	DH_COMPAT=2 dh_movefiles -p$(p_l) $(usr_lib$(2))/libquadmath.so.*

	debian/dh_doclink -p$(p_l) $(p_base)
	debian/dh_doclink -p$(p_d) $(p_base)

	dh_strip -p$(p_l) --dbg-package=$(p_d)
	dh_compress -p$(p_l) -p$(p_d)
	dh_fixperms -p$(p_l) -p$(p_d)
	$(cross_makeshlibs) dh_makeshlibs -p$(p_l)
	$(call cross_mangle_shlibs,$(p_l))
	$(cross_shlibdeps) dh_shlibdeps -p$(p_l) \
		$(call shlibdirs_to_search,,$(2))
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

do_qmath = $(call __do_qmath,lib$(1)quadmath$(QMATH_SONAME),$(1))

$(binary_stamp)-libqmath: $(install_stamp)
	$(call do_qmath,)

$(binary_stamp)-lib64qmath: $(install_stamp)
	$(call do_qmath,64)

$(binary_stamp)-lib32qmath: $(install_stamp)
	$(call do_qmath,32)

$(binary_stamp)-libn32qmath: $(install_stamp)
	$(call do_qmath,n32)

$(binary_stamp)-libx32qmath: $(install_stamp)
	$(call do_qmath,x32)

$(binary_stamp)-libhfqmath: $(install_stamp)
	$(call do_qmath,hf)

$(binary_stamp)-libsfqmath: $(install_stamp)
	$(call do_qmath,sf)
