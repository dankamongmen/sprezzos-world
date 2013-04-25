ifeq ($(with_libgcc),yes)
  $(lib_binaries)	+= libgcc
endif
ifeq ($(with_cdev),yes)
  $(lib_binaries)  += libgcc-dev
endif

ifeq ($(with_lib64gcc),yes)
  $(lib_binaries)	+= lib64gcc
endif
ifeq ($(with_lib64gccdev),yes)
  $(lib_binaries)  += lib64gcc-dev
endif
ifeq ($(with_lib32gcc),yes)
  $(lib_binaries)	+= lib32gcc
endif
ifeq ($(with_lib32gccdev),yes)
  $(lib_binaries)  += lib32gcc-dev
endif
ifeq ($(with_libn32gcc),yes)
  $(lib_binaries)	+= libn32gcc
endif
ifeq ($(with_libn32gccdev),yes)
  $(lib_binaries)  += libn32gcc-dev
endif
ifeq ($(with_libx32gcc),yes)
  $(lib_binaries)	+= libx32gcc
endif
ifeq ($(with_libx32gccdev),yes)
  $(lib_binaries)  += libx32gcc-dev
endif
ifeq ($(with_libhfgcc),yes)
  $(lib_binaries)	+= libhfgcc
endif
ifeq ($(with_libhfgccdev),yes)
  $(lib_binaries)  += libhfgcc-dev
endif
ifeq ($(with_libsfgcc),yes)
  $(lib_binaries)	+= libsfgcc
endif
ifeq ($(with_libsfgccdev),yes)
  $(lib_binaries)  += libsfgcc-dev
endif

header_files = \
	$(gcc_lib_dir)/include/std*.h \
	$(shell for h in \
		    README features.h arm_neon.h loongson.h \
		    {cpuid,decfloat,float,iso646,limits,mm3dnow,mm_malloc}.h \
		    {ppu_intrinsics,paired,spu2vmx,vec_types,si2vmx}.h \
		    {,a,b,e,i,n,p,s,t,w,x}mmintrin.h mmintrin-common.h \
		    {abm,avx,avx2,bmi,bmi2,f16c,fma,fma4,ia32,}intrin.h \
		    {lwp,lzcnt,popcnt,tbm,x86,xop,}intrin.h \
		    {cross-stdarg,syslimits,unwind,unwind-arm-common,varargs}.h; \
		do \
		  test -e $(d)/$(gcc_lib_dir)/include/$$h \
		    && echo $(gcc_lib_dir)/include/$$h; \
		  test -e $(d)/$(gcc_lib_dir)/include-fixed/$$h \
		    && echo $(gcc_lib_dir)/include-fixed/$$h; \
		done) \
	$(shell for d in \
		  asm bits gnu linux $(TARGET_ALIAS) \
		  $(subst $(DEB_TARGET_GNU_CPU),$(biarch_cpu),$(TARGET_ALIAS)); \
		do \
		  test -e $(d)/$(gcc_lib_dir)/include/$$d \
		    && echo $(gcc_lib_dir)/include/$$d; \
		  test -e $(d)/$(gcc_lib_dir)/include-fixed/$$d \
		    && echo $(gcc_lib_dir)/include-fixed/$$d; \
		done)

ifeq ($(with_libssp),yes)
    header_files += $(gcc_lib_dir)/include/ssp
endif
ifeq ($(with_gomp),yes)
    header_files += $(gcc_lib_dir)/include/omp.h
endif
ifeq ($(with_qmath),yes)
    header_files += $(gcc_lib_dir)/include/quadmath{,_weak}.h
endif

ifeq ($(DEB_TARGET_ARCH),ia64)
    header_files += $(gcc_lib_dir)/include/ia64intrin.h
endif

ifeq ($(DEB_TARGET_ARCH),m68k)
    header_files += $(gcc_lib_dir)/include/math-68881.h
endif

ifeq ($(DEB_TARGET_ARCH),$(findstring $(DEB_TARGET_ARCH),powerpc ppc64 powerpcspe))
    header_files += $(gcc_lib_dir)/include/{altivec.h,ppc-asm.h,spe.h}
endif

p_lgcc		= libgcc$(GCC_SONAME)$(cross_lib_arch)
p_lgccdbg	= libgcc$(GCC_SONAME)-dbg$(cross_lib_arch)
p_lgccdev	= libgcc-$(BASE_VERSION)-dev$(cross_lib_arch)
d_lgcc		= debian/$(p_lgcc)
d_lgccdbg	= debian/$(p_lgccdbg)
d_lgccdev	= debian/$(p_lgccdev)

p_l32gcc	= lib32gcc$(GCC_SONAME)$(cross_lib_arch)
p_l32gccdbg	= lib32gcc$(GCC_SONAME)-dbg$(cross_lib_arch)
p_l32gccdev	= lib32gcc-$(BASE_VERSION)-dev$(cross_lib_arch)
d_l32gcc	= debian/$(p_l32gcc)
d_l32gccdbg	= debian/$(p_l32gccdbg)
d_l32gccdev	= debian/$(p_l32gccdev)

p_l64gcc	= lib64gcc$(GCC_SONAME)$(cross_lib_arch)
p_l64gccdbg	= lib64gcc$(GCC_SONAME)-dbg$(cross_lib_arch)
p_l64gccdev	= lib64gcc-$(BASE_VERSION)-dev$(cross_lib_arch)
d_l64gcc	= debian/$(p_l64gcc)
d_l64gccdbg	= debian/$(p_l64gccdbg)
d_l64gccdev	= debian/$(p_l64gccdev)

p_ln32gcc	= libn32gcc$(GCC_SONAME)$(cross_lib_arch)
p_ln32gccdbg	= libn32gcc$(GCC_SONAME)-dbg$(cross_lib_arch)
p_ln32gccdev	= libn32gcc-$(BASE_VERSION)-dev$(cross_lib_arch)
d_ln32gcc	= debian/$(p_ln32gcc)
d_ln32gccdbg	= debian/$(p_ln32gccdbg)
d_ln32gccdev	= debian/$(p_ln32gccdev)

p_lx32gcc	= libx32gcc$(GCC_SONAME)$(cross_lib_arch)
p_lx32gccdbg	= libx32gcc$(GCC_SONAME)-dbg$(cross_lib_arch)
p_lx32gccdev	= libx32gcc-$(BASE_VERSION)-dev$(cross_lib_arch)
d_lx32gcc	= debian/$(p_lx32gcc)
d_lx32gccdbg	= debian/$(p_lx32gccdbg)
d_lx32gccdev	= debian/$(p_lx32gccdev)

p_lhfgcc	= libhfgcc$(GCC_SONAME)$(cross_lib_arch)
p_lhfgccdbg	= libhfgcc$(GCC_SONAME)-dbg$(cross_lib_arch)
p_lhfgccdev	= libhfgcc-$(BASE_VERSION)-dev$(cross_lib_arch)
d_lhfgcc	= debian/$(p_lhfgcc)
d_lhfgccdbg	= debian/$(p_lhfgccdbg)
d_lhfgccdev	= debian/$(p_lhfgccdev)

p_lsfgcc	= libsfgcc$(GCC_SONAME)$(cross_lib_arch)
p_lsfgccdbg	= libsfgcc$(GCC_SONAME)-dbg$(cross_lib_arch)
p_lsfgccdev	= libsfgcc-$(BASE_VERSION)-dev$(cross_lib_arch)
d_lsfgcc	= debian/$(p_lsfgcc)
d_lsfgccdbg	= debian/$(p_lsfgccdbg)
d_lsfgccdev	= debian/$(p_lsfgccdev)

# __do_gcc_devels(flavour,package,todir,fromdir)
define __do_gcc_devels
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	test -n "$(2)"
	rm -rf debian/$(2)
	dh_installdirs -p$(2) $(docdir) #TODO
	dh_installdirs -p$(2) $(3)

	$(call __do_gcc_devels2,$(1),$(2),$(3),$(4))

	debian/dh_doclink -p$(2) $(p_base)
	debian/dh_rmemptydirs -p$(2)

	dh_strip -p$(2)
	dh_compress -p$(2)
	$(cross_shlibdeps) dh_shlibdeps -p$(2)
	$(call cross_mangle_substvars,$(2))
	dh_fixperms -p$(2)
	dh_installdeb -p$(2)
	$(cross_gencontrol) dh_gencontrol -p$(2) -- -v$(DEB_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(2))
	dh_md5sums -p$(2)
	dh_builddeb -p$(2)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

# __do_gcc_devels2(flavour,package,todir,fromdir)
define __do_gcc_devels2
# stage1 builds static libgcc only
	$(if $(filter $(DEB_STAGE),stage1),,
		: # libgcc_s.so may be a linker script on some architectures
		set -e; \
		if [ -h $(4)/libgcc_s.so ]; then \
		  rm -f $(4)/libgcc_s.so; \
		  dh_link -p$(2) /$(libgcc_dir$(1))/libgcc_s.so.$(GCC_SONAME) \
		    $(3)/libgcc_s.so; \
		else \
		  mv $(4)/libgcc_s.so $(d)/$(3)/libgcc_s.so; \
		  dh_link -p$(2) /$(libgcc_dir$(1))/libgcc_s.so.$(GCC_SONAME) \
		    $(3)/libgcc_s.so.$(GCC_SONAME); \
		fi; \
		$(if $(1), dh_link -p$(2) /$(3)/libgcc_s.so \
		    $(gcc_lib_dir)/libgcc_s_$(1).so;)
	)
	DH_COMPAT=2 dh_movefiles -p$(2) \
		$(3)/{libgcc*,libgcov.a,*.o} \
		$(if $(1),,$(header_files)) # Only move headers for the "main" package
	# If building a flavour, add a lintian override
	$(if $(1),
		#TODO: use a file instead of a hacky echo
		# bu do we want to use one override file (in the source package) per
		# flavour or not since they are essentially the same?
		mkdir -p debian/$(2)/usr/share/lintian/overrides
		echo "$(2) binary: binary-from-other-architecture" \
			>> debian/$(2)/usr/share/lintian/overrides/$(2)
	)
	$(if $(filter yes, $(with_lib$(1)gmath)),
		$(call install_gcc_lib,libgcc-math,$(GCC_SONAME),$(1),$(2))
	)
	$(if $(filter yes, $(with_libssp)),
		$(call install_gcc_lib,libssp,$(SSP_SONAME),$(1),$(2))
	)
	$(if $(filter yes, $(with_ssp)),
		mv $(4)/libssp_nonshared.a debian/$(2)/$(3)/;
	)
	$(if $(filter yes, $(with_gomp)),
		$(call install_gcc_lib,libgomp,$(GOMP_SONAME),$(1),$(2))
	)
	$(if $(filter yes, $(with_itm)),
		$(call install_gcc_lib,libitm,$(ITM_SONAME),$(1),$(2))
	)
	$(if $(filter yes, $(with_qmath)),
		$(call install_gcc_lib,libquadmath,$(QMATH_SONAME),$(1),$(2))
	)
endef

# do_gcc_devels(flavour)
define do_gcc_devels
	$(call __do_gcc_devels,$(1),$(p_l$(1)gccdev),$(gcc_lib_dir$(1)),$(d)/$(usr_lib$(1)))
endef


define __do_libgcc
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l) $(d_d)

	dh_installdirs -p$(p_l) \
		$(docdir)/$(p_l) \
		$(libgcc_dir$(2))

	$(if $(filter yes,$(with_shared_libgcc)),
		mv $(d)/$(usr_lib$(2))/libgcc_s.so.$(GCC_SONAME) \
			$(d_l)/$(libgcc_dir$(2))/.
	)

	debian/dh_doclink -p$(p_l) $(if $(3),$(3),$(p_base))
	debian/dh_doclink -p$(p_d) $(if $(3),$(3),$(p_base))
	debian/dh_rmemptydirs -p$(p_l)
	debian/dh_rmemptydirs -p$(p_d)
	dh_strip -p$(p_l) --dbg-package=$(p_d)

	# see Debian #533843 for the __aeabi symbol handling; this construct is
	# just to include the symbols for dpkg versions older than 1.15.3 which
	# didn't allow bypassing the symbol blacklist
	$(if $(filter yes,$(with_shared_libgcc)),
		$(cross_makeshlibs) dh_makeshlibs -p$(p_l) -p$(p_d) \
			-- -v$(DEB_LIBGCC_VERSION)
		$(call cross_mangle_shlibs,$(p_l))
		$(if $(filter arm-linux-gnueabi%,$(DEB_TARGET_GNU_TYPE)),
			if head -1 $(d_l)/DEBIAN/symbols 2>/dev/null | grep -q '^lib'; then \
			  grep -q '^ __aeabi' $(d_l)/DEBIAN/symbols \
			    || cat debian/libgcc1.symbols.aeabi \
				>> $(d_l)/DEBIAN/symbols; \
			fi
		)
	)

	$(if $(DEB_STAGE),,
	    $(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_l) \
		$(call shlibdirs_to_search,,$(2))
	)
	$(call cross_mangle_substvars,$(p_l))

	$(if $(2),,	# only for native
		mkdir -p $(d_l)/usr/share/lintian/overrides
		echo '$(p_l): package-name-doesnt-match-sonames' \
			> $(d_l)/usr/share/lintian/overrides/$(p_l)
	)

	dh_compress -p$(p_l) -p$(p_d)
	dh_fixperms -p$(p_l) -p$(p_d)
	$(cross_gencontrol) dh_gencontrol -p$(p_l) -p$(p_d) \
		-- -v$(DEB_LIBGCC_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(p_l))

	dh_installdeb -p$(p_l) -p$(p_d)
	dh_md5sums -p$(p_l) -p$(p_d)
	dh_builddeb -p$(p_l) -p$(p_d)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

do_libgcc = $(call __do_libgcc,lib$(1)gcc$(GCC_SONAME),$(1),$(2))
# ----------------------------------------------------------------------

$(binary_stamp)-libgcc: $(install_dependencies)
ifeq ($(with_standalone_gcj),yes)
	$(call do_libgcc,,$(p_jbase))
else
	$(call do_libgcc,,)
endif

$(binary_stamp)-lib64gcc: $(install_dependencies)
	$(call do_libgcc,64,)

$(binary_stamp)-lib32gcc: $(install_dependencies)
	$(call do_libgcc,32,)

$(binary_stamp)-libn32gcc: $(install_dependencies)
	$(call do_libgcc,n32,)

$(binary_stamp)-libx32gcc: $(install_dependencies)
	$(call do_libgcc,x32,)

$(binary_stamp)-libhfgcc: $(install_dependencies)
	$(call do_libgcc,hf)

$(binary_stamp)-libsfgcc: $(install_dependencies)
	$(call do_libgcc,sf)

$(binary_stamp)-libgcc-dev: $(install_dependencies)
	$(call do_gcc_devels,)

$(binary_stamp)-lib64gcc-dev: $(install_dependencies)
	$(call do_gcc_devels,64)

$(binary_stamp)-lib32gcc-dev: $(install_dependencies)
	$(call do_gcc_devels,32)

$(binary_stamp)-libn32gcc-dev: $(install_dependencies)
	$(call do_gcc_devels,n32)

$(binary_stamp)-libx32gcc-dev: $(install_dependencies)
	$(call do_gcc_devels,x32)

$(binary_stamp)-libhfgcc-dev: $(install_dependencies)
	$(call do_gcc_devels,hf)

$(binary_stamp)-libsfgcc-dev: $(install_dependencies)
	$(call do_gcc_devels,sf)

