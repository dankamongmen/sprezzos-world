
ifeq ($(with_libcxx),yes)
  $(lib_binaries)  += libstdcxx
endif
ifeq ($(with_lib64cxx),yes)
  $(lib_binaries)  += lib64stdcxx
endif
ifeq ($(with_lib64cxxdev),yes)
  $(lib_binaries)	+= lib64stdcxx-dev
endif
ifeq ($(with_lib64cxxdbg),yes)
  $(lib_binaries)	+= lib64stdcxxdbg
endif
ifeq ($(with_lib32cxx),yes)
  $(lib_binaries)	+= lib32stdcxx
endif
ifeq ($(with_lib32cxxdev),yes)
  $(lib_binaries)	+= lib32stdcxx-dev
endif
ifeq ($(with_lib32cxxdbg),yes)
  $(lib_binaries)	+= lib32stdcxxdbg
endif
ifeq ($(with_libn32cxx),yes)
  $(lib_binaries)	+= libn32stdcxx
endif
ifeq ($(with_libn32cxxdev),yes)
  $(lib_binaries)	+= libn32stdcxx-dev
endif
ifeq ($(with_libn32cxxdbg),yes)
  $(lib_binaries)	+= libn32stdcxxdbg
endif
ifeq ($(with_libx32cxx),yes)
  $(lib_binaries)	+= libx32stdcxx
endif
ifeq ($(with_libx32cxxdev),yes)
  $(lib_binaries)	+= libx32stdcxx-dev
endif
ifeq ($(with_libx32cxxdbg),yes)
  $(lib_binaries)	+= libx32stdcxxdbg
endif
ifeq ($(with_libhfcxx),yes)
  $(lib_binaries)	+= libhfstdcxx
endif
ifeq ($(with_libhfcxxdev),yes)
    $(lib_binaries)	+= libhfstdcxx-dev
endif
ifeq ($(with_libhfcxxdbg),yes)
  $(lib_binaries)	+= libhfstdcxxdbg
endif
ifeq ($(with_libsfcxx),yes)
  $(lib_binaries)	+= libsfstdcxx
endif
ifeq ($(with_libsfcxxdev),yes)
  $(lib_binaries)	+= libsfstdcxx-dev
endif
ifeq ($(with_libsfcxxdbg),yes)
  $(lib_binaries)	+= libsfstdcxxdbg
endif

ifeq ($(with_cxxdev),yes)
  $(lib_binaries)  += libstdcxx-dev
  ifneq ($(DEB_CROSS),yes)
    indep_binaries := $(indep_binaries) libstdcxx-doc
  endif
endif

libstdc_ext = -$(BASE_VERSION)

p_lib	= libstdc++$(CXX_SONAME)$(cross_lib_arch)
p_lib64	= lib64stdc++$(CXX_SONAME)$(cross_lib_arch)
p_lib32	= lib32stdc++$(CXX_SONAME)$(cross_lib_arch)
p_libn32= libn32stdc++$(CXX_SONAME)$(cross_lib_arch)
p_libx32= libx32stdc++$(CXX_SONAME)$(cross_lib_arch)
p_libhf	= libhfstdc++$(CXX_SONAME)$(cross_lib_arch)
p_libsf	= libsfstdc++$(CXX_SONAME)$(cross_lib_arch)
p_dev	= libstdc++$(CXX_SONAME)$(libstdc_ext)-dev$(cross_lib_arch)
p_pic	= libstdc++$(CXX_SONAME)$(libstdc_ext)-pic$(cross_lib_arch)
p_dbg	= libstdc++$(CXX_SONAME)$(libstdc_ext)-dbg$(cross_lib_arch)
p_dbg64	= lib64stdc++$(CXX_SONAME)$(libstdc_ext)-dbg$(cross_lib_arch)
p_dbg32	= lib32stdc++$(CXX_SONAME)$(libstdc_ext)-dbg$(cross_lib_arch)
p_dbgn32= libn32stdc++$(CXX_SONAME)$(libstdc_ext)-dbg$(cross_lib_arch)
p_dbgx32= libx32stdc++$(CXX_SONAME)$(libstdc_ext)-dbg$(cross_lib_arch)
p_dbghf	= libhfstdc++$(CXX_SONAME)$(libstdc_ext)-dbg$(cross_lib_arch)
p_dbgsf	= libsfstdc++$(CXX_SONAME)$(libstdc_ext)-dbg$(cross_lib_arch)
p_libd	= libstdc++$(CXX_SONAME)$(libstdc_ext)-doc

d_lib	= debian/$(p_lib)
d_lib64	= debian/$(p_lib64)
d_lib32	= debian/$(p_lib32)
d_libn32= debian/$(p_libn32)
d_libx32= debian/$(p_libx32)
d_libhf	= debian/$(p_libhf)
d_libsf	= debian/$(p_libsf)
d_dev	= debian/$(p_dev)
d_pic	= debian/$(p_pic)
d_dbg	= debian/$(p_dbg)
d_dbg64	= debian/$(p_dbg64)
d_dbg32	= debian/$(p_dbg32)
d_dbghf	= debian/$(p_dbghf)
d_dbgsf	= debian/$(p_dbgsf)
d_libd	= debian/$(p_libd)

dirs_dev = \
	$(docdir)/$(p_base)/C++ \
	$(usr_lib) \
	$(gcc_lib_dir)/include \
	$(cxx_inc_dir)

files_dev = \
	$(cxx_inc_dir)/ \
	$(gcc_lib_dir)/libstdc++.{a,so} \
	$(gcc_lib_dir)/libsupc++.a

ifeq ($(with_multiarch_cxxheaders),yes)
  dirs_dev += \
	$(PF)/include/$(DEB_TARGET_MULTIARCH)/c++/$(BASE_VERSION)
  files_dev += \
	$(PF)/include/$(DEB_TARGET_MULTIARCH)/c++/$(BASE_VERSION)/bits
endif

dirs_dbg = \
	$(docdir) \
	$(PF)/lib/debug/$(usr_lib) \
	$(usr_lib)/debug \
	$(gcc_lib_dir)
files_dbg = \
	$(usr_lib)/debug/libstdc++.{a,so*}

ifneq ($(DEB_CROSS),yes)
  dirs_dbg  += $(PF)/share/gcc-$(BASE_VERSION)/python
  files_dbg += $(PF)/share/gcc-$(BASE_VERSION)/python/libstdcxx
endif

dirs_pic = \
	$(docdir) \
	$(gcc_lib_dir)
files_pic = \
	$(gcc_lib_dir)/libstdc++_pic.a

# ----------------------------------------------------------------------

gxx_baseline_dir = $(shell \
			sed -n '/^baseline_dir *=/s,.*= *\(.*\)$$,\1,p' \
			    $(buildlibdir)/libstdc++-v3/testsuite/Makefile)
gxx_baseline_file = $(gxx_baseline_dir)/baseline_symbols.txt

debian/README.libstdc++-baseline:
	cat debian/README.libstdc++-baseline.in \
		> debian/README.libstdc++-baseline

	baseline_name=`basename $(gxx_baseline_dir)`; \
	baseline_parentdir=`dirname $(gxx_baseline_dir)`; \
	compat_baseline_name=""; \
	if [ -f "$(gxx_baseline_file)" ]; then \
	  ( \
	    echo "A baseline file for $$baseline_name was found."; \
	    echo "Running the check-abi script ..."; \
	    echo ""; \
	    $(MAKE) -C $(buildlibdir)/libstdc++-v3/testsuite \
		check-abi; \
	  ) >> debian/README.libstdc++-baseline; \
	else \
	  ( \
	    echo "No baseline file found for $$baseline_name."; \
	    echo "Generating a new baseline file ..."; \
	    echo ""; \
	  ) >> debian/README.libstdc++-baseline; \
	  mkdir -p $(gxx_baseline_dir); \
	  $(MAKE) -C $(buildlibdir)/libstdc++-v3/testsuite new-abi-baseline; \
	  if [ -f $(gxx_baseline_file) ]; then \
	    cat $(gxx_baseline_file); \
	  else \
	    cat $$(find $(buildlibdir)/libstdc++-v3 $(srcdir)/libstdc++-v3 -name '.new') || true; \
	  fi >> debian/README.libstdc++-baseline; \
	fi

# ----------------------------------------------------------------------
define __do_libstdcxx
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_l)

	dh_installdirs -p$(p_l) \
		$(docdir) \
		$(usr_lib$(2))

	cp -a $(d)/$(usr_lib$(2))/libstdc++.so.*[0-9] \
		$(d_l)/$(usr_lib$(2))/.

	debian/dh_doclink -p$(p_l) $(p_base)
	debian/dh_rmemptydirs -p$(p_l)

	dh_strip -p$(p_l) --dbg-package=$(1)-$(BASE_VERSION)-dbg$(cross_lib_arch)
	dh_compress -p$(p_l)
	dh_fixperms -p$(p_l)

	$(cross_makeshlibs) dh_makeshlibs -p$(p_l) || echo FIXME: libstdc++ symbols
	$(call cross_mangle_shlibs,$(p_l))
	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_l) \
		$(call shlibdirs_to_search,$(subst stdc++$(CXX_SONAME),gcc$(GCC_SONAME),$(p_l)),$(2))
	$(call cross_mangle_substvars,$(p_l))

	$(cross_gencontrol) dh_gencontrol -p$(p_l) -- -v$(DEB_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(p_l))
	dh_installdeb -p$(p_l)
	dh_md5sums -p$(p_l)
	dh_builddeb -p$(p_l)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

define __do_libstdcxx_dbg
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_d)
	dh_installdirs -p$(p_d) \
		$(PF)/lib/debug/$(usr_lib$(2)) \
		$(usr_lib$(2))

	cp -p $(d)/$(usr_lib$(2))/libstdc++.so.*.py \
		$(d_d)/$(PF)/lib/debug/$(usr_lib$(2))/.;
	$(if $(filter yes,$(with_lib$(2)cxx)),
		cp -a $(d)/$(usr_lib$(2))/libstdc++.so.*[0-9] \
			$(d_d)/$(usr_lib$(2))/.;
		dh_strip -p$(p_d) --keep-debug;
		$(if $(filter yes,$(with_common_libs)),, # if !with_common_libs
			# remove the debug symbols for libstdc++
			# built by a newer version of GCC
			rm -rf $(d_d)/usr/lib/debug/$(PF);
		)
		rm -f $(d_d)/$(usr_lib$(2))/libstdc++.so.*[0-9]
	)

	$(if $(filter yes,$(with_debug)),
		mv $(d)/$(usr_lib$(2))/debug $(d_d)/$(usr_lib$(2))/.;
		rm -f $(d_d)/$(usr_lib$(2))/debug/libstdc++_pic.a
	)

	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_d) \
		$(call shlibdirs_to_search,$(subst $(pkg_ver),,$(subst stdc++$(CXX_SONAME),gcc$(GCC_SONAME),$(p_l))),$(2))
	$(call cross_mangle_substvars,$(p_d))

	debian/dh_doclink -p$(p_d) $(p_base)
	debian/dh_rmemptydirs -p$(p_d)

	dh_compress -p$(p_d)
	dh_fixperms -p$(p_d)
	$(cross_gencontrol) dh_gencontrol -p$(p_d) -- -v$(DEB_VERSION) $(common_substvars)
	$(call cross_mangle_control,$(p_d))

	dh_installdeb -p$(p_d)
	dh_md5sums -p$(p_d)
	dh_builddeb -p$(p_d)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

define __do_libstdcxx_dev
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	mv $(d)/$(usr_lib$(2))/libstdc++.a $(d)/$(usr_lib$(2))/libsupc++.a \
		$(d)/$(gcc_lib_dir$(2))/

	rm -rf $(d_l)
	dh_installdirs -p$(p_l) $(gcc_lib_dir$(2))

	DH_COMPAT=2 dh_movefiles -p$(p_l) \
		$(gcc_lib_dir$(2))/libstdc++.a \
		$(gcc_lib_dir$(2))/libsupc++.a \
		$(if $(with_multiarch_cxxheaders),$(PF)/include/$(DEB_TARGET_MULTIARCH)/c++/$(BASE_VERSION)/$(2))
	$(call install_gcc_lib,libstdc++,$(CXX_SONAME),$(2),$(p_l))

	debian/dh_doclink -p$(p_l) $(p_base)
	debian/dh_rmemptydirs -p$(p_l)

	dh_strip -p$(p_l)
	dh_compress -p$(p_l)
	dh_fixperms -p$(p_l)
	dh_shlibdeps -p$(p_l)
	dh_gencontrol -p$(p_l) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_l)
	dh_md5sums -p$(p_l)
	dh_builddeb -p$(p_l)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
endef

do_libstdcxx = $(call __do_libstdcxx,lib$(1)stdc++$(CXX_SONAME),$(1))
do_libstdcxx_dbg = $(call __do_libstdcxx_dbg,lib$(1)stdc++$(CXX_SONAME)$(libstdc_ext),$(1))
do_libstdcxx_dev = $(call __do_libstdcxx_dev,lib$(1)stdc++$(CXX_SONAME)-$(BASE_VERSION)-dev,$(1))

# ----------------------------------------------------------------------
$(binary_stamp)-libstdcxx: $(install_stamp)
	$(call do_libstdcxx,)

$(binary_stamp)-lib64stdcxx: $(install_stamp)
	$(call do_libstdcxx,64)

$(binary_stamp)-lib32stdcxx: $(install_stamp)
	$(call do_libstdcxx,32)

$(binary_stamp)-libn32stdcxx: $(install_stamp)
	$(call do_libstdcxx,n32)

$(binary_stamp)-libx32stdcxx: $(install_stamp)
	$(call do_libstdcxx,x32)

$(binary_stamp)-libhfstdcxx: $(install_stamp)
	$(call do_libstdcxx,hf)

$(binary_stamp)-libsfstdcxx: $(install_stamp)
	$(call do_libstdcxx,sf)

$(binary_stamp)-lib64stdcxxdbg: $(install_stamp)
	$(call do_libstdcxx_dbg,64)

$(binary_stamp)-lib32stdcxxdbg: $(install_stamp)
	$(call do_libstdcxx_dbg,32)

$(binary_stamp)-libn32stdcxxdbg: $(install_stamp)
	$(call do_libstdcxx_dbg,n32)

$(binary_stamp)-libx32stdcxxdbg: $(install_stamp)
	$(call do_libstdcxx_dbg,x32)

$(binary_stamp)-libhfstdcxxdbg: $(install_stamp)
	$(call do_libstdcxx_dbg,hf)

$(binary_stamp)-libsfstdcxxdbg: $(install_stamp)
	$(call do_libstdcxx_dbg,sf)

$(binary_stamp)-lib64stdcxx-dev: $(install_stamp)
	$(call do_libstdcxx_dev,64)

$(binary_stamp)-lib32stdcxx-dev: $(install_stamp)
	$(call do_libstdcxx_dev,32)

$(binary_stamp)-libn32stdcxx-dev: $(install_stamp)
	$(call do_libstdcxx_dev,n32)

$(binary_stamp)-libx32stdcxx-dev: $(install_stamp)
	$(call do_libstdcxx_dev,x32)

$(binary_stamp)-libhfstdcxx-dev: $(install_stamp)
	$(call do_libstdcxx_dev,hf)

$(binary_stamp)-libsfstdcxx-dev: $(install_stamp)
	$(call do_libstdcxx_dev,sf)

# ----------------------------------------------------------------------
libcxxdev_deps = $(install_stamp)
ifeq ($(with_libcxx),yes)
  libcxxdev_deps += $(binary_stamp)-libstdcxx
endif
ifeq ($(with_check),yes)
  libcxxdev_deps += debian/README.libstdc++-baseline
endif
$(binary_stamp)-libstdcxx-dev: $(libcxxdev_deps)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_dev) $(d_pic)
	dh_installdirs -p$(p_dev) $(dirs_dev)
	dh_installdirs -p$(p_pic) $(dirs_pic)
	dh_installdirs -p$(p_dbg) $(dirs_dbg)

	: # - correct libstdc++-v3 file locations
	mv $(d)/$(usr_lib)/libsupc++.a $(d)/$(gcc_lib_dir)/
	mv $(d)/$(usr_lib)/libstdc++.{a,so} $(d)/$(gcc_lib_dir)/
	ln -sf ../../../$(DEB_TARGET_GNU_TYPE)/libstdc++.so.$(CXX_SONAME) \
		$(d)/$(gcc_lib_dir)/libstdc++.so
	mv $(d)/$(usr_lib)/libstdc++_pic.a $(d)/$(gcc_lib_dir)/

	rm -f $(d)/$(usr_lib)/debug/libstdc++_pic.a
	rm -f $(d)/$(usr_lib64)/debug/libstdc++_pic.a

	: # remove precompiled headers
	-find $(d) -type d -name '*.gch' | xargs rm -rf

	for i in $(d)/$(PF)/include/c++/$(GCC_VERSION)/*-linux; do \
	  if [ -d $$i ]; then mv $$i $$i-gnu; fi; \
	done

	DH_COMPAT=2 dh_movefiles -p$(p_dev) $(files_dev)
	DH_COMPAT=2 dh_movefiles -p$(p_pic) $(files_pic)
ifeq ($(with_debug),yes)
	DH_COMPAT=2 dh_movefiles -p$(p_dbg) $(files_dbg)
endif

	dh_link -p$(p_dev) \
		/$(usr_lib)/libstdc++.so.$(CXX_SONAME) \
		/$(gcc_lib_dir)/libstdc++.so \
		/$(cxx_inc_dir) /$(PFL)/include/c++/$(GCC_VERSION)

	debian/dh_doclink -p$(p_dev) $(p_base)
	debian/dh_doclink -p$(p_pic) $(p_base)
	debian/dh_doclink -p$(p_dbg) $(p_base)
	cp -p $(srcdir)/libstdc++-v3/ChangeLog \
		$(d_dev)/$(docdir)/$(p_base)/C++/changelog.libstdc++
ifeq ($(with_check),yes)
	cp -p debian/README.libstdc++-baseline \
		$(d_dev)/$(docdir)/$(p_base)/C++/README.libstdc++-baseline.$(DEB_TARGET_ARCH)
	if [ -f $(buildlibdir)/libstdc++-v3/testsuite/current_symbols.txt ]; \
	then \
	  cp -p $(buildlibdir)/libstdc++-v3/testsuite/current_symbols.txt \
	    $(d_dev)/$(docdir)/$(p_base)/C++/libstdc++_symbols.txt.$(DEB_TARGET_ARCH); \
	fi
endif
	cp -p $(buildlibdir)/libstdc++-v3/src/libstdc++-symbols.ver \
		$(d_pic)/$(gcc_lib_dir)/libstdc++_pic.map

	cp -p $(d)/$(usr_lib)/libstdc++.so.*.py \
		$(d_dbg)/$(PF)/lib/debug/$(usr_lib)/
ifeq ($(with_libcxx),yes)
	cp -a $(d)/$(usr_lib)/libstdc++.so.*[0-9] \
		$(d_dbg)/$(usr_lib)/
	dh_strip -p$(p_dbg) --keep-debug
	rm -f $(d_dbg)/$(usr_lib)/libstdc++.so.*[0-9]
endif

	dh_strip -p$(p_dev) --dbg-package=$(p_dbg)
ifneq ($(with_common_libs),yes)
	: # remove the debug symbols for libstdc++ built by a newer version of GCC
	rm -rf $(d_dbg)/usr/lib/debug/$(PF)
endif
	dh_strip -p$(p_pic)

ifeq ($(with_cxxdev),yes)
	debian/dh_rmemptydirs -p$(p_dev)
	debian/dh_rmemptydirs -p$(p_pic)
	debian/dh_rmemptydirs -p$(p_dbg)
endif

	dh_compress -p$(p_dev) -p$(p_pic) -p$(p_dbg) -X.txt
	dh_fixperms -p$(p_dev) -p$(p_pic) -p$(p_dbg)
# XXX: what about biarchn32?
#ifeq ($(biarch64),yes)
#	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_dev) -p$(p_pic) -p$(p_dbg) -Xlib64
#else
#	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_dev) -p$(p_pic) -p$(p_dbg) -Xlib32/debug
#endif
	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps) dh_shlibdeps -p$(p_dev) -p$(p_pic) -p$(p_dbg)
	$(call cross_mangle_substvars,$(p_dbg))
	$(cross_gencontrol) dh_gencontrol -p$(p_dev) -p$(p_pic) -p$(p_dbg) \
		-- -v$(DEB_VERSION) $(common_substvars)

	dh_installdeb -p$(p_dev) -p$(p_pic) -p$(p_dbg)
	dh_md5sums -p$(p_dev) -p$(p_pic) -p$(p_dbg)
	dh_builddeb -p$(p_dev) -p$(p_pic) -p$(p_dbg)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------

doxygen_doc_dir = $(buildlibdir)/libstdc++-v3/doc

doxygen-docs: $(build_doxygen_stamp)
$(build_doxygen_stamp):
	$(MAKE) -C $(buildlibdir)/libstdc++-v3/doc SHELL=/bin/bash doc-html-doxygen
	$(MAKE) -C $(buildlibdir)/libstdc++-v3/doc SHELL=/bin/bash doc-man-doxygen
	-find $(doxygen_doc_dir)/doxygen/html -name 'struct*' -empty | xargs rm -f

	touch $@

$(binary_stamp)-libstdcxx-doc: $(install_stamp) doxygen-docs
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_libd)
	dh_installdirs -p$(p_libd) \
		$(docdir)/$(p_base)/libstdc++ \
		$(PF)/share/man

#	debian/dh_doclink -p$(p_libd) $(p_base)
	dh_link -p$(p_libd) /usr/share/doc/$(p_base) /usr/share/doc/$(p_libd)
	dh_installdocs -p$(p_libd)
	rm -f $(d_libd)/$(docdir)/$(p_base)/copyright

	cp -a $(srcdir)/libstdc++-v3/doc/html \
		$(d_libd)/$(docdir)/$(p_base)/libstdc++/.
	cp -a $(doxygen_doc_dir)/doxygen/html \
		$(d_libd)/$(docdir)/$(p_base)/libstdc++/.
	find $(d_libd)/$(docdir)/$(p_base)/libstdc++ -name '*.md5' \
		| xargs -r rm -f

	: FIXME: depending on the doxygen version
	if [ -d $(doxygen_doc_dir)/doxygen/man/man3cxx ]; then \
	  cp -a $(doxygen_doc_dir)/doxygen/man/man3cxx \
	    $(d_libd)/$(PF)/share/man/man3; \
	  if [ -d $(doxygen_doc_dir)/doxygen/man/man3 ]; then \
	    cp -a $(doxygen_doc_dir)/doxygen/man/man3/* \
	      $(d_libd)/$(PF)/share/man/man3/; \
	  fi; \
	elif [ -d $(doxygen_doc_dir)/doxygen/man/man3 ]; then \
	  cp -a $(doxygen_doc_dir)/doxygen/man/man3 \
	    $(d_libd)/$(PF)/share/man/man3; \
	fi

	for i in $(d_libd)/$(PF)/share/man/man3/*.3; do \
	  [ -f $${i} ] || continue; \
	  mv $${i} $${i}cxx; \
	done
	rm -f $(d_libd)/$(PF)/share/man/man3/todo.3*

	mkdir -p $(d_libd)/usr/share/lintian/overrides
	cp -p debian/$(p_libd).overrides \
		$(d_libd)/usr/share/lintian/overrides/$(p_libd)

	dh_compress -p$(p_libd) -Xhtml/17_intro -X.txt -X.tag -X.map
	dh_fixperms -p$(p_libd)
	dh_gencontrol -p$(p_libd) -- -v$(DEB_VERSION) $(common_substvars)

	dh_installdeb -p$(p_libd)
	dh_md5sums -p$(p_libd)
	dh_builddeb -p$(p_libd)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
