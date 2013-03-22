arch_binaries  := $(arch_binaries) neon

p_nlgcc		= libgcc$(GCC_SONAME)-neon
p_ngomp		= libgomp$(GOMP_SONAME)-neon
p_nlobjc	= libobjc$(OBJC_SONAME)-neon
p_nflib		= libgfortran$(FORTRAN_SONAME)-neon
p_nlcxx		= libstdc++$(CXX_SONAME)-neon

d_nlgcc		= debian/$(p_nlgcc)
d_ngomp		= debian/$(p_ngomp)
d_nlobjc	= debian/$(p_nlobjc)
d_nflib		= debian/$(p_nflib)
d_nlcxx		= debian/$(p_nlcxx)

neon_pkgs = -p$(p_nlgcc) -p$(p_ngomp) -p$(p_nlobjc) -p$(p_nflib) -p$(p_nlcxx)

# ----------------------------------------------------------------------
$(binary_stamp)-neon: $(install_neon_stamp)
	dh_testdir
	dh_testroot

	dh_installdirs -p$(p_nlgcc) \
		$(PF)/share/doc \
		lib/neon
	dh_installdirs -A -p$(p_ngomp) -p$(p_nlobjc) -p$(p_nflib) -p$(p_nlcxx) \
		$(PF)/share/doc \
		$(PF)/lib/neon

	cp -a $(d_neon)/$(PF)/lib/libgcc*.so.* \
		$(d_nlgcc)/lib/neon/
	cp -a $(d_neon)/$(PF)/lib/libgomp*.so.* \
		$(d_ngomp)/$(PF)/lib/neon/
	cp -a $(d_neon)/$(PF)/lib/libobjc*.so.* \
		$(d_nlobjc)/$(PF)/lib/neon/
	cp -a $(d_neon)/$(PF)/lib/libgfortran*.so.* \
		$(d_nflib)/$(PF)/lib/neon/
	cp -a $(d_neon)/$(PF)/lib/libstdc++*.so.* \
		$(d_nlcxx)/$(PF)/lib/neon/

	for p in $(p_nlgcc) $(p_ngomp) $(p_nlobjc) $(p_nflib) $(p_nlcxx); do \
	  ln -s ../$(p_base) debian/$$p/usr/share/doc/$$p; \
	done

	dh_strip $(neon_pkgs)
	dh_compress $(neon_pkgs)
	dh_fixperms $(neon_pkgs)
	dh_shlibdeps $(neon_pkgs)
	dh_gencontrol $(neon_pkgs) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb $(neon_pkgs)
	dh_md5sums $(neon_pkgs)
	dh_builddeb $(neon_pkgs)

	touch $@
