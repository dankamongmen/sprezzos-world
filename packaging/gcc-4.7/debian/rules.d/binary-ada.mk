arch_binaries := $(arch_binaries) gnatbase ada
ifneq ($(GFDL_INVARIANT_FREE),yes)
  indep_binaries := $(indep_binaries) ada-doc
endif

ifeq ($(with_libgnat),yes)
  arch_binaries := $(arch_binaries) libgnat
endif

p_gbase	= gnat-$(GNAT_VERSION)-base
p_gnat	= gnat-$(GNAT_VERSION)
p_lgnat	= libgnat-$(GNAT_VERSION)
p_lgnat_dbg = $(p_lgnat)-dbg
p_lgnatvsn = libgnatvsn$(GNAT_VERSION)
p_lgnatvsn_dev = $(p_lgnatvsn)-dev
p_lgnatvsn_dbg = $(p_lgnatvsn)-dbg
p_lgnatprj = libgnatprj$(GNAT_VERSION)
p_lgnatprj_dev = $(p_lgnatprj)-dev
p_lgnatprj_dbg = $(p_lgnatprj)-dbg
p_gnatd	= $(p_gnat)-doc

d_gbase	= debian/$(p_gbase)
d_gnat	= debian/$(p_gnat)
d_lgnat	= debian/$(p_lgnat)
d_lgnatvsn = debian/$(p_lgnatvsn)
d_lgnatprj = debian/$(p_lgnatprj)
d_gnatd	= debian/$(p_gnatd)

GNAT_TOOLS = gnat gnatbind gnatchop gnatclean gnatfind gnatkr gnatlink \
             gnatls gnatmake gnatname gnatprep gnatxref gnathtml

dirs_gnat = \
	$(docdir)/$(p_base) \
	$(PF)/bin \
	$(PF)/share/man/man1 \
	$(gcc_lib_dir) \
	$(gcc_lexec_dir)

files_gnat = \
	$(gcc_lexec_dir)/gnat1 \
	$(gcc_lib_dir)/{adalib,adainclude} \
	$(foreach i,$(GNAT_TOOLS),$(PF)/bin/$(i))
ifeq ($(with_gnat_zcx)-$(with_gnat_sjlj),yes-yes)
  files_gnat += \
	$(gcc_lib_dir)/rts-native
# rts-sjlj moved to a separate package
endif

dirs_lgnat = \
	$(docdir) \
	$(PF)/lib
files_lgnat = \
	$(PF)/$(libdir)/lib{gnat,gnarl}-$(GNAT_SONAME).so.1

$(binary_stamp)-gnatbase: $(install_stamp)
	dh_testdir
	dh_testroot
	dh_installdocs -p$(p_gbase) debian/README.gnat debian/README.maintainers
ifeq ($(PKGSOURCE),gnat-$(BASE_VERSION))
  ifeq ($(with_check),yes)
	dh_installdocs -p$(p_gbase) test-summary
  endif
endif
ifeq ($(PKGSOURCE),gnat-$(BASE_VERSION))
	mkdir -p $(d_gbase)/$(docdir)/$(p_base)
	ln -sf ../$(p_gbase) $(d_gbase)/$(docdir)/$(p_base)/Ada
endif
	dh_installchangelogs -p$(p_gbase) src/gcc/ada/ChangeLog
	dh_compress -p$(p_gbase)
	dh_fixperms -p$(p_gbase)
	dh_gencontrol -p$(p_gbase) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_gbase)
	dh_md5sums -p$(p_gbase)
	dh_builddeb -p$(p_gbase)
	touch $@


$(binary_stamp)-libgnat: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	: # libgnat
	rm -rf $(d_lgnat)
	dh_installdirs -p$(p_lgnat) $(dirs_lgnat)

	for lib in lib{gnat,gnarl}; do \
	  vlib=$$lib-$(GNAT_SONAME); \
	  mv $(d)/$(gcc_lib_dir)/adalib/$$vlib.so.1 $(d)/$(PF)/$(libdir)/. ; \
	  rm -f $(d)/$(gcc_lib_dir)/adalib/$$lib.so.1; \
	done
	dh_movefiles -p$(p_lgnat) $(files_lgnat)

	debian/dh_doclink -p$(p_lgnat) $(p_gbase)

	debian/dh_rmemptydirs -p$(p_lgnat)

	dh_strip -p$(p_lgnat) --dbg-package=$(p_lgnat_dbg)
	dh_compress -p$(p_lgnat)
	dh_fixperms -p$(p_lgnat)
	b=libgnat; \
	v=$(GNAT_VERSION); \
	for ext in preinst postinst prerm postrm; do \
	  for t in '' -dev -dbg; do \
	    if [ -f debian/$$b$$t.$$ext ]; then \
	      cp -pf debian/$$b$$t.$$ext debian/$$b$$v$$t.$$ext; \
	    fi; \
	  done; \
	done
	dh_makeshlibs -p$(p_lgnat) -V '$(p_lgnat) (>= $(DEB_VERSION))'

	mkdir -p $(d_lgnat)/usr/share/lintian/overrides
	cp -p debian/$(p_lgnat).overrides \
		$(d_lgnat)/usr/share/lintian/overrides/$(p_lgnat)

	dh_shlibdeps -p$(p_lgnat)
	dh_gencontrol -p$(p_lgnat) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_lgnat)
	dh_md5sums -p$(p_lgnat)
	dh_builddeb -p$(p_lgnat)

	: # $(p_lgnat_dbg)
	debian/dh_doclink -p$(p_lgnat_dbg) $(p_gbase)
	dh_compress -p$(p_lgnat_dbg)
	dh_fixperms -p$(p_lgnat_dbg)
	dh_gencontrol -p$(p_lgnat_dbg) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_lgnat_dbg)
	dh_md5sums -p$(p_lgnat_dbg)
	dh_builddeb -p$(p_lgnat_dbg)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)


$(binary_stamp)-libgnatvsn: $(binary_stamp)-libgnat
	: # $(p_lgnatvsn_dev)
	dh_movefiles -p$(p_lgnatvsn_dev) usr/lib/ada/adalib/gnatvsn
	dh_movefiles -p$(p_lgnatvsn_dev) usr/share/ada/adainclude/gnatvsn
	dh_install -p$(p_lgnatvsn_dev) \
	   debian/gnatvsn.gpr usr/share/ada/adainclude
	dh_movefiles -p$(p_lgnatvsn_dev) usr/$(libdir)/libgnatvsn.a
	dh_link -p$(p_lgnatvsn_dev) \
	   usr/$(libdir)/libgnatvsn.so.$(GNAT_VERSION) \
	   usr/$(libdir)/libgnatvsn.so
	dh_strip -p$(p_lgnatvsn_dev) -X.a --keep-debug
	dh_fixperms -p$(p_lgnatvsn_dev)
	debian/dh_doclink -p$(p_lgnatvsn_dev) $(p_gbase)
	dh_gencontrol -p$(p_lgnatvsn_dev) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_lgnatvsn_dev)
	dh_builddeb -p$(p_lgnatvsn_dev)

	: # $(p_lgnatvsn)
	mkdir -p $(d_lgnatvsn)/usr/share/lintian/overrides
	cp -p debian/$(p_lgnatvsn).overrides \
		$(d_lgnatvsn)/usr/share/lintian/overrides/$(p_lgnatvsn)
	dh_movefiles -p$(p_lgnatvsn) usr/$(libdir)/libgnatvsn.so.$(GNAT_VERSION)
	debian/dh_doclink -p$(p_lgnatvsn) $(p_gbase)
	dh_strip -p$(p_lgnatvsn) --dbg-package=$(p_lgnatvsn_dbg)
	dh_makeshlibs -p$(p_lgnatvsn) -V '$(p_lgnatvsn) (>= $(DEB_VERSION))'
	cat debian/$(p_lgnatvsn)/DEBIAN/shlibs >> debian/shlibs.local
	dh_shlibdeps -p$(p_lgnatvsn) -L$(p_lgnat)
	dh_gencontrol -p$(p_lgnatvsn) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_lgnatvsn)
	dh_md5sums -p$(p_lgnatvsn)
	dh_builddeb -p$(p_lgnatvsn)

	: # $(p_lgnatvsn_dbg)
	debian/dh_doclink -p$(p_lgnatvsn_dbg) $(p_gbase)
	dh_compress -p$(p_lgnatvsn_dbg)
	dh_fixperms -p$(p_lgnatvsn_dbg)
	dh_gencontrol -p$(p_lgnatvsn_dbg) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_lgnatvsn_dbg)
	dh_md5sums -p$(p_lgnatvsn_dbg)
	dh_builddeb -p$(p_lgnatvsn_dbg)
	touch $@

$(binary_stamp)-libgnatprj: $(binary_stamp)-libgnat $(binary_stamp)-libgnatvsn
	: # $(p_lgnatprj_dev)
	dh_movefiles -p$(p_lgnatprj_dev) usr/lib/ada/adalib/gnatprj
	dh_movefiles -p$(p_lgnatprj_dev) usr/share/ada/adainclude/gnatprj
	dh_install -p$(p_lgnatprj_dev) \
	   debian/gnatprj.gpr usr/share/ada/adainclude
	dh_movefiles -p$(p_lgnatprj_dev) usr/$(libdir)/libgnatprj.a
	dh_link -p$(p_lgnatprj_dev) \
	   usr/$(libdir)/libgnatprj.so.$(GNAT_VERSION) \
	   usr/$(libdir)/libgnatprj.so
	dh_strip -p$(p_lgnatprj_dev) -X.a --keep-debug
	dh_fixperms -p$(p_lgnatprj_dev)
	debian/dh_doclink -p$(p_lgnatprj_dev) $(p_gbase)
	dh_gencontrol -p$(p_lgnatprj_dev) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_lgnatprj_dev)
	dh_builddeb -p$(p_lgnatprj_dev)

	: # $(p_lgnatprj)
	mkdir -p $(d_lgnatprj)/usr/share/lintian/overrides
	cp -p debian/$(p_lgnatprj).overrides \
		$(d_lgnatprj)/usr/share/lintian/overrides/$(p_lgnatprj)
	dh_movefiles -p$(p_lgnatprj) usr/$(libdir)/libgnatprj.so.$(GNAT_VERSION)
	debian/dh_doclink -p$(p_lgnatprj) $(p_gbase)
	dh_strip -p$(p_lgnatprj) --dbg-package=$(p_lgnatprj_dbg)
	dh_makeshlibs -p$(p_lgnatprj) -V '$(p_lgnatprj) (>= $(DEB_VERSION))'
	cat debian/$(p_lgnatprj)/DEBIAN/shlibs >> debian/shlibs.local
	dh_shlibdeps -p$(p_lgnatprj) -L$(p_lgnat) -L$(p_lgnatvsn)
	dh_gencontrol -p$(p_lgnatprj) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_lgnatprj)
	dh_md5sums -p$(p_lgnatprj)
	dh_builddeb -p$(p_lgnatprj)

	: # $(p_lgnatprj_dbg)
	debian/dh_doclink -p$(p_lgnatprj_dbg) $(p_gbase)
	dh_compress -p$(p_lgnatprj_dbg)
	dh_fixperms -p$(p_lgnatprj_dbg)
	dh_gencontrol -p$(p_lgnatprj_dbg) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_lgnatprj_dbg)
	dh_md5sums -p$(p_lgnatprj_dbg)
	dh_builddeb -p$(p_lgnatprj_dbg)
	touch $@

ifeq ($(with_libgnat),yes)
$(binary_stamp)-ada: $(install_stamp) $(binary_stamp)-libgnat
$(binary_stamp)-ada: $(binary_stamp)-libgnatvsn
$(binary_stamp)-ada: $(binary_stamp)-libgnatprj
else
$(binary_stamp)-ada: $(install_stamp)
endif
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp
	: # gnat
	rm -rf $(d_gnat)
	dh_installdirs -p$(p_gnat) $(dirs_gnat)
	# Upstream does not install gnathtml.
	cp src/gcc/ada/gnathtml.pl debian/tmp/$(PF)/bin/gnathtml
	chmod 755 debian/tmp/$(PF)/bin/gnathtml
	dh_movefiles -p$(p_gnat) $(files_gnat)
ifeq ($(with_gnat_zcx)-$(with_gnat_sjlj),yes-yes)
	dh_installdirs -p$(p_gnat)-sjlj $(gcc_lib_dir)
	dh_movefiles -p$(p_gnat)-sjlj $(gcc_lib_dir)/rts-sjlj
	dh_link -p$(p_gnat)-sjlj \
	   $(gcc_lib_dir)/rts-sjlj usr/share/ada/adainclude/rts-sjlj
	dh_link -p$(p_gnat)-sjlj \
	   $(gcc_lib_dir)/rts-sjlj/adalib/libgnat.a \
	   $(gcc_lib_dir)/rts-sjlj/adalib/libgnat-$(GNAT_VERSION).a
	dh_link -p$(p_gnat)-sjlj \
	   $(gcc_lib_dir)/rts-sjlj/adalib/libgnarl.a \
	   $(gcc_lib_dir)/rts-sjlj/adalib/libgnarl-$(GNAT_VERSION).a
else
	dh_link -p$(p_gnat) \
	   $(gcc_lib_dir)/adalib/libgnarl.a \
	   $(gcc_lib_dir)/adalib/libgnarl-$(GNAT_VERSION).a
endif

ifeq (0,1)
ifeq ($(PKGSOURCE),gnat-$(BASE_VERSION))
	mkdir -p $(d_gnat)/$(libexecdir)/gcc/$(TARGET_ALIAS)/$(BASE_VERSION)
	ln -sf ../$(GCC_VERSION)/gnat1 \
	  $(d_gnat)/$(libexecdir)/gcc/$(TARGET_ALIAS)/$(BASE_VERSION)/gnat1
	mkdir -p $(d_gnat)/$(PF)/$(libdir)/gcc/$(TARGET_ALIAS)/$(BASE_VERSION)
	ln -sf ../$(GCC_VERSION)/adalib \
	  $(d_gnat)/$(PF)/$(libdir)/gcc/$(TARGET_ALIAS)/$(BASE_VERSION)/adalib
	ln -sf ../$(GCC_VERSION)/adainclude \
	  $(d_gnat)/$(PF)/$(libdir)/gcc/$(TARGET_ALIAS)/$(BASE_VERSION)/adainclude
endif
endif

ifeq ($(with_libgnat),yes)
	for lib in lib{gnat,gnarl}; do \
	  vlib=$$lib-$(GNAT_SONAME); \
	  dh_link -p$(p_gnat) \
	    /$(PF)/$(libdir)/$$vlib.so.1 /$(PF)/$(libdir)/$$vlib.so \
	    /$(PF)/$(libdir)/$$vlib.so.1 /$(PF)/$(libdir)/$$lib.so; \
	done
  ifeq ($(with_gnat_zcx)-$(with_gnat_sjlj),yes-yes)
	for lib in lib{gnat,gnarl}; do \
	  vlib=$$lib-$(GNAT_SONAME); \
	  dh_link -p$(p_gnat) \
	    /$(PF)/$(libdir)/$$vlib.so.1 $(gcc_lib_dir)/rts-native/adalib/$$lib.so; \
	done
  else
	for lib in lib{gnat,gnarl}; do \
	  vlib=$$lib-$(GNAT_SONAME); \
	  dh_link -p$(p_gnat) \
	    /$(PF)/$(libdir)/$$vlib.so.1 $(gcc_lib_dir)/adalib/$$lib.so; \
	done
  endif
endif
	debian/dh_doclink -p$(p_gnat)      $(p_gbase)
	debian/dh_doclink -p$(p_gnat)-sjlj $(p_gbase)
	for i in $(GNAT_TOOLS); do \
	  case "$$i" in \
	    gnat) cp -p debian/gnat.1 $(d_gnat)/$(PF)/share/man/man1/ ;; \
	    *) ln -sf gnat.1 $(d_gnat)/$(PF)/share/man/man1/$$i.1; \
	  esac; \
	done

	debian/dh_rmemptydirs -p$(p_gnat)

	dh_strip -p$(p_gnat)
	dh_compress -p$(p_gnat)
	dh_fixperms -p$(p_gnat)
	find $(d_gnat) -name '*.ali' | xargs chmod 444
	dh_shlibdeps -p$(p_gnat)
	dh_gencontrol -p$(p_gnat) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_gnat)
	dh_md5sums -p$(p_gnat)
	dh_builddeb -p$(p_gnat)

ifeq ($(with_libgnat)-$(with_gnat_zcx)-$(with_gnat_sjlj),yes-yes-yes)
	dh_strip -p$(p_gnat)-sjlj
	dh_compress -p$(p_gnat)-sjlj
	dh_fixperms -p$(p_gnat)-sjlj
	find $(d_gnat)-sjlj -name '*.ali' | xargs chmod 444
	dh_shlibdeps -p$(p_gnat)-sjlj
	dh_gencontrol -p$(p_gnat)-sjlj -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_gnat)-sjlj
	dh_md5sums -p$(p_gnat)-sjlj
	dh_builddeb -p$(p_gnat)-sjlj
endif

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)


ada_info_dir = $(d_gnatd)/$(PF)/share/info

$(binary_stamp)-ada-doc: $(build_html_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_gnatd)
	dh_installdirs -p$(p_gnatd) \
		$(PF)/share/info

	cd $(ada_info_dir) && \
	    makeinfo -I $(srcdir)/gcc/doc/include -I $(srcdir)/gcc/ada \
		-I $(builddir)/gcc \
		-o gnat_ugn-$(GNAT_VERSION).info \
		$(builddir)/gcc/doc/gnat_ugn.texi
	cd $(ada_info_dir) && \
	    makeinfo -I $(srcdir)/gcc/doc/include -I $(srcdir)/gcc/ada \
		-I $(builddir)/gcc \
		-o gnat_rm-$(GNAT_VERSION).info \
		$(srcdir)/gcc/ada/gnat_rm.texi
	cd $(ada_info_dir) && \
	    makeinfo -I $(srcdir)/gcc/doc/include -I $(srcdir)/gcc/ada \
		-I $(builddir)/gcc \
		-o gnat-style-$(GNAT_VERSION).info \
		$(srcdir)/gcc/ada/gnat-style.texi

	dh_installdocs -p$(p_gnatd) \
	    html/gnat_ugn.html html/gnat_rm.html html/gnat-style.html
	dh_installchangelogs -p$(p_gnatd)
	dh_compress -p$(p_gnatd)
	dh_fixperms -p$(p_gnatd)
	dh_installdeb -p$(p_gnatd)
	dh_gencontrol -p$(p_gnatd) -- -v$(DEB_VERSION) $(common_substvars)
	dh_md5sums -p$(p_gnatd)
	dh_builddeb -p$(p_gnatd)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
