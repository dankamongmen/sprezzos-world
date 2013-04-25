ifeq ($(with_gcj_base_only),yes)
  arch_binaries  := $(arch_binaries) jbase
else
ifeq ($(with_separate_libgcj),yes)
  ifeq ($(PKGSOURCE),gcj-$(BASE_VERSION))
    arch_binaries  := $(arch_binaries) jbase
  endif
else
  arch_binaries  := $(arch_binaries) jbase
endif

ifeq ($(with_libgcj),yes)
  ifeq ($(with_java),yes)
    arch_binaries  := $(arch_binaries) java gcjjre
    indep_binaries  := $(indep_binaries) libgcjjar
  endif

  ifeq ($(with_javadev),yes)
    arch_binaries  := $(arch_binaries) libgcjdev libgcjdbg
    ifneq ($(DEB_CROSS),yes)
      indep_binaries := $(indep_binaries) libgcjsrc
      ifeq ($(with_libgcj_doc),yes)
        indep_binaries := $(indep_binaries) libgcjdoc
      endif
    endif
  endif
endif

ifeq ($(with_gcj),yes)
    arch_binaries  := $(arch_binaries) gcj
endif
endif

p_jbase	= gcj$(pkg_ver)-base
p_jdk	= gcj$(pkg_ver)-jdk$(cross_bin_arch)
p_jrehl	= gcj$(pkg_ver)-jre-headless$(cross_bin_arch)
p_jre	= gcj$(pkg_ver)-jre$(cross_bin_arch)
p_jar	= gcj$(pkg_ver)-jre-lib$(cross_bin_arch)
p_jsrc	= gcj$(pkg_ver)-source
p_jlib	= libgcj$(PKG_LIBGCJ_EXT)$(cross_lib_arch)
p_jdbg	= libgcj$(PKG_GCJ_EXT)-dbg$(cross_lib_arch)
p_jlibx	= libgcj$(PKG_LIBGCJ_EXT)-awt$(cross_lib_arch)
p_jgtk	= libgcj$(PKG_GCJ_EXT)-awt-gtk$(cross_lib_arch)
p_jqt	= libgcj$(PKG_GCJ_EXT)-awt-qt$(cross_lib_arch)
p_jdev	= libgcj$(PKG_GCJ_EXT)-dev$(cross_lib_arch)
p_jdoc	= libgcj-doc

d_jbase	= debian/$(p_jbase)
d_jdk	= debian/$(p_jdk)
d_jrehl	= debian/$(p_jrehl)
d_jar	= debian/$(p_jar)
d_jsrc	= debian/$(p_jsrc)
d_jlib	= debian/$(p_jlib)
d_jdbg	= debian/$(p_jdbg)
d_jlibx	= debian/$(p_jlibx)
d_jgtk	= debian/$(p_jgtk)
d_jqt	= debian/$(p_jqt)
d_jdev	= debian/$(p_jdev)
d_jdoc	= debian/$(p_jdoc)
d_jre	= debian/$(p_jre)

GCJ_BASE_VERSION = $(BASE_VERSION)

gcj_vlibdir	= $(PF)/$(libdir)/gcj-$(BASE_VERSION)-$(GCJ_SONAME)

jre_tools = java keytool orbd rmid rmiregistry tnameserv
jdk_tools = appletviewer jar jarsigner javac javadoc javah native2ascii rmic serialver

dirs_jdk = \
	$(docdir)/$(p_jbase) \
	$(PF)/bin \
	$(PF)/share/man/man1 \
	$(PF)/share/info \
	$(gcc_lexec_dir) \
	$(jvm_dir)/bin

files_jdk = \
	$(PF)/bin/{gappletviewer,gjdoc,gcj,gc-analyze,gjar,gjarsigner,gcjh,gjavah,gnative2ascii,grmic,gserialver,jv-convert,jcf-dump}$(pkg_ver) \
	$(PF)/share/man/man1/{gappletviewer,gjdoc,gjar,gjarsigner,gcjh,gjavah,gnative2ascii,gserialver}$(pkg_ver).1 \
	$(gcc_lexec_dir)/{ecj1,jc1,jvgenmain} \
	$(gcc_lib_dir)/include/{jni.h,jni_md.h,jvmpi.h} \
	$(gcc_lib_dir)/include/{jawt.h,jawt_md.h} \
	$(gcc_lib_dir)/include/gcj/libgcj-config.h \
	$(PF)/$(libdir)/lib{gij,gcj,gcj-tools}.so \
	$(PF)/$(libdir)/libgcj.spec \
	$(jvm_dir)/include \
	$(jvm_dir)/bin/{appletviewer,jar,jarsigner,javadoc,javah,native2ascii,rmic,serialver} \
	$(PF)/lib/jvm-exports

ifneq ($(GFDL_INVARIANT_FREE),yes)
  files_jdk += \
	$(PF)/share/info/gcj* \
	$(PF)/share/man/man1/{gcj,gc-analyze,grmic,jv-convert,jcf-dump}$(pkg_ver).1
endif

dirs_jrehl = \
	$(docdir)/$(p_jbase) \
	$(PF)/bin \
	$(PF)/share/man/man1 \
	$(jvm_dir)/bin \
	$(jvm_dir)/jre/lib \
	$(jvm_dir)/lib \
	var/lib/gcj$(pkg_ver)

files_jrehl = \
	$(PF)/bin/{gij,gcj-dbtool,gorbd,grmid,grmiregistry,gkeytool,gtnameserv}$(pkg_ver) \
	$(PF)/share/man/man1/{gorbd,grmid,grmiregistry,gkeytool,gtnameserv}$(pkg_ver).1 \
	$(jvm_dir)/jre \
	$(jvm_dir)/bin/{java,keytool,orbd,rmid,rmiregistry,tnameserv} \
	$(jvm_dir)/jre/lib/rt.jar \
	$(jvm_dir)/lib/tools.jar

ifneq ($(GFDL_INVARIANT_FREE),yes)
  files_jrehl += \
	$(PF)/share/man/man1/{gij,gcj-dbtool}$(pkg_ver).1
endif

dirs_jlib = \
	$(docdir)/$(p_jbase) \
	$(gcj_vlibdir) \
	$(PF)/$(libdir) \
	$(jvm_dir)/jre/lib

files_jlib = \
	$(PF)/$(libdir)/libgij.so.* \
	$(PF)/$(libdir)/libgcj-tools.so.* \
	$(PF)/$(libdir)/libgcj.so.* \
	$(gcj_vlibdir)/libjvm.so \
	$(gcj_vlibdir)/libjavamath.so \
	$(jvm_dir)/jre/lib/security

#	$(gcj_vlibdir)/libgconfpeer.so

ifeq ($(with_java_alsa),yes)
  files_jlib += \
	$(gcj_vlibdir)/libgjsmalsa.so
endif

dirs_jar = \
	$(PF)/share/java

files_jar = \
	$(PF)/share/java/libgcj-$(BASE_VERSION).jar \
	$(PF)/share/java/libgcj-tools-$(BASE_VERSION).jar

dirs_jlibx = \
	$(PF)/$(libdir) \
	$(gcj_vlibdir) \
	$(PF)/share/java

files_jlibx = \
	$(gcj_vlibdir)/libjawt.so \
	$(gcj_vlibdir)/libgtkpeer.so

#files_jgtk = \
#	$(gcj_vlibdir)/libgtkpeer.so
#files_jqt = \
#	$(gcj_vlibdir)/libqtpeer.so

dirs_jdev = \
	$(PF)/{include,lib} \
	$(jvm_dir)/include

files_jdev = \
	$(cxx_inc_dir)/{org,gcj,java,javax} \
	$(cxx_inc_dir)/gnu/{awt,classpath,gcj,java,javax} \
	$(PF)/$(libdir)/pkgconfig/libgcj-$(BASE_VERSION).pc \
	$(gcj_vlibdir)/lib*peer.so

ifeq ($(with_static_java),yes)
  files_jdev += \
	$(PF)/$(libdir)/libgij.a \
	$(PF)/$(libdir)/libgcj.a \
	$(PF)/$(libdir)/libgcj-tools.a
endif

ifeq (,$(p_l64gcc))
  p_l64gcc = lib64gcc$(GCC_SONAME)
  d_l64gcc = debian/$(p_l64gcc)
endif

ifeq ($(with_standalone_gcj),yes)

  dirs_gcj += \
	$(gcc_lib_dir)/include \
	$(PF)/share/man/man1

# XXX: what about triarch mapping?
  files_gcj += \
	$(PF)/bin/{cpp,gcc,gcov}$(pkg_ver) \
	$(gcc_lexec_dir)/{collect2,lto1,lto-wrapper} \
	$(gcc_lexec_dir)/liblto_plugin.so{,.0,.0.0.0} \
	$(gcc_lib_dir)/{libgcc*,libgcov.a,*.o} \
	$(header_files) \
	$(shell test -e $(d)/$(gcc_lib_dir)/SYSCALLS.c.X \
		&& echo $(gcc_lib_dir)/SYSCALLS.c.X)

  ifneq ($(GFDL_INVARIANT_FREE),yes)
    files_gcj += \
	$(PF)/share/man/man1/{cpp,gcc,gcov}$(pkg_ver).1
  endif

  ifeq ($(biarch64),yes)
    files_gcj += $(gcc_lib_dir)/$(biarch64subdir)/{libgcc*,libgcov.a,*.o}
  endif
  ifeq ($(biarch32),yes)
    files_gcj += $(gcc_lib_dir)/$(biarch32subdir)/{libgcc*,*.o}
  endif
  ifeq ($(biarchn32),yes)
    files_gcj += $(gcc_lib_dir)/$(biarchn32subdir)/{libgcc*,libgcov.a,*.o}
  endif
  ifeq ($(biarchx32),yes)
    files_gcj += $(gcc_lib_dir)/$(biarchx32subdir)/{libgcc*,libgcov.a,*.o}
  endif
endif

# ----------------------------------------------------------------------
$(binary_stamp)-jbase: $(install_dependencies)
	dh_testdir
	dh_testroot
	rm -rf $(d_jbase)
	dh_installdirs -p$(p_jbase)
	dh_installdocs -p$(p_jbase)
	dh_installchangelogs -p$(p_jbase)
	dh_compress -p$(p_jbase)
	dh_fixperms -p$(p_jbase)
	dh_gencontrol -p$(p_jbase) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_jbase)
	dh_md5sums -p$(p_jbase)
	dh_builddeb -p$(p_jbase)
	touch $@

# ----------------------------------------------------------------------
$(binary_stamp)-libgcjjar: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	dh_installdirs -p$(p_jar) $(dirs_jar)
	DH_COMPAT=2 dh_movefiles -p$(p_jar) $(files_jar)

	ln -sf libgcj-$(BASE_VERSION).jar \
		$(d_jar)/$(PF)/share/java/libgcj-$(GCC_VERSION).jar
	ln -sf libgcj-tools-$(BASE_VERSION).jar \
		$(d_jar)/$(PF)/share/java/libgcj-tools-$(GCC_VERSION).jar
	debian/dh_doclink -p$(p_jar) $(p_jbase)
	debian/dh_rmemptydirs -p$(p_jar)
	dh_compress -p$(p_jar)
	dh_fixperms -p$(p_jar)
	dh_gencontrol -p$(p_jar) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_jar)
	dh_md5sums -p$(p_jar)
	dh_builddeb -p$(p_jar)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(build_javasrc_stamp): $(build_stamp)
	PATH=$(PWD)/bin:$$PATH \
	  $(MAKE) -C $(buildlibdir)/libjava src.zip
	touch $@

$(binary_stamp)-libgcjsrc: $(install_stamp) $(build_javasrc_stamp)
	dh_testdir
	dh_testroot

	dh_installdirs -p$(p_jsrc) $(PF)/share/java $(jvm_dir)
	cp -p $(buildlibdir)/libjava/src.zip \
	   $(d_jsrc)/$(PF)/share/java/libgcj-src-$(BASE_VERSION).zip
	dh_link -p$(p_jsrc) \
		$(PF)/share/java/libgcj-src-$(BASE_VERSION).zip \
		$(jvm_dir)/src.zip
	debian/dh_doclink -p$(p_jsrc) $(p_jbase)
	debian/dh_rmemptydirs -p$(p_jsrc)
	dh_compress -p$(p_jsrc)
	dh_fixperms -p$(p_jsrc)
	dh_gencontrol -p$(p_jsrc) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_jsrc)
	dh_md5sums -p$(p_jsrc)
	dh_builddeb -p$(p_jsrc)

	touch $@

# ----------------------------------------------------------------------
libgcj_version = $$($(builddir)/gcc/xgcc -B$(builddir)/gcc/ --version \
			 | sed -n '/^xgcc/s/[^)]*) *\(.*\)/\1/p' | sed 's/ \[[^[]*$$//')
libgcj_title = LibGCJ Classpath
libgcjhbox_href = http://gcc.gnu.org/java
libgcjhbox = <span class='logo'><a href='$(libgcjhbox_href)' target='_top'>$(title)</a> ($(libgcj_version))

$(build_javadoc_stamp): $(build_stamp) $(build_javasrc_stamp)
	mkdir -p $(builddir)/java-src
	cd $(builddir)/java-src && fastjar -xf $(buildlibdir)/libjava/src.zip

	mkdir -p $(builddir)/html
	$(buildlibdir)/libjava/gjdoc \
	    -licensetext \
	    -use \
	    -sourcepath "$(builddir)/java-src" \
	    -encoding UTF-8 \
	    -breakiterator \
	    -linksource \
	    -splitindex \
	    -d $(builddir)/html \
	    -doctitle "$(libgcj_title) $(libgcj_version)" \
	    -windowtitle "$(libgcj_title) $(libgcj_version) Documentation" \
	    -header "$(classpathbox)" \
	    -footer "$(classpathbox)" \
	    -subpackages gnu:java:javax:org

	touch $@

$(binary_stamp)-libgcjdoc: $(install_stamp) $(build_javadoc_stamp)
	dh_testdir
	dh_testroot

	dh_installdocs -p$(p_jdoc)
	dh_installchangelogs -p$(p_jdoc)
	mkdir -p $(d_jdoc)/usr/share/doc/$(p_jbase)
	cp -al $(builddir)/html $(d_jdoc)/usr/share/doc/$(p_jbase)/api
	ln -sf api $(d_jdoc)/usr/share/doc/$(p_jbase)/html
	ln -sf ../$(p_jbase)/api $(d_jdoc)/usr/share/doc/$(p_jdoc)/api
	ln -sf ../$(p_jbase)/html $(d_jdoc)/usr/share/doc/$(p_jdoc)/html
	dh_compress -p$(p_jdoc) -X.java -X.c
	dh_fixperms -p$(p_jdoc)
	dh_gencontrol -p$(p_jdoc) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_jdoc)
	dh_md5sums -p$(p_jdoc)
	dh_builddeb -p$(p_jdoc)

	touch $@

# ----------------------------------------------------------------------

#peer_pkgs =
#ifneq (,$(findstring gtk, $(java_awt_peers)))
#  peer_pkgs += -p$(p_jgtk)
#endif
#ifneq (,$(findstring qt, $(java_awt_peers)))
#  peer_pkgs += -p$(p_jqt)
#endif

$(binary_stamp)-java: $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	dh_installdirs -p$(p_jrehl)   $(dirs_jrehl)
	dh_installdirs -p$(p_jlib)  $(dirs_jlib)
	dh_installdirs -p$(p_jlibx) $(dirs_jlibx)

	DH_COMPAT=2 dh_movefiles -p$(p_jrehl)   $(files_jrehl)
	DH_COMPAT=2 dh_movefiles -p$(p_jlib)  $(files_jlib)
	DH_COMPAT=2 dh_movefiles -p$(p_jlibx) $(files_jlibx)
#ifneq (,$(findstring gtk, $(java_awt_peers)))
#	DH_COMPAT=2 dh_movefiles -p$(p_jgtk) $(files_jgtk)
#endif
#ifneq (,$(findstring qt, $(java_awt_peers)))
#	DH_COMPAT=2 dh_movefiles -p$(p_jqt) $(files_jqt)
#endif

# FIXME
#	  $(jvm_dir) $(PF)/lib/jvm/$(jvm_name_long) \

	dh_link -p$(p_jrehl) \
	  $(jvm_dir) $(PF)/lib/jvm/java-gcj$(pkg_ver) \
	  $(PF)/bin/gij$(pkg_ver) $(jvm_dir)/bin/gij \
	  $(PF)/bin/gij$(pkg_ver) $(jvm_dir)/jre/bin/gij \
	  $(PF)/bin/gcj-dbtool$(pkg_ver) $(jvm_dir)/bin/gcj-dbtool \
	  $(PF)/bin/gcj-dbtool$(pkg_ver) $(jvm_dir)/jre/bin/gcj-dbtool \
	  $(PF)/share/man/man1/gkeytool$(pkg_ver).1 $(jvm_dir)/man/man1/keytool.1 \
	  $(PF)/share/man/man1/gorbd$(pkg_ver).1 $(jvm_dir)/man/man1/orbd.1 \
	  $(PF)/share/man/man1/grmid$(pkg_ver).1 $(jvm_dir)/man/man1/rmid.1 \
	  $(PF)/share/man/man1/grmiregistry$(pkg_ver).1 $(jvm_dir)/man/man1/rmiregistry.1 \
	  $(PF)/share/man/man1/gtnameserv$(pkg_ver).1 $(jvm_dir)/man/man1/tnameserv.1 \

ifneq ($(GFDL_INVARIANT_FREE),yes)
	dh_link -p$(p_jrehl) \
	  $(PF)/share/man/man1/gij$(pkg_ver).1 $(jvm_dir)/man/man1/java.1 \
	  $(PF)/share/man/man1/grmic$(pkg_ver).1 $(jvm_dir)/man/man1/rmiregistry.1
endif

ifneq ($(DEB_TARGET_ARCH_CPU),$(java_cpu))
	ln -sf $(java_cpu) $(d_jlib)/$(jvm_dir)/jre/lib/$(DEB_TARGET_ARCH_CPU)
endif

	dh_link -p$(p_jlib) \
	  etc/java/cacerts-gcj $(jvm_dir)/jre/lib/cacerts \
	  $(foreach i, jvm javamath, \
		$(gcj_vlibdir)/lib$(i).so $(jvm_dir)/lib/lib$(i).so)

	dh_link -p$(p_jlibx) \
	  $(foreach i, jawt, $(gcj_vlibdir)/lib$(i).so $(jvm_dir)/lib/lib$(i).so)

ifeq ($(DEB_HOST_ARCH),hppa)
	mv $(d_jrehl)/$(PF)/bin/gij$(pkg_ver) \
		$(d_jrehl)/$(PF)/bin/gij$(pkg_ver).bin
	install -m755 debian/gij-hppa $(d_jrehl)/$(PF)/bin/gij$(pkg_ver)
endif

	ln -s ../libgcj.so.$(GCJ_SONAME) \
		$(d_jlib)/$(gcj_vlibdir)/libgcj_bc.so.1

	: # provide .jinfo file
	( \
	  echo 'name=$(jvm_name_short)'; \
	  echo 'alias=java-gcj$(pkg_ver)'; \
	  echo 'priority=$(priority)'; \
	  echo 'section=main'; \
	  echo ''; \
	  for i in $(jre_tools); do \
	    echo "jre $$i /$(jvm_dir)/jre/bin/$$i"; \
	  done; \
	  for i in $(jdk_tools); do \
	    echo "jdk $$i /$(jvm_dir)/bin/$$i"; \
	  done; \
	) > $(d_jrehl)/usr/lib/jvm/.java-gcj$(pkg_ver).jinfo

	cp -p $(srcdir)/libjava/{NEWS,README,THANKS} \
		$(d_jrehl)/usr/share/doc/$(p_jbase)/

	debian/dh_doclink -p$(p_jrehl) $(p_jbase)
	debian/dh_doclink -p$(p_jlib)  $(p_jbase)
	debian/dh_doclink -p$(p_jlibx) $(p_jbase)

#ifneq (,$(findstring gtk, $(java_awt_peers)))
#	debian/dh_doclink -p$(p_jgtk) $(p_jbase)
#endif
#ifneq (,$(findstring qt, $(java_awt_peers)))
#	debian/dh_doclink -p$(p_jqt) $(p_jbase)
#endif

ifeq ($(with_separate_libgcj),yes)
  ifeq ($(PKGSOURCE),gcj-$(BASE_VERSION))
    ifeq ($(with_check),yes)
	cp -p test-summary $(d_jrehl)/usr/share/doc/$(p_jbase)/test-summary
    endif
  endif
endif
	debian/dh_rmemptydirs -p$(p_jrehl)
	debian/dh_rmemptydirs -p$(p_jlib)
	debian/dh_rmemptydirs -p$(p_jlibx)

	mkdir -p $(d_jrehl)/var/lib/gcj$(pkg_ver)

	dh_makeshlibs -p$(p_jlib) -V '$(p_jlib) (>= $(DEB_GCJ_SOVERSION))'
	echo "libgcj_bc 1 libgcj-bc (>= 4.2.2-1)" >> debian/$(p_jlib)/DEBIAN/shlibs
	cat debian/$(p_jlib)/DEBIAN/shlibs >> debian/shlibs.local

	dh_makeshlibs -p$(p_jlibx) -V '$(p_jlibx) (>= $(DEB_GCJ_SOVERSION))'

#ifneq (,$(findstring gtk, $(java_awt_peers)))
#	dh_makeshlibs -p$(p_jgtk) -V '$(p_jgtk) (>= $(DEB_GCJ_SOVERSION))'
#endif
#ifneq (,$(findstring qt, $(java_awt_peers)))
#	dh_makeshlibs -p$(p_jqt) -V '$(p_jqt) (>= $(DEB_GCJ_SOVERSION))'
#endif

	dh_strip -X/bin/ --dbg-package=$(p_jdbg) \
		-p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs)
	rm -f $(d_jdbg)/$(gcc_lib_dir)/libgcj_bc.so

	dh_compress -p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs)
	dh_fixperms -p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs)
# the libstdc++ binary packages aren't built yet ...
	echo 'libstdc++ $(CXX_SONAME) libstdc++$(CXX_SONAME) (>= $(DEB_STDCXX_SOVERSION))' \
	    >> debian/shlibs.local
	-[ -d $(d_l64gcc) ] && mv $(d_l64gcc) $(d_l64gcc).saved
ifeq ($(with_separate_libgcj)-$(with_standalone_gcj),yes-no)
	dh_shlibdeps \
		-L$(p_jlib) \
		-l$(d_lib)/$(PF)/$(libdir):$(d_jlib)/$(PF)/$(libdir) \
		-p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs)
else
	dh_shlibdeps \
		-L$(p_lgcc) \
		-L$(p_jlib) \
		-l:$(d)/$(PF)/$(libdir):$(d_lib)/$(PF)/$(libdir):$(d_jlib)/$(PF)/$(libdir):$(d_lgcc)/lib \
		-p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs)
endif
	-[ -d $(d_l64gcc).saved ] && mv $(d_l64gcc).saved $(d_l64gcc)
	sed -e 's/$(p_jlib)[^,]*//' -e 's/, *,/,/' debian/$(p_jlib).substvars \
		>> debian/$(p_jlib).substvars.tmp \
	    && mv -f debian/$(p_jlib).substvars.tmp debian/$(p_jlib).substvars
	rm -f debian/shlibs.local

	dh_gencontrol \
		-p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs) \
		-- -v$(DEB_VERSION) $(common_substvars)

	mkdir -p $(d_jlib)/usr/share/lintian/overrides
	cp -p debian/$(p_jlib).overrides \
		$(d_jlib)/usr/share/lintian/overrides/$(p_jlib)
	mkdir -p $(d_jlibx)/usr/share/lintian/overrides
	cp -p debian/$(p_jlibx).overrides \
		$(d_jlibx)/usr/share/lintian/overrides/$(p_jlibx)
	mkdir -p $(d_jrehl)/usr/share/lintian/overrides
	cp -p debian/$(p_jrehl).overrides \
		$(d_jrehl)/usr/share/lintian/overrides/$(p_jrehl)

	dh_installdeb -p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs)
	dh_md5sums -p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs)
	dh_builddeb -p$(p_jrehl) -p$(p_jlib) -p$(p_jlibx) $(peer_pkgs)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-gcj: $(build_html_stamp) $(install_stamp)
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	rm -rf $(d_jdk)
	dh_installdirs -p$(p_jdk)  $(dirs_jdk)

#	mkdir -p $(d_jdk)/usr/share/lintian/overrides
#	cp -p debian/$(p_jdk).overrides \
#		$(d_jdk)/usr/share/lintian/overrides/$(p_jdk)
	cp -p $(srcdir)/gcc/java/ChangeLog \
		$(d_jdk)/usr/share/doc/$(p_jbase)/changelog.gcj
	cp -p $(srcdir)/libjava/ChangeLog \
		$(d_jdk)/usr/share/doc/$(p_jbase)/changelog.libjava

ifeq ($(with_standalone_gcj),yes)
	rm -f $(d)/$(PF)/$(libdir)/libgcc_s.so
	ln -sf /$(libdir)/libgcc_s.so.$(GCC_SONAME) $(d)/$(gcc_lib_dir)/libgcc_s.so
endif
	DH_COMPAT=2 dh_movefiles -p$(p_jdk)  $(files_jdk)

	for i in libgij libgcj libgcj-tools; do \
	  dh_link -p$(p_jdk) \
	    /$(PF)/$(libdir)/$$i.so.$(GCJ_SONAME) /$(gcc_lib_dir)/$$i.so \
	    /$(PF)/$(libdir)/$$i.so.$(GCJ_SONAME) /$(jvm_dir)/lib/$$i.so; \
	  rm -f $(d_jdk)/$(PF)/$(libdir)/$$i.{la,so}; \
	done

	ln -sf gcj$(pkg_ver) \
	    $(d_jdk)/$(PF)/bin/$(TARGET_ALIAS)-gcj$(pkg_ver)

	install -m755 debian/jdb.sh $(d_jdk)/$(jvm_dir)/bin/jdb

	mv $(d_jdk)/$(PF)/$(libdir)/libgcj.spec $(d_jdk)/$(gcc_lib_dir)/

	install -m 755 $(d)/$(PF)/$(libdir)/libgcj_bc.so.1 \
		$(d_jdk)/$(gcc_lib_dir)/libgcj_bc.so
	$(builddir)/gcc/xgcc -B$(builddir)/gcc/ -shared -fpic -xc /dev/null \
		-o build/libgcj.so -Wl,-soname,libgcj.so.$(GCJ_SONAME) -nostdlib
	$(builddir)/gcc/xgcc -B$(builddir)/gcc/ -shared -fpic \
		$(srcdir)/libjava/libgcj_bc.c \
		-o $(d_jdk)/$(gcc_lib_dir)/libgcj_bc.so \
		-Wl,-soname,libgcj_bc.so.1 $(builddir)/libgcj.so -shared-libgcc 

	dh_link -p$(p_jdk) \
	  $(gcc_lib_dir)/include/gcj $(jvm_dir)/include/gcj \
	  usr/bin/ecj $(jvm_dir)/bin/javac \
	  usr/bin/fastjar $(jvm_dir)/bin/jar \
	  $(PF)/share/man/man1/ecj.1 $(jvm_dir)/man/man1/javac.1 \
	  $(PF)/share/man/man1/fastjar.1 $(jvm_dir)/man/man1/jar.1

	dh_link -p$(p_jdk) \
	  $(PF)/bin/gcj$(pkg_ver) $(jvm_dir)/bin/gcj \
	  $(PF)/share/man/man1/gjarsigner$(pkg_ver).1 $(jvm_dir)/man/man1/jarsigner.1 \
	  $(PF)/share/man/man1/gjdoc$(pkg_ver).1 $(jvm_dir)/man/man1/javadoc.1 \
	  $(PF)/share/man/man1/gjavah$(pkg_ver).1 $(jvm_dir)/man/man1/javah.1 \
	  $(PF)/share/man/man1/gserialver$(pkg_ver).1 $(jvm_dir)/man/man1/serialver.1 \
	  $(PF)/share/man/man1/gappletviewer$(pkg_ver).1 $(jvm_dir)/man/man1/appletviewer.1

ifneq (,$(filter $(DEB_HOST_ARCH), arm armel))
	ln -sf ../../ecj1 $(d_jdk)/$(gcc_lexec_dir)/ecj1
endif

ifneq ($(GFDL_INVARIANT_FREE),yes)
	ln -sf gcj$(pkg_ver).1 \
	    $(d_jdk)/$(PF)/share/man/man1/$(TARGET_ALIAS)-gcj$(pkg_ver).1
	cp -p html/gcj.html $(d_jdk)/$(docdir)/$(p_jbase)/
endif

	debian/dh_doclink -p$(p_jdk) $(p_jbase)

	cp -p debian/FAQ.gcj $(d_jdk)/$(docdir)/$(p_jbase)/

	cp -p debian/gcj-wrapper$(pkg_ver) $(d_jdk)/$(PF)/bin/
	chmod 755 $(d_jdk)/$(PF)/bin/gcj-wrapper$(pkg_ver)
	cp -p debian/gcj-wrapper$(pkg_ver).1 $(d_jdk)/$(PF)/share/man/man1/

	debian/dh_rmemptydirs -p$(p_jdk)

	dh_strip -p$(p_jdk)
	dh_compress -p$(p_jdk) -X.java
	dh_fixperms -p$(p_jdk)
	dh_shlibdeps -p$(p_jdk) -l$(d_lib)/$(PF)/$(libdir):$(d_jlib)/$(PF)/lib -Xecj1
	dh_gencontrol -p$(p_jdk) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_jdk)
	dh_md5sums -p$(p_jdk)
	dh_builddeb -p$(p_jdk)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-libgcjdev: $(build_html_stamp) $(install_stamp) $(binary_stamp)-java
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	dh_installdirs -p$(p_jdev) $(dirs_jdev)

	DH_COMPAT=2 dh_movefiles -p$(p_jdev) $(files_jdev)

ifeq ($(with_static_java),yes)
	for i in libgij libgcj libgcj-tools; do \
	  mv $(d_jdev)/$(PF)/$(libdir)/$$i.a $(d_jdev)/$(gcc_lib_dir)/; \
	done
endif

	ln -sf libgcj-$(BASE_VERSION).pc \
		$(d_jdev)/$(PF)/$(libdir)/pkgconfig/libgcj$(PKG_GCJ_EXT).pc
	sed -i -e 's,-[IL][^ ]* *,,' \
		$(d_jdev)/$(PF)/$(libdir)/pkgconfig/libgcj-$(BASE_VERSION).pc

	debian/dh_doclink -p$(p_jdev) $(p_jbase)

	debian/dh_rmemptydirs -p$(p_jdev)

#	mkdir -p $(d_jdev)/usr/share/lintian/overrides
#	cp -p debian/libgcj$(PKG_GCJ_EXT)-dev.overrides \
#		$(d_jdev)/usr/share/lintian/overrides/$(p_jdev)

	DH_COMPAT=5 dh_strip -p$(p_jdev) --dbg-package=$(p_jdbg)
	dh_compress -p$(p_jdev) -X.java
	dh_fixperms -p$(p_jdev)
ifeq ($(with_separate_libgcj)-$(with_standalone_gcj),yes-no)
	dh_shlibdeps \
		-l$(d_lib)/$(PF)/$(libdir):$(d_jlib)/$(PF)/$(libdir) \
		-p$(p_jdev)
else
	dh_shlibdeps \
		-L$(p_lgcc) \
		-l:$(d)/$(PF)/$(libdir):$(d_lib)/$(PF)/$(libdir):$(d_jlib)/$(PF)/$(libdir):$(d_lgcc)/lib \
		-p$(p_jdev)
endif
	dh_gencontrol -p$(p_jdev) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_jdev)
	dh_md5sums -p$(p_jdev)
	dh_builddeb -p$(p_jdev)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-libgcjdbg: $(install_stamp) $(binary_stamp)-java $(binary_stamp)-libgcjdev $(binary_stamp)-gcjjre
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

	debian/dh_doclink -p$(p_jdbg) $(p_jbase)

	for i in libgij libgcj libgcj-tools; do \
	  if [ -f $(d_jdbg)/usr/lib/debug/usr/lib/$$i.so.$(GCJ_SONAME).0.0 ]; then \
	    ln -sf $$i.so.$(GCJ_SONAME).0.0 \
	      $(d_jdbg)/usr/lib/debug/usr/lib/$$i.so.$(GCJ_SONAME); \
	  fi; \
	done
#	ln -sf libgconfpeer.so.0.0.0 \
#	  $(d_jdbg)/usr/lib/debug/$(gcj_vlibdir)/libgconfpeer.so.0

	dh_compress -p$(p_jdbg)
	dh_fixperms -p$(p_jdbg)
	dh_gencontrol -p$(p_jdbg) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_jdbg)
	dh_md5sums -p$(p_jdbg)
	dh_builddeb -p$(p_jdbg)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)

# ----------------------------------------------------------------------
$(binary_stamp)-gcjjre: $(install_stamp) $(binary_stamp)-java
	dh_testdir
	dh_testroot
	mv $(install_stamp) $(install_stamp)-tmp

#	dh_installdirs -p$(p_jre) \
#		$(PF)/bin \
#		$(PF)/share/man/man1 \
#		$(jvm_dir)/bin

	debian/dh_doclink -p$(p_jre) $(p_jbase)
	DH_COMPAT=5 dh_strip -p$(p_jre) --dbg-package=$(p_jdbg)
	dh_compress -p$(p_jre)
	dh_fixperms -p$(p_jre)
	dh_gencontrol -p$(p_jre) -- -v$(DEB_VERSION) $(common_substvars)
	dh_installdeb -p$(p_jre)
	dh_md5sums -p$(p_jre)
	dh_builddeb -p$(p_jre)

	trap '' 1 2 3 15; touch $@; mv $(install_stamp)-tmp $(install_stamp)
