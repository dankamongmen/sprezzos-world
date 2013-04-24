arch_binaries  := $(arch_binaries) snapshot

ifneq (,$(findstring gcc-snapshot, $(PKGSOURCE)))
  p_snap = gcc-snapshot
else ifneq (,$(findstring gcc-linaro, $(PKGSOURCE)))
  p_snap = gcc-linaro
else
  $(error unknown build for single gcc package)
endif

ifeq ($(DEB_CROSS),yes)
  p_snap := $(p_snap)$(cross_bin_arch)
endif
d_snap	= debian/$(p_snap)

dirs_snap = \
	$(docdir)/$(p_snap) \
	usr/lib

ifeq ($(with_spu),yes)
  snapshot_depends = binutils-spu
  snapshot_recommends = newlib-spu
endif
ifeq ($(with_hppa64),yes)
  snapshot_depends = binutils-hppa64
endif

$(stampdir)/ecj_binaries: $(install_snap_stamp)
	mkdir -p $(builddir)/aot/jar $(builddir)/aot/bin
	cp $(ecj_jar) $(builddir)/aot/jar/ecj-standalone.jar
	zip -d $(builddir)/aot/jar/ecj-standalone.jar \
		'org/eclipse/jdt/core/JDTCompilerAdapter*'

	cd $(builddir)/aot/jar \
	  && fastjar xf ecj-standalone.jar \
	  && find -name '*.rsc' -o -name '*.properties' \
		| fastjar -c -@ - -f ../resources.jar
	rm -rf $(builddir)/aot/jar/META-INF $(builddir)/aot/jar/org
	$(d)/$(PF)/bin/gcj \
	    -c -O2 -g -fPIC -fjni -findirect-dispatch \
	    -o $(builddir)/aot/bin/resources.o $(builddir)/aot/resources.jar

	cp $(srcdir)/libjava/contrib/classfile.py $(builddir)/aot/
	cp $(buildlibdir)/libjava/contrib/*.py $(builddir)/aot/
	grep -v '^sys.path.insert' $(buildlibdir)/libjava/contrib/aot-compile \
	    > $(builddir)/aot/aot-compile
	chmod 755 $(builddir)/aot/aot-compile
	patch -p1 < debian/patches/aotcompile.diff
	LD_LIBRARY_PATH=$${LD_LIBRARY_PATH:+$$LD_LIBRARY_PATH:}$(CURDIR)/$(d)/$(PF)/lib \
	RPATH=-Wl,-rpath,/$(PF)/lib \
	PYTHONPATH=$(builddir)/aot \
	  python $(builddir)/aot/aot-compile \
	    --gcj=$(CURDIR)/$(d)/$(PF)/bin/gcj \
	    --dbtool=$(CURDIR)/$(d)/$(PF)/bin/gcj-dbtool \
	    $(builddir)/aot/jar $(builddir)/aot/bin
	touch $@

# ----------------------------------------------------------------------
$(binary_stamp)-snapshot: $(install_snap_stamp) \
    $(if $(filter $(with_ecj),yes),$(stampdir)/ecj_binaries)
	dh_testdir
	dh_testroot
	mv $(install_snap_stamp) $(install_snap_stamp)-tmp

	rm -rf $(d_snap)
	dh_installdirs -p$(p_snap) $(dirs_snap)

	mv $(d)/$(PF) $(d_snap)/usr/lib/

	find $(d_snap) -name '*.gch' -type d | xargs -r rm -rf
	find $(d_snap) -name '*.la' -o -name '*.lai' | xargs -r rm -f

	: # FIXME: libbacktrace is not installed by default
	for d in . 32 n32 64 sf hf; do \
	  if [ -f $(buildlibdir)/$$d/libbacktrace/.libs/libbacktrace.a ]; then \
	    install -m644 $(buildlibdir)/$$d/libbacktrace/.libs/libbacktrace.a \
	      $(d_snap)/$(gcc_lib_dir)/$$d; \
	  fi; \
	done
	if [ -f $(buildlibdir)/libbacktrace/backtrace-supported.h ]; then \
	  install -m644 $(buildlibdir)/libbacktrace/backtrace-supported.h \
	    $(d_snap)/$(gcc_lib_dir)/include/; \
	  install -m644 $(srcdir)/libbacktrace/backtrace.h \
	    $(d_snap)/$(gcc_lib_dir)/include/; \
	fi

	rm -rf $(d_snap)/$(PF)/lib/nof
ifeq ($(with_java),yes)
	mv $(d)/usr/lib/jvm $(d_snap)/usr/lib/

	dh_link -p$(p_snap) \
	  $(gcc_lib_dir)/include/gcj $(jvm_dir)/include/gcj \
	  usr/bin/ecj $(jvm_dir)/bin/javac

  ifneq ($(DEB_TARGET_ARCH_CPU),$(java_cpu))
	ln -sf $(java_cpu) $(d_snap)/$(jvm_dir)/jre/lib/$(DEB_TARGET_ARCH_CPU)
  endif
  ifeq ($(with_ecj),yes)
	install -m755 $(builddir)/aot/bin/javac $(d_snap)/$(jvm_dir)/bin/javac
	install -m755 $(builddir)/aot/bin/ecj1 $(d_snap)/$(gcc_lexec_dir)/ecj1

    ifeq (./,$(dir $(ecj_jar)))
	install -m 644 $(ecj_jar) $(d_snap)/$(jvm_dir)/lib/ecj.jar
    else
	dh_link -p$(p_snap) \
	  $(ecj_jar) $(jvm_dir)/lib/ecj.jar
    endif
  endif

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
	) > $(d_snap)/usr/lib/jvm/.java-gcj$(pkg_ver)-snap.jinfo
endif

ifeq ($(with_ada),yes FIXME: apply our ada patches)
	dh_link -p$(p_snap) \
	   $(gcc_lib_dir)/rts-sjlj/adalib/libgnat.a \
	   $(gcc_lib_dir)/rts-sjlj/adalib/libgnat-$(GNAT_VERSION).a
	dh_link -p$(p_snap) \
	   $(gcc_lib_dir)/rts-sjlj/adalib/libgnarl.a \
	   $(gcc_lib_dir)/rts-sjlj/adalib/libgnarl-$(GNAT_VERSION).a

	set -e; \
	for lib in lib{gnat,gnarl}; do \
	  vlib=$$lib-$(GNAT_SONAME); \
	  mv $(d_snap)/$(gcc_lib_dir)/adalib/$$vlib.so.1 $(d_snap)/$(PF)/$(libdir)/. ; \
	  rm -f $(d_snap)/$(gcc_lib_dir)/adalib/$$lib.so.1; \
	  dh_link -p$(p_snap) \
	    /$(PF)/$(libdir)/$$vlib.so.1 /$(PF)/$(libdir)/$$vlib.so \
	    /$(PF)/$(libdir)/$$vlib.so.1 /$(PF)/$(libdir)/$$lib.so \
	    /$(PF)/$(libdir)/$$vlib.so.1 /$(gcc_lib_dir)/rts-native/adalib/$$lib.so; \
	done
endif
ifeq ($(with_ada),yes)
	ln -sf gcc $(d_snap)/$(PF)/bin/gnatgcc
endif

ifeq ($(with_hppa64),yes)
	: # provide as and ld links
	dh_link -p $(p_snap) \
		/usr/bin/hppa64-linux-gnu-as \
		/$(PF)/libexec/gcc/hppa64-linux-gnu/$(GCC_VERSION)/as \
		/usr/bin/hppa64-linux-gnu-ld \
		/$(PF)/libexec/gcc/hppa64-linux-gnu/$(GCC_VERSION)/ld
endif

# don't do this; would create a b-d on gcc-snapshot
#ifeq ($(with_spu),yes)
#	dh_link -p $(p_snap) /usr/spu /$(PF)/spu
#endif

ifeq ($(with_check),yes)
	dh_installdocs -p$(p_snap) test-summary
  ifeq ($(with_pascal),yes)
	cp -p gpc-test-summary $(d_snap)/$(docdir)/$(p_snap)/
  endif
else
	dh_installdocs -p$(p_snap)
endif
	if [ -f $(buildlibdir)/libstdc++-v3/testsuite/current_symbols.txt ]; \
	then \
	  cp -p $(buildlibdir)/libstdc++-v3/testsuite/current_symbols.txt \
	    $(d_snap)/$(docdir)/$(p_snap)/libstdc++6_symbols.txt; \
	fi
	cp -p debian/README.snapshot \
		$(d_snap)/$(docdir)/$(p_snap)/README.Debian
	cp -p debian/README.Bugs \
		$(d_snap)/$(docdir)/$(p_snap)/
	dh_installchangelogs -p$(p_snap)
ifeq ($(DEB_TARGET_ARCH),hppa)
	dh_strip -p$(p_snap) -Xdebug -X.o -X.a
else
	dh_strip -p$(p_snap) -Xdebug
endif
	dh_compress -p$(p_snap) -X README.Bugs
	-find $(d_snap) -type d ! -perm 755 -exec chmod 755 {} \;
	dh_fixperms -p$(p_snap)
ifeq ($(with_ada),yes)
	find $(d_snap)/$(gcc_lib_dir) -name '*.ali' | xargs -r chmod 444
endif

	mkdir -p $(d_snap)/usr/share/lintian/overrides
	cp -p debian/gcc-snapshot.overrides \
		$(d_snap)/usr/share/lintian/overrides/$(p_snap)

	( \
	  echo 'libgcc_s $(GCC_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libobjc $(OBJC_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libgfortran $(FORTRAN_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libmudflap $(MUDFLAP_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libmudflapth $(MUDFLAP_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libffi $(FFI_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libgcj $(GCJ_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libgcj-tools $(GCJ_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libgij $(GCJ_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libgcj_bc 1 ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libgomp $(GOMP_SONAME) ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libgnat-$(GNAT_SONAME) 1 ${p_snap} (>= $(DEB_VERSION))'; \
	  echo 'libgnarl-$(GNAT_SONAME) 1 ${p_snap} (>= $(DEB_VERSION))'; \
	) > debian/shlibs.local

	$(ignshld)DIRNAME=$(subst n,,$(2)) $(cross_shlibdeps)  \
	  dh_shlibdeps -p$(p_snap) -l$(CURDIR)/$(d_snap)/$(PF)/lib:$(CURDIR)/$(d_snap)/$(PF)/$(if $(filter $(DEB_TARGET_ARCH),amd64 ppc64),lib32,lib64):/usr/$(DEB_TARGET_GNU_TYPE)/lib -Xlibgcj-tools -Xlibmudflap
	-sed -i -e 's/$(p_snap)[^,]*, //g' debian/$(p_snap).substvars

ifeq ($(with_multiarch_lib),yes)
	: # paths needed for relative lookups from startfile_prefixes
	for ma in $(xarch_multiarch_names); do \
	  mkdir -p $(d_snap)/lib/$$ma; \
	  mkdir -p $(d_snap)/usr/lib/$$ma; \
	done
endif

	dh_gencontrol -p$(p_snap) -- $(common_substvars) \
		'-Vsnap:depends=$(snapshot_depends)' '-Vsnap:recommends=$(snapshot_recommends)'
	dh_installdeb -p$(p_snap)
	dh_md5sums -p$(p_snap)
	dh_builddeb -p$(p_snap)

	trap '' 1 2 3 15; touch $@; mv $(install_snap_stamp)-tmp $(install_snap_stamp)
