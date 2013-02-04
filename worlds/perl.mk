.PHONY: libcairo-perl
libcairo-perl:$(LIBCAIROPERL)_$(ARCH).deb
$(LIBCAIROPERL): $(SPREZZ)/libcairo-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Cairo-$(libcairo-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libpango-perl
libpango-perl:$(LIBPANGOPERL)_$(ARCH).deb
$(LIBPANGOPERL): $(SPREZZ)/libpango-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Pango-$(libpango-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcrypt-openssl-bignum-perl
libcrypt-openssl-bignum-perl:$(LIBCRYPTOPENSSLBIGNUMPERL)_$(ARCH).deb
$(LIBCRYPTOPENSSLBIGNUMPERL): $(SPREZZ)/libcrypt-openssl-bignum-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Crypt-OpenSSL-Bignum-$(libcrypt-openssl-bignum-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libextutils-parsexs-perl
libextutils-parsexs-perl:$(LIBEXTUTILSPARSEXSPERL)_$(ARCH).deb
$(LIBEXTUTILSPARSEXSPERL): $(SPREZZ)/libextutils-parsexs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ExtUtils-ParseXS-$(shell echo $(libextutils-parsexs-perl_UPVER) | sed -e 's/0*$$//').tar.gz $(TARARGS) $@

.PHONY: libfont-freetype-perl
libfont-freetype-perl:$(LIBFONTFREETYPEPERL)_$(ARCH).deb
$(LIBFONTFREETYPEPERL): $(SPREZZ)/libfont-freetype-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Font-FreeType-$(libfont-freetype-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libglib-perl
libglib-perl:$(LIBGLIBPERL)_$(ARCH).deb
$(LIBGLIBPERL): $(SPREZZ)/libglib-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Glib-$(libglib-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgd-gd2-perl
libgd-gd2-perl:$(LIBGDGD2PERL)_$(ARCH).deb
$(LIBGDGD2PERL): $(SPREZZ)/libgd-gd2-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf GD-$(libgd-gd2-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgtk2-perl
libgtk2-perl:$(LIBGTK2PERL)_$(ARCH).deb
$(LIBGTK2PERL): $(SPREZZ)/libgtk2-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gtk2-$(libgtk2-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnet-dropbox-api-perl
libnet-dropbox-api-perl:$(LIBNETDROPBOXAPIPERL)_$(ARCH).deb
$(LIBNETDROPBOXAPIPERL): $(SPREZZ)/libnet-dropbox-api-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Net-Dropbox-API-$(libnet-dropbox-api-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libproc-processtable-perl
libproc-processtable-perl:$(LIBPROCPROCESSTABLEPERL)_$(ARCH).deb
$(LIBPROCPROCESSTABLEPERL): $(SPREZZ)/libproc-processtable-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Proc-ProcessTable-$(libproc-processtable-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmouse-perl
libmouse-perl:$(LIBMOUSEPERL)_$(ARCH).deb
$(LIBMOUSEPERL): $(SPREZZ)/libmouse-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Mouse-$(libmouse-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmoose-perl
libmoose-perl:$(LIBMOOSEPERL)_$(ARCH).deb
$(LIBMOOSEPERL): $(SPREZZ)/libmoose-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Moose-$(libmoose-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtest-leaktrace-perl
libtest-leaktrace-perl:$(LIBTESTLEAKTRACEPERL)_$(ARCH).deb
$(LIBTESTLEAKTRACEPERL): $(SPREZZ)/libtest-leaktrace-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Test-LeakTrace-$(libtest-leaktrace-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libpadwalker-perl
libpadwalker-perl:$(LIBPADWALKERPERL)_$(ARCH).deb
$(LIBPADWALKERPERL): $(SPREZZ)/libpadwalker-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf PadWalker-$(libpadwalker-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libclass-load-xs-perl
libclass-load-xs-perl:$(LIBCLASSLOADXSPERL)_$(ARCH).deb
$(LIBCLASSLOADXSPERL): $(SPREZZ)/libclass-load-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Class-Load-XS-$(libclass-load-xs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcrypt-openssl-rsa-perl
libcrypt-openssl-rsa-perl:$(LIBCRYPTOPENSSLRSAPERL)_$(ARCH).deb
$(LIBCRYPTOPENSSLRSAPERL): $(SPREZZ)/libcrypt-openssl-rsa-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Crypt-OpenSSL-RSA-$(libcrypt-openssl-rsa-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcrypt-openssl-random-perl
libcrypt-openssl-random-perl:$(LIBCRYPTOPENSSLRANDOMPERL)_$(ARCH).deb
$(LIBCRYPTOPENSSLRANDOMPERL): $(SPREZZ)/libcrypt-openssl-random-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Crypt-OpenSSL-Random-$(libcrypt-openssl-random-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgnome2-canvas-perl
libgnome2-canvas-perl:$(LIBGNOME2CANVASPERL)_$(ARCH).deb
$(LIBGNOME2CANVASPERL): $(SPREZZ)/libgnome2-canvas-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gnome2-Canvas-$(libgnome2-canvas-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgnome2-vfs-perl
libgnome2-vfs-perl:$(LIBGNOME2VFSPERL)_$(ARCH).deb
$(LIBGNOME2VFSPERL): $(SPREZZ)/libgnome2-vfs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gnome2-VFS-$(libgnome2-vfs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgnome2-wnck-perl
libgnome2-wnck-perl:$(LIBGNOME2WNCKPERL)_$(ARCH).deb
$(LIBGNOME2WNCKPERL): $(SPREZZ)/libgnome2-wnck-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gnome2-Wnck-$(libgnome2-wnck-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgnome2-perl
libgnome2-perl:$(LIBGNOME2PERL)_$(ARCH).deb
$(LIBGNOME2PERL): $(SPREZZ)/libgnome2-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gnome2-$(libgnome2-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsuper-perl
libsuper-perl:$(LIBSUPERPERL)_$(ARCH).deb
$(LIBSUPERPERL): $(SPREZZ)/libsuper-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SUPER-$(libsuper-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsub-identify-perl
libsub-identify-perl:$(LIBSUBIDENTIFYPERL)_$(ARCH).deb
$(LIBSUBIDENTIFYPERL): $(SPREZZ)/libsub-identify-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Sub-Identify-$(libsub-identify-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdata-random-perl
libdata-random-perl:$(LIBDATARANDOMPERL)_$(ARCH).deb
$(LIBDATARANDOMPERL): $(SPREZZ)/libdata-random-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Data-Random-$(libdata-random-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdate-calc-perl
libdate-calc-perl:$(LIBDATECALCPERL)_$(ARCH).deb
$(LIBDATECALCPERL): $(SPREZZ)/libdate-calc-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Date-Calc-$(libdate-calc-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcarp-clan-perl
libcarp-clan-perl:$(LIBCARPCLANPERL)_$(ARCH).deb
$(LIBCARPCLANPERL): $(SPREZZ)/libcarp-clan-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Carp-Clan-$(libcarp-clan-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libbit-vector-perl
libbit-vector-perl:$(LIBBITVECTORPERL)_$(ARCH).deb
$(LIBBITVECTORPERL): $(SPREZZ)/libbit-vector-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Bit-Vector-$(libbit-vector-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libyaml-libyaml-perl
libyaml-libyaml-perl:$(LIBYAMLLIBYAMLPERL)_$(ARCH).deb
$(LIBYAMLLIBYAMLPERL): $(SPREZZ)/libyaml-libyaml-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf YAML-LibYAML-$(libyaml-libyaml-perl_UPVER).tar.gz $(TARARGS) $@

