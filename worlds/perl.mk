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

.PHONY: libgnome2-vfs-perl
libgnome2-vfs-perl:$(LIBGNOME2VFSPERL)_$(ARCH).deb
$(LIBGNOME2VFSPERL): $(SPREZZ)/libgnome2-vfs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gnome2-VFS-$(libgnome2-vfs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgd-gd2-perl
libgd-gd2-perl:$(LIBGDGD2PERL)_$(ARCH).deb
$(LIBGDGD2PERL): $(SPREZZ)/libgd-gd2-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf GD-$(libgd-gd2-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgnome2-perl
libgnome2-perl:$(LIBGNOME2PERL)_$(ARCH).deb
$(LIBGNOME2PERL): $(SPREZZ)/libgnome2-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gnome2-$(libgnome2-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgtk2-perl
libgtk2-perl:$(LIBGTK2PERL)_$(ARCH).deb
$(LIBGTK2PERL): $(SPREZZ)/libgtk2-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gtk2-$(libgtk2-perl_UPVER).tar.gz $(TARARGS) $@

