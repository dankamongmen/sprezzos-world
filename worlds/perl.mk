.PHONY: libcairo-perl
libcairo-perl:$(LIBCAIROPERL)_$(ARCH).deb
$(LIBCAIROPERL): $(SPREZZ)/libcairo-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libcairo-perl-$(libcairo-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libpango-perl
libpango-perl:$(LIBPANGOPERL)_$(ARCH).deb
$(LIBPANGOPERL): $(SPREZZ)/libpango-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libpango-perl-$(libpango-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcrypt-openssl-bignum-perl
libcrypt-openssl-bignum-perl:$(LIBCRYPTOPENSSLBIGNUMPERL)_$(ARCH).deb
$(LIBCRYPTOPENSSLBIGNUMPERL): $(SPREZZ)/libcrypt-openssl-bignum-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libcrypt-openssl-bignum-perl-$(libcrypt-openssl-bignum-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libextutils-parsexs-perl
libextutils-parsexs-perl:$(LIBEXTUTILSPARSEXSPERL)_$(ARCH).deb
$(LIBEXTUTILSPARSEXSPERL): $(SPREZZ)/libextutils-parsexs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ExtUtils-ParseXS-$(shell echo $(libextutils-parsexs-perl_UPVER) | sed -e 's/0*$$//').tar.gz $(TARARGS) $@

