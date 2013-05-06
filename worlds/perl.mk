.PHONY: libipc-sharelite-perl
libipc-sharelite-perl:$(LIBIPCSHARELITEPERL)_$(ARCH).deb
$(LIBIPCSHARELITEPERL): $(SPREZZ)/libipc-sharelite-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf IPC-ShareLite-$(libipc-sharelite-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libipc-sharedcache-perl
libipc-sharedcache-perl:$(LIBIPCSHAREDCACHEPERL)_$(ARCH).deb
$(LIBIPCSHAREDCACHEPERL): $(SPREZZ)/libipc-sharedcache-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf IPC-SharedCache-$(libipc-sharedcache-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libalgorithm-diff-perl
libalgorithm-diff-perl:$(LIBALGORITHMDIFFPERL)_$(ARCH).deb
$(LIBALGORITHMDIFFPERL): $(SPREZZ)/libalgorithm-diff-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libalgorithm-diff-perl_$(libalgorithm-diff-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libalgorithm-merge-perl
libalgorithm-merge-perl:$(LIBALGORITHMMERGEPERL)_$(ARCH).deb
$(LIBALGORITHMMERGEPERL): $(SPREZZ)/libalgorithm-merge-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Algorithm-Merge-$(libalgorithm-merge-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libfile-libmagic-perl
libfile-libmagic-perl:$(LIBFILELIBMAGICPERL)_$(ARCH).deb
$(LIBFILELIBMAGICPERL): $(SPREZZ)/libfile-libmagic-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf File-LibMagic-$(libfile-libmagic-perl_UPVER).tgz $(TARARGS) $@

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

.PHONY: libgnome2-gconf-perl
libgnome2-gconf-perl:$(LIBGNOME2GCONFPERL)_$(ARCH).deb
$(LIBGNOME2GCONFPERL): $(SPREZZ)/libgnome2-gconf-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gnome2-GConf-$(libgnome2-gconf-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgtk2-appindicator-perl
libgtk2-appindicator-perl:$(LIBGTK2APPINDICATORPERL)_$(ARCH).deb
$(LIBGTK2APPINDICATORPERL): $(SPREZZ)/libgtk2-appindicator-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gtk2-AppIndicator-$(libgtk2-appindicator-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgtk2-imageview-perl
libgtk2-imageview-perl:$(LIBGTK2IMAGEVIEWPERL)_$(ARCH).deb
$(LIBGTK2IMAGEVIEWPERL): $(SPREZZ)/libgtk2-imageview-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gtk2-ImageView-$(libgtk2-imageview-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgtk2-unique-perl
libgtk2-unique-perl:$(LIBGTK2UNIQUEPERL)_$(ARCH).deb
$(LIBGTK2UNIQUEPERL): $(SPREZZ)/libgtk2-unique-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Gtk2-Unique-$(libgtk2-unique-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libjson-xs-perl
libjson-xs-perl:$(LIBJSONXSPERL)_$(ARCH).deb
$(LIBJSONXSPERL): $(SPREZZ)/libjson-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf JSON-XS-$(libjson-xs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgoo-canvas-perl
libgoo-canvas-perl:$(LIBGOOCANVASPERL)_$(ARCH).deb
$(LIBGOOCANVASPERL): $(SPREZZ)/libgoo-canvas-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Goo-Canvas-$(libgoo-canvas-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnet-dbus-glib-perl
libnet-dbus-glib-perl:$(LIBNETDBUSGLIBPERL)_$(ARCH).deb
$(LIBNETDBUSGLIBPERL): $(SPREZZ)/libnet-dbus-glib-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Net-DBus-GLib-$(libnet-dbus-glib-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libfcgi-perl
libfcgi-perl:$(LIBFCGIPERL)_$(ARCH).deb
$(LIBFCGIPERL): $(SPREZZ)/libfcgi-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf FCGI-$(libfcgi-perl_UPVER).tar.gz $(TARARGS) $@
.PHONY: ossp-uuid
ossp-uuid:$(OSSPUUID)_$(ARCH).deb
$(OSSPUUID): $(SPREZZ)/ossp-uuid/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uuid-$(ossp-uuid_UPVER).tar.gz $(TARARGS) $@

.PHONY: libyaml-syck-perl
libyaml-syck-perl:$(LIBYAMLSYCKPERL)_$(ARCH).deb
$(LIBYAMLSYCKPERL): $(SPREZZ)/libyaml-syck-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf YAML-Syck-$(libyaml-syck-perl_UPVER).tar.gz $(TARARGS) $@


.PHONY: libdevel-leak-perl
libdevel-leak-perl:$(LIBDEVELLEAKPERL)_$(ARCH).deb
$(LIBDEVELLEAKPERL): $(SPREZZ)/libdevel-leak-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Devel-Leak-$(libdevel-leak-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsoap-lite-perl
libsoap-lite-perl:$(LIBSOAPLITEPERL)_$(ARCH).deb
$(LIBSOAPLITEPERL): $(SPREZZ)/libsoap-lite-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SOAP-Lite-$(libsoap-lite-perl_UPVER).tar.gz $(TARARGS) $@
.PHONY: libterm-size-perl
libterm-size-perl:$(LIBTERMSIZEPERL)_$(ARCH).deb
$(LIBTERMSIZEPERL): $(SPREZZ)/libterm-size-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Term-Size-$(libterm-size-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libvariable-magic-perl
libvariable-magic-perl:$(LIBVARIABLEMAGICPERL)_$(ARCH).deb
$(LIBVARIABLEMAGICPERL): $(SPREZZ)/libvariable-magic-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Variable-Magic-$(libvariable-magic-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: liberror-perl
liberror-perl:$(LIBERRORPERL)_$(ARCH).deb
$(LIBERRORPERL): $(SPREZZ)/liberror-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Error-$(liberror-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtext-unidecode-perl
libtext-unidecode-perl:$(LIBTEXTUNIDECODEPERL)_$(ARCH).deb
$(LIBTEXTUNIDECODEPERL): $(SPREZZ)/libtext-unidecode-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Text-Unidecode-$(libtext-unidecode-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libintl-perl
libintl-perl:$(LIBINTLPERL)_$(ARCH).deb
$(LIBINTLPERL): $(SPREZZ)/libintl-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libintl-perl-$(libintl-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libscalar-number-perl
libscalar-number-perl:$(LIBSCALARNUMBERPERL)_$(ARCH).deb
$(LIBSCALARNUMBERPERL): $(SPREZZ)/libscalar-number-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Scalar-Number-$(libscalar-number-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgd-text-perl
libgd-text-perl:$(LIBGDTEXTPERL)_$(ARCH).deb
$(LIBGDTEXTPERL): $(SPREZZ)/libgd-text-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf GDTextUtil-$(libgd-text-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgd-graph-perl
libgd-graph-perl:$(LIBGDGRAPHPERL)_$(ARCH).deb
$(LIBGDGRAPHPERL): $(SPREZZ)/libgd-graph-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf GDGraph-$(libgd-graph-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libio-socket-ip-perl
libio-socket-ip-perl:$(LIBIOSOCKETIPPERL)_$(ARCH).deb
$(LIBIOSOCKETIPPERL): $(SPREZZ)/libio-socket-ip-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf IO-Socket-IP-$(libio-socket-ip-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: liburi-perl
liburi-perl:$(LIBURIPERL)_$(ARCH).deb
$(LIBURIPERL): $(SPREZZ)/liburi-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf URI-$(liburi-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libio-socket-ssl-perl
libio-socket-ssl-perl:$(LIBIOSOCKETSSLPERL)_$(ARCH).deb
$(LIBIOSOCKETSSLPERL): $(SPREZZ)/libio-socket-ssl-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf IO-Socket-SSL-$(libio-socket-ssl-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnet-libidn-perl
libnet-libidn-perl:$(LIBNETLIBIDNPERL)_$(ARCH).deb
$(LIBNETLIBIDNPERL): $(SPREZZ)/libnet-libidn-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Net-LibIDN-$(libnet-libidn-perl_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@

.PHONY: libtie-ixhash-perl
libtie-ixhash-perl:$(LIBTIEIXHASHPERL)_$(ARCH).deb
$(LIBTIEIXHASHPERL): $(SPREZZ)/libtie-ixhash-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Tie-IxHash-$(libtie-ixhash-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdatetime-timezone-perl
libdatetime-timezone-perl:$(LIBDATETIMETIMEZONEPERL)_$(ARCH).deb
$(LIBDATETIMETIMEZONEPERL): $(SPREZZ)/libdatetime-timezone-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf DateTime-TimeZone-$(libdatetime-timezone-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtest-output-perl
libtest-output-perl:$(LIBTESTOUTPUTPERL)_$(ARCH).deb
$(LIBTESTOUTPUTPERL): $(SPREZZ)/libtest-output-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Test-Output-$(libtest-output-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsub-exporter-perl
libsub-exporter-perl:$(LIBSUBEXPORTERPERL)_$(ARCH).deb
$(LIBSUBEXPORTERPERL): $(SPREZZ)/libsub-exporter-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Sub-Exporter-$(libsub-exporter-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtest-tester-perl
libtest-tester-perl:$(LIBTESTTESTERPERL)_$(ARCH).deb
$(LIBTESTTESTERPERL): $(SPREZZ)/libtest-tester-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Test-Tester-$(libtest-tester-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libroman-perl
libroman-perl:$(LIBROMANPERL)_$(ARCH).deb
$(LIBROMANPERL): $(SPREZZ)/libroman-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libroman-perl_$(libroman-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtext-format-perl
libtext-format-perl:$(LIBTEXTFORMATPERL)_$(ARCH).deb
$(LIBTEXTFORMATPERL): $(SPREZZ)/libtext-format-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Text-Format-$(libtext-format-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsgmls-perl
libsgmls-perl:$(LIBSGMLSPERL)_$(ARCH).deb
$(LIBSGMLSPERL): $(SPREZZ)/libsgmls-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SGMLSpm-$(libsgmls-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libclass-c3-xs-perl
libclass-c3-xs-perl:$(LIBCLASSC3XSPERL)_$(ARCH).deb
$(LIBCLASSC3XSPERL): $(SPREZZ)/libclass-c3-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Class-C3-XS-$(libclass-c3-xs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdate-calc-xs-perl
libdate-calc-xs-perl:$(LIBDATECALCXSPERL)_$(ARCH).deb
$(LIBDATECALCXSPERL): $(SPREZZ)/libdate-calc-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Date-Calc-XS-$(libdate-calc-xs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: perl-tk
perl-tk:$(PERLTK)_$(ARCH).deb
$(PERLTK): $(SPREZZ)/perl-tk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Tk-$(perl-tk_UPVER).tar.gz $(TARARGS) $@

.PHONY: libfile-pushd-perl
libfile-pushd-perl:$(LIBFILEPUSHDPERL)_$(ARCH).deb
$(LIBFILEPUSHDPERL): $(SPREZZ)/libfile-pushd-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libfile-pushd-perl_$(libfile-pushd-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libterm-readline-gnu-perl
libterm-readline-gnu-perl:$(LIBTERMREADLINEGNUPERL)_$(ARCH).deb
$(LIBTERMREADLINEGNUPERL): $(SPREZZ)/libterm-readline-gnu-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Term-ReadLine-Gnu-$(libterm-readline-gnu-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libunicode-string-perl
libunicode-string-perl:$(LIBUNICODESTRINGPERL)_$(ARCH).deb
$(LIBUNICODESTRINGPERL): $(SPREZZ)/libunicode-string-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libunicode-string-perl_$(libunicode-string-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libany-moose-perl
libany-moose-perl:$(LIBANYMOOSEPERL)_$(ARCH).deb
$(LIBANYMOOSEPERL): $(SPREZZ)/libany-moose-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libany-moose-perl_$(libany-moose-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libencode-hanextra-perl
libencode-hanextra-perl:$(LIBENCODEHANEXTRAPERL)_$(ARCH).deb
$(LIBENCODEHANEXTRAPERL): $(SPREZZ)/libencode-hanextra-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libencode-hanextra-perl_$(libencode-hanextra-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libencode-jis2k-perl
libencode-jis2k-perl:$(LIBENCODEJIS2KPERL)_$(ARCH).deb
$(LIBENCODEJIS2KPERL): $(SPREZZ)/libencode-jis2k-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libencode-jis2k-perl_$(libencode-jis2k-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libhtml-tidy-perl
libhtml-tidy-perl:$(LIBHTMLTIDYPERL)_$(ARCH).deb
$(LIBHTMLTIDYPERL): $(SPREZZ)/libhtml-tidy-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libhtml-tidy-perl_$(libhtml-tidy-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libsgml-parser-opensp-perl
libsgml-parser-opensp-perl:$(LIBSGMLPARSEROPENSPPERL)_$(ARCH).deb
$(LIBSGMLPARSEROPENSPPERL): $(SPREZZ)/libsgml-parser-opensp-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsgml-parser-opensp-perl_$(libsgml-parser-opensp-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libmath-random-isaac-xs-perl
libmath-random-isaac-xs-perl:$(LIBMATHRANDOMISAACXSPERL)_$(ARCH).deb
$(LIBMATHRANDOMISAACXSPERL): $(SPREZZ)/libmath-random-isaac-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmath-random-isaac-xs-perl_$(libmath-random-isaac-xs-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libclass-xsaccessor-perl
libclass-xsaccessor-perl:$(LIBCLASSXSACCESSORPERL)_$(ARCH).deb
$(LIBCLASSXSACCESSORPERL): $(SPREZZ)/libclass-xsaccessor-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libclass-xsaccessor-perl_$(libclass-xsaccessor-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libhtml-strip-perl
libhtml-strip-perl:$(LIBHTMLSTRIPPERL)_$(ARCH).deb
$(LIBHTMLSTRIPPERL): $(SPREZZ)/libhtml-strip-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libhtml-strip-perl_$(libhtml-strip-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libdevel-caller-perl
libdevel-caller-perl:$(LIBDEVELCALLERPERL)_$(ARCH).deb
$(LIBDEVELCALLERPERL): $(SPREZZ)/libdevel-caller-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libdevel-caller-perl_$(libdevel-caller-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libmojolicious-perl
libmojolicious-perl:$(LIBMOJOLICIOUSPERL)_$(ARCH).deb
$(LIBMOJOLICIOUSPERL): $(SPREZZ)/libmojolicious-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmojolicious-perl_$(libmojolicious-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtest-warn-perl
libtest-warn-perl:$(LIBTESTWARNPERL)_$(ARCH).deb
$(LIBTESTWARNPERL): $(SPREZZ)/libtest-warn-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtest-warn-perl_$(libtest-warn-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libcompress-raw-lzma-perl
libcompress-raw-lzma-perl:$(LIBCOMPRESSRAWLZMAPERL)_$(ARCH).deb
$(LIBCOMPRESSRAWLZMAPERL): $(SPREZZ)/libcompress-raw-lzma-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libcompress-raw-lzma-perl_$(libcompress-raw-lzma-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libsereal-decoder-perl
libsereal-decoder-perl:$(LIBSEREALDECODERPERL)_$(ARCH).deb
$(LIBSEREALDECODERPERL): $(SPREZZ)/libsereal-decoder-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsereal-decoder-perl_$(libsereal-decoder-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libyaml-tiny-perl
libyaml-tiny-perl:$(LIBYAMLTINYPERL)_$(ARCH).deb
$(LIBYAMLTINYPERL): $(SPREZZ)/libyaml-tiny-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libyaml-tiny-perl_$(libyaml-tiny-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtest-pod-perl
libtest-pod-perl:$(LIBTESTPODPERL)_$(ARCH).deb
$(LIBTESTPODPERL): $(SPREZZ)/libtest-pod-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtest-pod-perl_$(libtest-pod-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libclass-inspector-perl
libclass-inspector-perl:$(LIBCLASSINSPECTORPERL)_$(ARCH).deb
$(LIBCLASSINSPECTORPERL): $(SPREZZ)/libclass-inspector-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libclass-inspector-perl_$(libclass-inspector-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxml-compile-perl
libxml-compile-perl:$(LIBXMLCOMPILEPERL)_$(ARCH).deb
$(LIBXMLCOMPILEPERL): $(SPREZZ)/libxml-compile-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxml-compile-perl_$(libxml-compile-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libclass-methodmaker-perl
libclass-methodmaker-perl:$(LIBCLASSMETHODMAKERPERL)_$(ARCH).deb
$(LIBCLASSMETHODMAKERPERL): $(SPREZZ)/libclass-methodmaker-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libclass-methodmaker-perl_$(libclass-methodmaker-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtext-bidi-perl
libtext-bidi-perl:$(LIBTEXTBIDIPERL)_$(ARCH).deb
$(LIBTEXTBIDIPERL): $(SPREZZ)/libtext-bidi-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtext-bidi-perl_$(libtext-bidi-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtext-csv-xs-perl
libtext-csv-xs-perl:$(LIBTEXTCSVXSPERL)_$(ARCH).deb
$(LIBTEXTCSVXSPERL): $(SPREZZ)/libtext-csv-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtext-csv-xs-perl_$(libtext-csv-xs-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libmidi-perl
libmidi-perl:$(LIBMIDIPERL)_$(ARCH).deb
$(LIBMIDIPERL): $(SPREZZ)/libmidi-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmidi-perl_$(libmidi-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libnet-ipaddress-perl
libnet-ipaddress-perl:$(LIBNETIPADDRESSPERL)_$(ARCH).deb
$(LIBNETIPADDRESSPERL): $(SPREZZ)/libnet-ipaddress-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libnet-ipaddress-perl_$(libnet-ipaddress-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libnet-idn-nameprep-perl
libnet-idn-nameprep-perl:$(LIBNETIDNNAMEPREPPERL)_$(ARCH).deb
$(LIBNETIDNNAMEPREPPERL): $(SPREZZ)/libnet-idn-nameprep-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libnet-idn-nameprep-perl_$(libnet-idn-nameprep-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libshell-perl
libshell-perl:$(LIBSHELLPERL)_$(ARCH).deb
$(LIBSHELLPERL): $(SPREZZ)/libshell-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libshell-perl_$(libshell-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libio-socket-inet6-perl
libio-socket-inet6-perl:$(LIBIOSOCKETINET6PERL)_$(ARCH).deb
$(LIBIOSOCKETINET6PERL): $(SPREZZ)/libio-socket-inet6-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libio-socket-inet6-perl_$(libio-socket-inet6-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libstring-approx-perl
libstring-approx-perl:$(LIBSTRINGAPPROXPERL)_$(ARCH).deb
$(LIBSTRINGAPPROXPERL): $(SPREZZ)/libstring-approx-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libstring-approx-perl_$(libstring-approx-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: liblingua-sentence-perl
liblingua-sentence-perl:$(LIBLINGUASENTENCEPERL)_$(ARCH).deb
$(LIBLINGUASENTENCEPERL): $(SPREZZ)/liblingua-sentence-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liblingua-sentence-perl_$(liblingua-sentence-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxml-dom-perl
libxml-dom-perl:$(LIBXMLDOMPERL)_$(ARCH).deb
$(LIBXMLDOMPERL): $(SPREZZ)/libxml-dom-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxml-dom-perl_$(libxml-dom-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxml-perl
libxml-perl:$(LIBXMLPERL)_$(ARCH).deb
$(LIBXMLPERL): $(SPREZZ)/libxml-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxml-perl_$(libxml-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxml-regexp-perl
libxml-regexp-perl:$(LIBXMLREGEXPPERL)_$(ARCH).deb
$(LIBXMLREGEXPPERL): $(SPREZZ)/libxml-regexp-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxml-regexp-perl_$(libxml-regexp-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libstring-diff-perl
libstring-diff-perl:$(LIBSTRINGDIFFPERL)_$(ARCH).deb
$(LIBSTRINGDIFFPERL): $(SPREZZ)/libstring-diff-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libstring-diff-perl_$(libstring-diff-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libyaml-perl
libyaml-perl:$(LIBYAMLPERL)_$(ARCH).deb
$(LIBYAMLPERL): $(SPREZZ)/libyaml-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libyaml-perl_$(libyaml-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libyaml-shell-perl
libyaml-shell-perl:$(LIBYAMLSHELLPERL)_$(ARCH).deb
$(LIBYAMLSHELLPERL): $(SPREZZ)/libyaml-shell-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libyaml-shell-perl_$(libyaml-shell-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtest-base-perl
libtest-base-perl:$(LIBTESTBASEPERL)_$(ARCH).deb
$(LIBTESTBASEPERL): $(SPREZZ)/libtest-base-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtest-base-perl_$(libtest-base-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libextutils-pkgconfig-perl
libextutils-pkgconfig-perl:$(LIBEXTUTILSPKGCONFIGPERL)_$(ARCH).deb
$(LIBEXTUTILSPKGCONFIGPERL): $(SPREZZ)/libextutils-pkgconfig-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libextutils-pkgconfig-perl_$(libextutils-pkgconfig-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libconfig-file-perl
libconfig-file-perl:$(LIBCONFIGFILEPERL)_$(ARCH).deb
$(LIBCONFIGFILEPERL): $(SPREZZ)/libconfig-file-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libconfig-file-perl_$(libconfig-file-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libregexp-assemble-perl
libregexp-assemble-perl:$(LIBREGEXPASSEMBLEPERL)_$(ARCH).deb
$(LIBREGEXPASSEMBLEPERL): $(SPREZZ)/libregexp-assemble-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libregexp-assemble-perl_$(libregexp-assemble-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtest-pod-coverage-perl
libtest-pod-coverage-perl:$(LIBTESTPODCOVERAGEPERL)_$(ARCH).deb
$(LIBTESTPODCOVERAGEPERL): $(SPREZZ)/libtest-pod-coverage-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtest-pod-coverage-perl_$(libtest-pod-coverage-perl_UPVER).orig.tar.gz $(TARARGS) $@

