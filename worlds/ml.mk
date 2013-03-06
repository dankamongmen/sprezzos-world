.PHONY: findlib
findlib:$(FINDLIB)_$(ARCH).deb
$(FINDLIB): $(SPREZZ)/findlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf findlib-$(findlib_UPVER).tar.gz $(TARARGS) $@

