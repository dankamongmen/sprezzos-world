.PHONY: findlib
findlib:$(FINDLIB)_$(ARCH).deb
$(FINDLIB): $(SPREZZ)/findlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf findlib-$(findlib_UPVER).tar.gz $(TARARGS) $@

.PHONY: ocaml-ogg
ocaml-ogg:$(OCAMLOGG)_$(ARCH).deb
$(OCAMLOGG): $(SPREZZ)/ocaml-ogg/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-ogg-$(ocaml-ogg_UPVER).tar.gz $(TARARGS) $@

