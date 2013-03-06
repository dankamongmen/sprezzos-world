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

.PHONY: ocamlviz
ocamlviz:$(OCAMLVIZ)_$(ARCH).deb
$(OCAMLVIZ): $(SPREZZ)/ocamlviz/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocamlviz-$(ocamlviz_UPVER).tar.gz $(TARARGS) $@

.PHONY: cairo-ocaml
cairo-ocaml:$(CAIROOCAML)_$(ARCH).deb
$(CAIROOCAML): $(SPREZZ)/cairo-ocaml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cairo-ocaml-$(cairo-ocaml_UPVER).tar.gz $(TARARGS) $@

