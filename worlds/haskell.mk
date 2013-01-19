.PHONY: ghc
ghc:$(GHC)_$(ARCH).deb
$(GHC): $(SPREZZ)/ghc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghc-$(ghc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-ansi-terminal
haskell-ansi-terminal:$(HASKELLANSITERMINAL)_$(ARCH).deb
$(HASKELLANSITERMINAL): $(SPREZZ)/haskell-ansi-terminal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ansi-terminal-$(haskell-ansi-terminal_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-ansi-wl-pprint
haskell-ansi-wl-pprint:$(HASKELLANSIWLPPRINT)_$(ARCH).deb
$(HASKELLANSIWLPPRINT): $(SPREZZ)/haskell-ansi-wl-pprint/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ansi-wl-pprint-$(haskell-ansi-wl-pprint_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-base-unicode-symbols
haskell-base-unicode-symbols:$(HASKELLBASEUNICODESYMBOLS)_$(ARCH).deb
$(HASKELLBASEUNICODESYMBOLS): $(SPREZZ)/haskell-base-unicode-symbols/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf base-unicode-symbols-$(haskell-base-unicode-symbols_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-conduit
haskell-conduit:$(HASKELLCONDUIT)_$(ARCH).deb
$(HASKELLCONDUIT): $(SPREZZ)/haskell-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf conduit-$(haskell-conduit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-edit-distance
haskell-edit-distance:$(HASKELLEDITDISTANCE)_$(ARCH).deb
$(HASKELLEDITDISTANCE): $(SPREZZ)/haskell-edit-distance/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf edit-distance-$(haskell-edit-distance_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hamlet
haskell-hamlet:$(HASKELLHAMLET)_$(ARCH).deb
$(HASKELLHAMLET): $(SPREZZ)/haskell-hamlet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hamlet-$(haskell-hamlet_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hostname
haskell-hostname:$(HASKELLHOSTNAME)_$(ARCH).deb
$(HASKELLHOSTNAME): $(SPREZZ)/haskell-hostname/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hostname-$(haskell-hostname_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hunit
haskell-hunit:$(HASKELLHUNIT)_$(ARCH).deb
$(HASKELLHUNIT): $(SPREZZ)/haskell-hunit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HUnit-$(haskell-hunit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-json
haskell-json:$(HASKELLJSON)_$(ARCH).deb
$(HASKELLJSON): $(SPREZZ)/haskell-json/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf json-$(haskell-json_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-lifted-base
haskell-lifted-base:$(HASKELLLIFTEDBASE)_$(ARCH).deb
$(HASKELLLIFTEDBASE): $(SPREZZ)/haskell-lifted-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lifted-base-$(haskell-lifted-base_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-monad-control
haskell-monad-control:$(HASKELLMONADCONTROL)_$(ARCH).deb
$(HASKELLMONADCONTROL): $(SPREZZ)/haskell-monad-control/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monad-control-$(haskell-monad-control_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-mtl
haskell-mtl:$(HASKELLMTL)_$(ARCH).deb
$(HASKELLMTL): $(SPREZZ)/haskell-mtl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mtl-$(haskell-mtl_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-parsec
haskell-parsec:$(HASKELLPARSEC)_$(ARCH).deb
$(HASKELLPARSEC): $(SPREZZ)/haskell-parsec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf parsec-$(haskell-parsec_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-quickcheck
haskell-quickcheck:$(HASKELLQUICKCHECK)_$(ARCH).deb
$(HASKELLQUICKCHECK): $(SPREZZ)/haskell-quickcheck/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf QuickCheck-$(haskell-quickcheck_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-random
haskell-random:$(HASKELLRANDOM)_$(ARCH).deb
$(HASKELLRANDOM): $(SPREZZ)/haskell-random/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf random-$(haskell-random_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-resourcet
haskell-resourcet:$(HASKELLRESOURCET)_$(ARCH).deb
$(HASKELLRESOURCET): $(SPREZZ)/haskell-resourcet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf resourcet-$(haskell-resourcet_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-semigroups
haskell-semigroups:$(HASKELLSEMIGROUPS)_$(ARCH).deb
$(HASKELLSEMIGROUPS): $(SPREZZ)/haskell-semigroups/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf semigroups-$(haskell-semigroups_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-shakespeare
haskell-shakespeare:$(HASKELLSHAKESPEARE)_$(ARCH).deb
$(HASKELLSHAKESPEARE): $(SPREZZ)/haskell-shakespeare/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shakespeare-$(haskell-shakespeare_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-syb
haskell-syb:$(HASKELLSYB)_$(ARCH).deb
$(HASKELLSYB): $(SPREZZ)/haskell-syb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf syb-$(haskell-syb_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework
haskell-test-framework:$(HASKELLTESTFRAMEWORK)_$(ARCH).deb
$(HASKELLTESTFRAMEWORK): $(SPREZZ)/haskell-test-framework/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-$(haskell-test-framework_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-testpack
haskell-testpack:$(HASKELLTESTPACK)_$(ARCH).deb
$(HASKELLTESTPACK): $(SPREZZ)/haskell-testpack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf testpack-$(haskell-testpack_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-text
haskell-text:$(HASKELLTEXT)_$(ARCH).deb
$(HASKELLTEXT): $(SPREZZ)/haskell-text/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf text-$(haskell-text_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-transformers
haskell-transformers:$(HASKELLTRANSFORMERS)_$(ARCH).deb
$(HASKELLTRANSFORMERS): $(SPREZZ)/haskell-transformers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf transformers-$(haskell-transformers_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-transformers-base
haskell-transformers-base:$(HASKELLTRANSFORMERSBASE)_$(ARCH).deb
$(HASKELLTRANSFORMERSBASE): $(SPREZZ)/haskell-transformers-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf transformers-base-$(haskell-transformers-base_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-void
haskell-void:$(HASKELLVOID)_$(ARCH).deb
$(HASKELLVOID): $(SPREZZ)/haskell-void/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf void-$(haskell-void_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml-conduit
haskell-xml-conduit:$(HASKELLXMLCONDUIT)_$(ARCH).deb
$(HASKELLXMLCONDUIT): $(SPREZZ)/haskell-xml-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml-conduit-$(haskell-xml-conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml-hamlet
haskell-xml-hamlet:$(HASKELLXMLHAMLET)_$(ARCH).deb
$(HASKELLXMLHAMLET): $(SPREZZ)/haskell-xml-hamlet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml-hamlet-$(haskell-xml-hamlet_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml-types
haskell-xml-types:$(HASKELLXMLTYPES)_$(ARCH).deb
$(HASKELLXMLTYPES): $(SPREZZ)/haskell-xml-types/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml-types-$(haskell-xml-types_UPVER).tar.gz $(TARARGS) $@
