.PHONY: cpphs
cpphs:$(CPPHS)_$(ARCH).deb
$(CPPHS): $(SPREZZ)/cpphs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cpphs-$(cpphs_UPVER).tar.gz $(TARARGS) $@

.PHONY: ghc
ghc:$(GHC)_$(ARCH).deb
$(GHC): $(SPREZZ)/ghc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghc-$(ghc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-aes
haskell-aes:$(HASKELLAES)_$(ARCH).deb
$(HASKELLAES): $(SPREZZ)/haskell-aes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cipher-aes-$(haskell-aes_UPVER).tar.gz $(TARARGS) $@
	
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
	
.PHONY: haskell-asn1-data
haskell-asn1-data:$(HASKELLASN1DATA)_$(ARCH).deb
$(HASKELLASN1DATA): $(SPREZZ)/haskell-asn1-data/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf asn1-data-$(haskell-asn1-data_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-attoparsec
haskell-attoparsec:$(HASKELLATTOPARSEC)_$(ARCH).deb
$(HASKELLATTOPARSEC): $(SPREZZ)/haskell-attoparsec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf attoparsec-$(haskell-attoparsec_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-attoparsec-conduit
haskell-attoparsec-conduit:$(HASKELLATTOPARSECCONDUIT)_$(ARCH).deb
$(HASKELLATTOPARSECCONDUIT): $(SPREZZ)/haskell-attoparsec-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf attoparsec-conduit-$(haskell-attoparsec-conduit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-attoparsec-enumerator
haskell-attoparsec-enumerator:$(HASKELLATTOPARSECENUMERATOR)_$(ARCH).deb
$(HASKELLATTOPARSECENUMERATOR): $(SPREZZ)/haskell-attoparsec-enumerator/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf attoparsec-enumerator-$(haskell-attoparsec-enumerator_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-base-unicode-symbols
haskell-base-unicode-symbols:$(HASKELLBASEUNICODESYMBOLS)_$(ARCH).deb
$(HASKELLBASEUNICODESYMBOLS): $(SPREZZ)/haskell-base-unicode-symbols/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf base-unicode-symbols-$(haskell-base-unicode-symbols_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-base64-bytestring
haskell-base64-bytestring:$(HASKELLBASE64BYTESTRING)_$(ARCH).deb
$(HASKELLBASE64BYTESTRING): $(SPREZZ)/haskell-base64-bytestring/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf base64-bytestring-$(haskell-base64-bytestring_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-blaze-builder
haskell-blaze-builder:$(HASKELLBLAZEBUILDER)_$(ARCH).deb
$(HASKELLBLAZEBUILDER): $(SPREZZ)/haskell-blaze-builder/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-builder-$(haskell-blaze-builder_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-blaze-html
haskell-blaze-html:$(HASKELLBLAZEHTML)_$(ARCH).deb
$(HASKELLBLAZEHTML): $(SPREZZ)/haskell-blaze-html/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-html-$(haskell-blaze-html_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-blaze-markup
haskell-blaze-markup:$(HASKELLBLAZEMARKUP)_$(ARCH).deb
$(HASKELLBLAZEMARKUP): $(SPREZZ)/haskell-blaze-markup/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blaze-markup-$(haskell-blaze-markup_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-case-insensitive
haskell-case-insensitive:$(HASKELLCASEINSENSITIVE)_$(ARCH).deb
$(HASKELLCASEINSENSITIVE): $(SPREZZ)/haskell-case-insensitive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf case-insensitive-$(haskell-case-insensitive_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cereal
haskell-cereal:$(HASKELLCEREAL)_$(ARCH).deb
$(HASKELLCEREAL): $(SPREZZ)/haskell-cereal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cereal-$(haskell-cereal_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-certificate
haskell-certificate:$(HASKELLCERTIFICATE)_$(ARCH).deb
$(HASKELLCERTIFICATE): $(SPREZZ)/haskell-certificate/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf certificate-$(haskell-certificate_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-clientsession
haskell-clientsession:$(HASKELLCLIENTSESSION)_$(ARCH).deb
$(HASKELLCLIENTSESSION): $(SPREZZ)/haskell-clientsession/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf clientsession-$(haskell-clientsession_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-conduit
haskell-conduit:$(HASKELLCONDUIT)_$(ARCH).deb
$(HASKELLCONDUIT): $(SPREZZ)/haskell-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf conduit-$(haskell-conduit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cookie
haskell-cookie:$(HASKELLCOOKIE)_$(ARCH).deb
$(HASKELLCOOKIE): $(SPREZZ)/haskell-cookie/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cookie-$(haskell-cookie_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cprng-aes
haskell-cprng-aes:$(HASKELLCPRNGAES)_$(ARCH).deb
$(HASKELLCPRNGAES): $(SPREZZ)/haskell-cprng-aes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cprng-aes-$(haskell-cprng-aes_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto
haskell-crypto:$(HASKELLCRYPTO)_$(ARCH).deb
$(HASKELLCRYPTO): $(SPREZZ)/haskell-crypto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Crypto-$(haskell-crypto_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cpu
haskell-cpu:$(HASKELLCPU)_$(ARCH).deb
$(HASKELLCPU): $(SPREZZ)/haskell-cpu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cpu-$(haskell-cpu_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cryptocipher
haskell-cryptocipher:$(HASKELLCRYPTOCIPHER)_$(ARCH).deb
$(HASKELLCRYPTOCIPHER): $(SPREZZ)/haskell-cryptocipher/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cryptocipher-$(haskell-cryptocipher_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-cryptohash
haskell-cryptohash:$(HASKELLCRYPTOHASH)_$(ARCH).deb
$(HASKELLCRYPTOHASH): $(SPREZZ)/haskell-cryptohash/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cryptohash-$(haskell-cryptohash_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-api
haskell-crypto-api:$(HASKELLCRYPTOAPI)_$(ARCH).deb
$(HASKELLCRYPTOAPI): $(SPREZZ)/haskell-crypto-api/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-api-$(haskell-crypto-api_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-numbers
haskell-crypto-numbers:$(HASKELLCRYPTONUMBERS)_$(ARCH).deb
$(HASKELLCRYPTONUMBERS): $(SPREZZ)/haskell-crypto-numbers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-numbers-$(haskell-crypto-numbers_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-pubkey
haskell-crypto-pubkey:$(HASKELLCRYPTOPUBKEY)_$(ARCH).deb
$(HASKELLCRYPTOPUBKEY): $(SPREZZ)/haskell-crypto-pubkey/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-pubkey-$(haskell-crypto-pubkey_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-pubkey-types
haskell-crypto-pubkey-types:$(HASKELLCRYPTOPUBKEYTYPES)_$(ARCH).deb
$(HASKELLCRYPTOPUBKEYTYPES): $(SPREZZ)/haskell-crypto-pubkey-types/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-pubkey-types-$(haskell-crypto-pubkey-types_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-crypto-random-api
haskell-crypto-random-api:$(HASKELLCRYPTORANDOMAPI)_$(ARCH).deb
$(HASKELLCRYPTORANDOMAPI): $(SPREZZ)/haskell-crypto-random-api/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf crypto-random-api-$(haskell-crypto-random-api_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-data-default
haskell-data-default:$(HASKELLDATADEFAULT)_$(ARCH).deb
$(HASKELLDATADEFAULT): $(SPREZZ)/haskell-data-default/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf data-default-$(haskell-data-default_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-dataenc
haskell-dataenc:$(HASKELLDATAENC)_$(ARCH).deb
$(HASKELLDATAENC): $(SPREZZ)/haskell-dataenc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dataenc-$(haskell-dataenc_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-dlist
haskell-dlist:$(HASKELLDLIST)_$(ARCH).deb
$(HASKELLDLIST): $(SPREZZ)/haskell-dlist/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dlist-$(haskell-dlist_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-edit-distance
haskell-edit-distance:$(HASKELLEDITDISTANCE)_$(ARCH).deb
$(HASKELLEDITDISTANCE): $(SPREZZ)/haskell-edit-distance/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf edit-distance-$(haskell-edit-distance_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-entropy
haskell-entropy:$(HASKELLENTROPY)_$(ARCH).deb
$(HASKELLENTROPY): $(SPREZZ)/haskell-entropy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf entropy-$(haskell-entropy_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-enumerator
haskell-enumerator:$(HASKELLENUMERATOR)_$(ARCH).deb
$(HASKELLENUMERATOR): $(SPREZZ)/haskell-enumerator/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf enumerator-$(haskell-enumerator_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-extensible-exceptions
haskell-extensible-exceptions:$(HASKELLEXTENSIBLEEXCEPTIONS)_$(ARCH).deb
$(HASKELLEXTENSIBLEEXCEPTIONS): $(SPREZZ)/haskell-extensible-exceptions/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf extensible-exceptions-$(haskell-extensible-exceptions_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-failure
haskell-failure:$(HASKELLFAILURE)_$(ARCH).deb
$(HASKELLFAILURE): $(SPREZZ)/haskell-failure/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf failure-$(haskell-failure_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-fast-logger
haskell-fast-logger:$(HASKELLFASTLOGGER)_$(ARCH).deb
$(HASKELLFASTLOGGER): $(SPREZZ)/haskell-fast-logger/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fast-logger-$(haskell-fast-logger_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-gsasl
haskell-gsasl:$(HASKELLGSASL)_$(ARCH).deb
$(HASKELLGSASL): $(SPREZZ)/haskell-gsasl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gsasl-$(haskell-gsasl_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hamlet
haskell-hamlet:$(HASKELLHAMLET)_$(ARCH).deb
$(HASKELLHAMLET): $(SPREZZ)/haskell-hamlet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hamlet-$(haskell-hamlet_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hashable
haskell-hashable:$(HASKELLHASHABLE)_$(ARCH).deb
$(HASKELLHASHABLE): $(SPREZZ)/haskell-hashable/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hashable-$(haskell-hashable_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hinotify
haskell-hinotify:$(HASKELLHINOTIFY)_$(ARCH).deb
$(HASKELLHINOTIFY): $(SPREZZ)/haskell-hinotify/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hinotify-$(haskell-hinotify_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hostname
haskell-hostname:$(HASKELLHOSTNAME)_$(ARCH).deb
$(HASKELLHOSTNAME): $(SPREZZ)/haskell-hostname/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hostname-$(haskell-hostname_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hsh
haskell-hsh:$(HASKELLHSH)_$(ARCH).deb
$(HASKELLHSH): $(SPREZZ)/haskell-hsh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HSH-$(haskell-hsh_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hslogger
haskell-hslogger:$(HASKELLHSLOGGER)_$(ARCH).deb
$(HASKELLHSLOGGER): $(SPREZZ)/haskell-hslogger/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hslogger-$(haskell-hslogger_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hslogger
haskell-hspec:$(HASKELLHSPEC)_$(ARCH).deb
$(HASKELLHSPEC): $(SPREZZ)/haskell-hspec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hspec-$(haskell-hspec_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hspec-expectations
haskell-hspec-expectations:$(HASKELLHSPECEXPECTATIONS)_$(ARCH).deb
$(HASKELLHSPECEXPECTATIONS): $(SPREZZ)/haskell-hspec-expectations/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hspec-expectations-$(haskell-hspec-expectations_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hunit
haskell-hunit:$(HASKELLHUNIT)_$(ARCH).deb
$(HASKELLHUNIT): $(SPREZZ)/haskell-hunit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HUnit-$(haskell-hunit_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-hxt
haskell-hxt:$(HASKELLHXT)_$(ARCH).deb
$(HASKELLHXT): $(SPREZZ)/haskell-hxt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hxt-$(haskell-hxt_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-json
haskell-json:$(HASKELLJSON)_$(ARCH).deb
$(HASKELLJSON): $(SPREZZ)/haskell-json/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf json-$(haskell-json_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-largeword
haskell-largeword:$(HASKELLLARGEWORD)_$(ARCH).deb
$(HASKELLLARGEWORD): $(SPREZZ)/haskell-largeword/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf largeword-$(haskell-largeword_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-lens
haskell-lens:$(HASKELLLENS)_$(ARCH).deb
$(HASKELLLENS): $(SPREZZ)/haskell-lens/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lens-$(haskell-lens_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-libxml-sax
haskell-libxml-sax:$(HASKELLLIBXMLSAX)_$(ARCH).deb
$(HASKELLLIBXMLSAX): $(SPREZZ)/haskell-libxml-sax/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxml-sax-$(haskell-libxml-sax_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-lifted-base
haskell-lifted-base:$(HASKELLLIFTEDBASE)_$(ARCH).deb
$(HASKELLLIFTEDBASE): $(SPREZZ)/haskell-lifted-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lifted-base-$(haskell-lifted-base_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-missingh
haskell-missingh:$(HASKELLMISSINGH)_$(ARCH).deb
$(HASKELLMISSINGH): $(SPREZZ)/haskell-missingh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf MissingH-$(haskell-missingh_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-monad-control
haskell-monad-control:$(HASKELLMONADCONTROL)_$(ARCH).deb
$(HASKELLMONADCONTROL): $(SPREZZ)/haskell-monad-control/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monad-control-$(haskell-monad-control_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-monads-tf
haskell-monads-tf:$(HASKELLMONADSTF)_$(ARCH).deb
$(HASKELLMONADSTF): $(SPREZZ)/haskell-monads-tf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf monads-tf-$(haskell-monads-tf_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-mtl
haskell-mtl:$(HASKELLMTL)_$(ARCH).deb
$(HASKELLMTL): $(SPREZZ)/haskell-mtl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mtl-$(haskell-mtl_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-network
haskell-network:$(HASKELLNETWORK)_$(ARCH).deb
$(HASKELLNETWORK): $(SPREZZ)/haskell-network/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf network-$(haskell-network_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-network-protocol-xmpp
haskell-network-protocol-xmpp:$(HASKELLNETWORKPROTOCOLXMPP)_$(ARCH).deb
$(HASKELLNETWORKPROTOCOLXMPP): $(SPREZZ)/haskell-network-protocol-xmpp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf network-protocol-xmpp-$(haskell-network-protocol-xmpp_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-parallel
haskell-parallel:$(HASKELLPARALLEL)_$(ARCH).deb
$(HASKELLPARALLEL): $(SPREZZ)/haskell-parallel/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf parallel-$(haskell-parallel_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-parsec
haskell-parsec:$(HASKELLPARSEC)_$(ARCH).deb
$(HASKELLPARSEC): $(SPREZZ)/haskell-parsec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf parsec-$(haskell-parsec_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-pem
haskell-pem:$(HASKELLPEM)_$(ARCH).deb
$(HASKELLPEM): $(SPREZZ)/haskell-pem/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pem-$(haskell-pem_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-primitive
haskell-primitive:$(HASKELLPRIMITIVE)_$(ARCH).deb
$(HASKELLPRIMITIVE): $(SPREZZ)/haskell-primitive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf primitive-$(haskell-primitive_UPVER).tar.gz $(TARARGS) $@

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

.PHONY: haskell-rc4
haskell-rc4:$(HASKELLRC4)_$(ARCH).deb
$(HASKELLRC4): $(SPREZZ)/haskell-rc4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cipher-rc4-$(haskell-rc4_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: haskell-regex-base
haskell-regex-base:$(HASKELLREGEXBASE)_$(ARCH).deb
$(HASKELLREGEXBASE): $(SPREZZ)/haskell-regex-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-base-$(haskell-regex-base_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-regex-compat
haskell-regex-compat:$(HASKELLREGEXCOMPAT)_$(ARCH).deb
$(HASKELLREGEXCOMPAT): $(SPREZZ)/haskell-regex-compat/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-compat-$(haskell-regex-compat_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-regex-posix
haskell-regex-posix:$(HASKELLREGEXPOSIX)_$(ARCH).deb
$(HASKELLREGEXPOSIX): $(SPREZZ)/haskell-regex-posix/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf regex-posix-$(haskell-regex-posix_UPVER).tar.gz $(TARARGS) $@

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

.PHONY: haskell-sha
haskell-sha:$(HASKELLSHA)_$(ARCH).deb
$(HASKELLSHA): $(SPREZZ)/haskell-sha/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SHA-$(haskell-sha_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-shakespeare
haskell-shakespeare:$(HASKELLSHAKESPEARE)_$(ARCH).deb
$(HASKELLSHAKESPEARE): $(SPREZZ)/haskell-shakespeare/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shakespeare-$(haskell-shakespeare_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-silently
haskell-silently:$(HASKELLSILENTLY)_$(ARCH).deb
$(HASKELLSILENTLY): $(SPREZZ)/haskell-silently/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf silently-$(haskell-silently_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-skein
haskell-skein:$(HASKELLSKEIN)_$(ARCH).deb
$(HASKELLSKEIN): $(SPREZZ)/haskell-skein/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf skein-$(haskell-skein_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-socks
haskell-socks:$(HASKELLSOCKS)_$(ARCH).deb
$(HASKELLSOCKS): $(SPREZZ)/haskell-socks/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf socks-$(haskell-socks_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-stm
haskell-stm:$(HASKELLSTM)_$(ARCH).deb
$(HASKELLSTM): $(SPREZZ)/haskell-stm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf stm-$(haskell-stm_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-syb
haskell-syb:$(HASKELLSYB)_$(ARCH).deb
$(HASKELLSYB): $(SPREZZ)/haskell-syb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf syb-$(haskell-syb_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-system-filepath
haskell-system-filepath:$(HASKELLSYSTEMFILEPATH)_$(ARCH).deb
$(HASKELLSYSTEMFILEPATH): $(SPREZZ)/haskell-system-filepath/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf system-filepath-$(haskell-system-filepath_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tagged
haskell-tagged:$(HASKELLTAGGED)_$(ARCH).deb
$(HASKELLTAGGED): $(SPREZZ)/haskell-tagged/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tagged-$(haskell-tagged_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework
haskell-test-framework:$(HASKELLTESTFRAMEWORK)_$(ARCH).deb
$(HASKELLTESTFRAMEWORK): $(SPREZZ)/haskell-test-framework/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-$(haskell-test-framework_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework-hunit
haskell-test-framework-hunit:$(HASKELLTESTFRAMEWORKHUNIT)_$(ARCH).deb
$(HASKELLTESTFRAMEWORKHUNIT): $(SPREZZ)/haskell-test-framework-hunit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-hunit-$(haskell-test-framework-hunit_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-test-framework-quickcheck2
haskell-test-framework-quickcheck2:$(HASKELLTESTFRAMEWORKQUICKCHECK2)_$(ARCH).deb
$(HASKELLTESTFRAMEWORKQUICKCHECK2): $(SPREZZ)/haskell-test-framework-quickcheck2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf test-framework-quickcheck2-$(haskell-test-framework-quickcheck2_UPVER).tar.gz $(TARARGS) $@

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

.PHONY: haskell-tls
haskell-tls:$(HASKELLTLS)_$(ARCH).deb
$(HASKELLTLS): $(SPREZZ)/haskell-tls/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tls-$(haskell-tls_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-tls-extra
haskell-tls-extra:$(HASKELLTLSEXTRA)_$(ARCH).deb
$(HASKELLTLSEXTRA): $(SPREZZ)/haskell-tls-extra/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tls-extra-$(haskell-tls-extra_UPVER).tar.gz $(TARARGS) $@

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

.PHONY: haskell-unordered-containers
haskell-unordered-containers:$(HASKELLUNORDEREDCONTAINERS)_$(ARCH).deb
$(HASKELLUNORDEREDCONTAINERS): $(SPREZZ)/haskell-unordered-containers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unordered-containers-$(haskell-unordered-containers_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vault
haskell-vault:$(HASKELLVAULT)_$(ARCH).deb
$(HASKELLVAULT): $(SPREZZ)/haskell-vault/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vault-$(haskell-vault_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-vector
haskell-vector:$(HASKELLVECTOR)_$(ARCH).deb
$(HASKELLVECTOR): $(SPREZZ)/haskell-vector/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vector-$(haskell-vector_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-void
haskell-void:$(HASKELLVOID)_$(ARCH).deb
$(HASKELLVOID): $(SPREZZ)/haskell-void/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf void-$(haskell-void_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai
haskell-wai:$(HASKELLWAI)_$(ARCH).deb
$(HASKELLWAI): $(SPREZZ)/haskell-wai/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-$(haskell-wai_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-wai-logger
haskell-wai-logger:$(HASKELLWAILOGGER)_$(ARCH).deb
$(HASKELLWAILOGGER): $(SPREZZ)/haskell-wai-logger/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wai-logger-$(haskell-wai-logger_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-xml
haskell-xml:$(HASKELLXML)_$(ARCH).deb
$(HASKELLXML): $(SPREZZ)/haskell-xml/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xml-$(haskell-xml_UPVER).tar.gz $(TARARGS) $@

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

.PHONY: haskell-zlib
haskell-zlib:$(HASKELLZLIB)_$(ARCH).deb
$(HASKELLZLIB): $(SPREZZ)/haskell-zlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zlib-$(haskell-zlib_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-zlib-bindings
haskell-zlib-bindings:$(HASKELLZLIBBINDINGS)_$(ARCH).deb
$(HASKELLZLIBBINDINGS): $(SPREZZ)/haskell-zlib-bindings/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zlib-bindings-$(haskell-zlib-bindings_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-zlib-conduit
haskell-zlib-conduit:$(HASKELLZLIBCONDUIT)_$(ARCH).deb
$(HASKELLZLIBCONDUIT): $(SPREZZ)/haskell-zlib-conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zlib-conduit-$(haskell-zlib-conduit_UPVER).tar.gz $(TARARGS) $@
