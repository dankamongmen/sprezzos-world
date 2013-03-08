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

.PHONY: mlpost
mlpost:$(MLPOST)_$(ARCH).deb
$(MLPOST): $(SPREZZ)/mlpost/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mlpost-$(mlpost_UPVER).tar.gz $(TARARGS) $@

.PHONY: lablgtk2
lablgtk2:$(LABLGTK2)_$(ARCH).deb
$(LABLGTK2): $(SPREZZ)/lablgtk2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lablgtk-$(lablgtk2_UPVER).tar.gz $(TARARGS) $@

.PHONY: lablgl
lablgl:$(LABLGL)_$(ARCH).deb
$(LABLGL): $(SPREZZ)/lablgl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lablgl-$(lablgl_UPVER).tar.gz $(TARARGS) $@

.PHONY: ocaml-bitstring
ocaml-bitstring:$(OCAMLBITSTRING)_$(ARCH).deb
$(OCAMLBITSTRING): $(SPREZZ)/ocaml-bitstring/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-bitstring-$(ocaml-bitstring_UPVER).tar.gz $(TARARGS) $@

.PHONY: ocaml-cry
ocaml-cry:$(OCAMLCRY)_$(ARCH).deb
$(OCAMLCRY): $(SPREZZ)/ocaml-cry/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-cry_$(ocaml-cry_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-vorbis
ocaml-vorbis:$(OCAMLVORBIS)_$(ARCH).deb
$(OCAMLVORBIS): $(SPREZZ)/ocaml-vorbis/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-vorbis_$(ocaml-vorbis_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-mad
ocaml-mad:$(OCAMLMAD)_$(ARCH).deb
$(OCAMLMAD): $(SPREZZ)/ocaml-mad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-mad_$(ocaml-mad_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-dtools
ocaml-dtools:$(OCAMLDTOOLS)_$(ARCH).deb
$(OCAMLDTOOLS): $(SPREZZ)/ocaml-dtools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-dtools_$(ocaml-dtools_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-taglib
ocaml-taglib:$(OCAMLTAGLIB)_$(ARCH).deb
$(OCAMLTAGLIB): $(SPREZZ)/ocaml-taglib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-taglib_$(ocaml-taglib_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-ao
ocaml-ao:$(OCAMLAO)_$(ARCH).deb
$(OCAMLAO): $(SPREZZ)/ocaml-ao/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-ao_$(ocaml-ao_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-duppy
ocaml-duppy:$(OCAMLDUPPY)_$(ARCH).deb
$(OCAMLDUPPY): $(SPREZZ)/ocaml-duppy/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-duppy_$(ocaml-duppy_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-alsa
ocaml-alsa:$(OCAMLALSA)_$(ARCH).deb
$(OCAMLALSA): $(SPREZZ)/ocaml-alsa/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-alsa_$(ocaml-alsa_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-ladspa
ocaml-ladspa:$(OCAMLLADSPA)_$(ARCH).deb
$(OCAMLLADSPA): $(SPREZZ)/ocaml-ladspa/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-ladspa_$(ocaml-ladspa_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-soundtouch
ocaml-soundtouch:$(OCAMLSOUNDTOUCH)_$(ARCH).deb
$(OCAMLSOUNDTOUCH): $(SPREZZ)/ocaml-soundtouch/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-soundtouch_$(ocaml-soundtouch_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-lastfm
ocaml-lastfm:$(OCAMLLASTFM)_$(ARCH).deb
$(OCAMLLASTFM): $(SPREZZ)/ocaml-lastfm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-lastfm_$(ocaml-lastfm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-speex
ocaml-speex:$(OCAMLSPEEX)_$(ARCH).deb
$(OCAMLSPEEX): $(SPREZZ)/ocaml-speex/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-speex_$(ocaml-speex_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-gavl
ocaml-gavl:$(OCAMLGAVL)_$(ARCH).deb
$(OCAMLGAVL): $(SPREZZ)/ocaml-gavl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-gavl_$(ocaml-gavl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-theora
ocaml-theora:$(OCAMLTHEORA)_$(ARCH).deb
$(OCAMLTHEORA): $(SPREZZ)/ocaml-theora/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-theora_$(ocaml-theora_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-portaudio
ocaml-portaudio:$(OCAMLPORTAUDIO)_$(ARCH).deb
$(OCAMLPORTAUDIO): $(SPREZZ)/ocaml-portaudio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-portaudio_$(ocaml-portaudio_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-samplerate
ocaml-samplerate:$(OCAMLSAMPLERATE)_$(ARCH).deb
$(OCAMLSAMPLERATE): $(SPREZZ)/ocaml-samplerate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-samplerate_$(ocaml-samplerate_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-xmlplaylist
ocaml-xmlplaylist:$(OCAMLXMLPLAYLIST)_$(ARCH).deb
$(OCAMLXMLPLAYLIST): $(SPREZZ)/ocaml-xmlplaylist/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-xmlplaylist_$(ocaml-xmlplaylist_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-magic
ocaml-magic:$(OCAMLMAGIC)_$(ARCH).deb
$(OCAMLMAGIC): $(SPREZZ)/ocaml-magic/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-magic_$(ocaml-magic_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-bjack
ocaml-bjack:$(OCAMLBJACK)_$(ARCH).deb
$(OCAMLBJACK): $(SPREZZ)/ocaml-bjack/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-bjack_$(ocaml-bjack_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-faad
ocaml-faad:$(OCAMLFAAD)_$(ARCH).deb
$(OCAMLFAAD): $(SPREZZ)/ocaml-faad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-faad_$(ocaml-faad_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-mm
ocaml-mm:$(OCAMLMM)_$(ARCH).deb
$(OCAMLMM): $(SPREZZ)/ocaml-mm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-mm_$(ocaml-mm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-flac
ocaml-flac:$(OCAMLFLAC)_$(ARCH).deb
$(OCAMLFLAC): $(SPREZZ)/ocaml-flac/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-flac_$(ocaml-flac_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-schroedinger
ocaml-schroedinger:$(OCAMLSCHROEDINGER)_$(ARCH).deb
$(OCAMLSCHROEDINGER): $(SPREZZ)/ocaml-schroedinger/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-schroedinger_$(ocaml-schroedinger_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-voaacenc
ocaml-voaacenc:$(OCAMLVOAACENC)_$(ARCH).deb
$(OCAMLVOAACENC): $(SPREZZ)/ocaml-voaacenc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-voaacenc_$(ocaml-voaacenc_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-dssi
ocaml-dssi:$(OCAMLDSSI)_$(ARCH).deb
$(OCAMLDSSI): $(SPREZZ)/ocaml-dssi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-dssi_$(ocaml-dssi_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-lo
ocaml-lo:$(OCAMLLO)_$(ARCH).deb
$(OCAMLLO): $(SPREZZ)/ocaml-lo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-lo_$(ocaml-lo_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-gstreamer
ocaml-gstreamer:$(OCAMLGSTREAMER)_$(ARCH).deb
$(OCAMLGSTREAMER): $(SPREZZ)/ocaml-gstreamer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-gstreamer_$(ocaml-gstreamer_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: camlidl
camlidl:$(CAMLIDL)_$(ARCH).deb
$(CAMLIDL): $(SPREZZ)/camlidl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf camlidl_$(camlidl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pcre-ocaml
pcre-ocaml:$(PCREOCAML)_$(ARCH).deb
$(PCREOCAML): $(SPREZZ)/pcre-ocaml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pcre-ocaml_$(pcre-ocaml_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocaml-pulseaudio
ocaml-pulseaudio:$(OCAMLPULSEAUDIO)_$(ARCH).deb
$(OCAMLPULSEAUDIO): $(SPREZZ)/ocaml-pulseaudio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-pulseaudio_$(ocaml-pulseaudio_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: yojson
yojson:$(YOJSON)_$(ARCH).deb
$(YOJSON): $(SPREZZ)/yojson/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yojson_$(yojson_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: easy-format
easy-format:$(EASYFORMAT)_$(ARCH).deb
$(EASYFORMAT): $(SPREZZ)/easy-format/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf easy-format_$(easy-format_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: caml2html
caml2html:$(CAML2HTML)_$(ARCH).deb
$(CAML2HTML): $(SPREZZ)/caml2html/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf caml2html_$(caml2html_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: camlmix
camlmix:$(CAMLMIX)_$(ARCH).deb
$(CAMLMIX): $(SPREZZ)/camlmix/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf camlmix_$(camlmix_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xmlm
xmlm:$(XMLM)_$(ARCH).deb
$(XMLM): $(SPREZZ)/xmlm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xmlm_$(xmlm_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: ocaml-lame
ocaml-lame:$(OCAMLLAME)_$(ARCH).deb
$(OCAMLLAME): $(SPREZZ)/ocaml-lame/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocaml-lame_$(ocaml-lame_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cppo
cppo:$(CPPO)_$(ARCH).deb
$(CPPO): $(SPREZZ)/cppo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cppo_$(cppo_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: biniou
biniou:$(BINIOU)_$(ARCH).deb
$(BINIOU): $(SPREZZ)/biniou/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf biniou_$(biniou_UPVER).orig.tar.gz $(TARARGS) $@

