# Transitional packages are empty

.PHONY: fonts-inconsolata
fonts-inconsolata:$(FONTSINCONSOLATA)_$(ARCH).deb
$(FONTSINCONSOLATA): $(SPREZZ)/fonts-inconsolata/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	tar cJv -T /dev/null -f fonts-inconsolata_$(fonts-inconsolata_UPVER).orig.tar.xz

