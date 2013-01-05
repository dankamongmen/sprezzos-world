# Packages whose contents originated with Sprezzatech/SprezzOS itself.

.PHONY: grubtheme
sprezzos-grub2theme:$(SPREZZOSGRUB2THEME)_$(ARCH).deb
$(SPREZZOSGRUB2THEME): $(SPREZZ)/sprezzos-grub2theme/debian/changelog
	mkdir -p $@
	cp -r $(<D)/..* $@/
	tar cJf sprezzos-grub2theme-$(sprezzos-grub2theme_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -s sprezzos-grub2theme-$(sprezzos-grub2theme_UPVER).tar.xz \
		sprezzos-grub2theme_$(sprezzos-grub2theme_UPVER).orig.tar.xz

