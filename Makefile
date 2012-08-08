.DELETE_ON_ERROR:
.PHONY: world all clean
.DEFAULT_GOAL:=all

DEBKEY:=9978711C
DEBFULLNAME:='nick black'
DEBEMAIL:=nick.black@sprezzatech.com

# This is all *very* proof-of-concept. At a bare minimum, we'll be
# automatically generating all of this stuff. Preferably, we'll just induct it
# from the packaging data itself somehow. FIXME FIXME FIXME --nlb

PACKAGES:=growlight omphalos fbterm valgrind sprezzos-grub2theme

SPREZZ:=packaging

include $(addprefix sprezzos-world/,$(PACKAGES))

sprezzos-world/%: $(SPREZZ)/%/debian/changelog
	@[ -d $(@D) ] || mkdir -p $(@D)
	( echo "# Automatically generated from $<" && \
	 echo -n "$(@F)_VERSION:=" && \
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource .) > $@

GROWLIGHT=growlight_$(growlight_VERSION)
OMPHALOS=omphalos_$(omphalos_VERSION)
VALGRIND=valgrind_$(valgrind_VERSION)
FBTERM=fbterm_$(fbterm_VERSION)
GRUBTHEME=sprezzos-grub2theme_$(grubtheme_VERSION)
ADOBE=fonts-adobe-sourcesanspro_$(adobe_VERSION)

DEBS:=$(GROWLIGHT) $(OMPHALOS) $(FBTERM) $(GRUBTHEME) $(VALGRIND) $(ADOBE)
DEBS:=$(addsuffix .deb,$(DEBS))

UPACKAGES:=$(GROWLIGHT)
UPACKAGES:=$(addsuffix .udeb,$(UPACKAGES))

all: world

world: $(DEBS)

%/configure: $(shell echo % | cut -d. -f-3)
	cd $(@D) && autoreconf -fi

%.orig.tar.bz2: %/configure
	tar cjf $(shell echo $@ | cut -d- -f1 | cut -d. -f-3).orig.tar.bz2 $(shell echo $@ | cut -d. -f-3) --exclude=.git --exclude=debian

%/debian: %.orig.tar.bz2
	cp -r $(SPREZZ)/$(shell echo $< | cut -d_ -f1)/debian $@

%.deb: %/debian
	cd $(<D) && apt-get -y build-dep $(shell echo $@ | cut -d_ -f1) || true # source package might not exist
	cd $(<D) && dpkg-buildpackage -k$(DEBKEY)

.PHONY: growlight
growlight: $(GROWLIGHT).deb
$(GROWLIGHT): $(SPREZZ)/growlight/debian/changelog
	git clone https://github.com/dankamongmen/growlight.git $@

.PHONY: omphalos
omphalos:$(OMPHALOS).deb
$(OMPHALOS): $(SPREZZ)/omphalos/debian/changelog
	git clone https://github.com/dankamongmen/omphalos.git $@

.PHONY: fbterm
fbterm:$(FBTERM).deb
$(FBTERM): fbterm-1.7.0.tar.gz
	tar xzvf $<

fbterm-1.7.0.tar.gz:
	wget -nc http://fbterm.googlecode.com/files/fbterm-1.7.0.tar.gz -O $@

.PHONY: valgrind
valgrind:$(VALGRIND).deb
$(VALGRIND): $(SPREZZ)/valgrind/debian/changelog
	svn co svn://svn.valgrind.org/valgrind/trunk $@

.PHONY: grubtheme
sprezzos-grub2theme:$(GRUBTHEME).deb
$(GRUBTHEME): $(SPREZZ)/sprezzos-grub2theme/debian/changelog
	mkdir -p $@

# FIXME think we'll need to fetch it, no? using uscan?
.PHONY: adobe
adobe:$(ADOBE).deb
$(ADOBE): $(SPREZZ)/fonts-adobe-sourcesanspro/debian/changelog
	mkdir -p $@

clean:
	rm -rf sprezzos-world
