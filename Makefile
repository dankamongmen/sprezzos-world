.DELETE_ON_ERROR:
.PHONY: world all clean
.DEFAULT_GOAL:=all

DEBKEY:=9978711C
DEBFULLNAME:='nick black'
DEBEMAIL:=nick.black@sprezzatech.com

# This is all *very* proof-of-concept. At a bare minimum, we'll be
# automatically generating all of this stuff. Preferably, we'll just induct it
# from the packaging data itself somehow. FIXME FIXME FIXME --nlb

PACKAGES:=growlight omphalos fbterm valgrind

SPREZZ:=packaging

include $(addprefix sprezzos-world/,$(PACKAGES))

sprezzos-world/%: $(SPREZZ)/%/changelog
	@[ -d $(@D) ] || mkdir -p $(@D)
	( echo "# Automatically generated from $<" && \
	 echo -n "$(@F)_VERSION:=" && \
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource .) > $@

GROWLIGHT=growlight_$(growlight_VERSION)
OMPHALOS=omphalos_$(omphalos_VERSION)
VALGRIND=valgrind_$(valgrind_VERSION)
FBTERM=fbterm_$(fbterm_VERSION)

DEBS:=$(GROWLIGHT) $(OMPHALOS) $(FBTERM) $(VALGRIND)
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
	cp -r $(SPREZZ)/$(shell echo $< | cut -d_ -f1) $@

%.deb: %/debian
	cd $(<D) && apt-get -y build-dep $(shell echo $@ | cut -d_ -f1) || true # source package might not exist
	cd $(<D) && dpkg-buildpackage -k$(DEBKEY)

.PHONY: growlight
growlight: $(GROWLIGHT).deb
$(GROWLIGHT): $(SPREZZ)/growlight/changelog
	git clone https://github.com/dankamongmen/growlight.git $@

.PHONY: omphalos
omphalos:$(OMPHALOS).deb
$(OMPHALOS): $(SPREZZ)/omphalos/changelog
	git clone https://github.com/dankamongmen/omphalos.git $@

.PHONY: fbterm
fbterm:$(FBTERM).deb
$(FBTERM): $(SPREZZ)/fbterm/changelog
	svn checkout http://fbterm.googlecode.com/svn/trunk/ $@

.PHONY: valgrind
valgrind:$(VALGRIND).deb
$(VALGRIND): $(SPREZZ)/valgrind/changelog
	svn co svn://svn.valgrind.org/valgrind/trunk $@

clean:
	rm -rf sprezzos-world
