.DELETE_ON_ERROR:
.PHONY: world all clean
.DEFAULT_GOAL:=all

# This is all *very* proof-of-concept. At a bare minimum, we'll be
# automatically generating all of this stuff. Preferably, we'll just induct it
# from the packaging data itself somehow. FIXME FIXME FIXME --nlb

SPREZZ:=packaging

include sprezzos-world/*

versions/growlight: $(SPREZZ)/growlight/changelog
versions/libbluray: $(SPREZZ)/libbluray/changelog
versions/omphalos: $(SPREZZ)/omphalos/changelog

GROWLIGHT=growlight_$(GROWLIGHT_VERSION)
LIBBLURAY=libbluray_$(LIBBLURAY_VERSION)
OMPHALOS=omphalos_$(OMPHALOS_VERSION)

PACKAGES:=$(GROWLIGHT) \
	$(LIBBLURAY) \
	$(OMPHALOS)
PACKAGES:=$(addsuffix .deb,$(PACKAGES))

UPACKAGES:=$(GROWLIGHT)
UPACKAGES:=$(addsuffix .udeb,$(UPACKAGES))

all: world

world: $(PACKAGES)

$(PACKAGES): $(basename $(PACKAGES))

.PHONY: growlight
growlight: $(GROWLIGHT)
$(GROWLIGHT): $(SPREZZ)/growlight/changelog
	git clone https://github.com/dankamongmen/growlight.git $@

.PHONY: libbluray
libbluray: $(LIBBLURAY)
$(LIBBLURAY): $(SPREZZ)/libbluray/changelog
	git clone git://git.videolan.org/libbluray.git $@

.PHONY: omphalos
omphalos:$(OMPHALOS)
$(OMPHALOS): $(SPREZZ)/omphalos/changelog
	git clone https://github.com/dankamongmen/omphalos.git $@

clean:
	rm -rf 
