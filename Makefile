.DELETE_ON_ERROR:
.PHONY: world all clean
.DEFAULT_GOAL:=all

# This is all *very* proof-of-concept. At a bare minimum, we'll be
# automatically generating all of this stuff. Preferably, we'll just induct it
# from the packaging data itself somehow. FIXME FIXME FIXME --nlb

PACKAGES:=growlight omphalos

SPREZZ:=packaging

include $(addprefix sprezzos-world/,$(PACKAGES))

sprezzos-world/%: $(SPREZZ)/%/changelog
	@[ -d $(@D) ] || mkdir -p $(@D)
	( echo "# Automatically generated from $<" && \
	 echo -n "$(@F)_VERSION:=" && \
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource .) > $@

GROWLIGHT=growlight_$(growlight_VERSION)
OMPHALOS=omphalos_$(omphalos_VERSION)

DEBS:=$(GROWLIGHT) \
	$(OMPHALOS)
DEBS:=$(addsuffix .deb,$(DEBS))

UPACKAGES:=$(GROWLIGHT)
UPACKAGES:=$(addsuffix .udeb,$(UPACKAGES))

all: world

world: $(DEBS)

$(DEBS): $(basename $(DEBS))

.PHONY: growlight
growlight: $(GROWLIGHT)
$(GROWLIGHT): $(SPREZZ)/growlight/changelog
	git clone https://github.com/dankamongmen/growlight.git $@

.PHONY: omphalos
omphalos:$(OMPHALOS)
$(OMPHALOS): $(SPREZZ)/omphalos/changelog
	git clone https://github.com/dankamongmen/omphalos.git $@

clean:
	rm -rf sprezzos-world
