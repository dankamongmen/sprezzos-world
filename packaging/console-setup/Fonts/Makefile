SHELL = /bin/sh

build : build-linux build-freebsd
build-linux : gzipped_good_psf_fonts build-any
build-freebsd : uuencoded_good_raw_fonts scrnmaps terminfo termcap build-any
build-any:


%.gz : %
	gzip -9 <$< >$@

# All available non-legacy font faces
FACES = Fixed13 Fixed14 Fixed15 Fixed16 \
	Fixed18 Fixed24x12 \
	FixedBold13 FixedBold14 FixedBold15 FixedBold16 \
	FixedBold18 FixedBold24x12  \
	FixedOblique13 \
	Terminus12x6 Terminus14 Terminus16 Terminus22x11 Terminus24x12 \
	Terminus20x10 Terminus28x14 Terminus32x16 \
	TerminusBold12x6 TerminusBold14 TerminusBold16 TerminusBold20x10 \
	TerminusBold22x11 TerminusBold24x12 TerminusBold28x14 \
	TerminusBold32x16 TerminusBoldVGA14 TerminusBoldVGA16 \
	Courier13 Courier14 Courier15 Courier16 \
	CourierBold13 CourierBold14 CourierBold15 \
	Lucid12 Lucid13 Lucid15 Lucid16 Lucid22x12 Lucid29x16 \
	LucidBold11 LucidBold13 LucidBold15 \
	LucidBold16 LucidBold22x12 LucidBold29x16 \
	Goha12 Goha14 Goha16 GohaClassic12 GohaClassic14 GohaClassic16

LEGACY_FACES = VGA8 VGA10 VGA12 VGA14 VGA16 VGA19 VGA28x16 VGA32x16

ALL_FACES = $(FACES) $(LEGACY_FACES)

all-CODESETS = Arabic Armenian CyrAsia CyrKoi CyrSlav Ethiopian Georgian Greek Hebrew Lao Lat15 Lat2 Lat38 Lat7 Thai Uni1 Uni2 Uni3 Vietnamese

Fixed13-BDFS = 8x13-IL2.bdf+8x13.bdf+8x13c.bdf
Fixed14-BDFS = georgian14.bdf+7x14-IL2.bdf+7x14.bdf+7x14c.bdf+h14.bdf+etl14-unicode.bdf
Fixed15-BDFS = 9x15-IL2.bdf+9x15.bdf+9x15c.bdf
Fixed16-BDFS = georgian16.bdf+unifont.bdf+h16.bdf+etl16-unicode.bdf
Fixed18-BDFS = 9x18.bdf
Fixed24x12-BDFS = h24.bdf+etl24-unicode.bdf

FixedBold13-BDFS = 8x13B.bdf+8x13Bc.bdf
FixedBold14-BDFS = h14_b.bdf+7x14B-IL2.bdf+7x14B.bdf+7x14c.bdf
FixedBold15-BDFS = 9x15B-IL2.bdf+9x15B.bdf+9x15Bc.bdf
FixedBold16-BDFS = h16_b.bdf
FixedBold18-BDFS = 9x18B.bdf
FixedBold24x12-BDFS = h24_b.bdf

# We have to complement 8x13O.bdf because it doesn't contain box drawings
FixedOblique13-BDFS = 8x13O.bdf+8x13.bdf

Terminus12x6-BDFS = ter-u12n.bdf
Terminus14-BDFS = ter-u14n.bdf
Terminus16-BDFS = ter-u16n.bdf
Terminus20x10-BDFS = ter-u20n.bdf
Terminus22x11-BDFS = ter-u22n.bdf
Terminus24x12-BDFS = ter-u24n.bdf
Terminus28x14-BDFS = ter-u28n.bdf
Terminus32x16-BDFS = ter-u32n.bdf
TerminusBold12x6-BDFS = ter-u12b.bdf
TerminusBold14-BDFS = ter-u14b.bdf
TerminusBold16-BDFS = ter-u16b.bdf
TerminusBold20x10-BDFS = ter-u20b.bdf
TerminusBold22x11-BDFS = ter-u22b.bdf
TerminusBold24x12-BDFS = ter-u24b.bdf
TerminusBold28x14-BDFS = ter-u28b.bdf
TerminusBold32x16-BDFS = ter-u32b.bdf
TerminusBoldVGA14-BDFS = ter-u14v.bdf
TerminusBoldVGA16-BDFS = ter-u16v.bdf

Courier13-BDFS = 75dpi-courR12-IL2.bdf+75dpi-courR12.bdf+75dpi-courR12c.bdf
Courier14-BDFS = 100dpi-courR10-IL2.bdf+100dpi-courR10.bdf
Courier15-BDFS = 75dpi-courR14-IL2.bdf+75dpi-courR14.bdf+75dpi-courR14c.bdf
Courier16-BDFS = 8x16c.bdf+8x16-IL2.bdf

CourierBold13-BDFS = 75dpi-courB12-IL2.bdf+75dpi-courB12.bdf+75dpi-courB12c.bdf
CourierBold14-BDFS = 100dpi-courB10-IL2.bdf+100dpi-courB10.bdf
CourierBold15-BDFS = 75dpi-courB14-IL2.bdf+75dpi-courB14.bdf+75dpi-courB14c.bdf

Lucid12-BDFS = 100dpi-lutRS08-IL2.bdf+100dpi-lutRS08.bdf
Lucid13-BDFS = 75dpi-lutRS12-IL2.bdf+75dpi-lutRS12.bdf+75dpi-lutRS12c.bdf
Lucid15-BDFS = 75dpi-lutRS14-IL2.bdf+75dpi-lutRS14.bdf+75dpi-lutRS14c.bdf
Lucid16-BDFS = 100dpi-lutRS10-IL2.bdf+100dpi-lutRS10.bdf
Lucid22x12-BDFS = 100dpi-lutRS14-IL2.bdf+100dpi-lutRS14.bdf
Lucid29x16-BDFS = 100dpi-lutRS19.bdf

LucidBold11-BDFS = 100dpi-lutBS08-IL2.bdf+100dpi-lutBS08.bdf
LucidBold13-BDFS = 75dpi-lutBS12-IL2.bdf+75dpi-lutBS12.bdf+75dpi-lutBS12c.bdf
LucidBold15-BDFS = 75dpi-lutBS14-IL2.bdf+75dpi-lutBS14.bdf+75dpi-lutBS14c.bdf
LucidBold16-BDFS = 100dpi-lutBS10-IL2.bdf+100dpi-lutBS10.bdf
LucidBold22x12-BDFS = 100dpi-lutBS14-IL2.bdf+100dpi-lutBS14.bdf
LucidBold29x16-BDFS = 100dpi-lutBS19.bdf

Goha12-BDFS = Goha12.bdf
Goha14-BDFS = Goha14.bdf
Goha16-BDFS = Goha16.bdf

GohaClassic12-BDFS = GohaClassic12.bdf
GohaClassic14-BDFS = GohaClassic14.bdf
GohaClassic16-BDFS = GohaClassic16.bdf

###################################################################

VGA8-CODESETS = Arabic CyrKoi CyrSlav Greek Hebrew Lat15 Lat2 Lat38 Lat7 Uni1 Uni2
VGA10-CODESETS = Lat15 Lat2
VGA12-CODESETS = Lat15 Lat2
VGA14-CODESETS = Arabic CyrKoi CyrSlav Greek Hebrew Lat15 Lat2 Lat38 Lat7 Uni1 Uni2
VGA16-CODESETS = Arabic CyrKoi CyrSlav Greek Hebrew Lat15 Lat2 Lat38 Lat7 Uni1 Uni2
VGA19-CODESETS = Lat15

VGA28x16-CODESETS = $(VGA14-CODESETS)
VGA32x16-CODESETS = $(VGA16-CODESETS)

Arabic-legacy14-BDFS = legacy14i.bdf+legacy14f.bdf+legacy14l.bdf+legacy14b.bdf
Arabic-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16c.bdf+legacy16f.bdf+legacy16k.bdf
Arabic-legacy8-BDFS = legacy8f.bdf+legacy8d.bdf+legacy8i.bdf+legacy8b.bdf
CyrKoi-legacy14-BDFS = legacy14b.bdf+legacy14a.bdf+legacy14c.bdf+legacy14g.bdf
CyrKoi-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16b.bdf+legacy16a.bdf
CyrKoi-legacy8-BDFS = legacy8b.bdf+legacy8a.bdf
CyrSlav-legacy14-BDFS = legacy14a.bdf+legacy14l.bdf+legacy14c.bdf+legacy14e.bdf+legacy14i.bdf
CyrSlav-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16a.bdf+legacy16k.bdf+legacy16c.bdf+legacy16b.bdf
CyrSlav-legacy8-BDFS = legacy8a.bdf+legacy8i.bdf+legacy8b.bdf+legacy8c.bdf+legacy8f.bdf
Greek-legacy14-BDFS = legacy14g.bdf+legacy14l.bdf+legacy14b.bdf+legacy14i.bdf
Greek-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16a.bdf+legacy16k.bdf+legacy16c.bdf
Greek-legacy8-BDFS = legacy8a.bdf+legacy8i.bdf+legacy8b.bdf+legacy8f.bdf
Hebrew-legacy14-BDFS = legacy14h.bdf+legacy14l.bdf+legacy14i.bdf+legacy14b.bdf
Hebrew-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16g.bdf+legacy16k.bdf+legacy16c.bdf
Hebrew-legacy8-BDFS = legacy8e.bdf+legacy8i.bdf+legacy8f.bdf+legacy8b.bdf
Lat15-legacy10-BDFS = legacy10b.bdf+legacy10a.bdf+legacy10d.bdf
Lat15-legacy12-BDFS = legacy12a.bdf+legacy12d.bdf+legacy12c.bdf
Lat15-legacy14-BDFS = legacy14i.bdf+legacy14l.bdf+legacy14a.bdf+legacy14b.bdf
Lat15-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16c.bdf+legacy16k.bdf+legacy16a.bdf
Lat15-legacy19-BDFS = legacy19a.bdf
Lat15-legacy8-BDFS = legacy8a.bdf+legacy8i.bdf+legacy8b.bdf+legacy8f.bdf
Lat2-legacy10-BDFS = legacy10d.bdf+legacy10a.bdf+legacy10b.bdf
Lat2-legacy12-BDFS = legacy12c.bdf+legacy12d.bdf+legacy12a.bdf
Lat2-legacy14-BDFS = legacy14k.bdf+legacy14l.bdf+legacy14i.bdf+legacy14b.bdf
Lat2-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16c.bdf+legacy16k.bdf
Lat2-legacy8-BDFS = legacy8f.bdf+legacy8i.bdf+legacy8b.bdf
Lat38-legacy14-BDFS = legacy14e.bdf+legacy14i.bdf+legacy14b.bdf
Lat38-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16e.bdf+legacy16h.bdf+legacy16a.bdf
Lat38-legacy8-BDFS = legacy8c.bdf+legacy8f.bdf+legacy8b.bdf
Lat7-legacy14-BDFS = legacy14l.bdf+legacy14e.bdf+legacy14i.bdf+legacy14b.bdf
Lat7-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16e.bdf+legacy16k.bdf+legacy16c.bdf
Lat7-legacy8-BDFS = legacy8i.bdf+legacy8c.bdf+legacy8f.bdf+legacy8b.bdf
Uni1-legacy14-BDFS = legacy14a.bdf+legacy14e.bdf+legacy14f.bdf+legacy14l.bdf+legacy14h.bdf+legacy14c.bdf
Uni1-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16e.bdf+legacy16a.bdf+legacy16f.bdf+legacy16g.bdf+legacy16k.bdf+legacy16b.bdf
Uni1-legacy8-BDFS = legacy8a.bdf+legacy8c.bdf+legacy8d.bdf+legacy8i.bdf+legacy8e.bdf+legacy8b.bdf
Uni2-legacy14-BDFS = legacy14e.bdf+legacy14a.bdf+legacy14g.bdf+legacy14l.bdf+legacy14c.bdf+legacy14i.bdf
Uni2-legacy16-BDFS = u_vga16_fix.bdf+u_vga16.bdf+legacy16a.bdf+legacy16e.bdf+legacy16h.bdf+legacy16k.bdf+legacy16b.bdf
Uni2-legacy8-BDFS = legacy8a.bdf+legacy8c.bdf+legacy8i.bdf+legacy8b.bdf+legacy8f.bdf

adddouble = $(subst .bdf,-double.bdf, $(1))

Arabic-legacy32x16-BDFS = $(call adddouble, $(Arabic-legacy16-BDFS))
Armenian-legacy32x16-BDFS = $(call adddouble, $(Armenian-legacy16-BDFS))
CyrAsia-legacy32x16-BDFS = $(call adddouble, $(CyrAsia-legacy16-BDFS))
CyrKoi-legacy32x16-BDFS = $(call adddouble, $(CyrKoi-legacy16-BDFS))
CyrSlav-legacy32x16-BDFS = $(call adddouble, $(CyrSlav-legacy16-BDFS))
Ethiopian-legacy32x16-BDFS = $(call adddouble, $(Ethiopian-legacy16-BDFS))
Georgian-legacy32x16-BDFS = $(call adddouble, $(Georgian-legacy16-BDFS))
Greek-legacy32x16-BDFS = $(call adddouble, $(Greek-legacy16-BDFS))
Hebrew-legacy32x16-BDFS = $(call adddouble, $(Hebrew-legacy16-BDFS))
Lao-legacy32x16-BDFS = $(call adddouble, $(Lao-legacy16-BDFS))
Lat15-legacy32x16-BDFS = $(call adddouble, $(Lat15-legacy16-BDFS))
Lat2-legacy32x16-BDFS = $(call adddouble, $(Lat2-legacy16-BDFS))
Lat38-legacy32x16-BDFS = $(call adddouble, $(Lat38-legacy16-BDFS))
Lat7-legacy32x16-BDFS = $(call adddouble, $(Lat7-legacy16-BDFS))
Thai-legacy32x16-BDFS = $(call adddouble, $(Thai-legacy16-BDFS))
Uni1-legacy32x16-BDFS = $(call adddouble, $(Uni1-legacy16-BDFS))
Uni2-legacy32x16-BDFS = $(call adddouble, $(Uni2-legacy16-BDFS))
Uni3-legacy32x16-BDFS = $(call adddouble, $(Uni3-legacy16-BDFS))
Vietnamese-legacy32x16-BDFS = $(call adddouble, $(Vietnamese-legacy16-BDFS))

Arabic-legacy28x16-BDFS = $(call adddouble, $(Arabic-legacy14-BDFS))
Armenian-legacy28x16-BDFS = $(call adddouble, $(Armenian-legacy14-BDFS))
CyrAsia-legacy28x16-BDFS = $(call adddouble, $(CyrAsia-legacy14-BDFS))
CyrKoi-legacy28x16-BDFS = $(call adddouble, $(CyrKoi-legacy14-BDFS))
CyrSlav-legacy28x16-BDFS = $(call adddouble, $(CyrSlav-legacy14-BDFS))
Ethiopian-legacy28x16-BDFS = $(call adddouble, $(Ethiopian-legacy14-BDFS))
Georgian-legacy28x16-BDFS = $(call adddouble, $(Georgian-legacy14-BDFS))
Greek-legacy28x16-BDFS = $(call adddouble, $(Greek-legacy14-BDFS))
Hebrew-legacy28x16-BDFS = $(call adddouble, $(Hebrew-legacy14-BDFS))
Lao-legacy28x16-BDFS = $(call adddouble, $(Lao-legacy14-BDFS))
Lat15-legacy28x16-BDFS = $(call adddouble, $(Lat15-legacy14-BDFS))
Lat2-legacy28x16-BDFS = $(call adddouble, $(Lat2-legacy14-BDFS))
Lat38-legacy28x16-BDFS = $(call adddouble, $(Lat38-legacy14-BDFS))
Lat7-legacy28x16-BDFS = $(call adddouble, $(Lat7-legacy14-BDFS))
Thai-legacy28x16-BDFS = $(call adddouble, $(Thai-legacy14-BDFS))
Uni1-legacy28x16-BDFS = $(call adddouble, $(Uni1-legacy14-BDFS))
Uni2-legacy28x16-BDFS = $(call adddouble, $(Uni2-legacy14-BDFS))
Uni3-legacy28x16-BDFS = $(call adddouble, $(Uni3-legacy14-BDFS))
Vietnamese-legacy28x16-BDFS = $(call adddouble, $(Vietnamese-legacy14-BDFS))

###################################################################

# $(call add_bdf, xxx+yyy+zzz) = bdf/xxx.bdf+bdf/yyy.bdf+bdf/zzz.bdf
empty =
space = $(empty) $(empty)
add_bdf = $(patsubst +%, %, \
		$(subst $(space),+, \
			$(addprefix bdf/,$(subst +, , $(1)))))

ALL_PSF_FONTS = $(sort $(foreach face, $(ALL_FACES), \
			$(addsuffix -$(face).psf, $(all-CODESETS))))

GOOD_PSF_FONTS = $(filter \
		%-Fixed13.psf %-Fixed14.psf %-Fixed15.psf \
		%-Fixed16.psf %-Fixed18.psf \
		%-Terminus12x6.psf %-Terminus14.psf %-Terminus16.psf	\
		%-Terminus22x11.psf %-Terminus24x12.psf %-Terminus20x10.psf \
		%-Terminus28x14.psf %-Terminus32x16.psf			\
		%-TerminusBold14.psf %-TerminusBold16.psf		\
		%-TerminusBold20x10.psf %-TerminusBold22x11.psf         \
		%-TerminusBold24x12.psf %-TerminusBold28x14.psf         \
		%-TerminusBold32x16.psf %-TerminusBoldVGA14.psf         \
		%-TerminusBoldVGA16.psf                                 \
		%-Goha12.psf %-Goha14.psf %-Goha16.psf \
		%-GohaClassic12.psf %-GohaClassic14.psf %-GohaClassic16.psf \
		%-VGA8.psf %-VGA14.psf %-VGA16.psf \
		%-VGA28x16.psf %-VGA32x16.psf, \
	$(PSF_FONTS))

GOOD_8_FONTS = $(filter \
		%-Fixed14.psf %-Fixed16.psf %-Terminus14.psf		\
                %-Terminus16.psf %-TerminusBold14.psf			\
                %-TerminusBold16.psf %-TerminusBoldVGA14.psf		\
                %-TerminusBoldVGA16.psf %-Goha14.psf %-Goha16.psf	\
                %-GohaClassic14.psf %-GohaClassic16.psf %-VGA8.psf	\
                %-VGA14.psf %-VGA16.psf, \
	$(PSF_FONTS))

GZIPPED_GOOD_PSF_FONTS=$(addsuffix .gz, $(GOOD_PSF_FONTS))
GZIPPED_PSF_FONTS=$(addsuffix .gz, $(PSF_FONTS))
GZIPPED_ALL_PSF_FONTS=$(addsuffix .gz, $(ALL_PSF_FONTS))
UUENCODED_GOOD_RAW_FONTS=$(subst .raw,.fnt,$(GOOD_RAW_FONTS))
UUENCODED_RAW_FONTS=$(subst .raw,.fnt,$(RAW_FONTS))
UUENCODED_ALL_RAW_FONTS=$(subst .raw,.fnt,$(ALL_RAW_FONTS))

# These fonts can be used only with framebuffer
FB_PSF_FONTS = $(filter %x6.psf %x10.psf %x11.psf %x12.psf %x14.psf %x16.psf, $(ALL_PSF_FONTS))

# These fonts can be used both in text-mode and with framebuffer
NONFB_PSF_FONTS = $(filter-out %x6.psf %x10.psf %x11.psf %x12.psf %x14.psf %x16.psf, $(ALL_PSF_FONTS))

# Filter small charsets (256 symbols) and non-framebuffer fonts
psf_to_raw = $(subst .psf,.raw, $(filter Armenian% Cyr% Georgian% Greek% Hebrew% Lao% Lat% Thai%, $(filter-out %x6.psf %x10.psf %x11.psf %x12.psf %x14.psf %x16.psf, $(1))))

RAW_FONTS = $(call psf_to_raw, $(PSF_FONTS))
GOOD_RAW_FONTS = $(call psf_to_raw, $(GOOD_8_FONTS))
ALL_RAW_FONTS = $(call psf_to_raw, $(ALL_PSF_FONTS))

ALL_FONTS = $(ALL_PSF_FONTS) $(ALL_RAW_FONTS)

doubled_bdfs = $(call adddouble, $(BDFS_TO_DOUBLE))

doubled_bdfs : $(doubled_bdfs)

bdf/%-double.bdf : source = $(subst -double.bdf,.bdf,$@)
bdf/%-double.bdf : 
	bdfresize -f 2 -b 1 $(source) >$@

# Use rm soft.Makefile && make soft.Makefile in order to update soft.Makefile
include soft.Makefile
# The script 'fontcodesets' computes which codesets a font supports
soft.Makefile : $(doubled_bdfs)
	echo "PSF_FONTS = " $(sort $(foreach face, $(FACES), \
	$(addsuffix -$(face).psf, $(shell ./fontcodesets $($(face)-BDFS)))) \
	$(foreach face, $(LEGACY_FACES), \
		$(addsuffix -$(face).psf, $($(face)-CODESETS)))) \
	>soft.Makefile
	echo "BDFS_TO_DOUBLE = " $(wildcard bdf/legacy16?.bdf bdf/legacy14?.bdf bdf/u_vga16_fix.bdf bdf/u_vga16.bdf) >>soft.Makefile
	echo -n ENCODING_FONTSET = >>soft.Makefile
	for a in ../acm/*.acm; do aa=`basename $$a .acm`; for f in fontsets/*.256; do ff=`basename $$f .256`; echo -n ' ' $${aa}_$${ff}; done; done >>soft.Makefile
	echo >>soft.Makefile

# i.e. Arabic/univga16
$(ALL_PSF_FONTS) : codeset/face = $(subst -,/,$(@:%.psf=%))
$(ALL_RAW_FONTS) : codeset/face = $(subst -,/,$(@:%.raw=%))
# i.e. Arabic/
$(ALL_FONTS) : codeset/ = $(dir $(codeset/face))
# i.e. Arabic
$(ALL_FONTS) : codeset = $(subst /,-,$(codeset/:%/=%))
# i.e. univga16
$(ALL_FONTS) : face = $(strip $(notdir $(codeset/face)))
# legacy hack
$(ALL_FONTS)  : VGA8-BDFS = $(if $(strip $($(codeset)-legacy8-BDFS)), \
			$($(codeset)-legacy8-BDFS),legacy8a.bdf)
$(ALL_FONTS)  : VGA10-BDFS = $(if $(strip $($(codeset)-legacy10-BDFS)),\
			$($(codeset)-legacy10-BDFS),legacy10a.bdf)
$(ALL_FONTS)  : VGA12-BDFS = $(if $(strip $($(codeset)-legacy12-BDFS)),\
			$($(codeset)-legacy12-BDFS),legacy12a.bdf)
$(ALL_FONTS)  : VGA14-BDFS = $(if $(strip $($(codeset)-legacy14-BDFS)),\
			$($(codeset)-legacy14-BDFS),legacy14a.bdf)
$(ALL_FONTS)  : VGA16-BDFS = $(if $(strip $($(codeset)-legacy16-BDFS)),\
			$($(codeset)-legacy16-BDFS),u_vga16.bdf)
$(ALL_FONTS)  : VGA19-BDFS = $(if $(strip $($(codeset)-legacy19-BDFS)),\
			$($(codeset)-legacy19-BDFS),legacy19a.bdf)
$(ALL_FONTS)  : VGA28x16-BDFS = $(if $(strip $($(codeset)-legacy28x16-BDFS)),\
			$($(codeset)-legacy28x16-BDFS),legacy14a-double.bdf)
$(ALL_FONTS)  : VGA32x16-BDFS = $(if $(strip $($(codeset)-legacy32x16-BDFS)),\
			$($(codeset)-legacy32x16-BDFS),u_vga16-double.bdf)
# i.e. bdf/univga16.bdf
$(ALL_FONTS) : bdfs = $(call add_bdf, $($(face)-BDFS))
# i.e. fontsets/Arabic.512
$(ALL_FONTS) : codesetfile = $(wildcard fontsets/$(codeset).*)
# i.e. 512
$(ALL_FONTS) : size = $(subst .,,$(suffix $(codesetfile)))

$(ALL_FONTS) : equivalents = $(if $(findstring Uni1, $(codeset)),	 \
				standard.equivalents+arabic.equivalents, \
				standard.equivalents)

$(NONFB_PSF_FONTS) :
	./bdf2psf --log $(@:%.psf=%.log) $(bdfs) \
		$(equivalents) ascii.set+linux.set+$(codesetfile)+:useful.set \
                $(size) $@ $(@:%.psf=%.sfm)

$(FB_PSF_FONTS) :
	./bdf2psf --fb --log $(@:%.psf=%.log) $(bdfs) \
		$(equivalents) ascii.set+linux.set+$(codesetfile)+:useful.set \
                $(size) $@ $(@:%.psf=%.sfm)

$(ALL_RAW_FONTS) :
	./bdf2psf --raw --log $(@:%.raw=%.raw.log) $(bdfs) \
		$(equivalents) ascii.set+freebsd.set+$(codesetfile)+:useful.set \
                $(size) $(@:%.raw=%.raw) $(@:%.raw=%.raw.sfm)

%.fnt : %.raw
	uuencode $< $* >$@

good_psf_fonts : $(GOOD_PSF_FONTS)

gzipped_good_psf_fonts : $(GZIPPED_GOOD_PSF_FONTS)

uuencoded_good_raw_fonts : $(UUENCODED_GOOD_RAW_FONTS)


SCRNMAPS = $(addsuffix .scm, $(ENCODING_FONTSET))

# i.e. ISO-8859-7/Greek
$(SCRNMAPS) : encoding/fontset = $(subst _,/,$(@:%.scm=%))
# i.e. ISO-8859-7/
$(SCRNMAPS) : encoding/ = $(dir $(encoding/fontset))
# i.e. ISO-8859-7
$(SCRNMAPS) : encoding = $(encoding/:%/=%)
# i.e. Greek
$(SCRNMAPS) : fontset = $(strip $(notdir $(encoding/fontset)))

$(SCRNMAPS) : $(UUENCODED_GOOD_RAW_FONTS) freebsd.set fbsdmap
	./fbsdmap --binaryscm freebsd.set `ls $(fontset)-*.raw.sfm|{ read x y; echo $$x;}` ../acm/$(encoding).acm| uuencode $(@:%.scm=%) >$@

scrnmaps : $(SCRNMAPS)

# This builds all CODESET-TYPEFASE combinations, even when the font 
# doesn't define symbols for the particular CODESET
fullall : $(ALL_FONTS)

terminfo : fbsdmap
	./fbsdmap --terminfo >$@

termcap : fbsdmap
	./fbsdmap --termcap >$@

.PHONY: clean
clean:
	-rm -f *~
	-rm -f *.psf *.psf.gz *.sfm *.log *.raw *.fnt *.scm
	-rm -f fontsets/*~
	-rm -f terminfo termcap

.PHONY: maintainer-clean
maintainer-clean: clean
	rm $(doubled_bdfs)
	$(MAKE) $(doubled_bdfs)
	rm soft.Makefile
	$(MAKE) soft.Makefile
