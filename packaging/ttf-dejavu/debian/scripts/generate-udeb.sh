#! /bin/sh

# resulting ttf's go inside udeb-generated/
# stripped sfd's go inside udeb-build/
# customize using $FONTS, $STRIP_RANGES
#
# see /usr/share/unicode/Blocks.txt for range details

FONTS="DejaVuSans DejaVuSans-Bold DejaVuSans-Oblique DejaVuSansMono"
STRIP_RANGES=""
STRIP_RANGES="$STRIP_RANGES u0500:u052F" # Cyrillic Supplement
STRIP_RANGES="$STRIP_RANGES u0530:u058F" # Armenian
STRIP_RANGES="$STRIP_RANGES u0600:u06FF" # Arabic
STRIP_RANGES="$STRIP_RANGES u0700:u074F" # Syriac
STRIP_RANGES="$STRIP_RANGES u07C0:u07FF" # NKo
STRIP_RANGES="$STRIP_RANGES u0E80:u0EFF" # Lao
STRIP_RANGES="$STRIP_RANGES u1400:u167F" # Canadian Syllabics
STRIP_RANGES="$STRIP_RANGES u1680:u169F" # Ogham
STRIP_RANGES="$STRIP_RANGES u1D00:u1DBF" # Phonetic Extensions
STRIP_RANGES="$STRIP_RANGES u1F00:u1FFF" # Greek Extended
STRIP_RANGES="$STRIP_RANGES u2070:u209f" # Superscripts and Subscripts
STRIP_RANGES="$STRIP_RANGES u20a0:u20cf" # Currency Symbols
STRIP_RANGES="$STRIP_RANGES u2150:u218f" # Number Forms
STRIP_RANGES="$STRIP_RANGES u2190:u21ff" # Arrows
STRIP_RANGES="$STRIP_RANGES u2200:u24FF" # Mathematical, Techical, Control, OCR

STRIP_RANGES="$STRIP_RANGES u2580:u25CE" # [u2580:u25FF] Block elements, Geometric Shapes
STRIP_RANGES="$STRIP_RANGES u25D0:u25FF" # u25CF is needed

STRIP_RANGES="$STRIP_RANGES u2600:u26FF" # Miscellaneous Symbols
STRIP_RANGES="$STRIP_RANGES u2700:u27EF" # Dingbats, Miscellaneous Mathematical Symbols-A
STRIP_RANGES="$STRIP_RANGES u27F0:u27FF" # Supplemental Arrows-A
STRIP_RANGES="$STRIP_RANGES u2800:u28FF" # Braille Patterns
STRIP_RANGES="$STRIP_RANGES u2900:u2BFF" # Suppl. Arrows-B, Misc Math Symbols-B, Supplath Ops, Misc Symbols and Arrows
STRIP_RANGES="$STRIP_RANGES u2C60:u2C7F" # Latin Extended-C
STRIP_RANGES="$STRIP_RANGES u2D30:u2D7F" # Tifinagh
STRIP_RANGES="$STRIP_RANGES u2E00:u2E7F" # Supplemental Punctuation
STRIP_RANGES="$STRIP_RANGES u3000:u30FF" # CJK Symbols and Punctuation
STRIP_RANGES="$STRIP_RANGES u4DC0:u4DFF" # Yijing Hexagram Symbols
STRIP_RANGES="$STRIP_RANGES uA640:uA69F" # Cyrillic Extended-B
STRIP_RANGES="$STRIP_RANGES uA700:uA71F" # Modifier Tone Letters
STRIP_RANGES="$STRIP_RANGES uA720:uA7FF" # Latin Extended-D
STRIP_RANGES="$STRIP_RANGES uFB50:uFBFF" # Arabic Presentation Forms-A
STRIP_RANGES="$STRIP_RANGES uFE70:uFEFF" # Arabic Presentation Forms-B
STRIP_RANGES="$STRIP_RANGES u10300:u1032F" # Old Italic
STRIP_RANGES="$STRIP_RANGES u1D300:u1D356" # Tai Xuan Jing Symbols
STRIP_RANGES="$STRIP_RANGES u1D400:u1D7FF" # Mathematical Alphanumeric Symbols
STRIP_RANGES="$STRIP_RANGES u1F030:u1F09F" # Domino Tiles
STRIP_RANGES="$STRIP_RANGES u1F0A0:u1F0FF" # Playing Cards
STRIP_RANGES="$STRIP_RANGES u1F400:u1F4FF" # Miscellaneous Symbols And Pictographs
STRIP_RANGES="$STRIP_RANGES u1F600:u1F64F" # Emoticons
# ---------------------------------------------------------------------------
set -e

udir="udeb-build"
result="udeb-generated"

rm -rf "$udir" "$result"
mkdir "$udir"
mkdir "$udir/generated"

for f in ${FONTS}; do
  echo "`basename $0`: stripping $f.sfd"

  ORIG_RANGES=${STRIP_RANGES}

  if [ $f = "DejaVuSansMono" ] ; then
      STRIP_RANGES="${STRIP_RANGES} u2100:u214F"   # Letterlike Symbols
      STRIP_RANGES="${STRIP_RANGES} u2500:u257F"   # Box Drawing
      STRIP_RANGES="${STRIP_RANGES} u25A0:u25FF"   # Geometric Shapes
  fi

  fontforge -script debian/scripts/strip_glyphs.pe "src/$f.sfd" "$udir/$f.sfd" ${STRIP_RANGES}
  
  STRIP_RANGES=${ORIG_RANGES}
done

(cd "$udir" && ../scripts/generate.pe *.sfd && for ttf in *.sfd.ttf ; do mv $ttf generated/$(echo $ttf|sed s+"\.sfd\.ttf+.ttf+g") ; done)

./scripts/ttpostproc.pl "$udir"/generated/*.ttf
mv "$udir/generated" "$result"

# vim:set tw=0 nowrap ts=8 sw=2 sts=2:
