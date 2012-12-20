#/usr/bin/awk -f

# Usage: { cat some_font.bdf; zcat /usr/share/i18n/charmaps/some_enc.gz; } | 
#               awk -f bdf-charset.awk >symbols_not_in_the_font

/^ENCODING/ {
  hex = sprintf ("%04X", $2);
  has [hex] = "yes";
}

/^<U/ {
  code = $1;
  gsub ("^<U", "", code);
  gsub (">$", "", code);
  if (has [code] != "yes") {
    print;
  }
}

