#!/bin/sh
exec sed -r \
  -e '1,3 d' \
  -e 's| ?\\ref | |g' \
  -e 's|\\link ||g' \
  -e 's|\\endlink||g' \
  -e 's|\\#|#|g' \
  -e 's|<p>||' \
  -e 's|<a href=[^>]*>||g' \
  -e 's|</a>||g' \
  -e 's|<b> *(.*)</b>|\1|g' \
  -e 's|</?[Uu][Ll]>||g' \
  -e 's|</?[Tt][Tt]>||g' \
  -e 's|( *)<li>|\n\1*|' \
  -e 's|<br>$||' \
  -e 's|&ouml;|ö|g' \
  -e '/^[*][/]$/ d' \
| tr '\n' '$' \
| sed -r -e 's|\$\$+|\n\n|g' -e 's| *\$ *| |g' \
| fmt
