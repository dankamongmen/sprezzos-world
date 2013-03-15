#!/bin/csh

foreach ocode ( 00 8.0 )
  if ( $ocode == "00" ) then
    set ofile = blt-index.html
    set title = "BLT"
  else
    set ofile = blt$ocode-index.html
    set title = "BLT$ocode"
  endif
  rm -f $ofile
  
  echo '<HTML>' > $ofile
  echo '<HEAD>' >> $ofile
  echo '<TITLE>'$title' Command Information</TITLE>' >> $ofile
  echo '</HEAD>' >> $ofile
  echo '<BODY bgcolor=white>' >> $ofile
  echo '<H1>Table of Contents</H1>' >> $ofile
  echo ' ' >> $ofile
  echo '<UL>' >> $ofile
  foreach name ( `cd ../html; ls *.html` )
    if ( "$name" != "BLT.html" ) then
      echo '<LI><A HREF="'$name'">'$name:r'</A>' >> $ofile
    endif
  end
  echo '</UL>' >> $ofile
  echo '</BODY></HTML>' >> $ofile
end
