#! /usr/bin/awk -f

BEGIN { print "<?xml version=\"1.0\"?>";
        level=1 }

title == 1 { print "<book title=\"" gensub("^>(.*)</TITLE$","\\1",1) "\" name =\"" name "\" link=\"index.html\">";
             print "<chapters>";
             title=0 }
/^><TITLE/ { title=1 }

/^><DD$/ { level++ }
/^><\/DD$/ { level-- }

intext == 0 && href != "" && /^>([^<>]*)<.*$/  { n=n gensub("^>(.*)<.*$", "\\1",1); }
intext == 1 && href != "" && /^([^<>]*)$/  { n=n $0 " "; }
intext == 0 && href != "" && /^>([^<>]*)$/  { n=n gensub("^>(.*)$", "\\1",1) " "; intext=1 }
intext == 1 && href != "" && /^([^<>]*)<.*$/  { n=n gensub("^([^<>]*)<.*$", "\\1",1); intext=0 }
href != "" && /<\/A$/ { for (i=level; i<lastlevel+1; i++) print "</sub>";
                        print "<sub name=\"" n "\" link=\"" href "\">";
                        href = "";
                        lastlevel = level }
ending != 1 && /^HREF=".*"$/ { href=gensub("^HREF=\"(.*)\"$","\\1",1);
                               n = "";
                               intext = 0 }
/^CLASS="NAVFOOTER"$/ { ending = 1 }

END { for (i=0; i<lastlevel; i++) print "</sub>";
      print "</chapters>";
      print "</book>" }
