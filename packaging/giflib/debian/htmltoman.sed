/doctype/ {
 N
 N
 s/.*/.\\" Process this file with\
.\\" groff -man -Tascii foo.1\
.\\"/
}

s/<TITLE>\(.*\)/.TH \1 1 "giflib-tools"/g

/link/d

/<BODY>/ {
 N
 N
 d
}

/<CENTER><H1>/ {
 N
 s/\n/ - /
} 

 s/<CENTER><H1>/.SH NAME\
/

/<OL>/d

/<LI>/ {
 N
 s/<LI>\n\(.*\)/ - \1/g
}

s/<H1>/.SH /g
s/Usage:/USAGE/g
s/Memory required:/MEMORY REQUIRED/g
s/Options:/OPTIONS/
s/Interactive mode:/INTERACTIVE MODE/g
s/Notes:/NOTES/g
s/Bugs:/BUGS/g
s/Author:/AUTHOR/g

s/<CENTER><H2>/.SH /g

/<pre>/ {
 N
 s/<pre>\n\(.*\)/.B \1/g 
}

/<DL>/d

s/<DT> *\([^ ]*\)$/.IP \1/g
s/<DT> *\(.*\)$/.IP "\1/g

s/<DD>/\
/g
s/ +/ /g
s/^[ 	]*//g

s/<ADDRESS>Eric S. Raymond <A HREF="mailto:esr@thyrsus.com">&lt;esr@snark.thyrsus.com&gt;/Man page created by T.Gridel <tgridel@free.fr>, originally written by Eric S. Raymond <esr@thyrsus.com>/g

s/<\/.*>//g
s/<CODE>//g
s/<P>//g
s/<p>//g
s/<em>//g
s/<UL>//g
s/<HR>//g

s/-/\\-/g
s/&lt;/</g
s/&gt;/>/g
s/&quot;/""/g
