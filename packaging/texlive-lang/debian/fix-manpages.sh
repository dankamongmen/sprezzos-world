#!/bin/bash
# fix-manpages.sh
#
# Several manpages in TeX live have smaller or bigger flaws like:
# - international characters from latin1 are not written in their
#   encoded form (ä instead of \[:a])
# - problems in the first line of the manpage defining wrong section
#   or syntactical wrong files
# We fix all these errors on the installed man pages, otherwise lintian
# complains.
#
# Norbert Preining, 2005
# GPL

set -e

tmpfile=`mktemp`

for i in `find debian/ -wholename 'debian/texlive-*/usr/share/man/man?/*' -type f` ; do
	bn=`basename $i`
	case "$bn" in 
	ttf2pt1.1)
		# fix ttf2pt1.1 first line error
		(echo '.TH "ttf2pt1" "1" "Nov 2005" "TeX live" "TeX live"' ; tail --lines=+2 $i ) > $tmpfile
		cat $tmpfile > $i
		;;
	vlna.1)
		# fix the NAZEV to NAME in vlna.1
		cat $i | sed -e 's/^\.SH NAZEV/.SH NAME/' > $tmpfile
		cat $tmpfile > $i
		;;
	makeindex.1)
		# fix section
		cat $i | sed -e 's/^\.TH MAKEINDEX 1L /.TH MAKEINDEX 1 /' > $tmpfile
		cat $tmpfile > $i
		;;
	detex.1)
		# fix section
		cat $i | sed -e 's/^\.TH DETEX 1L /.TH DETEX 1 /' > $tmpfile
		cat $tmpfile > $i
		;;
	dvi2tty.1)
		# fix section
		cat $i | sed -e 's/^\.TH DVI2TTY Local /.TH DVI2TTY 1 /' > $tmpfile
		cat $tmpfile > $i
		;;
	dvidvi.1)
		# fix section
		cat $i | sed -e 's/^\.TH DVIDVI L /.TH DVIDVI 1 /' > $tmpfile
		cat $tmpfile > $i
		;;
	fmtutil.1)
		# fix section
		cat $i | sed -e 's/^\.TH "fmtutil" "8"/.TH "fmtutil" "1"/' > $tmpfile
		cat $tmpfile > $i
		;;
	texlinks.1)
		# fix section
		cat $i | sed -e 's/^\.TH "texlinks" "8"/.TH "texlinks" "1"/' > $tmpfile
		cat $tmpfile > $i
		;;
	tie.1)
		# fix section
		cat $i | sed -e 's/^\.TH TIE 1L /.TH TIE 1 /' > $tmpfile
		cat $tmpfile > $i
		;;
	esac
	cat $i | sed 	-e "s/\ó/\\['o]/g" 		\
			-e "s/\é/\\['e]/g"		\
			-e 's/\ü/\\[:u]/g'		\
			-e 's/\ä/\\[:a]/g'		\
			-e 's/\ö/\\[:o]/g'		\
			-e 's/\©/\\[co]/g'		\
				> $tmpfile
	cat $tmpfile > $i
done

rm $tmpfile

