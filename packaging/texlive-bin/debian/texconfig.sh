#!/bin/sh
#
# texconfig replacement script in Debian, which does not ship the
# original texconfig anymore
#
# Copyright (C) 2012 Norbert Preining, Julian Gilbey
#
# Public domain
#

cat <<'EOF'
The original texconfig is not shipped on Debian due to the way
texconfig handles config files. Furthermore, all of the texconfig 
features can be performed in other ways on a Debian system.

EOF

if [ "$1" = '--help' ]
then
  cat <<'EOF'
conf
        Not needed

dvipdfm paper
        Use paperconf

dvips add PRINTERNAME
        Use
		touch /etc/texmf/dvips/config/config.PRINTERNAME

dvips del PRINTERNAME
	For a printer you have added:
        	rm /etc/texmf/dvips/config/config.PRINTERNAME
	You should not remove printers that you
	have not created, or that are shipped by Debian

dvips mode
        Not supported

dvips [-P PRINTER] mode MODE
dvips [-P PRINTER] offset x-OFFSET,y-OFFSET
dvips [-P PRINTER] printcmd CMD
        Make a copy of /usr/share/texlive/texmf/dvips/config/config.PRINTER
        in /etc/texmf/dvips/config/ and edit the file
        (but that should not be necessary)

faq
        Not needed

font vardir DIR, font ro, font rw
        Please read section 2.5, font caching, in the TeX-on-Debian
        documentation in /usr/share/doc/tex-common/.

formats
hyphen FORMAT
        Should not be used on Debian, see TeX-on-Debian, update-fmtutil
        for details.

init [format]
        Run
                fmtutil-sys --all
        or
                fmtutil-sys --byfmt format

mode-list
        not supported

mode MODE
        Edit /etc/texmf/web2c/mktex.cnf

paper [a4|letter]
pdftex paper [a4|letter]
xdvi paper PAPER
        Use paperconf

rehash
        Run
		mktexlsr
EOF
else
  cat <<'EOF'
To read a list of replacements for the texconfig features on Debian,
run texconfig --help
EOF
fi

