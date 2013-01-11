#!/bin/sh
# Moinmoin Desktop Edition is a set of script to quickly *test* Moinmoin
# (see http://moinmo.in/DesktopEdition )

# Installation instructions:
mkdir --mode=750 mywiki
cd mywiki
cp /usr/share/doc/python-moinmoin/examples/desktop-edition/* ./
cp -r /usr/share/moin wiki

# Then start it:
python wikiserver.py
# And finally, open a web browser: http://localhost:8080/


# SECURITY WARNING: Anyone who connects to 'Desktop Edition' can create a
# superuser account then execute arbitrary command (using 'your' credentials).
# However, the wiki can only be accessed localy (using 127.0.0.1)... So make
# sure that no one else can logon your computer while Moinmoin Desktop
# Edition is running.
