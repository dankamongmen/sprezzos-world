### This is sed code: "s" lines must end with a tab! ###
# Also beware of leading spaces in some text strings...

# This script is used for all conffiles.

## Use system-wide config and underlay
# ARRGH - read-only underlay not supported currently :-(
#s	^\([[:space:]]*data_underlay_dir[[:space:]]\+=\).*	\1 '/usr/share/moin/underlay'	

# Disable sample sites
s	^\([[:space:]]*\)\(("\(moinmaster\|moinmoin\)",.*\)	\1\#\2	

# Normalize whitespace so upstream changes to it don't provoke
# changed-conffile prompts.
# 1. Convert tabs to four spaces (the indendation used by upstream).
s/	/    /g
# 2. Eliminate trailing whitespace at ends of lines.
s/ \+$//g
