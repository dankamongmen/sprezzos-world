### This is sed code: "s" lines must end with a tab! ###
# Also beware of leading spaces in some text strings...

# This file is used for all server scripts

# Enable system-wide farmconfig, and avoid wikiconfig
s	^\([[:space:]\#]*\)\(sys.path.insert([[:digit:]]\+,[[:space:]]\+\)'/path/to/wikiconfigdir'\().*\)	#\2'/etc/moin'\3	
s	^\([[:space:]\#]*\)\(sys.path.insert([[:digit:]]\+,[[:space:]]\+\)'/path/to/farmconfigdir'\().*\)	\2'/etc/moin'\3	
