# Installation paths for use in debian/rules of Octave-related packages
# Written by Rafael Laboissiere <rafael@debian.org>

MDIR = $(shell octave-config --print LOCALFCNFILEDIR)
OCTDIR = $(shell octave-config --print LOCALOCTFILEDIR)
