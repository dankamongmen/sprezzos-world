# emacs: -*- mode: shell-script; c-basic-offset: 4; tab-width: 4; indent-tabs-mode: t -*- 
# ex: set sts=4 ts=4 sw=4 noet:

#
# Primitive script to assure presence of the path to CMTK binaries
# Just source it if you are planing to use CMTK
#

addpath () {
	echo $PATH | tr ':' '\n' | grep -q "^$1\$" \
		|| export PATH=$1:$PATH;
}

addpath /usr/lib/cmtk/bin
