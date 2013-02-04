#! /bin/sh
set -e

dir=${LOCALDEBS:-$MIRROR}

if [ ! -d "$dir" ]; then
	echo "error: directory '$dir' does not exist"
	echo "Check that the MIRROR or LOCALDEBS variable is set correctly."
	exit 1
fi
cd $dir

DI=
if [ "$1" = "-i" ]; then
	DI=1
	shift
fi

distr=$1
arch=$2
if [ -z "$distr" ] || [ -z "$arch" ]; then
	echo "Usage: $(basename $0) [-i] <codename> <arch>"
	exit 1
elif [ ! -d dists/$distr/local/ ]; then
	echo "No local repository matching '$distr' was found"
	exit 1
fi

if [ -z "$DI" ]; then
	repo="dists/$distr/local/binary-$arch"
else
	repo="dists/$distr/local/debian-installer/binary-$arch"
fi
[ -d $repo ] || mkdir -p $repo
echo Creating Packages file for $repo...
apt-ftparchive packages $repo | gzip >$repo/Packages.gz
