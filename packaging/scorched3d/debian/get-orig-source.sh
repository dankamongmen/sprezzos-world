#!/bin/sh

set -e
set -x

VER=43.2a

svn co https://scorched3d.svn.sourceforge.net/svnroot/scorched3d/tags/\
${VER}/scorched scorched3d-${VER}.dfsg

tar --exclude=".svn" \
	--exclude="*.dll" \
	--exclude="data/avatars" \
	-czf scorched3d_${VER}.dfsg.orig.tar.gz scorched3d-${VER}.dfsg
