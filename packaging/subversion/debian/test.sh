#!/bin/sh
# This is the meager start to a script that will test the package.

SVN=/usr/bin/svn
SVNADMIN=/usr/bin/svnadmin

# create test repository
rm -rf /var/lib/svn/*

$SVNADMIN create --fs-type fsfs /var/lib/svn
$SVN mkdir -m '' file:///var/lib/svn/repos

# uncomment repos stuff

/etc/init.d/apache2 restart

