#
# This file will have to be sourced where needed
#

# Unset all optional variables first to start from a clean state
unset NONUS             || true
unset FORCENONUSONCD1   || true
unset NONFREE           || true
unset CONTRIB           || true
unset EXTRANONFREE      || true
unset LOCAL             || true
unset LOCALDEBS         || true
unset SECURED           || true
unset SECURITY          || true
unset BOOTDIR           || true
unset BOOTDISKS         || true
unset SYMLINK           || true
unset COPYLINK          || true
unset MKISOFS           || true
unset MKISOFS_OPTS      || true
unset ISOLINUX          || true
unset EXCLUDE           || true
unset SRCEXCLUDE        || true
unset NORECOMMENDS      || true
unset NOSUGGESTS        || true
unset IMAGESUMS         || true
unset JIGDOCMD          || true
unset JIGDOTEMPLATEURL  || true
#unset JIGDOFALLBACKURLS || true
unset JIGDOINCLUDEURLS  || true
unset JIGDOSCRIPT       || true
unset JIGDO_OPTS        || true
#unset DEFBINSIZE        || true
#unset DEFSRCSIZE        || true
unset FASTSUMS          || true
unset PUBLISH_URL       || true
unset PUBLISH_NONUS_URL || true
unset PUBLISH_PATH      || true
unset UDEB_INCLUDE      || true
unset UDEB_EXCLUDE      || true
unset BASE_INCLUDE      || true
unset BASE_EXCLUDE      || true
#unset INSTALLER_CD      || true


# The debian-cd dir
# Where I am (hoping I'm in the debian-cd dir)
export BASEDIR=`pwd`

# Building wheezy cd set ...
export CODENAME=wheezy

if [ ! "$DI_CODENAME" ]
then
  export DI_CODENAME=$CODENAME
fi

# Version number, "2.2 r0", "2.2 r1" etc.
#export DEBVERSION="Lenny-DI-rc2"
export DEBVERSION="testing"

# Official or non-official set.
# NOTE: THE "OFFICIAL" DESIGNATION IS ONLY ALLOWED FOR IMAGES AVAILABLE
# ON THE OFFICIAL DEBIAN CD WEBSITE http://cdimage.debian.org
#export OFFICIAL="Unofficial"
export OFFICIAL="Official Snapshot"
#export OFFICIAL="Official RC"
#export OFFICIAL="Official"

# ... for arch  
if [ ! "$ARCH" ]
then
  export ARCH=`dpkg --print-installation-architecture`
fi

# IMPORTANT : The 4 following paths must be on the same partition/device.
#	      If they aren't then you must set COPYLINK below to 1. This
#	      takes a lot of extra room to create the sandbox for the ISO
#	      images, however. Also, if you are using an NFS partition for
#	      some part of this, you must use this option.
# Paths to the mirrors
if [ "$MIRROR"x = ""x ] ; then
    export MIRROR=/org/cdbuilder.debian.org/src/ftp/debian
fi

NUM_ARCHES=`echo $ARCH | wc -w`
if [ "$NUM_ARCHES"x = "1"x ] ; then
    OUTARCH=$ARCH
else
    OUTARCH=multi-arch
fi

# Path of the temporary directory
export TDIR=/org/cdbuilder.debian.org/src/deb-cd/tmp/"$INSTALLER_CD""$DI""$OUTARCH"

# Path where the images will be written
if [ "$OUT"x = ""x ] ; then
    export OUT=/org/cdbuilder.debian.org/dst/deb-cd/out/"$INSTALLER_CD""$DI""$OUTARCH"
fi

# Where we keep the temporary apt stuff.
# This cannot reside on an NFS mount.
export APTTMP=$TDIR/apt

# Do I want to have NONFREE merged in the CD set
# export NONFREE=1

# Do I want to have CONTRIB merged in the CD set
export CONTRIB=1

# Do I want to have NONFREE on a separate CD (the last CD of the CD set)
# WARNING: Don't use NONFREE and EXTRANONFREE at the same time !
# export EXTRANONFREE=1

# If you have a $MIRROR/dists/$CODENAME/local/binary-$ARCH dir with 
# local packages that you want to put on the CD set then
# uncomment the following line 
# export LOCAL=1

# If your local packages are not under $MIRROR, but somewhere else, 
# you can uncomment this line and edit to to point to a directory
# containing dists/$CODENAME/local/binary-$ARCH
# export LOCALDEBS=/home/joey/debian/va/debian

# If you want a <codename>-secured tree with a copy of the signed
# Release.gpg and files listed by this Release file, then
# uncomment this line
# export SECURED=1

# Where to find the security patches.  This directory should be the
# top directory of a security.debian.org mirror.
#export SECURITY="$TOPDIR"/debian/debian-security

# Sparc only : bootdir (location of cd.b and second.b)
# export BOOTDIR=/boot

# Symlink farmers should uncomment this line :
# export SYMLINK=1

# Use this to force copying the files instead of symlinking or hardlinking
# them. This is useful if your destination directories are on a different
# partition than your source files.
# export COPYLINK=1

# Options
#export MKISOFS="$BASEDIR/../mkisofs/usr/bin/mkisofs"
#export MKISOFS="$BASEDIR/../genisoimage/usr/bin/genisoimage"
#export MKISOFS="$BASEDIR/../genisoimage"
#export MKISOFS_OPTS="-jigdo-template-compress bzip2 -r -checksum_algorithm_iso md5,sha1,sha256,sha512"
#export MKISOFS_OPTS="-joliet-long -jigdo-template-compress bzip2 -r -checksum_algorithm_iso md5,sha1,sha256,sha512" #-checksum_algorithm_iso md5,sha1"
# export MKISOFS_OPTS="-r"		#For normal users
# export MKISOFS_OPTS="-r -F ."	#For symlink farmers
export MKISOFS="/home/93sam/xorriso"
export MKISOFS_OPTS="-as mkisofs -r -checksum_algorithm_iso md5,sha1,sha256,sha512"

# Override for i386,amd64,multi to use xorriso.
# BE AWARE: for multi-arch the order of the arches here will have to
# match the order they're declared in the build
#export i386_MKISOFS="/home/93sam/xorriso"
#export i386_MKISOFS_OPTS="-as mkisofs -r -checksum_algorithm_iso md5,sha1,sha256,sha512"
#export amd64_MKISOFS="/home/93sam/xorriso"
#export amd64_MKISOFS_OPTS="-as mkisofs -r -checksum_algorithm_iso md5,sha1,sha256,sha512"
#export amd64_i386_MKISOFS="/home/93sam/xorriso"
#export amd64_i386_MKISOFS_OPTS="-as mkisofs -r -checksum_algorithm_iso md5,sha1,sha256,sha512"
#export i386_amd64_source_MKISOFS="/home/93sam/xorriso"
#export i386_amd64_source_MKISOFS_OPTS="-as mkisofs -joliet-long -r -checksum_algorithm_iso md5,sha1,sha256,sha512"
export powerpc_MKISOFS="$BASEDIR/../genisoimage"
export powerpc_MKISOFS_OPTS="-joliet-long -jigdo-template-compress bzip2 -r -checksum_algorithm_iso md5,sha1,sha256,sha512" #-checksum_algorithm_iso md5,sha1"

# ISOLinux support for multiboot on CD1 for i386
export ISOLINUX=1

# uncomment this to if you want to see more of what the Makefile is doing
export VERBOSE_MAKE=1

# The maximum size allowed for an individual package, in bytes; if
# larger than this, it will be excluded (and all dependents, of
# course)
#export MAX_PKG_SIZE=600000000

# uncoment this to make build_all.sh try to build a simple CD image if
# the proper official CD run does not work
#ATTEMPT_FALLBACK=yes

if [ "$DISKTYPE"x = ""x ] ; then
    DISKTYPE=CD
fi
export DISKTYPE

# List of languages for which language tasks from tasksel should be
# included. See tasks/README.tasksel for further info.
export TASK_LANGLIST=tasksel_d-i.languages

# We don't want certain packages to take up space on CD1...
#export EXCLUDE1=exclude
# ...but they are okay for other CDs (UNEXCLUDEx == will be included on CD x if not already covered)
#export UNEXCLUDE2=unexclude-CD2
# Any packages listed in EXCLUDE but not in any UNEXCLUDE will be
# excluded completely.

# We also exclude some source packages
#export SRCEXCLUDE=exclude-src

# Set this if the recommended packages should be skipped when adding 
# package on the CD.  The default is 'false'.
export NORECOMMENDS=0

# Set this if the suggested packages should be skipped when adding 
# package on the CD.  The default is 'true'.
#export NOSUGGESTS=1

# Set to 1 to generate MD5 and SHA1 sums for generated images
export IMAGESUMS=1

# We may have to extract files from packages to put them onto the CD
# (e.g. bootloader files). If you make those packages (and their
# sources) available somewhere, list it here so that README.source
# can point to it
export ARCHIVE_EXTRACTED_SOURCES="http://cdimage.debian.org/cdimage/cd-sources/"

# Produce iso/jigdo files: specify how many iso/jigdo files should be
# produced in your set. If not set or when the value is "ALL" they will
# be created for all images. One of the variables can be set to zero if
# either iso or jigdo files are not wanted, but not both.
# Replaces the old "DOJIGDO" setting with something much more flexible.
#export MAXISOS=0
#export MAXJIGDOS=0

# Space-separated list of "include URLs" to add to the .jigdo file. 
# The included files are used to provide an up-to-date list of Debian
# mirrors to the jigdo _GUI_application_ (_jigdo-lite_ doesn't support
# "[Include ...]").
export JIGDOINCLUDEURLS="http://cdimage.debian.org/debian-cd/debian-servers.jigdo"
#
# $JIGDOTEMPLATEURL and $JIGDOINCLUDEURLS are passed to
# "tools/jigdo_header", which is used by default to generate the
# [Image] and [Servers] sections of the .jigdo file. You can provide
# your own script if you need the .jigdo file to contain different
# data.
#export JIGDOSCRIPT="myscript"

# If set, use the md5sums from the main archive, rather than calculating
# them locally
export FASTSUMS=1

# A couple of things used only by publish_cds, so it can tweak the
# jigdo files, and knows where to put the results.
# You need to run publish_cds manually, it is not run by the Makefile.
#export PUBLISH_URL="http://cdimage.debian.org/jigdo-area"
#export PUBLISH_NONUS_URL="http://non-US.cdimage.debian.org/jigdo-area"
#export PUBLISH_PATH="/home/jigdo-area/"

# Specify files and directories to *exclude* from jigdo processing. These
# files on each CD are expected to be different to those on the mirror, or
# are often subject to change. Any files matching entries in this list will
# simply be placed straight into the template file.
export JIGDO_EXCLUDE="'README*' /doc/ /md5sum.txt /.disk/ /pics/ 'Release*' 'Packages*' 'Sources*'"

# Specify the minimum file size to consider for jigdo processing. Any files
# smaller than this will simply be placed straight into the template file.
export JIGDO_OPTS="-jigdo-min-file-size 1024"

for EXCL in $JIGDO_EXCLUDE
do
    JIGDO_OPTS="$JIGDO_OPTS -jigdo-exclude $EXCL"
done

export IGNORE_MISSING_BOOT_SCRIPT=0

# Where to find the boot disks
#export BOOTDISKS=$TOPDIR/ftp/skolelinux/boot-floppies

# File with list of packages to include when fetching modules for the
# first stage installer (debian-installer). One package per line.
# Lines starting with '#' are comments.  The package order is
# important, as the packages will be installed in the given order.
#export UDEB_INCLUDE="$BASEDIR"/data/$CODENAME/udeb_include

# File with list of packages to exclude as above.
#export UDEB_EXCLUDE="$BASEDIR"/data/$CODENAME/udeb_exclude

# File with list of packages to include when running debootstrap from
# the first stage installer (currently only supported in
# debian-installer). One package per line.  Lines starting with '#'
# are comments.  The package order is important, as the packages will
# be installed in the given order.
#export BASE_INCLUDE="$BASEDIR"/data/$CODENAME/base_include

# File with list of packages to exclude as above.
#export BASE_EXCLUDE="$BASEDIR"/data/$CODENAME/base_exclude

# Only put the installer onto the cd (set NORECOMMENDS,... as well).
# INSTALLER_CD=0: nothing special (default)
# INSTALLER_CD=1: just add debian-installer (use TASK=debian-installer)
# INSTALLER_CD=2: add d-i and base (use TASK=debian-installer+kernel)
#export INSTALLER_CD=0

# Set to 1 to save space by omitting the release notes
# If so we will link to them on the web site.
export OMIT_RELEASE_NOTES=1

# Set this to override the defaul location
#export RELEASE_NOTES_LOCATION="http://www.debian.org/releases/$CODENAME"

case "$OFFICIAL"x in
       "Official"x)
               export OFFICIAL_VAL=2
               ;;
       "Official Beta"x|"Official Snapshot"x)
               export OFFICIAL_VAL=1
               ;;
       *)
               export OFFICIAL_VAL=0
               ;;
esac

# Base link for snapshot.debian.org or similar
# "SNAPDATETIME" will be replaced at runtime with the correct data
# Leave this unset to not add this entry
export SNAPURL=Debian=http://snapshot.debian.org/archive/debian/SNAPDATETIME/
