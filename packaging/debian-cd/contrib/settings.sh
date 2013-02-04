export TRACE=/org/cdbuilder.debian.org/src/ftp/debian/project/trace/pettersson.debian.org
export ARCH_DI_DIR=/org/cdbuilder.debian.org/src/deb-cd/d-i
export HOSTNAME=`hostname -f`

export PUBDIR=/org/cdbuilder.debian.org/dst/deb-cd

export MIRROR=/org/cdbuilder.debian.org/src/ftp/debian
export BASEDIR=~/build.wheezy/debian-cd
export MKISOFS=~/build.wheezy/mkisofs/usr/bin/mkisofs
if [ "$DATE"x = ""x ] ; then
    export DATE=`date -u +%Y%m%d`
fi

export EXTRACTED_SOURCES=/mnt/nfs-cdimage/cd-sources
