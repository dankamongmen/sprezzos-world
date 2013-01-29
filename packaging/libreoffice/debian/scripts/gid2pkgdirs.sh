#!/bin/sh

## create split package dirs out of LibreOffices gid_* files
## copied from former ooo-build/libreoffice-build package-ooo
## (c) 2005 Chris Halls <halls@debian.org>
## (c) 2005-2011 Rene Engelhard <rene@debian.org>

export OODESTDIR=$DESTDIR

cd $DESTDIR

echo "Copying gid files...."
rm gid_*
cp $DESTDIR/../../file-lists/orig/gid_* .

echo "Creating package directories..."

test -d pkg && rm -r pkg || :

# Create package tree (needed by Debian's dpkg)
# create_package_directory <list_file> <directory_name>
create_package_directory()
{
listfile=$1
directory="$2"
perl -nl \
        -e " if(/^%dir (.*)/)
                        {system('mkdir', '-p', '-m', '755', \"$directory\".\$1);}
                else
                        {rename('./'.\$_, \"$directory\".\$_);}
                " \
        $listfile
}

# move_wrappers <directory_name> <name> [...]
move_wrappers()
{
directory=$1
shift
mkdir -m755 -p "$directory"/usr/bin
while test -n "$1"; do
        mv usr/*bin/"$1$BINSUFFIX" "$directory"/usr/bin
        shift
done
}

create_package_directory gid_Module_Root_Ure_Hidden             pkg/ure
create_package_directory gid_Module_Root                        pkg/libreoffice-common
create_package_directory gid_Module_Root_Brand                  pkg/libreoffice-common
# FIXME: This is added to gid_Module_Root_Brand but doesn't get installed....
mkdir -p pkg/libreoffice-common/usr/share/mimelnk/application
mv ./usr/share/mimelnk/application/* \
	pkg/libreoffice-common/usr/share/mimelnk/application
# done by dh_installman
#mkdir -p pkg/libreoffice-common/usr/share/man/man1
#mv usr/share/man/man1/libreoffice$BINSUFFIX.1.gz \
#	pkg/libreoffice-common/usr/share/man/man1
#for i in ./usr/share/man/man1/*; do \
#	if [ "$i" = "unopkg.1.gz" -o "$i" = "lofromtemplate.1.gz" \
#	   -o "$i" = "loffice.1.gz" ]; then p=common; \
#	else p=`basename $i .1.gz | sed -e s/^lo//`; \
#	fi
#	mkdir -p pkg/libreoffice-$p/usr/share/man/man1
#	mv $i \
#		pkg/libreoffice-$p/usr/share/man/man1
#done
for i in ./usr/share/applications/*.desktop; do \
	if [ "`basename $i`" = "libreoffice-startcenter.desktop" ]; then p=libreoffice-common; \
	elif [ "`basename $i`" = "libreoffice-xsltfilter.desktop" ]; then p=libreoffice-common; \
	elif [ "`basename $i`" = "libreoffice-binfilter.desktop" ]; then p=libreoffice-filter-binfilter; else \
	p=`basename $i .desktop`; fi
	mkdir -p pkg/$p/usr/share/applications
	mv $i \
		pkg/$p/usr/share/applications
done
mkdir -p pkg/libreoffice-common/usr/share
mv ./usr/share/icons \
	pkg/libreoffice-common/usr/share
mv ./usr/share/application-registry \
	pkg/libreoffice-common/usr/share
mv ./usr/share/mime* \
	pkg/libreoffice-common/usr/share

create_package_directory gid_Module_Root_Files_Images           pkg/libreoffice-common
create_package_directory gid_Module_Oo_Linguistic               pkg/libreoffice-common
create_package_directory gid_Module_Optional_Xsltfiltersamples  pkg/libreoffice-common
create_package_directory gid_Module_Optional_Javafilter         pkg/libreoffice-common
create_package_directory gid_Module_Optional_Binfilter          pkg/libreoffice-filter-binfilter
create_package_directory gid_Module_Optional_Grfflt             pkg/libreoffice-draw
create_package_directory gid_Module_Prg_Calc_Bin                pkg/libreoffice-calc
create_package_directory gid_Module_Prg_Math_Bin                pkg/libreoffice-math
create_package_directory gid_Module_Prg_Draw_Bin                pkg/libreoffice-draw
create_package_directory gid_Module_Prg_Wrt_Bin                 pkg/libreoffice-writer
create_package_directory gid_Module_Prg_Impress_Bin             pkg/libreoffice-impress
create_package_directory gid_Module_Prg_Base_Bin                pkg/libreoffice-base
create_package_directory gid_Module_Brand_Prg_Calc              pkg/libreoffice-calc
create_package_directory gid_Module_Brand_Prg_Math              pkg/libreoffice-math
create_package_directory gid_Module_Brand_Prg_Draw              pkg/libreoffice-draw
create_package_directory gid_Module_Brand_Prg_Wrt               pkg/libreoffice-writer
create_package_directory gid_Module_Brand_Prg_Impress           pkg/libreoffice-impress
create_package_directory gid_Module_Brand_Prg_Base              pkg/libreoffice-base
create_package_directory gid_Module_Optional_Pyuno              pkg/python-uno
create_package_directory gid_Module_Optional_Extensions_Script_Provider_For_Python		pkg/libreoffice-script-provider-python
create_package_directory gid_Module_Optional_Gnome              pkg/libreoffice-gnome
create_package_directory gid_Module_Optional_Kde                pkg/libreoffice-kde
create_package_directory gid_Module_Root_Files_2                pkg/libreoffice-common
create_package_directory gid_Module_Root_Files_3                pkg/libreoffice-common
create_package_directory gid_Module_Root_Files_4                pkg/libreoffice-common
create_package_directory gid_Module_Root_Files_5                pkg/libreoffice-common
create_package_directory gid_Module_Root_Files_6                pkg/libreoffice-common
create_package_directory gid_Module_Root_Files_7                pkg/libreoffice-common
create_package_directory gid_Module_Optional_OGLTrans		pkg/libreoffice-ogltrans
create_package_directory gid_Module_Root_SDK                    pkg/libreoffice-dev
create_package_directory gid_Module_Optional_Extensions_Script_Provider_For_Beanshell	pkg/libreoffice-script-provider-bsh
create_package_directory gid_Module_Optional_Extensions_Script_Provider_For_Javascript  pkg/libreoffice-script-provider-js
create_package_directory gid_Module_Optional_Extensions_MEDIAWIKI	pkg/libreoffice-wiki-publisher
create_package_directory gid_Module_Optional_Extensions_MINIMIZER	pkg/libreoffice-presentation-minimizer
create_package_directory gid_Module_Optional_Extensions_MySQLConnector  pkg/libreoffice-mysql-connector
create_package_directory gid_Module_Optional_Extensions_PDFIMPORT	pkg/libreoffice-pdfimport
create_package_directory gid_Module_Optional_Extensions_PRESENTER_SCREEN	pkg/libreoffice-presenter-console
create_package_directory gid_Module_Optional_Extensions_REPORTDESIGN	pkg/libreoffice-report-builder
create_package_directory gid_Module_Optional_PostgresqlSdbc     pkg/libreoffice-sdbc-postgresql
move_wrappers pkg/libreoffice-common soffice unopkg
move_wrappers pkg/libreoffice-common libreoffice loffice lofromtemplate
move_wrappers pkg/libreoffice-base lobase
move_wrappers pkg/libreoffice-writer lowriter loweb
move_wrappers pkg/libreoffice-calc localc
move_wrappers pkg/libreoffice-impress loimpress
move_wrappers pkg/libreoffice-math lomath
move_wrappers pkg/libreoffice-draw lodraw

for l in `echo $OOO_LANGS_LIST`; do
        for p in Impress Draw Math Calc Base Writer; do
                create_package_directory  gid_Module_Langpack_${p}_`echo $l | sed -e s/-/_/g`   pkg/libreoffice-l10n-$l
        done
        create_package_directory gid_Module_Langpack_Basis_`echo $l | sed -e s/-/_/g`   pkg/libreoffice-l10n-$l
        create_package_directory gid_Module_Langpack_Brand_`echo $l | sed -e s/-/_/g`   pkg/libreoffice-l10n-$l
        create_package_directory gid_Module_Langpack_Resource_`echo $l | sed -e s/-/_/g`        pkg/libreoffice-l10n-$l
	if [ -f gid_Module_Helppack_Help_`echo $l | sed -e s/-/_/g` ]; then
        	create_package_directory gid_Module_Helppack_Help_`echo $l | sed -e s/-/_/g`    pkg/libreoffice-help-$l
	fi
        if [ -f gid_Module_Optional_Binfilter ]; then
                if [ "$l" = "en-US" ]; then
                        create_package_directory gid_Module_Langpack_Binfilter_en_US    pkg/libreoffice-filter-binfilter
                else
                        create_package_directory gid_Module_Langpack_Binfilter_`echo $l | sed -e s/-/_/g`    pkg/libreoffice-l10n-$l
                fi
        fi
        # some help files are in _Langpack_{Writer,Impress,...}_<lang>
        # move them from -l10n to -help
        if [ "$l" = "en-US" ]; then d=en; else d=$l; fi
        mv pkg/libreoffice-l10n-$l/$OOINSTBASE/help/$d/* \
                pkg/libreoffice-help-$l/$OOINSTBASE/help/$d && \
        rmdir pkg/libreoffice-l10n-$l/$OOINSTBASE/help/$d
done
	
# Move all libraries and binaries from -common to -core
if [ ! -d $OODESTDIR/pkg/libreoffice-core/$OOINSTBASE/program ]; then \
mkdir -p $OODESTDIR/pkg/libreoffice-core/$OOINSTBASE/program; \
fi &&
( cd pkg/libreoffice-common/$OOINSTBASE/program
  find -maxdepth 1 \
     -regex '\./\(.*\.so.*\|.*\.bin\|pagein\|nsplugin\|kdefilepicker\|msfontextract\|.*\.rdb\|javaldx\|oosplash\|uri-encode\)' \
     -exec mv {} $OODESTDIR/pkg/libreoffice-core/$OOINSTBASE/program \;
);

mkdir -p pkg/libreoffice-common/etc/bash_completion.d
mv etc/bash_completion.d/libreoffice$BINSUFFIX.sh \
	pkg/libreoffice-common/etc/bash_completion.d

mv .$OOINSTBASE/program/java-set-classpath \
	pkg/libreoffice-common/$OOINSTBASE/program
if echo $OOO_LANGS_LIST | grep -q en-US; then
        for i in forms/resume.ott officorr/project-proposal.ott; do \
                mkdir -p pkg/libreoffice-common/$OOINSTBASE/share/template/en-US/`dirname $i`; \
                mv .$OOINSTBASE/share/template/en-US/$i \
                        pkg/libreoffice-common/$OOINSTBASE/share/template/en-US/$i; \
        done; \
fi

# Warn for any remaining files
find . -path './pkg' -prune -o -not -name 'gid_Module_*' -not -type d -exec echo "File not packaged: {}" \;

