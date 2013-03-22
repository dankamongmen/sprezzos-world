#!/bin/sh
LC_COLLATE=C
export LC_COLLATE

echo Updating library manifest...
for package in $@; do
    manifest=debian/$package.manifest
    # file format version number
    echo "  VERSION 2" > $manifest; \
    for i in $(find $(pwd)/debian/$package -type f \
         \( -name '*.so' -or -name '*.so.*' \))
    do
	version=`dpkg-parsechangelog |awk '/Version:/ { print $2 }'`
	objdump -p $i | sed -n -e"s/\( SONAME.*\)/\\1 $version/p" >> $manifest
	objdump -T $i | grep -E ' [gw] .*\.text|__cxa_pure_virtual' \
	| grep -vE '\b((__gmon_start__|_ftext)\b|_(rest|save)[fg]pr)' \
	| cut -b34- | c++filt | sort >> $manifest
    done
done
