#!/bin/sh

ruby_libdir=$1
pkg_name=$2
ext_dir=$3

if [ -d "$ext_dir/lib" ]
then
  for f in `find "$ext_dir/lib" -type f -name '*.rb'`
  do
    dh_movefiles -p"$pkg_name" "$ruby_libdir"/`expr "$f" : "$ext_dir/lib/\(.*\)"`
  done
fi

