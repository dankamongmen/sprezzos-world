#!/bin/sh

ruby_libdir=usr/lib/ruby/1.8
pkg_name="$1"
ext_dir="$2"

if [ -d "$ext_dir/lib" ]
then
  for f in `find "$ext_dir/lib" -type f -name '*.rb'`
  do
    dh_movefiles -p"$pkg_name" "$ruby_libdir"/`expr "$f" : "$ext_dir/lib/\(.*\)"`
  done
fi

