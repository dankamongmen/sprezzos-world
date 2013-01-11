#!/bin/sh

pkg_name="$1"
ext_dir="$2"

for f in `find "$ext_dir" -name '*.rb' -type f -not -name 'extconf.rb' -not -path ' */lib/*'`
do
  dh_installexamples -p"$pkg_name" $f
done

