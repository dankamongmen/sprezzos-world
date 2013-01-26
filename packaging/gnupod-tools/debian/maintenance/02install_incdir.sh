#!/bin/sh

set -e

dir="$1"
version="$2"

file="$dir/tools/gnupod_install.pl"
test -s "$file"

sed -e 's,^#Check .*,my $include_dir = "/usr/share/perl5";,g;s,$INC\[0\],$include_dir,g' "$file" > "$file.new"

chmod --reference="$file" "$file.new"
mv -f "$file.new" "$file"

exit 0
