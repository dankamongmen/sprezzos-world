#!/bin/sh

dir="$1"
version="$2"

find "$dir/src" -name "*.pl" -printf "%f\n" | sed -e 's,\.pl$,,g' | while read pname; do
	if [ ! -e "$dir/src/$pname.pl" ]; then
		echo "E: $dir/src/$pname.pl missing"
		exit 1
	fi
	manfile="$dir/man/$pname.pl.1.gz"
	if [ -e "$manfile" ]; then
		zcat "$manfile" | sed -e "s,\"[[:digit:]][[:digit:]]\",\"1\",g;s,\"[[:digit:]][[:digit:]]\",\"1\",g;s,$pname\.pl,$pname,g;s,$pname [[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+,$pname $version,g;s,manual page for gnupod_INIT.*,prepares a 'virgin' iPod for GNUpod,g;s,manual page for gnupod_addsong.*,upload music to the iPod,g;s,manual page for gnupod_check.*,check for lost/zombie files on the iPod,g;s,manual page for gnupod_otgsync.*,helper script for On-The-Go stuff,g;s,manual page for mktunes.*,convert GNUtunesDB's XML into iTunesDB format,g;s,manual page for tunes2pod.*,convert an iTunesDB into GNUtunesDB's XML,g;s,manual page for gnupod_search.*,search and remove files,g" | gzip -c --best > "$manfile.new"
		chmod --reference="$manfile" "$manfile.new"
		mv -f "$manfile.new" "$manfile"
		echo "Patched $manfile"
	else
		docbook-to-man debian/gnupod_convert_ALL.sgml | gzip -c --best > "$manfile"
		echo "Created $manfile"
	fi
	for f in `grep -rl "$pname\.pl" "$dir"`; do
		sed -e "s,$pname\.pl,$pname,g" "$f" > "$f.new"
		chmod --reference="$f" "$f.new"
		diff -Naur "$f" "$f.new"
		mv -f "$f.new" "$f"
#		echo "Patched $f"
	done
done

