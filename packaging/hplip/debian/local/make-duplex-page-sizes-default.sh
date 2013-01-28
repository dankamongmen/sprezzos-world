#!/bin/sh
f=$1

echo "Renaming page sizes in $f"

# Correct entries for small margin paper sizes which were already there
perl -p -i -e 's:SM/:.SM/:g' $f
perl -p -i -e 's:SmallMargins:Small Margins:g' $f

# Find which page sizes support duplex
duplexsizes=`grep -v '^//' $f | grep CustomMedia | grep '\.Duplex' | cut -d " " -f 4 | cut -d "/" -f 1 | cut -d '"' -f 2 | perl -p -e "s/\.Duplex//" | sort | uniq`

# Rename CustomMedia entries: duplex sizes -> standard sizes, standard sizes ->
# small margin sizes
echo -n "  Renaming paper size definitions"
finished=0
while [ $finished = 0 ]; do
    perl -e 'my $content = join("", <>); $content =~ s:(CustomMedia\s*\")([^\s\.\/]+)(\/([^\"]+?\s+|))(\S+\"([^\n]*\n){0,4}\s*CustomMedia\s*\")(\2)(.Duplex)(\/[^\"]*?\s*)(AutoDuplex ):\1\2.SM\3Small Margins \5\7\9:smgi; print $content' $f > $f.new 2>/dev/null
    if diff $f $f.new >/dev/null 2>/dev/null; then
	rm $f.new
	echo " Done"
	finished=1
    else
	rm $f && mv $f.new $f
	echo -n "."
    fi
done

# Update the UIConstraints entries of the paper sizes which support duplex
for size in $duplexsizes; do
    echo -n "  Updating UI Constraints for $size:"
    perl -p -e 's/(UIConstraints\s*\"\s*\*PageSize\s+)('"$size"')(\s+\*Duplex\s*\")/\1\2.SM\3/i' $f > $f.new 2>/dev/null
    if diff $f $f.new >/dev/null 2>/dev/null; then
	rm $f.new
	echo " No changes"
	finished=1
    else
	rm $f && mv $f.new $f
	echo " Done"
    fi
done

# Remove remaining ".Duplex" from paper sizes in UIConstraints
perl -p -i -e 's:\.Duplex::g' $f
