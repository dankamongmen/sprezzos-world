#! /usr/bin/gawk -f

BEGIN {
	sep = "============================================================================="
}

FILENAME ~ /debian$/ {
	nextfile
}

/^== END PATCH$/ {
	print ""
	nextfile
}

FNR == 1 {
	print FILENAME "\n" substr(sep, 0, length(FILENAME)) "\n"
	next
}

{
	print
}
