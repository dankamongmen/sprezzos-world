#! /usr/bin/gawk -f

BEGIN {
	skip=0
	warn=0
}

/^-(FAIL|ERROR|UNRESOLVED|WARNING)/ {
	next
}

# only compare gcc, g++, g77 and objc results
/=== treelang tests ===/ {
	skip=1
}

# omit extra files appended to test-summary
/^\+Compiler version/ {
	skip=1
}

skip == 0 {
	print
	next
}

/^\+(FAIL|ERROR|UNRESOLVED|WARNING)/ {
	warn=1
}

END {
	exit warn
}
