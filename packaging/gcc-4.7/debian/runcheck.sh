#! /bin/sh

mkdir -p build

abi=${CC##* }
base=build/runcheck$abi

cat >$base.c <<EOF
#include <stdio.h>
int main()
{
	printf("$abi");
	return 0;
}
EOF


if m=$(${CC:-gcc} -o $base $base.c 2>&1); then
    m=$($base 2>&1)
    printf "%s" ${m#* } > $base.out
    printf "%s" ${m#* }
else
    printf "%s" ${m##*:} > $base.out
    printf "%s" ${m##*:}
fi
