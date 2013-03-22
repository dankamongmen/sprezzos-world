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


if ${CC:-gcc} -o $base $base.c 2>/dev/null; then
  if [ "$($base 2>&1)" = "$abi" ]; then
    printf "%s" $abi > $base.out
    printf "%s" $abi
  fi
fi
