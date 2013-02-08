#!/bin/sh
FILE=$1
if [ "$FILE" = "" -o ! -f "$FILE" ]; then
        echo "Please specify a .c file"
        exit 1
fi

gcc -MM -MG $FILE |
  sed -e 's@^\(.*\)\.o:@\1.o \1.d:@'
