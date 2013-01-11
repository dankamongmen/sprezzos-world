#!/bin/bash
ruby="$1"
target_dir="$2"

for f in `find "$target_dir" -type f 2>/dev/null`
do
  textflag=0
  filetype="`file -b $f`"
  for ft in $filetype
  do
    if [ "${#ft}" -lt 4 ]
    then
      continue
    fi
    if [ "${ft:0:4}" == "text" ]
    then
      textflag=1
      break
    fi 
  done

  if [ "$textflag" -eq 0 ]
  then
    continue
  fi

  cp -pf $f $f.tmp
  sed -e '1,1s,^#![ 	]*\([^ 	]*\)/\(ruby\|env ruby\)$,#!/usr/bin/'$ruby',' \
      -e '1,1s,^#![ 	]*\([^ 	]*\)/\(wish\|perl\)$,#!/usr/bin/\2,' < $f > $f.tmp
  if ! cmp $f $f.tmp >/dev/null
  then
      mv -f $f.tmp $f
  else
      rm -f $f.tmp
  fi
done
