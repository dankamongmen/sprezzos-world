#!/bin/sh

# (C) 2007 Canonical Ltd.
# Author: Alexander Sack <asac@jwsdot.com>
# License: GNU General Public License, version 2 or any later version

if test x$NIF_FILE = x; then
   NIF_FILE=/etc/network/interfaces
fi

auto_ifs=$(cat $NIF_FILE | \
    egrep "^auto|^allow-" | \
    sed -e 's/auto//' | \
    sed -e 's/allow-[^ ].* //')

ifaces_to_disable=""

echo Auto interfaces found: $auto_ifs

# iterate over all auto interfaces
for i in $auto_ifs; do
  IFS_old=$IFS; IFS=""

  NIF_FILE_content=$(cat $NIF_FILE | \
      sed -e 's/^[ \t]*auto.*$//' | \
      sed -e 's/^[ \t]*allow-.*$//' | \
      sed -e 's/^[ \t]*#.*$//' | grep -v ^$)

  # '--' is inserted by grep -A1 if there are multiple iface blocks
  lines=$(echo $NIF_FILE_content | grep -A1 "^iface.*$i.*dhcp" | grep -v '\--')
  IFS="
"

  # if there is no iface line for that interface, we would still get a line
  # count of 1 ... so use word_count 0 below to exclude ifaces that have no
  # configuration at all.
  word_count=$(echo $lines | wc -w)
  line_count=0
  for line in $lines; do
      nulled_line=$(echo "$line" | sed -e 's/[# ]//' | grep -v ^iface)
      if test x$nulled_line != x; then
	  line_count=$(expr $line_count + 1)
      fi
  done

  if test $line_count -eq 0 -a $word_count -gt 0; then
     ifaces_to_disable="$ifaces_to_disable $i"
     echo iface to disable = $i
  fi
  IFS=$IFS_old
done

backup_suffix=0
while test -e ${NIF_FILE}.bak-${backup_suffix}; do
   backup_suffix=$(expr $backup_suffix + 1)
done

if [ -n "$ifaces_to_disable" ]; then
    cp $NIF_FILE "$NIF_FILE.bak-${backup_suffix}"
    for i in $ifaces_to_disable; do
	echo -n "Disabling interface: $i ... "
	sed -i -e "s/^\([ \t]*iface.*[ \t]$i[ \t].*\)$/#NetworkManager#\1/" $NIF_FILE
	echo done.
    done
fi

