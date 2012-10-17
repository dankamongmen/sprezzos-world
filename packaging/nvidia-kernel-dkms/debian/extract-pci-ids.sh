#!/bin/sh
# Based on the nvidia_supported script from the nvidia-current package in
# Ubuntu maverick.
set -e


[ -n "$1" ] || {
  echo "USAGE: $0 path/to/nv/nv-kernel.o" >&2
  exit 1
}

device_ids() {
  local object="$1"

  local ret=1

  local symbols="$(mktemp)"
  local readme_list="$(mktemp)"
  local object_list="$(mktemp)"
  local diff="$(mktemp)"

  # The README.txt contains a partial list only
  sed -e '0,/A. Supported\|APPENDIX A: SUPPORTED/d' \
    -e '0,/Appendix A. Supported\|APPENDIX A: SUPPORTED/d' \
    -e '0,/^Below\|APPENDIX B/{/ 0x/s/.*  0x\([0-9a-fA-F]\{4\}\).*/\1/p};d' \
    NVIDIA-Linux/README.txt \
    | tr A-F a-f | sort | uniq >"$readme_list"

  local readme_length="$(grep -Ec . "$readme_list")"

  objdump --section=.rodata --syms "$object" |
  sed -nr '/SYMBOL TABLE/,/^$/ {
    s/^([0-9a-f]+)\s+l\s+O\s+\S+\s+([0-9a-f]+)\s+\S+.*/\2 \1/p
  }' | sort -r >"$symbols"

  while read length start; do
    [ "$((0x$length))" -gt 0 ] || continue

    objdump --section=.rodata --full-contents \
      --start-address="0x$start" \
      --stop-address="$((0x$start+0x$length))" "$object" |
    sed -nr 's/^ [0-9a-f]+ ([0-9a-f]{2})([0-9a-f]{2}).*/\2\1/p' |
    sort | uniq | (grep -vx 0000 || :) >"$object_list"

    local object_length="$(grep -Ec . "$object_list")"

    diff -u "$readme_list" "$object_list" | tail -n +3 >"$diff"
    local num_deletions="$(grep -Ec '^-' "$diff")"
    local num_additions="$(grep -Ec '^\+' "$diff")"

    if [ -n "$extract_verbose$extract_debug" ] && [ "$num_deletions" -lt "$readme_length" ]; then
      echo "start=0x$start length=0x$length readme=$readme_length object=$object_length deletions=$num_deletions additions=$num_additions kept=$(($readme_length - num_deletions))" >&2
      if [ -n "$extract_debug" ]; then
        cp "$readme_list" list.readme
        cp "$object_list" list.$start
        cp "$diff" list.$start.diff
      fi
    fi

    # Some thresholds for now.
    if [ "$num_deletions" -eq 0 ] &&
       [ "$num_additions" -le "$(($readme_length*3/2))" ]; then
      >&2 printf 'DEBUG: readme:%d object:%d deletions:%d additions:%d\n' \
        "$readme_length" "$object_length" "$num_deletions" "$num_additions"
      ret=0
      break
    fi
  done <"$symbols"

  if [ "$ret" -eq 0 ]; then
    while read id; do
      echo 10de$id
    done <"$object_list"
  else
    >&2 printf '%s\n' 'Failed to find the list.'
  fi

  rm -f "$symbols" "$readme_list" "$object_list" "$diff"

  return "$ret"
}

device_ids "$@"

# vim:set et sw=2 sts=2:
