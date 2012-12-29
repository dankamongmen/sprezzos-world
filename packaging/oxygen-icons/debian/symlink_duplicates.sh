#!/bin/bash

# Recursively identifies duplicate files and symlinks them
# Arguments:
# $1 Root directory for absolute path calculation
# $2 Directory for searching duplicate files

echo "Symlinking duplicate files, this may take some time..."

symlink_root=${1/%\//}
symlink_search_path=$2

target=""
fdupes -r $symlink_search_path | while read line; do
	if [ -z "$line" ]; then
		# New group of duplicate files begins
		target=""
	elif [ -z "$target" ]; then
		# Setting a new target for symlinking
		target=${line/$symlink_root/}
	else
		# Symlink duplicate
		ln -sf "$target" "$line"
	fi
done

echo "...finished"
