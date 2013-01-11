#!/bin/sh

set -e

target_dir=$1
[ -d $target_dir ] || exit 0
find "$target_dir" -name '*.rb' -type f -exec sed -i -e '1,1{ /^#!/d }' {} \+
