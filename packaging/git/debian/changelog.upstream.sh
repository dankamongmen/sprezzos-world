#!/bin/sh
set -e
: >debian/changelog.upstream
exec <debian/versions.upstream
read old
while read new; do
  exec >debian/changelog.upstream."$new"
  echo "Version $new; changes since $old:"
  echo "Version $new; changes since $old:" |tr '[:print:]' -
  echo
  git shortlog --no-merges "$old".."$new"
  echo
  cat debian/changelog.upstream
  mv debian/changelog.upstream."$new" debian/changelog.upstream
  old="$new"
done
