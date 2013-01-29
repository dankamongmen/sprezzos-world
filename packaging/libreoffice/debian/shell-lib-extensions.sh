validate_extensions() {
  INSTDIR=`mktemp -d`
  if HOME=$INSTDIR /usr/lib/libreoffice/program/unopkg list --bundled >/dev/null 2>/dev/null; then
	HOME=$INSTDIR /usr/lib/libreoffice/program/unopkg validate -v --bundled
  fi
}

make_lo_sync_extensions() {
	touch /usr/lib/libreoffice/share/extensions
}
