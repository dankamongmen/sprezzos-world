#!/bin/sh
# Looks for expired keys in our active keyrings
set -e

find_expired () {
	k=$1
	gpg --no-options --no-auto-check-trustdb --no-default-keyring \
		--keyring "./output/keyrings/$k" --list-keys --with-colons \
		| grep '^pub' \
		| awk -F: --assign keyring=$1 \
		'$2 == "e" {print keyring ":\t0x" $5 " expired on " $7}'
}

fail=0
for keyring in debian-keyring.gpg debian-maintainers.gpg \
		debian-nonupload.gpg; do
	find_expired $keyring
done

exit $fail
