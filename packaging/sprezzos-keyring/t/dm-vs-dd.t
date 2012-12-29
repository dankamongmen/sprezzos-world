#!/bin/sh
# Compares the DM keyring with the DD keyring. If the same name or email is
# in both keyrings, that's an error.
set -e

list_uids () {
	gpg --no-options --no-auto-check-trustdb --no-default-keyring \
		--keyring "$1" --list-keys | grep '^uid' | sed 's/^uid *//' |
		egrep -v '\[jpeg image of size .*\]'
}

list_names () {
	sed 's/ <.*>//'
}

list_emails () {
	sed 's/.* <\(.*\)>/\1/'
}

fail=0

dd_uids=$(list_uids ./output/keyrings/debian-keyring.gpg)
(
	echo "$dd_uids" | list_emails
	echo "$dd_uids" | list_names
	echo "$dd_uids"
) | sort | uniq > dd-list.tmp

IFS="
"
for uid in $(list_uids ./output/keyrings/debian-maintainers.gpg | sort | uniq); do
	name=$(echo "$uid" | list_names)
	email=$(echo "$uid" | list_emails)
	if grep -q "^$uid$" dd-list.tmp; then
		echo "$uid is in both the DD and DM keyrings"
		fail=1
	elif grep  "^$name$" dd-list.tmp; then
		echo "warning: name $name is in both the DD and DM keyrings"
	elif grep  "^$email$" dd-list.tmp; then
		echo "email $email is in both the DD and DM keyrings"
		fail=1
	fi
done

rm -f dd-list.tmp

exit $fail
