all: output/keyrings/debian-keyring.gpg output/keyrings/debian-maintainers.gpg output/keyrings/debian-nonupload.gpg output/keyrings/debian-role-keys.gpg output/keyrings/emeritus-keyring.gpg output/keyrings/emeritus-keyring.pgp output/keyrings/extra-keys.pgp output/keyrings/removed-keys.gpg output/keyrings/removed-keys.pgp output/sha512sums.txt output/README output/changelog

output/keyrings/debian-keyring.gpg: debian-keyring-gpg/0x*
	cat debian-keyring-gpg/0x* > output/keyrings/debian-keyring.gpg

output/keyrings/debian-maintainers.gpg: debian-maintainers-gpg/0x*
	cat debian-maintainers-gpg/0x* > output/keyrings/debian-maintainers.gpg

output/keyrings/debian-nonupload.gpg: debian-nonupload-gpg/0x*
	cat debian-nonupload-gpg/0x* > output/keyrings/debian-nonupload.gpg

output/keyrings/debian-role-keys.gpg: debian-role-keys-gpg/0x*
	cat debian-role-keys-gpg/0x* > output/keyrings/debian-role-keys.gpg

output/keyrings/emeritus-keyring.gpg: emeritus-keyring-gpg/0x*
	cat emeritus-keyring-gpg/0x* > output/keyrings/emeritus-keyring.gpg

output/keyrings/emeritus-keyring.pgp: emeritus-keyring-pgp/0x*
	cat emeritus-keyring-pgp/0x* > output/keyrings/emeritus-keyring.pgp

output/keyrings/extra-keys.pgp: extra-keys-pgp/0x*
	cat extra-keys-pgp/0x* > output/keyrings/extra-keys.pgp

output/keyrings/removed-keys.gpg: removed-keys-gpg/0x*
	cat removed-keys-gpg/0x* > output/keyrings/removed-keys.gpg

output/keyrings/removed-keys.pgp: removed-keys-pgp/0x*
	cat removed-keys-pgp/0x* > output/keyrings/removed-keys.pgp

output/sha512sums.txt: output/keyrings/debian-keyring.gpg output/keyrings/debian-maintainers.gpg output/keyrings/debian-nonupload.gpg output/keyrings/debian-role-keys.gpg output/keyrings/emeritus-keyring.gpg output/keyrings/emeritus-keyring.pgp output/keyrings/extra-keys.pgp output/keyrings/removed-keys.gpg output/keyrings/removed-keys.pgp
	cd output; sha512sum keyrings/* > sha512sums.txt

output/README: README
	cp README output/

output/changelog: debian/changelog
	cp debian/changelog output/

test:
	./runtests

clean:
	rm -f output/keyrings/*.pgp output/keyrings/*.gpg output/sha512sums.txt output/README output/changelog output/keyrings/*~
