all: output/keyrings/sprezzos-keyring.gpg output/keyrings/sprezzos-maintainers.gpg output/keyrings/sprezzos-nonupload.gpg output/keyrings/sprezzos-role-keys.gpg output/keyrings/emeritus-keyring.gpg output/keyrings/emeritus-keyring.pgp output/keyrings/extra-keys.pgp output/keyrings/removed-keys.gpg output/keyrings/removed-keys.pgp output/sha512sums.txt output/README output/changelog

output/keyrings/sprezzos-keyring.gpg: sprezzos-keyring-gpg/0x*
	cat sprezzos-keyring-gpg/0x* > output/keyrings/sprezzos-keyring.gpg

output/keyrings/sprezzos-maintainers.gpg: sprezzos-maintainers-gpg/0x*
	cat sprezzos-maintainers-gpg/0x* > output/keyrings/sprezzos-maintainers.gpg

output/keyrings/sprezzos-nonupload.gpg: sprezzos-nonupload-gpg/0x*
	cat sprezzos-nonupload-gpg/0x* > output/keyrings/sprezzos-nonupload.gpg

output/keyrings/sprezzos-role-keys.gpg: sprezzos-role-keys-gpg/0x*
	cat sprezzos-role-keys-gpg/0x* > output/keyrings/sprezzos-role-keys.gpg

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

output/sha512sums.txt: output/keyrings/sprezzos-keyring.gpg output/keyrings/sprezzos-maintainers.gpg output/keyrings/sprezzos-nonupload.gpg output/keyrings/sprezzos-role-keys.gpg output/keyrings/emeritus-keyring.gpg output/keyrings/emeritus-keyring.pgp output/keyrings/extra-keys.pgp output/keyrings/removed-keys.gpg output/keyrings/removed-keys.pgp
	cd output; sha512sum keyrings/* > sha512sums.txt

output/README: README
	cp README output/

output/changelog: sprezzos/changelog
	cp sprezzos/changelog output/

test:
	./runtests

clean:
	rm -f output/keyrings/*.pgp output/keyrings/*.gpg output/sha512sums.txt output/README output/changelog output/keyrings/*~
