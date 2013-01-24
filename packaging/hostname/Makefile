CFLAGS+=-O2 -Wall

# uncomment the following line if you want to install to a different base dir.
#BASEDIR=/mnt/test

OBJS=hostname.o

hostname: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) -lnsl
	ln -fs hostname dnsdomainname
	ln -fs hostname domainname
	ln -fs hostname ypdomainname
	ln -fs hostname nisdomainname

install: hostname
	install -d ${BASEDIR}/usr/share/man/man1
	install -o root -g root -m 0644 hostname.1 ${BASEDIR}/usr/share/man/man1
	ln -fs hostname.1 ${BASEDIR}/usr/share/man/man1/dnsdomainname.1
	ln -fs hostname.1 ${BASEDIR}/usr/share/man/man1/domainname.1
	ln -fs hostname.1 ${BASEDIR}/usr/share/man/man1/ypdomainname.1
	ln -fs hostname.1 ${BASEDIR}/usr/share/man/man1/nisdomainname.1
	#install -o root -g root -m 0644 hostname.1.fr ${BASEDIR}/usr/share/man/fr/man1/hostname.1

	install -d ${BASEDIR}/bin
	install -o root -g root -m 0755 hostname ${BASEDIR}/bin
	ln -f hostname ${BASEDIR}/bin/dnsdomainname
	ln -f hostname ${BASEDIR}/bin/domainname
	ln -f hostname ${BASEDIR}/bin/nisdomainname
	ln -f hostname ${BASEDIR}/bin/ypdomainname

clean:
	-rm -f $(OBJS) hostname dnsdomainname domainname nisdomainname ypdomainname

.PHONY: clean install
