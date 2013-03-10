#	BSDI	$Id: Makefile,v 1.3 1995/01/07 05:34:35 donn Exp $

MAN8=	checksendmail.0

all checksendmail: ${MAN8}

clean depend tags:

install: maninstall
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
		${.CURDIR}/checksendmail.perl ${DESTDIR}${BINDIR}/checksendmail
	install -c -o ${BINOWN} -g ${BINGRP} -m 0444  \
		${.CURDIR}/address.resolve ${DESTDIR}/usr/contrib/lib/

.include <bsd.prog.mk>
