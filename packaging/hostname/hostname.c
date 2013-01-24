/*
 * hostname	This file contains an implementation of the command
 *		that maintains the host name and the domain name. It
 *		is also used to show the FQDN and the IP-Addresses.
 *
 * Usage:	hostname [-a|-A|-d|-f|-i|-I|-s|-y]
 *		hostname [-h|-V]
 *		hostname [-b] {name|-F file}
 *		dnsdomainname
 *		domainname
 *		ypdomainname
 *		nisdomainname
 *
 * Authors:	Peter Tobias <tobias@et-inf.fho-emden.de>
 *		Bernd Eckenfels
 *		Graham Wilson <graham@debian.org>
 *		Michael Meskes <meskes@debian.org>
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 *
 *		The localdomain and localhost functions are copyright
 *		(C) 1996 Free Software Foundation, Inc. and were
 *		written by Miles Bader <miles@gnu.ai.mit.edu> and
 *		Marcus Brinkmann <brinkmd@debian.org>.
 */

#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#define __USE_GNU 1
#include <string.h>
#include <netdb.h>
#include <errno.h>
#include <ctype.h>
#include <err.h>
#include <rpcsvc/ypclnt.h>

#define VERSION "3.12"

enum type_t { DEFAULT, DNS, FQDN, SHORT, ALIAS, IP, NIS, NIS_DEF, ALL_FQDNS, ALL_IPS };

char *progname;

/*
 * Return the name of the nis default domain. This is just a wrapper for
 * yp_get_default_domain.  If something goes wrong, program exits.
 */
char *
localnisdomain()
{
	char *buf = 0;
	int myerror = 0;

	myerror = yp_get_default_domain(&buf);

	/* yp_get_default_domain failed, abort. */
	if (myerror) {
		printf("%s: %s\n", progname, yperr_string(myerror));
		exit (1);
	}

	return buf;
}

/*
 * Return the name of the localdomain. This is just a wrapper for
 * getdomainname, which takes care of allocating a big enough buffer, and
 * caches the result after the first call (so the result should be copied
 * before modification). If something goes wrong, program exits.
 */
char *
localdomain()
{
	char *buf = 0;
	size_t buf_len = 0;
	int myerror = 0;

	do {
		errno = 0;

		if (buf) {
			buf_len += buf_len;
			if ((buf = realloc (buf, buf_len)) == NULL)
				err(1, NULL);
		} else {
			buf_len = 128;        /* Initial guess */
			if ((buf = malloc (buf_len)) == NULL)
				err(1, NULL);
		}
	} while (((myerror = getdomainname(buf, buf_len)) == 0 && !memchr (buf, '\0', buf_len))
	        || errno == ENAMETOOLONG);

	/* getdomainname failed, abort. */
	if (myerror)
		err(1, NULL);

	return buf;
}

/*
 * Return the name of the localhost. This is just a wrapper for gethostname,
 * which takes care of allocating a big enough buffer, and caches the result
 * after the first call (so the result should be copied before modification).
 * If something goes wrong, program exits.
 */
char *
localhost()
{
	char *buf = 0;
	size_t buf_len = 0;
	int myerror = 0;

	do {
		errno = 0;

		if (buf) {
			buf_len += buf_len;
			if ((buf = realloc (buf, buf_len)) == NULL)
				err(1, NULL);
		} else {
			buf_len = 128;        /* Initial guess */
			if ((buf = malloc (buf_len)) == NULL)
				err(1, NULL);
		}
	} while (((myerror = gethostname(buf, buf_len)) == 0 && !memchr (buf, '\0', buf_len))
	        || errno == ENAMETOOLONG);

	/* gethostname failed, abort. */
	if (myerror)
		err(1, NULL);

	return buf;
}

void
usage(FILE *stream)
{
	fprintf(stream,
		"Usage: hostname [-v] [-b] {hostname|-F file}         set host name (from file)\n"
		"       hostname [-v] [-a|-A|-d|-f|-i|-I|-s|-y]       display formatted name\n"
		"       hostname [-v]                                 display host name\n"
		"\n"
		"       {yp,nis,}domainname [-v] {nisdomain|-F file}  set NIS domain name (from file)\n"
		"       {yp,nis,}domainname [-v]                      display NIS domain name\n"
		"\n"
		"       dnsdomainname [-v]                            display dns domain name\n"
		"\n"
		"       hostname -V|--version|-h|--help               print info and exit\n"
		"\n"
		"Program name:\n"
		"       {yp,nis,}domainname=hostname -y\n"
		"       dnsdomainname=hostname -d\n"
		"\n"
		"Program options:\n"
		"    -a, --alias            alias names\n"
		"    -A, --all-fqdns        all long host names (FQDNs)\n"
		"    -b, --boot             set default hostname if none available\n"
		"    -d, --domain           DNS domain name\n"
		"    -f, --fqdn, --long     long host name (FQDN)\n"
		"    -F, --file             read host name or NIS domain name from given file\n"
		"    -i, --ip-address       addresses for the host name\n"
		"    -I, --all-ip-addresses all addresses for the host\n"
		"    -s, --short            short host name\n"
		"    -y, --yp, --nis        NIS/YP domain name\n"
		"\n"
		"Description:\n"
		"   This command can get or set the host name or the NIS domain name. You can\n"
		"   also get the DNS domain or the FQDN (fully qualified domain name).\n"
		"   Unless you are using bind or NIS for host lookups you can change the\n"
		"   FQDN (Fully Qualified Domain Name) and the DNS domain name (which is\n"
		"   part of the FQDN) in the /etc/hosts file.\n");
	exit(-1);
}

/*
 * Check the format of a user-specified hostname. Uses the rules from RFC 1035,
 * section 2.3.1.
 */
int
check_name(char *name)
{
	int i, len = strlen(name);

	/* Handle leading and trailing hyphen now. */
	if (!len || !isalnum(name[0]) || !isalnum(name[len-1]))
		return 0;

	for (i = 0; i < len; i++) {
		if (!isalnum(name[i]) && name[i] != '-' && name[i] != '.')
			return 0;
		if (name[i] == '-' && (name[i - 1] == '.' || name[i + 1] == '.'))
			return 0;
		if (name[i] == '.' && name[i - 1] == '.')
			return 0;
	}

	return 1;
}

void
set_name(enum type_t type, char *name)
{
	int i;

	switch (type) {
	case DEFAULT:
		/* Whitespaces are invalid characters in a hostname. */
		/* Thus remove trailing and leading whitespaces. */
		while (isspace(*name)) { name++; }
		for (i = strlen(name) - 1; i >= 0 && isspace(name[i]); i--);       
		name[i+1] = '\0';

		/* Now check for a valid name. */
		if (!check_name(name))
			errx(1, "the specified hostname is invalid");

		if (sethostname(name, strlen(name))) {
			if (errno == EPERM)
				errx(1, "you must be root to change the host name");
			else if (errno == EINVAL)
				errx(1, "name too long");
		}
		break;

	case NIS:
	case NIS_DEF:
		if (setdomainname(name, strlen(name))) {
			if (errno == EPERM)
				errx(1, "you must be root to change the domain name");
			else if (errno == EINVAL)
				errx(1, "name too long");
		}
		break;

	default:
		/*
		 * Only the host name and the domain name can be set using this
		 * command.
		 */
		usage(stderr);
	}
}

void
show_name(enum type_t type)
{
	struct addrinfo *res;
	struct addrinfo hints;
	struct ifaddrs *ifa, *ifap;
	char *p;
	int ret;

	/* Handle a few cases specially. */
	switch(type)
	{
		case DEFAULT: 
			printf("%s\n", localhost());
			break;
		case SHORT:
			p = localhost();
			*(strchrnul(p, '.')) = '\0';
			printf("%s\n", p);
			break;
		case NIS:
			printf("%s\n", localdomain());
			break;
		case NIS_DEF:
			printf("%s\n", localnisdomain());
			break;
		case ALL_IPS:
		case ALL_FQDNS: {
			char buf[INET6_ADDRSTRLEN];
			int flags, ret, family, addrlen;

			/* What kind of information do we want from getnameinfo()? */
			flags = (type == ALL_IPS) ? NI_NUMERICHOST : NI_NAMEREQD;

			if (getifaddrs(&ifa) != 0)
				errx(1, "%s", strerror(errno));
			for (ifap = ifa; ifap != NULL; ifap = ifap->ifa_next) {
				/* Skip interfaces that have no configured addresses */
				if (ifap->ifa_addr == NULL)
					continue;
				/* Skip the loopback interface */
				if (ifap->ifa_flags & IFF_LOOPBACK)
					continue;
				/* Skip interfaces that are not UP */
				if (!(ifap->ifa_flags & IFF_UP))
					continue;

				/* Only handle IPv4 and IPv6 addresses */
				family = ifap->ifa_addr->sa_family;
				if (family != AF_INET && family != AF_INET6)
					continue;

				addrlen = (family == AF_INET) ? sizeof(struct sockaddr_in) :
								sizeof(struct sockaddr_in6);

				/* Skip IPv6 link-local addresses */
				if (family == AF_INET6) {
					struct sockaddr_in6 *sin6;

					sin6 = (struct sockaddr_in6 *)ifap->ifa_addr;
					if (IN6_IS_ADDR_LINKLOCAL(&sin6->sin6_addr) ||
							IN6_IS_ADDR_MC_LINKLOCAL(&sin6->sin6_addr))
						continue;
				}

				ret = getnameinfo(ifap->ifa_addr, addrlen,
						  buf, sizeof(buf), NULL, 0, flags);

				/* Just skip addresses that cannot be translated */
				if (ret != 0) {
				    if (type != ALL_FQDNS && ret != EAI_NONAME)
					errx(1, "%s", gai_strerror(ret));
				} else
					printf("%s ", buf);
			}
			printf("\n");
			freeifaddrs(ifa);
			break;
		}
		default:
			memset(&hints, 0, sizeof(struct addrinfo));
			hints.ai_socktype = SOCK_DGRAM;
			hints.ai_flags = AI_CANONNAME;

			p = localhost();
			if ((ret = getaddrinfo(p, NULL, &hints, &res)) != 0)
				errx(1, "%s", gai_strerror(ret));
			
			p = strchr(res->ai_canonname, '.');

			switch (type) {
			case ALIAS: {
				struct hostent *hp;
				int i;

				if ((hp = gethostbyname(localhost())) == NULL)
					errx(1, "%s", hstrerror(h_errno));

				for (i = 0; hp->h_aliases[i]; i++) {
					if (i > 0)
						printf(" ");
					printf("%s", hp->h_aliases[i]);
				}
				printf("\n");
				break;
			}

			case IP: {
				char buf[INET6_ADDRSTRLEN];
				int ret;

				struct addrinfo *walk;

				for (walk = res; walk != NULL; walk = walk->ai_next) {
					if ((ret = getnameinfo(walk->ai_addr, walk->ai_addrlen,
								buf, sizeof(buf), NULL, 0,
								NI_NUMERICHOST)) != 0)
						errx(1, "%s", gai_strerror(ret));

					if (walk != res)
						printf(" ");

					printf("%s", buf);
				}
				printf("\n");
				break;
			}

			case DNS:
				if (p != NULL)
					printf("%s\n", ++p);
				break;

			case FQDN:
				printf("%s\n", res->ai_canonname);
				break;

			default:
				break;
			}
			break;
	}
}

char *
read_file(char *filename, int boot)
{
	struct stat st;
  	FILE *fp;
	char *buf;

	if ((fp = fopen(filename, "r")) == NULL) {
		if (boot)
			return NULL;
		else
			err(1, NULL);
	}

	if (fstat(fileno(fp), &st) == -1
	   || (buf = (char *) malloc(st.st_size + 1)) == NULL)
		err(1, NULL);

	while (fgets(buf, st.st_size + 1, fp) != NULL) {
		char *p;

		if (buf[0] == '\n' || buf[0] == '#')
			continue;

		if ((p = strchr(buf, '\n')) != NULL)
			*p = '\0';
		break;
	}

	fclose(fp);
	return buf;
}

int
main(int argc, char **argv)
{
	char *file = NULL, *name = NULL;
	enum type_t type = DEFAULT;
	int boot = 0;
	int o;

	static const struct option long_options[] =
	{
		{"domain", no_argument, 0, 'd'},
		{"boot", no_argument, 0, 'b'},
		{"file", required_argument, 0, 'F'},
		{"fqdn", no_argument, 0, 'f'},
		{"all-fqdns", no_argument, 0, 'A'},
		{"help", no_argument, 0, 'h'},
		{"long", no_argument, 0, 'f'},
		{"short", no_argument, 0, 's'},
		{"version", no_argument, 0, 'V'},
		{"verbose", no_argument, 0, 'v'},
		{"alias", no_argument, 0, 'a'},
		{"ip-address", no_argument, 0, 'i'},
		{"all-ip-addresses", no_argument, 0, 'I'},
		{"nis", no_argument, 0, 'y'},
		{"yp", no_argument, 0, 'y'},
		{0, 0, 0, 0}
	};

	/* If called as 'dnsdomainname', by default show the DNS domain name. */
	progname = (rindex(argv[0], '/')) ? rindex(argv[0], '/') + 1 : argv[0];
	if (!strcmp(progname, "dnsdomainname"))
		type = DNS;
	else if (!strcmp(progname, "domainname"))
		type = NIS;
	else if (!strcmp(progname, "ypdomainname"))
		type = NIS_DEF;
	else if (!strcmp(progname, "nisdomainname"))
		type = NIS_DEF;

	while((o = getopt_long(argc, argv, "aAdfbF:h?iIsVvy", long_options, NULL)) != -1)
		switch (o) {
		case 'd':
			type = DNS;
			break;
		case 'a':
			type = ALIAS;
			break;
		case 'f':
			type = FQDN;
			break;
		case 'A':
			type = ALL_FQDNS;
			break;
		case 'i':
			type = IP;
			break;
		case 'I':
			type = ALL_IPS;
			break;
		case 's':
			type = SHORT;
			break;
		case 'y':
			type = NIS_DEF;
			break;
		case 'b':
			boot = 1;
			break;
		case 'F':
			file = optarg;
			break;
		case 'v':
			/* Does not do anything. */
			break;
		case 'V':
			printf("hostname %s\n", VERSION);
			return 0;
		case '?':
		case 'h':
			usage(stdout);
			break;
		default:
			usage(stderr);
		}

	/*
	 * Hostname may be read from a file, it's ok for this file to not
	 * exist or be empty if -b is given in which case we default to
	 * "localhost" if there is not one already set.
	 */
	if (file) {
		name = read_file(file, boot);
		if (boot && (name == NULL || name[0] == '\0')) {
			free(name);

			name = localhost();
			if (name[0] == '\0' || !strcmp(name,"(none)"))
				strcpy(name, "localhost");
		}
	}

	/* Otherwise the hostname is the first non-option argument. */
	if (optind < argc) {
		/*
		 * It is an error to specify a host name as an argument, and to
		 * be read from a file.
		 */
		if (name)
			usage(stderr);

		if ((name = strdup(argv[optind])) == NULL)
			err(1, NULL);
		optind++;
	}

	/* Check for any remaining arguments. */
	if (optind < argc)
		usage(stderr);

	if (name) {
		set_name(type, name);
		free(name);
	} else
		show_name(type);

	return 0;
}

/* vim: set noexpandtab: */
