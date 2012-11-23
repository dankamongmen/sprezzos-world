/*
 * $Id: tundev.c 116 2005-10-30 14:18:08Z guillem $
 */

#define _GNU_SOURCE /* asprintf */
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <net/if.h>
#include <linux/if_tun.h>

/* Tiny code to open tap/tun device, and hand the fd to qemu.
   Run as root, drops to given user. */
int main(int argc, char *argv[])
{
	struct ifreq ifr;
	struct passwd *p;
	unsigned int i;
	char *newargs[argc + 1];
	int fd;

	if (argc < 4) {
		fprintf(stderr,
			"Usage: tundev user logfile qemu <qemu options>...\n");
		exit(1);
	}

	fd = open("/dev/net/tun", O_RDWR);
	if (fd < 0) {
		perror("Could not open /dev/net/tun");
		exit(1);
	}

	memset(&ifr, 0, sizeof(ifr));
	ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
	strncpy(ifr.ifr_name, "tun%d", IFNAMSIZ);
	if (ioctl(fd, TUNSETIFF, (void *) &ifr) != 0) {
		perror("Could not get tun device");
		exit(1);
	}

	/* Set userid. */
	p = getpwnam(argv[1]);
	if (!p) {
		fprintf(stderr, "No user '%s'\n", argv[1]);
		exit(1);
	}
	setgroups(0, NULL);
	setgid(p->pw_gid);
	if (setuid(p->pw_uid) != 0) {
		perror("setting uid");
		exit(1);
	}

	/* Insert -tun-fd */
	newargs[0] = argv[3];
	newargs[1] = "-tun-fd";
	asprintf(&newargs[2], "%d", fd);
	for (i = 4; i <= argc; i++)
		newargs[i-1] = argv[i];

	if (strcmp(argv[2], "-") == 0) {
		execvp(newargs[0], newargs);
		exit(1);
	}

	switch (fork()) {
	case 0: {
		close(1);
		close(2);
		open(argv[2], O_WRONLY|O_APPEND);
		open(argv[2], O_WRONLY|O_APPEND);
		close(0);
		execvp(newargs[0], newargs);
		exit(1);
	}
	case -1:
		perror("fork failed");
		exit(1);
	}
	printf("%s\n", ifr.ifr_name);
	exit(0);
}
