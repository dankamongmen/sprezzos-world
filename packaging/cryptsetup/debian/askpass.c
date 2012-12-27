/*
 * askpass.c - prompts a user for a passphrase using any suitable method
 *             and prints the result to stdout.
 *
 * Copyright (C) 2008   David Härdeman <david@hardeman.nu>
 *
 * This package is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This package is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 */


#define _GNU_SOURCE
#define _BSD_SOURCE
#define _POSIX_C_SOURCE 1
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <termios.h>
#include <sys/klog.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <dirent.h>
#include <linux/vt.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/uio.h>

#define DEBUG 0

#define ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))

static bool disable_method(const char *method);

/*****************************************************************************
 * Utility functions                                                         *
 *****************************************************************************/
static void
debug(const char *fmt, ...)
{
	va_list ap;
	static bool first = true;
	static FILE *dbgfile;

	if (!DEBUG)
		return;

	if (first) {
		first = false;
		dbgfile = fopen("/tmp/askpass.debug", "a");
	}

	if (!dbgfile)
		return;

	va_start(ap, fmt);
	vfprintf(dbgfile, fmt, ap);
	va_end(ap);
}

static void
usage(const char *arg0, const char *errmsg)
{
	if (errmsg)
		fprintf(stderr, "Error: %s\nUsage: %s PROMPT\n", errmsg, arg0);
	else
		fprintf(stderr, "Usage: %s PROMPT\n", arg0);
	exit(EXIT_FAILURE);
}

static void
fifo_common_finish(int fd, char **buf, size_t *used, size_t *size)
{
	if (fd >= 0)
		close(fd);

	if (!*buf)
		return;

	memset(*buf, '\0', *size);
	free(*buf);
	*buf = NULL;
	*used = 0;
	*size = 0;
}

static bool
fifo_common_read(int fd, char **buf, size_t *used, size_t *size)
{
	ssize_t result;

again:
	if ((*size - *used) == 0) {
		*size += 4096;
		*buf = realloc(*buf, *size);
		if (!*buf) {
			*size = 0;
			*used = 0;
			debug("Failed to allocate memory for passphrase\n");
			return false;
		}
	}

reread:
	result = read(fd, *buf + *used, *size - *used);

	if (result < 0) {
		if (errno == EAGAIN)
			return false;
		if (errno == EINTR)
			goto reread;
		debug("Error when reading from fifo\n");
		return false;
	}

	debug("Read %i bytes from fifo\n", (int)result);
	*used += result;

	if (result == 0)
		return true;

	goto again;
}


/*****************************************************************************
 * splashy functions                                                         *
 *****************************************************************************/

/* It might be better style to just do a popen of splashy_update ? */

#define SPLASHY_SOCK	"\0/splashy"
static size_t splashyused = 0;
static size_t splashysize = 0;
static char *splashybuf = NULL;

static int
splashy_prepare(const char *prompt)
{
	int fd;
	struct sockaddr addr = {AF_UNIX, SPLASHY_SOCK};
	struct iovec iov[2];

	if ((fd = socket (PF_UNIX, SOCK_STREAM, 0)) == -1) {
		return -1;
	}

	if (connect (fd, &addr, sizeof addr) == -1) {
		close (fd);
		return -1;
	}

	iov[0].iov_base = "getpass ";
	iov[0].iov_len = strlen ("getpass ");
	iov[1].iov_base = prompt;
	iov[1].iov_len = strlen (prompt) + 1;

	if (writev (fd, iov, 2) == -1) {
		close (fd);
		return -1;
	}

	/* Shutdown write? */

	return fd;
}

static bool
splashy_read(int fd, char **buf, size_t *size)
{
	debug("In splashy_read\n");
	if (fifo_common_read(fd, &splashybuf, &splashyused, &splashysize)) {
		*buf = splashybuf;
		*size = splashyused;
		return true;
	}

	return false;
}


static void
splashy_finish(int fd)
{
	fifo_common_finish (fd, &splashybuf, &splashyused, &splashysize);
}

/*****************************************************************************
 * fifo functions                                                            *
 *****************************************************************************/
#define FIFO_PATH "/lib/cryptsetup/passfifo"
static size_t fifoused = 0;
static size_t fifosize = 0;
static char *fifobuf = NULL;

static void
fifo_finish(int fd)
{
	fifo_common_finish(fd, &fifobuf, &fifoused, &fifosize);
}

static bool
fifo_read(int fd, char **buf, size_t *size)
{
	debug("In fifo_read\n");
	if (fifo_common_read(fd, &fifobuf, &fifoused, &fifosize)) {
		*buf = fifobuf;
		*size = fifoused;
		return true;
	}

	return false;
}

static int
fifo_prepare(const char *prompt)
{
	int ret;

	ret = mkfifo(FIFO_PATH, 0600);
	if (ret && errno != EEXIST)
		return -1;

	return open(FIFO_PATH, O_RDONLY | O_NONBLOCK);
}

/*****************************************************************************
 * console functions                                                         *
 *****************************************************************************/
#define CONSOLE_PATH "/dev/console"
static struct termios term_old;
static bool term_set = false;
static char *consolebuf = NULL;
static size_t consolebuflen = 0;

static void
console_finish(int fd)
{
	if (consolebuf) {
		memset(consolebuf, '\0', consolebuflen);
		free(consolebuf);
		consolebuf = NULL;
		consolebuflen = 0;
	}

	if (!term_set || fd < 0)
		return;

	term_set = false;
	tcsetattr(fd, TCSAFLUSH, &term_old);
	fprintf(stderr, "\n");
	klogctl(7, NULL, 0);
}

bool
console_read(int fd, char **buf, size_t *size)
{
	ssize_t nread;

	/* Console is in ICANON mode so we'll get entire lines */
	nread = getline(&consolebuf, &consolebuflen, stdin);

	if (nread < 0)
		return NULL;

	/* Strip trailing newline, if any */
	if (nread > 0 && consolebuf[nread - 1] == '\n') {
		nread--;
		consolebuf[nread] = '\0';
	}

	*size = nread;
	*buf = consolebuf;

	return true;
}

static int
console_prepare(const char *prompt)
{
	struct termios term_new;
	char *prompt_ptr = prompt;
	char *newline = NULL;

	if (!isatty(STDIN_FILENO)) {
		if (access(CONSOLE_PATH, R_OK | W_OK)) {
			debug("No access to console device " CONSOLE_PATH "\n");
			return -1;
		}

		if (!freopen(CONSOLE_PATH, "r", stdin)  ||
		    !freopen(CONSOLE_PATH, "a", stdout) ||
		    !freopen(CONSOLE_PATH, "a", stderr) ||
		    !isatty(STDIN_FILENO)) {
			debug("Failed to open console\n");
			return -1;
		}
	}

	if (tcgetattr(STDIN_FILENO, &term_old)) {
		debug("Failed to get terminal settings\n");
		return -1;
	}

	term_new = term_old;
	term_new.c_lflag &= ~ECHO;
	term_new.c_lflag |= ICANON;

	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &term_new)) {
		debug("Failed to disable echoing\n");
		return -1;
	}

	/* handle any non-literal embedded newlines in prompt */
	while ( (newline = strstr(prompt_ptr,"\\n")) != NULL ) {
		/* Calculate length of string leading up to newline. */
		int line_len = newline - prompt_ptr;

		/* Force trimming of prompt to location of newline. */
		if (fwrite(prompt_ptr, line_len, 1, stderr) < 1 ||
		    fwrite("\n", 1, 1, stderr) < 1) {
			debug("Failed to print prompt\n");
			tcsetattr(STDIN_FILENO, TCSAFLUSH, &term_old);
			return -1;
		}

		/* Skip over newline. */
		prompt_ptr = newline + 2;
	}
	if (fputs(prompt_ptr, stderr) < 0) {
		debug("Failed to print prompt\n");
		tcsetattr(STDIN_FILENO, TCSAFLUSH, &term_old);
		return -1;
	}

	/* Disable printk to console */
	klogctl(6, NULL, 0);
	term_set = true;
	return STDIN_FILENO;
}

/*****************************************************************************
 * main functions                                                            *
 *****************************************************************************/

struct method {
	const char *name;
	int (*prepare)(const char *prompt);
	bool (*read)(int fd, char **buf, size_t *size);
	void (*finish)(int fd);
	bool active;
	bool enabled;
	int fd;
};

static struct method methods[] = {
	{ "splashy", splashy_prepare, splashy_read, splashy_finish, false, true, -1 },
	{ "fifo", fifo_prepare, fifo_read, fifo_finish, false, true, -1 },
	{ "console", console_prepare, console_read, console_finish, false, true, -1 }
};

static bool
disable_method(const char *method)
{
	int i;
	bool result = false;

	debug("Disabling method %s\n", method ? method : "ALL");

	for (i = 0; i < ARRAY_SIZE(methods); i++) {
		/* A NULL method means all methods should be disabled */
		if (method && strcmp(methods[i].name, method))
			continue;
		if (!methods[i].enabled)
			continue;
		if (methods[i].active)
			methods[i].finish(methods[i].fd);

		methods[i].active = false;
		methods[i].fd = -1;
		methods[i].enabled = false;
		result = true;
	}

	return result;
}

int
main(int argc, char **argv, char **envp)
{
	char *pass = NULL;
	size_t passlen = 0;
	int i;
	int nfds;
	fd_set fds;
	int ret;
	bool done = false;
	sigset_t sigset;

	if (argc != 2)
		usage(argv[0], "incorrect number of arguments");

	sigfillset(&sigset);
	sigprocmask(SIG_BLOCK, &sigset, NULL);

	for (i = 0; i < ARRAY_SIZE(methods); i++) {
		if (!methods[i].enabled)
			continue;
		debug("Enabling method %s\n", methods[i].name);
		methods[i].fd = methods[i].prepare(argv[1]);
		if (methods[i].fd < 0)
			methods[i].active = false;
		else
			methods[i].active = true;
	}

	while (!done) {
		nfds = 0;
		FD_ZERO(&fds);
		for (i = 0; i < ARRAY_SIZE(methods); i++) {
			if (!methods[i].enabled || methods[i].fd < 0)
				continue;
			debug("method %i has fd %i and name %s\n", i, methods[i].fd, methods[i].name);
			FD_SET(methods[i].fd, &fds);
			if (methods[i].fd + 1 > nfds)
				nfds = methods[i].fd + 1;
		}

		if (nfds == 0) {
			debug("All methods disabled\n");
			exit(EXIT_FAILURE);
		}

		debug("Starting select with nfds %i\n", nfds);
		ret = select(nfds, &fds, NULL, NULL, NULL);

		if (ret <= 0) {
			if (ret == 0 || errno == EINTR)
				continue;
			debug("Select failed\n");
			disable_method(NULL);
			exit(EXIT_FAILURE);
		}

		for (i = 0; i < ARRAY_SIZE(methods); i++) {
			if (!methods[i].enabled || methods[i].fd < 0)
				continue;
			if (!FD_ISSET(methods[i].fd, &fds))
				continue;
			if (methods[i].read(methods[i].fd, &pass, &passlen) && pass) {
				done = true;
				break;
			}
		}
	}

	debug("Writing %i bytes to stdout\n", (int)passlen);
	write(STDOUT_FILENO, pass, passlen);
	disable_method(NULL);
	exit(EXIT_SUCCESS);
}

