#ifndef BASH_PREINST_H
#define BASH_PREINST_H

/*
 * This file is in the public domain.
 * You may freely use, modify, distribute, and relicense it.
 */

#define _XOPEN_SOURCE 700
#include <stdio.h>
#include <stdarg.h>
#include <sys/types.h>

#if !defined(__GNUC__) && !defined(__attribute__)
# define __attribute__(x)
#endif
#define NORETURN __attribute__((__noreturn__))
#define PRINTFLIKE __attribute__((format(printf, 1, 2)))

enum wait_or_die_flags {
	ERROR_OK = 1,
	SIGPIPE_OK = 2
};

extern NORETURN PRINTFLIKE void die_errno(const char *fmt, ...);
extern NORETURN PRINTFLIKE void die(const char *fmt, ...);

extern int exists(const char *path);
extern void set_cloexec(int fd);
extern void xpipe(int pipefd[2]);

extern void wait_or_die(pid_t child, const char *desc, int flags);
extern pid_t spawn(const char * const cmd[], int outfd, int errfd);
extern void run(const char * const cmd[]);	/* spawn and wait */
extern FILE *spawn_pipe(pid_t *pid, const char * const cmd[], int errfd);

#endif
