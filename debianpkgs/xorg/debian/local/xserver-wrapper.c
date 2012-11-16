/* xserver-wrapper.c - a simple wrapper for X servers that decides whether to
 * let them be run.
 *
 * By Stephen Early
 *
 * Stephen Early: modified to use /etc/X11/X symlink with security level of
 *                'Console' if /etc/X11/Xserver does not exist
 * Mark W. Eichin: permit non-privileged -showconfig (6 May 1997)
 * Mark W. Eichin: fix sense of error check for -showconfig (11 May 1997)
 * Mark W. Eichin: drop privileges on alternate -config, even if we do pass the
 *                 security check, to prevent using the error handling to read
 *                 the first line of any protected file (19 Sep 1997)
 * Erik Troan: prevent buffer overruns (25 Mar 1998)
 * Topi Miettinen: plug file descriptor leak (26 Apr 1998)
 * Branden Robinson: only fclose() if file was opened (3 May 1999)
 * Colin Phipps: minor device number check should be < 64, not < 128, or we
 *               catch serial terminals (26 Feb 2000)
 * Branden Robinson: ensure sanity of X server socket directory (13 Jun 2000)
 * Branden Robinson: make all paths #defines
 *                   more helpful socket dir error messages (29 Jun 2000)
 * Branden Robinson: bail out if the config file contains only the silly
 *                   default X server name (XF86_NONE) (30 Jul 2000)
 * Branden Robinson: increase verbosity when wrapper config not found
 *                   (2 Oct 2000)
 * Branden Robinson:
 *   - new configuration file, Xwrapper.config, with different format
 *     (name=value)
 *   - now just exec's /etc/X11/X; whatever this symlink points
 *     to will be used as the X server
 *   - config file specifies allowed user types as before (root only,
 *     console users, anyone)
 *   - config file specifies nice value to use for server
 *   (17 Nov 2000)
 * Branden Robinson: now accepts hyphens in variable contents (24 Nov 2000)
 * Branden Robinson: fix dumb errors left over from debugging (3 Dec 2000)
 * Branden Robinson: let root start the server even if he isn't on a
 *                    console, and the security level is console (11 Dec 2000)
 * Branden Robinson: check out the X server symlink with readlink; abort if
 *                   it's not a symlink, or if it points back to this wrapper
 *                   (24 Feb 2001)
 * Branden Robinson: whoops; readlink() doesn't null-terminate the target
 *                   string (27 Feb 20001)
 * Branden Robinson: add more info to "suspicious" error messages (16 Mar 2001)
 * Branden Robinson: also allow unprivileged use of "-version" option
 *                   (13 Jul 2001)
 * Branden Robinson: check mode of DRI device directory, if it exists, and warn
 *                   if it is weird (28 Aug 2001)
 * Branden Robinson: skip lines in Xwrapper.config that don't match expected
 *                   format (9 Dec 2001)
 * Branden Robinson: fix logic that was supposed to also allow unprivileged use
 *                   of "-version" option but which actually forbade both
 *                   "-showconfig" and "-verbose"; also let unprivileged users
 *                   specify "-help" option to get a usage message (26 Dec 2001)
 * Branden Robinson: change nice() usage to fit SuSv2 semantics; see Debian Bug
 *                   #140012 (2 Apr 2002)
 * Branden Robinson: *sigh* Ben Collins changed our FROZEN C library back to
 *                   pre-SuSv2 nice() semantics, so rewrote the nice() error
 *                   handling; also correct limits on legal nice values from
 *                   -20 <= x <= 20 to -20 <= x <= 19 (29 Apr 2002)
 * Branden Robinson: make the nice() error handling switchable with a #define
 *                   between SuSv2 semantics and old-style semantics
 *                   (16 Oct 2002)
 * Branden Robinson: stop using the GNU extension strnlen() to appease the
 *                   Debian GNU/NetBSD geeks, who are using BSD's C library
 *                   (16 Oct 2002)
 * Branden Robinson: chdir() to the directory where the X server symlink is kept
 *                   before executing its target, so that relative symlinks work
 *                   (1 Aug 2003)
 * Guillem Jover: add console detection support for GNU/kFreeBSD, and some
 *                messages at build and run time to allow the user to know
 *                what failed on unsupported systems
 *                (30 Mar 2007)
 * Brice Goglin: drop privileges on alternate config file given with
 *               -xf86config (14 Jun 2007)
 * Loïc Minier: on Linux, also consider alternate tty devices (major 5 and
 *              minor < 64) as consoles (24 Sep 2008)
 * Julien Cristau: remove the nice_value option
 * Julien Cristau: recognize /usr/bin/X as a path to this wrapper (6 Jun 2009)
 * Julien Cristau: don't print an error message if Xwrapper.config doesn't exist
 *                 (11 Aug 2009)
 * Julien Cristau: allow unprivileged -showDefaultModulePath and
 *                 -showDefaultLibPath options (11 Aug 2009)
 * Julien Cristau: don't check the mode of the DRI device directory
 *                 (11 Aug 2009)
 * Julien Cristau: also drop group privileges (1 Nov 2011)
 * Julien Cristau: disallow major 5 again for consoles (15 Dec 2011)
 *
 * This is free software; you may redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 * This is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License with
 * the Debian operating system, in /usr/share/common-licenses/GPL;  if
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 */

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#if defined(__linux__)
#define TTY_MAJOR_DEV 4
#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
#include <sys/consio.h>
#endif

#define X_WRAPPER_CONFIG_FILE "/etc/X11/Xwrapper.config"
#define X_SERVER_SYMLINK_DIR "/etc/X11"
#define X_SERVER_SYMLINK "/etc/X11/X"
#define X_SOCKET_DIR "/tmp/.X11-unix"
#define X_SOCKET_DIR_MODE (S_ISVTX | S_IRWXU | S_IRWXG | S_IRWXO)

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

typedef enum {
  RootOnly,
  Console,
  Anybody
} SecurityLevel;

static SecurityLevel
getSecLevel(char *security)
{
  char *c;

  for (c = security; *c; c++) *c = toupper(*c);

  if (strncmp(security,"ROOTONLY",8) == 0) return RootOnly;
  if (strncmp(security,"CONSOLE",7) == 0) return Console;
  if (strncmp(security,"ANYBODY",7) == 0) return Anybody;
  return RootOnly;
}

static int
onConsole()
{
#if defined(__linux__)
  struct stat s;

  /* see if stdin is a virtual console device */
  if (fstat(0, &s) != 0) {
    (void) fprintf(stderr, "X: cannot stat stdin\n");
    return FALSE;
  }
  if (S_ISCHR(s.st_mode) &&
        (major(s.st_rdev) == TTY_MAJOR_DEV &&
         minor(s.st_rdev) < 64)) {
    return TRUE;
  }
#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
  int idx;

  if (ioctl(0, VT_GETINDEX, &idx) != -1)
    return TRUE;
#else
#warning This program needs porting to your kernel.
  (void) fprintf(stderr, "X: unable to determine if running on a console\n");
#endif

  return FALSE;
}

static int
checkSecLevel(SecurityLevel level)
{
  switch (level) {
  case RootOnly:
    if (getuid() == 0) { /* real uid is root */
      return TRUE;
    } else {
      return FALSE;
    }
    break;
  case Console:
    if (getuid() == 0) return TRUE; /* root */
    return onConsole();
    break;
  case Anybody:
    return TRUE;
  }
  return FALSE;
}

int
main(int argc, char **argv)
{
  FILE *cf;
  struct stat statbuf;
  char xserver[1025];
  char line[1024];
  char var[65];
  char value[257];
  int length;
  int i;
  char *val;
  mode_t mask;
  SecurityLevel level = RootOnly;

  /* attempt to use our config file */
  cf = fopen(X_WRAPPER_CONFIG_FILE, "r");

  if (cf) {
    /* parse it */

    val = fgets(line, 1024, cf);

    while (val != NULL) {
      var[0] = '\0';
      value[0] = '\0';
      if (sscanf(line, " %64[A-Za-z0-9_] = %256[A-Za-z0-9_ -] ",
                 var, value) > 0) {
        /* truncate extra spaces at end of value */
        length = strlen(value);
        if (length > 256) {
          length = 256;
        }
        for (i = (length - 1); (value[i] == ' '); i--) {
          value[i] = '\0';
        }
        /* DEBUG (void) fprintf(stderr, "var: %s, value: %s.\n", var, value); */
        if (strncasecmp(var, "allowed_users", 64) == 0) {
          level = getSecLevel(value);
          /* DEBUG (void) fprintf(stderr, "security level set to %d\n", level); */
        }
      }
      val = fgets(line, 1024, cf);
    }

    (void) fclose(cf);
  } else {
    /* DEBUG (void) fprintf(stderr, "X: unable to open wrapper config file %s\n",
                   X_WRAPPER_CONFIG_FILE); */
  }

  if (lstat(X_SERVER_SYMLINK, &statbuf)) {
    (void) fprintf(stderr, "X: cannot stat %s (%s), aborting.\n",
                   X_SERVER_SYMLINK, strerror(errno));
    exit(1);
  }

  i = readlink(X_SERVER_SYMLINK, xserver, 1024);

  if (i < 0) {
    (void) fprintf(stderr, "X: cannot read %s symbolic link (%s), aborting.\n",
                   X_SERVER_SYMLINK, strerror(errno));
    exit(1);
  }

  xserver[i] = '\0'; /* readlink() does not null-terminate the string */

  if ((strcmp(xserver, "/usr/bin/X11/X") == 0) ||
      (strcmp(xserver, "/usr/X11R6/bin/X") == 0) ||
      (strcmp(xserver, "/usr/bin/X") == 0)) {
    (void) fprintf(stderr, "X: %s points back to X wrapper executable, "
                   "aborting.\n", X_SERVER_SYMLINK);
    exit(1);
  }

  if (access(X_SERVER_SYMLINK, X_OK)) { /* access() uses real uid */
    (void) fprintf(stderr, "%s is not executable\n", X_SERVER_SYMLINK);
    exit(1);
  }

  /* do we have permission to run the X server? */
  if (checkSecLevel(level)) {
    /* check for a sane server socket dir */
    mask = umask(0);
    /* some stupid kernels can't set the sticky bit during a mkdir() */
    if (!(mkdir(X_SOCKET_DIR, X_SOCKET_DIR_MODE))) {
      (void) chmod(X_SOCKET_DIR, X_SOCKET_DIR_MODE);
    }
    (void) umask(mask);

    /* do paranoid checks on the directory where the X server creates its socket */
    if (lstat(X_SOCKET_DIR, &statbuf)) {
      (void) fprintf(stderr, "X: cannot stat %s (%s), aborting.\n",
                     X_SOCKET_DIR, strerror(errno));
      exit(1);
    }

    if ((statbuf.st_uid != 0) || (statbuf.st_gid != 0)) {
      (void) fprintf(stderr, "X: %s has suspicious ownership (not root:root), "
                     "aborting.\n", X_SOCKET_DIR);
      exit(1);
    }

    if (statbuf.st_mode != (S_IFDIR | X_SOCKET_DIR_MODE)) {
      (void) fprintf(stderr, "X: %s has suspicious mode (not %o) or is not a "
                     "directory, aborting.\n", X_SOCKET_DIR, X_SOCKET_DIR_MODE);
      exit(1);
    }

    for (i = 1; i < argc; i++) {
      if (!strcmp(argv[i], "-config") || !strcmp(argv[i], "-xf86config")) {
        if (setgid(getgid()) || setuid(getuid())) {
          perror("X unable to drop setuid privileges for alternate config");
          exit(1);
        }
      } else if (strlen(argv[i]) > 256) {
        if (setgid(getgid()) || setuid(getuid())) {
          perror("X unable to drop setuid privileges for suspiciously long "
                 "argument");
          exit(1);
        }
      }
    }

    /* run the X server */
    seteuid(0);

    /* DEBUG exit(0); */

    /*
     * change to the directory where the X server symlink is so that a relative
     * symlink will work and execute the X server
     */
    if (chdir(X_SERVER_SYMLINK_DIR)) {
      (void) fprintf(stderr, "X: cannot chdir() to %s (%s), aborting.\n",
                     X_SERVER_SYMLINK_DIR, strerror(errno));
      exit(1);
    }
    (void) execv(xserver, argv);
    (void) fprintf(stderr, "X: exec of %s failed\n", xserver);
    exit(1);

  } else {
      /* DEBUG fprintf(stderr, "argc = %d, argv[1] = \"%s\"\n", argc, argv[1]); */
      /* DEBUG fprintf(stderr, "strcmp(argv[1], \"-showconfig\") = %d, strcmp(argv[1],
        \"-version\" = %d\n", (strcmp(argv[1], "-showconfig")), (strcmp(argv[1],
        "-version"))); */
      if (argc == 2 && ( (strcmp(argv[1], "-help") == 0) ||
                         (strcmp(argv[1], "-showconfig") == 0) ||
                         (strcmp(argv[1], "-version") == 0) ||
                         (strcmp(argv[1], "-showDefaultModulePath") == 0) ||
                         (strcmp(argv[1], "-showDefaultLibPath") == 0) ) ) {
          if (setgid(getgid()) || setuid(getuid())) {
              perror("X unable to drop setuid privileges");
              exit(1);
          }
          execv(xserver,argv);
          (void) fprintf(stderr, "X: unprivileged exec of %s failed, "
                         "aborting.\n", xserver);
          exit(1);
      } else {
          (void) fprintf(stderr, "X: user not authorized to run the X "
                         "server, aborting.\n");
          exit(1);
      }
  }

  (void) fprintf(stderr, "X: Impossible!  Unreachable statement reached!\n");
  exit(1);
}

/*
 * vim:set cindent et fo=tcroq sts=2 sw=2 tw=80:
 */
