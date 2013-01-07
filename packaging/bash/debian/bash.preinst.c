/*
 * This file is in the public domain.
 * You may freely use, modify, distribute, and relicense it.
 */

#include "bash.preinst.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

static void backup(const char *file, const char *dest)
{
	const char * const cmd[] = {"cp", "-dp", file, dest, NULL};
	if (exists(file))
		run(cmd);
}

static void force_symlink(const char *target, const char *link,
						const char *temp)
{
	/*
	 * Forcibly create a symlink to "target" from "link".
	 * This is performed in two stages with an
	 * intermediate temporary file because symlink(2) cannot
	 * atomically replace an existing file.
	 */
	if ((unlink(temp) && errno != ENOENT) ||
	    symlink(target, temp) ||
	    rename(temp, link))
		die_errno("cannot create symlink %s -> %s", link, target);
}

static void reset_diversion(const char *package, const char *file,
						const char *distrib)
{
	const char * const remove_old_diversion[] =
		{"dpkg-divert", "--package", "bash", "--remove", file, NULL};
	const char * const new_diversion[] =
		{"dpkg-divert", "--package", package,
		"--divert", distrib, "--add", file, NULL};
	run(remove_old_diversion);
	run(new_diversion);
}

static int has_binsh_line(FILE *file)
{
	char item[sizeof("/bin/sh\n")];

	while (fgets(item, sizeof(item), file)) {
		int ch;

		if (!memcmp(item, "/bin/sh\n", strlen("/bin/sh\n") + 1))
			return 1;
		if (strchr(item, '\n'))
			continue;

		/* Finish the line. */
		for (ch = 0; ch != '\n' && ch != EOF; ch = fgetc(file))
			; /* just reading */
		if (ch == EOF)
			break;
	}
	if (ferror(file))
		die_errno("cannot read pipe");
	return 0;
}

static int binsh_in_filelist(const char *package)
{
	const char * const cmd[] = {"dpkg-query", "-L", package, NULL};
	pid_t child;
	int sink;
	FILE *in;
	int found;

	/*
	 * dpkg -L $package 2>/dev/null | ...
	 *
	 * Redirection of stderr is for quieter output
	 * when $package is not installed.  If opening /dev/null
	 * fails, no problem; leave stderr alone in that case.
	 */
	sink = open("/dev/null", O_WRONLY);
	if (sink >= 0)
		set_cloexec(sink);
	in = spawn_pipe(&child, cmd, sink);

	/* ... | grep "^/bin/sh\$" */
	found = has_binsh_line(in);
	if (fclose(in))
		die_errno("cannot close read end of pipe");

	/*
	 * dpkg -L will error out if $package is not already installed.
	 *
	 * We stopped reading early if we found a match, so
	 * tolerate SIGPIPE in that case.
	 */
	wait_or_die(child, "dpkg-query -L", ERROR_OK |
						(found ? SIGPIPE_OK : 0));
	return found;
}

static int undiverted(const char *path)
{
	const char * const cmd[] =
		{"dpkg-divert", "--listpackage", path, NULL};
	pid_t child;
	char packagename[sizeof("bash\n")];
	size_t len;
	FILE *in = spawn_pipe(&child, cmd, -1);
	int diverted = 1;

	/* Is $path diverted by someone other than bash? */

	len = fread(packagename, 1, sizeof(packagename), in);
	if (ferror(in))
		die_errno("cannot read from dpkg-divert");
	if (len == 0)
		diverted = 0;	/* No diversion. */
	if (len == strlen("bash\n") && !memcmp(packagename, "bash\n", len))
		diverted = 0;	/* Diverted by bash. */

	if (fclose(in))
		die_errno("cannot close read end of pipe");
	wait_or_die(child, "dpkg-divert", ERROR_OK | SIGPIPE_OK);
	return !diverted;
}

int main(int argc, char *argv[])
{
	/* /bin/sh needs to point to a valid target. */

	if (access("/bin/sh", X_OK)) {
		backup("/bin/sh", "/bin/sh.distrib");
		backup("/usr/share/man/man1/sh.1.gz",
			"/usr/share/man/man1/sh.distrib.1.gz");

		force_symlink("bash", "/bin/sh", "/bin/sh.temp");
		force_symlink("bash.1.gz", "/usr/share/man/man1/sh.1.gz",
			"/usr/share/man/man1/sh.1.gz.temp");
	}
	if (!binsh_in_filelist("bash"))
		/* Ready. */
		return 0;

	/*
	 * In bash (<= 4.1-3), the bash package included symlinks for
	 * /bin/sh and the sh(1) manpage in its data.tar.
	 *
	 * Unless we are careful, unpacking the new version of bash
	 * will remove them.  So we tell dpkg that the files from bash
	 * to be removed are elsewhere, using a diversion on behalf of
	 * another package.
	 *
	 * Based on an idea by Michael Stone.
	 * “You're one sick individual.” -- Anthony Towns
	 * http://bugs.debian.org/cgi-bin/bugreport.cgi?msg=85;bug=34717
	 */
	if (undiverted("/bin/sh"))
		reset_diversion("dash", "/bin/sh", "/bin/sh.distrib");
	if (undiverted("/usr/share/man/man1/sh.1.gz"))
		reset_diversion("dash", "/usr/share/man/man1/sh.1.gz",
				"/usr/share/man/man1/sh.distrib.1.gz");
	return 0;
}
