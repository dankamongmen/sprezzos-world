#include "udpkg.h"

#include <errno.h>
#include <fcntl.h>
#include <search.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <utime.h>
#include <getopt.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <debian-installer.h>

static int force_configure = 0;
static int loadtemplate = 1;

static const char *data_member_base = "data.tar.";

/* 
 * Main udpkg implementation routines
 */

static int is_file(const char *fn)
{
	struct stat statbuf;
	
	if (stat(fn, &statbuf) < 0) return 0;
	return S_ISREG(statbuf.st_mode);
}

static int dpkg_print_architecture()
{
	puts(ARCH_TEXT);
	return 0;
}

static int dpkg_print_os()
{
	puts(OS_TEXT);
	return 0;
}

static int dpkg_copyfile(const char *src, const char *dest)
{
	/* copy a (regular) file if it exists, preserving the mode, mtime 
	 * and atime */
	char buf[8192];
	int infd, outfd;
	int r;
	struct stat srcStat;
	struct utimbuf times;

	if (stat(src, &srcStat) < 0) 
	{
		if (errno == ENOENT) return 0; else return -1;
	}
	if ((infd = open(src, O_RDONLY)) < 0) 
		return -1;
	if ((outfd = open(dest, O_WRONLY|O_CREAT|O_TRUNC, srcStat.st_mode)) < 0)
		return -1;
	while ((r = read(infd, buf, sizeof(buf))) > 0)
	{
		if (write(outfd, buf, r) < 0)
			return -1;
	}
	close(outfd);
	close(infd);
	if (r < 0) return -1;
	times.actime = srcStat.st_atime;
	times.modtime = srcStat.st_mtime;
	if (utime(dest, &times) < 0) return -1;
	return 1;
}

static int dpkg_doconfigure(struct package_t *pkg)
{
	int r;
	char postinst[1024];
	char config[1024];
	char buf[1024];
	DPRINTF("Configuring %s [force=%d]\n", pkg->package, force_configure);

	if ((pkg->status & STATUS_STATUSINSTALLED) != 0 && !force_configure)
	{
		PRINTF("Package %s is already installed and configured\n",
			pkg->package);
		return 1;
	}

	pkg->status &= STATUS_STATUSMASK;

	snprintf(config, sizeof(config), "%s%s.config", INFODIR, pkg->package);
	if (is_file(config))
	{
		snprintf(buf, sizeof(buf), "exec %s configure", config);
		if ((r = di_exec_shell_log(buf)) != 0)
		{
			FPRINTF(stderr, "config exited with status %d\n", r);
			pkg->status |= STATUS_STATUSHALFCONFIGURED;
			return 1;
		}
	}

	snprintf(postinst, sizeof(postinst), "%s%s.postinst", INFODIR, pkg->package);
	if (is_file(postinst))
	{
		snprintf(buf, sizeof(buf), "exec %s configure", postinst);
		if ((r = di_exec_shell_log(buf)) != 0)
		{
			FPRINTF(stderr, "%s's postinst exited with status %d\n",
				pkg->package, di_exec_mangle_status(r));
			pkg->status |= STATUS_STATUSHALFCONFIGURED;
			return di_exec_mangle_status(r);
		}
	}

	pkg->status |= STATUS_STATUSINSTALLED;
	
	return 0;
}

typedef enum compression_type compression_type;
enum compression_type {
	gz_compression,
	xz_compression,
	unknown_compression,
};

static const char *compression_extension (const compression_type t) {
	switch (t) {
		case gz_compression: return "gz";
		case xz_compression: return "xz";
		default: return "";
	}
}

static const char *decompression_tool (const compression_type t) {
	switch (t) {
		case gz_compression: return "gunzip";
		case xz_compression: return "unxz";
		default: return "";
	}
}

static compression_type get_compression_type (struct package_t *pkg) {
	FILE *infp = NULL;
	char buf[1024];
	char *extension = NULL;

	snprintf(buf, sizeof(buf), "ar -t %s", pkg->file);
	if ((infp = popen(buf, "r")) == NULL)
	{
		FPRINTF(stderr, "Cannot retrieve archive members of %s: %s\n",
			pkg->file, strerror(errno));
		return unknown_compression;
	}

	while (fgets(buf, sizeof(buf), infp)) {
		if (strncmp(buf, data_member_base, strlen(data_member_base)) == 0) {
			extension = buf + strlen(data_member_base);
			if (extension[strlen(extension) - 1] == '\n')
				extension[strlen(extension) - 1] = '\0';
			break;
		}
	}
	pclose(infp);

	if (extension == NULL) {
		FPRINTF(stderr, "No %s* found in %s\n",
			data_member_base, pkg->file);
		return unknown_compression;
	}

	if (strcmp(extension, compression_extension(gz_compression)) == 0) {
		return gz_compression;
	}
	else if (strcmp(extension, compression_extension(xz_compression)) == 0) {
		return xz_compression;
	}
	else {
		FPRINTF(stderr, "Invalid compression type for %s* of %s\n",
			data_member_base, pkg->file);
		return unknown_compression;
	}
}

static int dpkg_dounpack(struct package_t *pkg)
{
	int r = 0;
	char *cwd;
	char buf[1024], buf2[1024];
	unsigned int i;
	const char *adminscripts[] = {
		"prerm", "postrm", "preinst", "postinst",
	        "conffiles", "md5sums", "shlibs",
		"templates", "menutest", "isinstallable",
		"config"
	};
#ifdef DOREMOVE
	char *p;
	FILE *outfp = NULL;
	FILE *infp = NULL;
#endif
	compression_type compression_type;

	DPRINTF("Unpacking %s\n", pkg->package);

	compression_type = get_compression_type(pkg);
	if (compression_type == unknown_compression)
		return 1;

	cwd = getcwd(0, 0);
	chdir("/");

	snprintf(buf, sizeof(buf), "ar -p %s %s%s|%s -c|tar -x",
		pkg->file, data_member_base,
		compression_extension(compression_type),
		decompression_tool(compression_type));
	if ((r = di_exec_shell_log(buf)) == 0)
	{
		/* Installs the package scripts into the info directory */
		for (i = 0; i < sizeof(adminscripts) / sizeof(adminscripts[0]);
		     i++)
		{
			int ret;

			snprintf(buf, sizeof(buf), "%s%s/%s",
				DPKGCIDIR, pkg->package, adminscripts[i]);
			snprintf(buf2, sizeof(buf), "%s%s.%s", 
				INFODIR, pkg->package, adminscripts[i]);

			ret = dpkg_copyfile(buf, buf2);

			if (ret < 0)
			{
				FPRINTF(stderr, "Cannot copy %s to %s: %s\n", 
					buf, buf2, strerror(errno));
				r = 1;
				break;
			}
			else if (ret > 0)
			{
#ifdef DOLOADTEMPLATE
				/* Is this the templates file?  If
				 * so, call debconf-loadtemplate on it
				 */
				if (loadtemplate &&
				    strcmp(adminscripts[i], "templates") == 0) {
					/* Possibly reduce templates prior
					 * to loading. Done on lowmem
					 * installs. */
					snprintf(buf, sizeof(buf),
					         "trimtemplates %s", buf2);
					di_exec_shell_log(buf);
					
					snprintf(buf, sizeof(buf),
						 "debconf-loadtemplate %s %s",
						 pkg->package, buf2);
					if ((r = di_exec_shell_log(buf)) != 0)
						FPRINTF(stderr,
							"debconf-loadtemplate "
							"exited with status "
							"%d\n", r);
					/* Delete templates after loading. */
					unlink(buf2);
				}
#endif

			}
		}

// Only generate .list files when we need --remove support
#ifdef DOREMOVE
		/* ugly hack to create the list file; should
		 * probably do something more elegant
		 *
		 * why oh why does dpkg create the list file
		 * so oddly...
		 */
		snprintf(buf, sizeof(buf),
			"ar -p %s %s%s|%s -c|tar -t",
			pkg->file, data_member_base,
			compression_extension(compression_type),
			decompression_tool(compression_type));
		snprintf(buf2, sizeof(buf2),
			"%s%s.list", INFODIR, pkg->package);
		if ((infp = popen(buf, "r")) == NULL ||
		    (outfp = fopen(buf2, "w")) == NULL)
		{
			FPRINTF(stderr, "Cannot create %s\n",
				buf2);
			r = 1;
		}
		else
			while (fgets(buf, sizeof(buf), infp) &&
			       !feof(infp))
			{
				p = buf;
				if (*p == '.') p++;
				if (*p == '/' && *(p+1) == '\n')
				{
					*(p+1) = '.';
					*(p+2) = '\n';
					*(p+3) = 0;
				}
				if (p[strlen(p)-2] == '/')
				{
					p[strlen(p)-2] = '\n';
					p[strlen(p)-1] = 0;
				}
				fputs(p, outfp);
			}
		pclose(infp);
		fclose(outfp);
#endif

		pkg->status &= STATUS_WANTMASK;
		pkg->status |= STATUS_WANTINSTALL;
		pkg->status &= STATUS_FLAGMASK;
		pkg->status |= STATUS_FLAGOK;
		pkg->status &= STATUS_STATUSMASK;
		if (r == 0)
			pkg->status |= STATUS_STATUSUNPACKED;
		else
			pkg->status |= STATUS_STATUSHALFINSTALLED;
	}
	else
		FPRINTF(stderr, "%s exited with status %d\n", buf, r);

	chdir(cwd);
	return r;
}

static int dpkg_doinstall(struct package_t *pkg)
{
	DPRINTF("Installing %s\n", pkg->package);
	return (dpkg_dounpack(pkg) || dpkg_doconfigure(pkg));
}

static int dpkg_unpackcontrol(struct package_t *pkg)
{
	int r;
	char *cwd = 0;
	char *p;
	char buf[1024], buf2[1024];
	FILE *f;

	p = strrchr(pkg->file, '/');
	if (p) p++; else p = pkg->file;
	p = pkg->package = strdup(p);
	while (*p != 0 && *p != '_' && *p != '.') p++;
	*p = 0;
	p = pkg->package;

	cwd = getcwd(0, 0);
	snprintf(buf, sizeof(buf), "%s%s", DPKGCIDIR, pkg->package);
	DPRINTF("dir = %s\n", buf);
	if (mkdir(buf, S_IRWXU) != 0)
	{
		FPRINTF(stderr, "mkdir %s: %s\n", buf, strerror(errno));
		return 1;
	}
	if (chdir(buf) != 0)
	{
		FPRINTF(stderr, "chdir %s: %s\n", buf, strerror(errno));
		return 1;
	}
	snprintf(buf, sizeof(buf), "ar -p %s control.tar.gz|tar -xzf -",
		pkg->file);
	if ((r = di_exec_shell_log(buf)) != 0)
	{
		FPRINTF(stderr, "%s exited with status %d\n", buf, r);
		return r;
	}
	if ((f = fopen("control", "r")) == NULL) 
	{
		FPRINTF(stderr, "fopen control: %s\n", strerror(errno));
		return 1;
	}
	control_read(f, pkg);
	if (strcmp(pkg->package, p) != 0) 
	{
		snprintf(buf, sizeof(buf), "%s%s", DPKGCIDIR, p);
		snprintf(buf2, sizeof(buf2), "%s%s", DPKGCIDIR, pkg->package);
		if (rename(buf, buf2) != 0)
		{
			FPRINTF(stderr, "rename %s %s: %s\n",
				buf, buf2, strerror(errno));
			return 1;
		}
	}
	free(p);

	chdir(cwd);
	free(cwd);
	return 0;
}

static int dpkg_unpack(struct package_t *pkgs)
{
	int r = 0;
	struct package_t *pkg;
	void *status = status_read();

	if (di_exec_shell_log("rm -rf -- " DPKGCIDIR) != 0 ||
	    mkdir(DPKGCIDIR, S_IRWXU) != 0)
	{
		perror("mkdir");
		return 1;
	}
	
	for (pkg = pkgs; pkg != 0; pkg = pkg->next)
	{
		dpkg_unpackcontrol(pkg);
		r = dpkg_dounpack(pkg);
		if (r != 0) break;
	}
	status_merge(status, pkgs);
	if (di_exec_shell_log("rm -rf -- " DPKGCIDIR) != 0)
		r = 1;
	return r;
}

static int dpkg_configure(struct package_t *pkgs)
{
	int r = 0;
	void *found;
	struct package_t *pkg;
	void *status = status_read();
	for (pkg = pkgs; pkg != 0 && r == 0; pkg = pkg->next)
	{
		found = tfind(pkg, &status, package_compare);
		if (found == 0)
		{
			FPRINTF(stderr, "Trying to configure %s, but it is not installed\n", pkg->package);
			r = 1;
		}
		else
		{
			/* configure the package listed in the status file;
			 * not pkg, as we have info only for the latter */
			r = dpkg_doconfigure(*(struct package_t **)found);
		}
	}
	status_merge(status, 0);
	return r;
}

static int dpkg_install(struct package_t *pkgs)
{
	struct package_t *p, *ordered = 0;
	void *status = status_read();
	if (di_exec_shell_log("rm -rf -- " DPKGCIDIR) != 0 ||
	    mkdir(DPKGCIDIR, S_IRWXU) != 0)
	{
		perror("mkdir");
		return 1;
	}
	
	/* Stage 1: parse all the control information */
	for (p = pkgs; p != 0; p = p->next)
		if (dpkg_unpackcontrol(p) != 0)
		{
			/* force loop break, and prevents further ops */
			pkgs = 0;
		}
	
	/* Stage 2: resolve dependencies */
#ifdef DODEPENDS
	ordered = depends_resolve(pkgs, status);
#else
	ordered = pkgs;
#endif
	
	/* Stage 3: install */
	for (p = ordered; p != 0; p = p->next)
	{
		p->status &= STATUS_WANTMASK;
		p->status |= STATUS_WANTINSTALL;

		/* for now the flag is always set to ok... this is probably
		 * not what we want
		 */
		p->status &= STATUS_FLAGMASK;
		p->status |= STATUS_FLAGOK;

		/* don't worry about errors here; error messages are printed
		 * internally
		 */
		dpkg_doinstall(p);
	}
	
	if (ordered != 0)
		status_merge(status, pkgs);
	return di_exec_shell_log("rm -rf -- " DPKGCIDIR);
}

static void reqarg (struct package_t *pkg) {
	if (pkg == NULL) {
		FPRINTF(stderr, "udpkg: Missing argument.\n");
		exit(1);
	}
}

static int dpkg_fields(struct package_t *pkg)
{  
	char *command;
	int ret;

	reqarg(pkg);

	asprintf(&command, "ar -p %s control.tar.gz|tar -xzOf - ./control", pkg->file);
	ret = system(command);
	free(command);
	return ret;
}

static int dpkg_contents(struct package_t *pkg)
{  
	char *command;
	int ret;
	compression_type compression_type;

	reqarg(pkg);

	compression_type = get_compression_type(pkg);
	if (compression_type == unknown_compression)
		return 1;

	asprintf(&command, "ar -p %s %s%s|%s|tar t",
		pkg->file,
		data_member_base,
		compression_extension(compression_type),
		decompression_tool(compression_type));
	ret = system(command);
	free(command);
	return ret;
}

static int dpkg_remove(struct package_t *pkgs)
{
#ifdef DOREMOVE
	int r=0;
	struct package_t *p;
	char buf[1024], buf2[1024];
	FILE *fp;
	void *status = status_read();

	for (p = pkgs; p != 0; p = p->next)
	{
		di_debug("Start removing  package %s", p->package);
		snprintf(buf, sizeof(buf),
			 "%s%s.list", INFODIR, p->package);
		if ((fp = fopen(buf, "r")) == NULL)
		{
			FPRINTF(stderr, "Cannot read %s\n",
				buf);
			r = 1;
		}
		else
		{
			while (fgets(buf, sizeof(buf), fp) &&
			       !feof(fp))
			{
				/* we remove only files, not directories */
				if (is_file(buf))
				{
					snprintf(buf2, sizeof(buf2), "rm -f -- %s", buf);
					if (di_exec_shell_log(buf2) != 0)
						r = 1;
					di_debug("File %s removed",buf);
				}
			}
		}

		if (r == 0) {
		  snprintf(buf, sizeof(buf),
			   "rm -f -- %s%s.*", INFODIR, p->package);
		  if (di_exec_shell_log(buf) != 0)
		    r = 1;
		}
		p->status &= STATUS_WANTMASK;
		p->status |= STATUS_WANTDEINSTALL;
		p->status &= STATUS_FLAGMASK;
		p->status |= STATUS_FLAGOK;
		p->status &= STATUS_STATUSMASK;
		if (r == 0)
		  p->status |= STATUS_STATUSNOTINSTALLED;
		else
		  /* do not know which best status flag
		   * should be used */
		  p->status |= STATUS_STATUSHALFINSTALLED;
	}
	status_merge(status, pkgs);
	return r;
#else
	FPRINTF(stderr, "udpkg: No support for -r.\n");
	return 1;
	pkgs = 0; /* avoid -W warning */
#endif
}

#ifdef UDPKG_MODULE
int udpkg(int argc, char **argv)
#else
int main(int argc, char **argv)
#endif
{
	int opt = 0;
	struct package_t *p, *packages = NULL;
	char *cwd = getcwd(0, 0);
	char **origargv = argv;
	struct option longopts[] = {
		/* name, has_arg, flag, val */
		{ "unpack", 0, 0, 'u' },
		{ "configure", 0, 0, 'C' },
		{ "print-architecture", 0, 0, 'a' } ,
		{ "print-os", 0, 0, 'o' } ,
		{ "force-configure", 0, &force_configure, 1 },
		{ "no-loadtemplate", 0, &loadtemplate, 0 },
		{ 0, 0, 0, 0 },
	};

	while (*++argv)
	{
		if (**argv != '-') 
		{
			p = (struct package_t *)di_malloc(sizeof(struct package_t));
			memset(p, 0, sizeof(struct package_t));
			if (**argv == '/')
				p->file = *argv;
			else
				asprintf(&p->file, "%s/%s", cwd, *argv);
			p->package = strdup(*argv);
			p->next = packages;
			packages = p;
		}
	}

	/* let's do this in a silly way, the first pass lets us
	 * set flags (e.g. --force-configure), whereas the second
	 * will actually do stuff
	 */
	while (getopt_long(argc, origargv, "irfc", longopts, 0) >= 0)
		/* nothing */;
	optind = 1;

	while ((opt = getopt_long(argc, origargv, "irfc", longopts, 0)) >= 0)
	{
		switch (opt)
		{
			case 'i': return dpkg_install(packages); break;
			case 'r': return dpkg_remove(packages); break;
			case 'u': return dpkg_unpack(packages); break;
			case 'C': return dpkg_configure(packages); break;
			case 'a': return dpkg_print_architecture(); break;
			case 'o': return dpkg_print_os(); break;
			case 'f': return dpkg_fields(packages); break;
			case 'c': return dpkg_contents(packages); break;
			case 0: /* option, not action */; break;
		}
	}

	/* if it falls through to here, some of the command line options were
	   wrong */
	FPRINTF(stderr, "udpkg [--force-configure] <-i|-r|--unpack|--configure|--print-architecture|--print-os|-f|-c> my.deb\n");
	return 0;
}
