#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "anna.h"
#include "util.h"

static void oom_die (void)
{
	fputs ("Out of memory!\n", stderr);
	exit (1);
}

char *xvasprintf (const char *fmt, va_list ap) {
	char *ret;

	if (vasprintf (&ret, fmt, ap) < 0) {
		if (errno == ENOMEM)
			oom_die();
		return NULL;
	}

	return ret;
}

char *xasprintf (const char *fmt, ...) {
	va_list ap;
	char *ret;

	va_start (ap, fmt);
	ret = xvasprintf (fmt, ap);
	va_end (ap);

	return ret;
}

int get_lowmem_level (void) {
	int l;
	l=open(LOWMEM_STATUS_FILE, O_RDONLY);
	if (l != -1) {
		char buf[2];
		if (read(l, buf, 1) != 1) {
			close(l);
			return 0;
		}
		close(l);
		return atoi(buf);
	}
	return 0;
}

int is_queued(di_package *package) {
	FILE *fp;
	char buf[1024];

	if ((fp = fopen("/var/lib/anna-install/queue", "r")) != NULL) {
		while (fgets(buf, sizeof(buf), fp)) {
			buf[strlen(buf) - 1] = '\0';

			if (strcmp(buf, package->package) == 0) {
				fclose(fp);
				return 1;
			}
		}
		fclose(fp);
	}

	return 0;
}

/* This function checks if p is in the given list of installed packages
 * and compares versions. */
bool is_installed(di_package *p, di_packages *status) {
	di_package *q;
	di_package_version *pv, *qv;
	bool ret;

	/* If we don't understand the version number, we play safe
	 * and assume we should install it */
	if (p->version == NULL || !(pv = di_package_version_parse(p)))
		return false;
	q = di_packages_get_package(status, p->package, 0);
	if (q == NULL || q->version == NULL || !(qv = di_package_version_parse(q)))
		return false;
	ret = (di_package_version_compare(pv, qv) <= 0);
	di_package_version_free(pv);
	di_package_version_free(qv);

	return ret;
}

static size_t choice_strcpy(char *dest, char *src, size_t size) {
	size_t n=0;

	while (*src && (n < size-2)) {
		if (*src == ',')
			dest[n++] = '\\';
		dest[n++] = *src++;
	}
	dest[n] = '\0';

	return n;
}

size_t package_to_choice(di_package *package, char *buf, size_t size) {
	int n;
	n  = choice_strcpy(buf, package->package, size);
	n += choice_strcpy(buf+n, ": ", size-n);
	n += choice_strcpy(buf+n, package->short_description, size-n);
	return n;
}

char *list_to_choices(di_package **packages, int c_values) {
	char buf[200], *ret;
	int count = 0;
	size_t ret_size = 1024, ret_used = 1, size;
	di_package *p;

	ret = di_malloc(1024);
	ret[0] = '\0';
	while ((p = packages[count])) {
		if (c_values)
			size = choice_strcpy(buf, p->package, 200);
		else
			size = package_to_choice(p, buf, 200);
		if (ret_used + size + 2 > ret_size) {
			ret_size += 1024;
			ret = di_realloc(ret, ret_size);
		}
		strcat(ret, buf);
		ret_used += size + 2;
		count++;
		if (packages[count])
			strcat(ret, ", ");
	}

	return ret;
}

int unpack_package (const char *pkgfile) {
	char *command;
	int ret;

	command = xasprintf("%s %s", DPKG_UNPACK_COMMAND, pkgfile);
	ret = !di_exec_shell_log(command);
	free(command);
	return ret;
}

int configure_package (const char *package) {
	char *command;
	int ret;

	command = xasprintf("%s %s", DPKG_CONFIGURE_COMMAND, package);
	ret = !di_exec_shell_log(command);
	free(command);
	return ret;
}

#ifdef LOADTEMPLATES
int load_templates (di_packages *packages) {
	di_slist_node *node;
	size_t command_size = 1024, command_len;
	char *command = di_malloc(command_size);
	bool found_templates = false;
	int ret;

	if (!command)
		return 0;

	strcpy(command, "debconf-loadtemplate d-i");
	command_len = strlen(command);

	for (node = packages->list.head; node; node = node->next) {
		di_package *package = node->data;
		char *arg = NULL;
		struct stat st;

		if (package->type != di_package_type_real_package ||
		    package->status_want != di_package_status_want_install)
			continue;
		arg = xasprintf("%s/%s.templates", INFO_DIR, package->package);
		if (stat(arg, &st) == -1) {
			free(arg);
			continue;
		}
		while (command_len + 1 + strlen(arg) >= command_size) {
			command_size *= 2;
			command = di_realloc(command, command_size);
			if (!command) {
				free(arg);
				return 0;
			}
		}
		command[command_len] = ' ';
		strcpy(command + command_len + 1, arg);
		command_len += 1 + strlen(arg);
		free(arg);
		found_templates = true;
	}
	if (!found_templates)
		return 0;
	ret = !di_exec_shell_log(command);
	di_free(command);

	/* Delete templates after loading. */
	for (node = packages->list.head; node; node = node->next) {
		di_package *package = node->data;
		char *arg = NULL;

		if (package->type != di_package_type_real_package ||
		    package->status_want != di_package_status_want_install)
			continue;
		arg = xasprintf("%s/%s.templates", INFO_DIR, package->package);
		unlink(arg);
		free(arg);
	}

	return ret;
}
#endif /* LOADTEMPLATES */

/* Check whether the md5sum of file matches sum. If not, return 0. */
int md5sum(const char *sum, const char *file) {
	FILE *fp;
	char line[1024];

	/* Trivially true if the Packages file doesn't have md5sum lines */
	if (sum == NULL)
		return 1;
	snprintf(line, sizeof(line), "/usr/bin/md5sum %s", file);
	fp = popen(line, "r");
	if (fp == NULL)
		return 0;
	if (fgets(line, sizeof(line), fp) != NULL) {
		pclose(fp);
		if (strlen(line) < 32)
			return 0;
		line[32] = '\0';
		return !strcmp(line, sum);
	}
	pclose(fp);
	return 0;
}

/* Used to qsort a package array by name. */
int package_name_compare(const void *v1, const void *v2) {
	di_package *p1, *p2;

	p1 = *(di_package **)v1;
	p2 = *(di_package **)v2;
	return strcmp(p1->package, p2->package);
}

/* The INCLUDE_FILE lists packages that should be installed by default. */
void take_includes(di_packages *packages) {
	di_package *p;
	FILE *fp;
	char buf[1024], *ptr;
	di_slist_node *node;

	if ((fp = fopen(INCLUDE_FILE, "r")) == NULL)
		return;
	while (fgets(buf, sizeof(buf), fp) != NULL) {
		if (buf[0] == '#')
			continue;
		if ((ptr = strchr(buf, '\n')) != NULL)
			*ptr = '\0';

		for (node = packages->list.head; node; node = node->next) {
			p = node->data;
			if (strcmp(p->package, buf) == 0)
				p->status_want = di_package_status_want_install;
		}
	}
	fclose(fp);
}

/* While the EXCLUDE file lists packages that should not be installed by
 * default. */
void drop_excludes(di_packages *packages) {
	di_package *p;
	FILE *fp;
	char buf[1024], *ptr;
	di_slist_node *node;

	if ((fp = fopen(EXCLUDE_FILE, "r")) == NULL)
		return;
	while (fgets(buf, sizeof(buf), fp) != NULL) {
		if (buf[0] == '#')
			continue;
		if ((ptr = strchr(buf, '\n')) != NULL)
			*ptr = '\0';

		for (node = packages->list.head; node; node = node->next) {
			p = node->data;
			if (strcmp(p->package, buf) == 0)
				p->status_want = di_package_status_want_deinstall;
		}
	}
	fclose(fp);
}
