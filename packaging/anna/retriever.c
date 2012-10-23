/*
 * Interface to retrievers. See retriever.txt in the d-i developers
 * documentation for the retriever spec. The retriever_* functions
 * closely correspond to the commands in the spec. Note that those that
 * have int return codes return inverted (shell-style) unless otherwise
 * noted.
 *
 */
#include <cdebconf/debconfclient.h>
#include "anna.h"
#include "retriever.h"
#include "util.h"

char *retriever_command = NULL;
int retriever_newdefault = 0;
int retriever_usecached = 0;

/* Select the name of the retriever to use (for example, "cdrom").
 * If isdefault, the retriever is stored in the debconf db for use as the
 * default retriever later. */
void set_retriever(const char *retriever, int isdefault) {
	asprintf(&retriever_command, "%s/%s", RETRIEVER_DIR, retriever);
	if (isdefault) {
		debconf_set(debconf, DEFAULT_RETRIEVER_VAR, retriever);
		if (get_lowmem_level() == 0) {
			retriever_newdefault = 1;
		}
	}
}

/* Returns the full path of the retriever command, either as set with
 * set_retriever or as stored in the debconf database. */
char *get_retriever(void) {
	if (! retriever_command) {
		debconf_get(debconf, DEFAULT_RETRIEVER_VAR);
		set_retriever(debconf->value, 0);
		if (get_lowmem_level() == 0) {
			retriever_usecached = 1;
		}
	}
	return retriever_command;
}

int retriever_retrieve (di_package *package, char *dest) {
	int ret;
	char *command;

	if (asprintf(&command, "%s retrieve %s %s", get_retriever(), package->filename, dest) == -1)
		return 1;
	ret = di_exec_shell_log(command);
	free(command);
	return ret;
}

int retriever_config(void) {
	char *command;
	int ret;

	if (asprintf(&command, "%s config", get_retriever()) == -1)
		return 1;
	ret = di_exec_shell_log(command);
	free(command);
	return ret;
}

/* Gets the package list from the retriever and parses it. If a new default
 * retriever has been set, caches the file (except in low memory mode), will
 * use the cached file later when asked to install udebs using the default
 * retriever. */
di_packages *retriever_packages(di_packages_allocator *allocator) {
	di_packages *packages;
	char *command;
	int ret;

	if (! retriever_usecached) {
		if (asprintf(&command, "%s packages " DOWNLOAD_PACKAGES, get_retriever()) == -1)
			return NULL;
		ret = di_exec_shell_log(command);
		free(command);
		if (ret != 0)
			return NULL;
		packages = di_system_packages_read_file(DOWNLOAD_PACKAGES, allocator);
	}
	else {
		packages = di_system_packages_read_file(DOWNLOAD_PACKAGES_DEFAULT, allocator);
	}

	if (retriever_newdefault && packages) {
		rename(DOWNLOAD_PACKAGES, DOWNLOAD_PACKAGES_DEFAULT);
	}
	else {
		unlink(DOWNLOAD_PACKAGES);
	}

	if (!packages) {
		di_log(DI_LOG_LEVEL_ERROR, "can't find packages file");
	}
	return packages;
}

/* Retrns 1 if the retriever handled the error and the operation should
 * be retried, -1 if the retriever does not support error handling, and
 * 0 if it was unable to handle the error. */
signed int retriever_error (const char *failing_command) {
	char *command;
	int ret;

	/* There is no mechanism to handle backing up from here, so turn it
	 * off. */
	debconf_capb(debconf, "");

	if (asprintf(&command, "%s error %s", get_retriever(), failing_command) == -1)
		return 0;
	ret = di_exec_shell_log(command);
	free(command);

	debconf_capb(debconf, "backup");

	switch (ret) {
		case 0:
			return 1;
		case 2:
			return 0;
		default:
			return -1;
	}
}

void retriever_cleanup(void) {
	char *command;

	if (asprintf(&command, "%s cleanup", get_retriever()) != -1) {
		di_exec_shell_log(command);
		free(command);
	}
}
