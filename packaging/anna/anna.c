#include <sys/utsname.h>
#include "anna.h"
#include "util.h"
#include "retriever.h"

struct debconfclient *debconf = NULL;
static char *running_kernel = NULL;
static const char *subarchitecture;
static int quiet = 0, verbose = 0;

di_packages *get_packages (void) {
	di_packages_allocator *packages_allocator = di_system_packages_allocator_alloc();
	di_packages *packages = retriever_packages(packages_allocator);

	while (packages == NULL) {
		int r=retriever_error("packages");
		di_log(DI_LOG_LEVEL_WARNING, "bad d-i Packages file");
		if (r != 1) {
			/* Failed to handle error. */
			return NULL;
		}
		else {
			/* Error handled, retry. */
			packages_allocator = di_system_packages_allocator_alloc();
			packages = retriever_packages(packages_allocator);
		}
	}

	return packages;
}

/* Go through the available packages to see if it contains at least
 * one package that is valid for the subarchitecture and corresponds
 * to the kernel version we are running */
int packages_ok (di_packages *packages) {
	di_slist_node *node;
	di_package *package;
	bool kernel_packages_present = false;

	for (node = packages->list.head; node; node = node->next) {
		package = node->data;

		if (!di_system_package_check_subarchitecture(package, subarchitecture))
			continue;
	 	if (((di_system_package *)package)->kernel_version) {
			if (running_kernel &&
			    strcmp(running_kernel, ((di_system_package *)package)->kernel_version) == 0) {
				kernel_packages_present = true;
				break;
			}
		}
	}

	if (!kernel_packages_present) {
		di_log(DI_LOG_LEVEL_WARNING, "no packages matching running kernel %s in archive", running_kernel);
#ifdef __GNU__
		/* GNU Mach does not have modules */
#else
		debconf_input(debconf, "critical", "anna/no_kernel_modules");
		if (debconf_go(debconf) == 30)
			return 0;
		debconf_get(debconf, "anna/no_kernel_modules");
		if (strcmp(debconf->value, "false") == 0)
			return 0;
#endif
	}

	return 1;
}

static int choose_modules(di_packages *status, di_packages **packages) {
	char *choose_modules_question = "anna/choose_modules" ;
	char *question_priority = "medium";
	char *choices_c, *choices;
	int package_count = 0;
	di_package *package, *status_package, **package_array;
	di_slist_node *node, *node1;
	bool standard_modules = true;
	bool lowmem_mode = false;

	/* Test lowmem level to know if packages with want_install status
	   will be shown */
	if ( get_lowmem_level() >= 2) {
		lowmem_mode = true;
		choose_modules_question="anna/choose_modules_lowmem";
		/* force priority to show question even in a non expert mode */
		question_priority = "high";
		di_log (DI_LOG_LEVEL_DEBUG,
			"lowmem_mode, want_install status packages will be shown");
	}

	for (node = status->list.head; node; node = node->next) {
		status_package = node->data;
		package = di_packages_get_package(*packages, status_package->package, 0);
		if (!package)
			continue;
		package->status = status_package->status;
		if (status_package->status == di_package_status_unpacked || status_package->status == di_package_status_installed) {
			for (node1 = package->depends.head; node1; node1 = node1->next) {
				di_package_dependency *d = node1->data;
				if (d->type == di_package_dependency_type_reverse_enhances) {
					package->status_want = di_package_status_want_install;
					if (verbose)
						di_log (DI_LOG_LEVEL_DEBUG, "install %s, enhances installed packages %s", package->package, status_package->package);
				}
			}
		}
	}

	debconf_get(debconf, "anna/standard_modules");
	if (strcmp(debconf->value, "false") == 0)
		standard_modules = false;

	for (node = (*packages)->list.head; node; node = node->next) {
		package = node->data;

		package->status_want = di_package_status_want_deinstall;

		if (package->type != di_package_type_real_package)
			continue;
		if (is_installed(package, status))
			continue;
		if (!di_system_package_check_subarchitecture(package, subarchitecture))
			continue;

		if (((di_system_package *)package)->kernel_version) {
			if (running_kernel && strcmp(running_kernel, ((di_system_package *)package)->kernel_version) == 0) {
				package->status_want = di_package_status_want_unknown;
				if (verbose)
					di_log (DI_LOG_LEVEL_DEBUG, "ask for %s, matches kernel", package->package);
			}
			else {
				continue;
			}

			for (node1 = package->depends.head; node1; node1 = node1->next) {
				di_package_dependency *d = node1->data;
				if (d->type == di_package_dependency_type_provides
				    && d->ptr && is_queued(d->ptr)) {
					package->status_want = di_package_status_want_install;
					if (verbose)
						di_log (DI_LOG_LEVEL_DEBUG, "install %s, queued by anna-install", package->package);
					continue;
				}
			}
		}

		if (package->priority >= di_package_priority_standard) {
			if (standard_modules || ((di_system_package *)package)->kernel_version) {
				package->status_want = di_package_status_want_install;
				if (verbose)
					di_log (DI_LOG_LEVEL_DEBUG, "install %s, priority >= standard", package->package);
			}
			else {
				package->status_want = di_package_status_want_unknown;
				if (verbose)
					di_log (DI_LOG_LEVEL_DEBUG, "ask for %s, priority >= standard", package->package);
			}
		}
		else if (is_queued(package)) {
			package->status_want = di_package_status_want_install;
			if (verbose)
				di_log (DI_LOG_LEVEL_DEBUG, "install %s, queued by anna-install", package->package);
		}
		else if (((di_system_package *)package)->installer_menu_item
			/* we don't want to see installed packages in choices list*/
		         && package->status != di_package_status_installed) {
			package->status_want = di_package_status_want_unknown;
			if (verbose)
				di_log (DI_LOG_LEVEL_DEBUG, "ask for %s, is menu item", package->package);
		}
	}

	/* Include packages in udeb_include */
	take_includes(*packages);

	/* Drop packages in udeb_exclude */
	drop_excludes(*packages);

	/* in lowmem mode, we add all packages in the instlist by setting
	   status to want_unknown except those which are menu item and
	   their dependencies (calculated by the next function) */
	if (lowmem_mode) {
		for (node = (*packages)->list.head; node; node = node->next) {
			package = node->data;
			if (package->status_want == di_package_status_want_install &&
			    ((di_system_package *)package)->installer_menu_item == 0 &&
			    !is_queued(package)) {
				package->status_want = di_package_status_want_unknown;
			}
		}
	}

	di_system_packages_resolve_dependencies_mark_anna(*packages, subarchitecture, running_kernel);

	/* Slight over-allocation, but who cares */
	package_array = di_new0(di_package *, di_hash_table_size((*packages)->table));
	/* Now build the asklist, figuring out which packages have been
	 * pulled into instlist */
	for (node = (*packages)->list.head; node; node = node->next) {
		package = node->data;
		if (package->status_want == di_package_status_want_unknown)
			package_array[package_count++] = package;
	}

	qsort(package_array, package_count, sizeof(di_package *), package_name_compare);
	choices_c = list_to_choices(package_array, true);
	choices = list_to_choices(package_array, false);
	debconf_subst(debconf, choose_modules_question, "CHOICES-C", choices_c);
	debconf_subst(debconf, choose_modules_question, "CHOICES", choices);

	debconf_input(debconf, question_priority, choose_modules_question);

	di_free(choices_c);
	di_free(choices);
	di_free(package_array);

	if (debconf_go(debconf) == 30)
		return 1;

	debconf_get(debconf, choose_modules_question);
	if (debconf->value != NULL) {
		char *choices = debconf->value;

		for (node = (*packages)->list.head; node; node = node->next) {
			package = node->data;
			/* Not very safe, but at least easy ;) */
			if (strstr(choices, package->package) != NULL)
				package->status_want = di_package_status_want_install;
		}
	}

	return 0;
}

void resume_progress_bar (int progress_step, int step_count, di_package *package) {
	debconf_progress_start(debconf, 0, step_count, "anna/progress_title");
	debconf_progress_set(debconf, progress_step);
	debconf_subst(debconf, "anna/progress_step_retr", "PACKAGE", package->package);
	debconf_progress_info(debconf, "anna/progress_step_retr");
}

static int
install_modules(di_packages *status, di_packages *packages) {
	di_slist_node *node;
	di_package *package;
	char *f, *fp, *dest_file;
	int step_count = 0;
	int progress_step=0;

	di_system_packages_resolve_dependencies_mark_anna(packages, subarchitecture, running_kernel);

	for (node = packages->list.head; node; node = node->next) {
		package = node->data;
		if (package->status_want == di_package_status_want_install && !is_installed(package, status)) {
			if (package->type == di_package_type_real_package)
				step_count++;
		} else
			package->status_want = di_package_status_want_unknown;
	}

	/* Short-circuit if there's no packages to install. */
	if (step_count <= 0)
		return 0;

#ifdef LOADTEMPLATES
	/* One step for loading templates. */
	step_count++;
#endif /* LOADTEMPLATES */

	for (node = packages->list.head; node; node = node->next) {
		package = node->data;
		/* One extra step for each package to be configured
		 * immediately.
		 */
		if (package->type == di_package_type_real_package && package->status_want == di_package_status_want_install && !((di_system_package *)package)->installer_menu_item)
			step_count++;
	}

	if (!quiet)
		debconf_progress_start(debconf, 0, step_count, "anna/progress_title");

	/* Retrieval and unpack pass. */
	for (node = packages->list.head; node; node = node->next) {
		package = node->data;
		if (package->type == di_package_type_real_package && package->status_want == di_package_status_want_install) {
			if (!package->filename) {
				di_log(DI_LOG_LEVEL_ERROR, "no Filename field for %s, ignoring", package->package);
				continue;
			}
			for (f = fp = package->filename; *fp != 0; fp++) {
				if (*fp == '/')
					f = ++fp;
			}
			dest_file = xasprintf("%s/%s", DOWNLOAD_DIR, f);

			di_log (DI_LOG_LEVEL_DEBUG, "retrieving %s %s", package->package, package->version);
			if (!quiet) {
				debconf_subst(debconf, "anna/progress_step_retr", "PACKAGE", package->package);
				debconf_progress_info(debconf, "anna/progress_step_retr");
			}
			for (;;) {
				if (retriever_retrieve(package, dest_file)) {
					di_log(DI_LOG_LEVEL_WARNING, "package retrieval failed");
					if (!quiet)
						/* error handling may use a progress bar, so stop the current one */
						debconf_progress_stop(debconf);
					if (retriever_error("retrieve") != 1) {
						/* Failed to handle error. */
						free(dest_file);
						return 6;
					}
					else {
						/* Handled error, retry. */
						if (!quiet)
							resume_progress_bar(progress_step, step_count, package);
						continue;
					}
				}

				if (! md5sum(package->md5sum, dest_file)) {
					di_log(DI_LOG_LEVEL_WARNING, "bad md5sum");
					if (!quiet)
						/* error handling may use a progress bar, so stop the current one */
						debconf_progress_stop(debconf);
					if (retriever_error("retrieve") != 1) {
						/* Failed to handle error. */
						unlink(dest_file);
						free(dest_file);
						return 7;
					}
					else {
						/* Handled error, retry. */
						if (!quiet)
							resume_progress_bar(progress_step, step_count, package);
						continue;
					}
				}

				break;
			}

			if (!unpack_package(dest_file)) {
				if (!quiet)
					debconf_progress_stop(debconf);
				debconf_subst(debconf, "anna/install_failed", "PACKAGE", package->package);
				debconf_input(debconf, "critical", "anna/install_failed");
				debconf_go(debconf);
				unlink(dest_file);
				free(dest_file);
				return 8;
			}

			unlink(dest_file);
			free(dest_file);
			if (!quiet) {
				debconf_progress_step(debconf, 1);
				progress_step++;
			}
		}
	}

#ifdef LOADTEMPLATES
	/* Load debconf templates. We do this just once to avoid having to
	 * rewrite the templates database over and over again.
	 */
	debconf_progress_info(debconf, "anna/progress_step_loadtemplates");
	load_templates(packages);
	if (!quiet) {
		debconf_progress_step(debconf, 1);
		progress_step++;
	}
#endif /* LOADTEMPLATES */

	/* Configuration pass. */
	for (node = packages->list.head; node; node = node->next) {
		package = node->data;
		if (package->type == di_package_type_real_package && package->status_want == di_package_status_want_install) {
			if (!package->filename) {
				di_log(DI_LOG_LEVEL_ERROR, "no Filename field for %s, ignoring", package->package);
				continue;
			}
			if (((di_system_package *)package)->installer_menu_item)
				continue;

			if (!quiet) {
				debconf_subst(debconf, "anna/progress_step_conf", "PACKAGE", package->package);
				debconf_progress_info(debconf, "anna/progress_step_conf");
			}

			if (!configure_package(package->package)) {
				if (!quiet)
					debconf_progress_stop(debconf);
				debconf_subst(debconf, "anna/install_failed", "PACKAGE",
				              package->package);
				debconf_input(debconf, "critical", "anna/install_failed");
				debconf_go(debconf);
				return 8;
			}

			if (!quiet) {
				debconf_progress_step(debconf, 1);
				progress_step++;
			}
		}
	}

	if (!quiet)
		debconf_progress_stop(debconf);

	return 0;
}

int main(int argc, char **argv) {
	int ret;
	di_packages *packages, *status;
	di_packages_allocator *status_allocator;
	struct utsname uts;
	const char *quiet_env, *verbose_env;

	debconf = debconfclient_new();
	debconf_capb(debconf, "backup");

	di_system_init("anna");

	subarchitecture = di_system_subarch_analyze();

	if (uname(&uts) == 0) {
#ifdef __GNU__
		if (!strncmp(uts.version, "GNU-Mach ", 9)) {
			char *slash = index(uts.version, '/');
			if (slash)
				running_kernel = strndup (uts.version + 9, slash - (uts.version + 9));
			else
				running_kernel = strdup (uts.version + 9);
		} else
			running_kernel = strdup(uts.version);
#else
		running_kernel = strdup(uts.release);
#endif
	}

	quiet_env = getenv("ANNA_QUIET");
	if (quiet_env && strcmp(quiet_env, "1") == 0)
		quiet = 1;
	verbose_env = getenv("ANNA_VERBOSE");
	if (verbose_env && strcmp(verbose_env, "1") == 0)
		verbose = 1;

	status_allocator = di_system_packages_allocator_alloc();
	status = di_system_packages_status_read_file(DI_SYSTEM_DPKG_STATUSFILE, status_allocator);

	if (argc <= 1) {
		fprintf(stderr, "need parameters\n");
		exit(1);
	}
	else if (strcmp(argv[1], "install") == 0) {
		di_slist_node *node;
		di_package *package = NULL;
		int i;

		if (get_retriever() == NULL) {
			fprintf(stderr, "no default retriever set\n");
			exit(1);
		}
		retriever_config();

		packages = get_packages();
		if (! packages) {
			retriever_cleanup();
			return 10;
		}

		for (node = packages->list.head; node; node = node->next) {
                	package = node->data;
			package->status_want = di_package_status_want_deinstall;
		}

		for (i = 2; i < argc; i++) {
			int installed = 0;
			int found = 0;

			for (node = status->list.head; node; node = node->next) {
				di_slist_node *node1;
				package = node->data;
				if (strcmp(package->package, argv[i]) == 0) {
					installed = 1;
					continue;
				}
				for (node1 = package->depends.head; node1; node1 = node1->next) {
					di_package_dependency *d = node1->data;
					if (d->type == di_package_dependency_type_provides
					    && d->ptr && strcmp(d->ptr->package, argv[i]) == 0) {
						installed = 1;
						continue;
					}
				}
			}

			if (installed) {
				//di_log (DI_LOG_LEVEL_DEBUG, "skipping already installed %s", argv[i]);
				continue;
			}

			for (node = packages->list.head; node; node = node->next) {
				package = node->data;

				if (!di_system_package_check_subarchitecture(package, subarchitecture))
					continue;

				if (strcmp(package->package, argv[i]) == 0) {
					package->status_want = di_package_status_want_install;
					found = 1;
					continue;
				}

				if (((di_system_package *)package)->kernel_version) {
		                        if (running_kernel && strcmp(running_kernel, ((di_system_package *)package)->kernel_version) != 0) {
						continue;
					}
					di_slist_node *node1;
					for (node1 = package->depends.head; node1; node1 = node1->next) {
						di_package_dependency *d = node1->data;
						if (d->type == di_package_dependency_type_provides
						    && d->ptr && strcmp(d->ptr->package, argv[i]) == 0) {
							package->status_want = di_package_status_want_install;
							found = 1;
							continue;
						}
					}
				}
			}
			if (! found) {
				fprintf(stderr, "unknown udeb %s\n", argv[i]);
				exit(1);
			}
		}
		ret = install_modules(status, packages);
	}
	else {
		if (argc == 2) {
			set_retriever(argv[1], 0);
		}
		else if (strcmp(argv[2], "default") == 0) {
			set_retriever(argv[1], 1);
		}
		retriever_config();

		packages = get_packages();
		if (! packages || ! packages_ok(packages)) {
			retriever_cleanup();
			return 10;
		}

		if (choose_modules(status, &packages) != 0) {
			ret = 10;
		}
		else  {
			ret = install_modules(status, packages);
		}
	}

	retriever_cleanup();
	return ret;
}
