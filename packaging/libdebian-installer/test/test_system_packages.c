#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <autounit/autounit.h>

#include <debian-installer/package.h>
#include <debian-installer/string.h>
#include <debian-installer/system/packages.h>

#define DOWNLOAD_PACKAGES       "Packages"

static di_packages *packages = NULL;
static di_packages_allocator *packages_allocator = NULL;

gint test_size(autounit_test_t *t) {
    di_slist_node *node;
    di_package *package;
    int count;

    count =0;
    for (node = (packages)->list.head; node; node = node->next) {
      package = node->data;
      count++;
    }

    au_assert(t,di_hash_table_size(packages->table) == count,
	      "hash table size is different with the number of data inserted");
 
    return TRUE;
}

gint test_dependencies(autounit_test_t *t) {

    di_slist_node *node;
    di_package *package;

    /* verify packages initial status_want */
    for (node = (packages)->list.head; node; node = node->next) {
      package = node->data;
      au_assert(t, package->status_want == di_package_status_want_unknown,
		"package initial status_want is not status_want_unknown");
    }

    /* we want to install countrychooser */
    node = (packages)->list.head;
    package = node->data;
    package->status_want = di_package_status_want_install;

    /* check dependencies */
    di_system_packages_resolve_dependencies_mark_anna(packages,NULL,NULL);

    /* verify packages status_want is status_want_install*/
    for (node = (packages)->list.head; node; node = node->next) {
      package = node->data;
      au_assert(t, package->status_want == di_package_status_want_install,
		"package status_want is not status_want_install");
    }

  return TRUE;
}

gint setup(autounit_test_t *t) {
  packages_allocator = di_system_packages_allocator_alloc();
  packages = di_system_packages_read_file(DOWNLOAD_PACKAGES, packages_allocator);

  return TRUE;
}

gint teardown(autounit_test_t *t) {
  di_packages_free(packages);
  di_packages_allocator_free(packages_allocator);
  packages = NULL;
  packages_allocator = NULL;

  return TRUE;
}

autounit_test_group_t test_system_packages_tests[] = {
  {"test_size", test_size, TRUE, TRUE},
  {"test_dependencies", test_dependencies, TRUE, TRUE},
  {0, 0, FALSE, FALSE}
};

int test_system_packages_run() {
  autounit_suite_t *c_unit_test_suite;
  int result;

  c_unit_test_suite =
    au_new_suite(g_string_new("test di_system_packages"), setup, teardown);
  au_add_test_group(c_unit_test_suite, test_system_packages_tests);

  result = au_run_suite(c_unit_test_suite);

  au_delete_suite(c_unit_test_suite);

  return result;
};
