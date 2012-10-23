#include <debian-installer.h>
#include <stdbool.h>

int get_lowmem_level (void);
int is_queued(di_package *package);
bool is_installed(di_package *p, di_packages *status);
size_t package_to_choice(di_package *package, char *buf, size_t size);
char *list_to_choices(di_package **packages, int c_values);
int get_package (di_package *package, char *dest);
int md5sum(const char* sum, const char *file);
int skip_package(di_package *p);
int package_name_compare(const void *v1, const void *v2);
void take_includes(di_packages *packages);
void drop_excludes(di_packages *packages);
int unpack_package (const char *pkgfile);
int configure_package (const char *package);
#ifdef LOADTEMPLATES
int load_templates (di_packages *packages);
#endif /* LOADTEMPLATES */
