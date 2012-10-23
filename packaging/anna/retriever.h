#include <debian-installer.h>

#define DEFAULT_RETRIEVER_VAR "anna/retriever"

void set_retriever(const char *retriever, int isdefault);
char *get_retriever(void);

int retriever_retrieve (di_package *package, char *dest);
int retriever_config(void);
di_packages *retriever_packages(di_packages_allocator *allocator);
signed int retriever_error (const char *failing_command);
void retriever_cleanup(void);
