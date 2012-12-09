#include <stdio.h>
#include <dlfcn.h>
#include "npapi.h"

int main(int argc, char *argv[])
{
	void *dlh;
	char* (*NP_GetMIMEDescription_func)();
	NPError (*NP_GetValue_func)(void*, NPPVariable, void*);
	char *buf;

	if (argc != 2) {
		fprintf(stderr, "Usage: %s plugin.so\n", argv[0]);
		return 1;
	}

	dlh = dlopen(argv[1], RTLD_LAZY);
	if (dlh == NULL) {
		fprintf(stderr, "Failed to open plugin: %s\n", argv[1]);
		return 1;
	}

	NP_GetMIMEDescription_func = dlsym(dlh, "NP_GetMIMEDescription");
	NP_GetValue_func = dlsym(dlh, "NP_GetValue");
	if ((NP_GetValue_func == NULL) ||
	    (NP_GetMIMEDescription_func == NULL)) {
		fprintf(stderr, "It doesn't look like this is a plugin: %s\n", argv[1]);
		return 1;
	}

	if (NP_GetValue_func(NULL, NPPVpluginNameString, &buf)
	    == NPERR_NO_ERROR)
		printf("Name: %s\n", buf);
	
	if (NP_GetValue_func(NULL, NPPVpluginDescriptionString, &buf)
	    == NPERR_NO_ERROR)
		printf("Description: %s\n", buf);

	buf = NP_GetMIMEDescription_func();
		printf("MimeTypes: %s\n", buf);

	return 0;
}
