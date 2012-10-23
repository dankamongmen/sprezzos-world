#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include <debian-installer/system/subarch.h>

struct map {
	char *entry;
	char *ret;
};

static struct map map_hardware[] = {
    { "Linksys NSLU2", "ixp4xx" },
    { NULL, NULL }
};

const char *di_system_subarch_analyze(void)
{
	FILE *cpuinfo;
	char line[1024];
	char entry[256];
	char *pos;
	int i;

	cpuinfo = fopen("/proc/cpuinfo", "r");
	if (cpuinfo == NULL)
		return "unknown";

	while (fgets(line, sizeof(line), cpuinfo) != NULL)
	{
	    if (strstr(line, "Hardware") == line)
	    {
	        pos = strchr(line, ':');
		if (pos == NULL)
			   continue;
		while (*++pos && (*pos == '\t' || *pos == ' '));

		strncpy(entry, pos, sizeof(entry));
		break;
	    }
	}

	fclose(cpuinfo);

	for (i = 0; map_hardware[i].entry; i++)
	{
	    if (!strncasecmp(map_hardware[i].entry, entry,
			strlen(map_hardware[i].entry)))
	    {
		return( map_hardware[i].ret );
	    }
	}

	return "unknown";
}
