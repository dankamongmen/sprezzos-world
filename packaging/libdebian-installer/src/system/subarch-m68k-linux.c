#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include <debian-installer/system/subarch.h>

struct map {
	char *model;
	char *subarch;
};

static struct map map_model[] = {
	{ "Amiga", "amiga" },
	{ "ARAnyM", "atari" },
	{ "Atari", "atari" },
	{ "Macintosh", "mac" },
	{ "BVME", "bvme6000" },
	{ "Motorola MVME147", "mvme147" },
	{ "Motorola", "mvme16x" },
	{ "Q40", "q40" },
	{ "Sun 3/160 Series", "sun3" },
	{ "Sun 3/50", "sun3" },
	{ "Sun 3/260 Series", "sun3" },
	{ "Sun 3/110 Series", "sun3" },
	{ "Sun 3/60", "sun3" },
	{ "Sun 3/E", "sun3" },
	{ "Sun 3/460 Series", "sun3x" },
	{ "Sun 3/80", "sun3x" },
	{ NULL, NULL }
};


const char *di_system_subarch_analyze(void)
{
	FILE *file;
	char line[1024];
	char model[256];
	char *pos;
	int i;

	file = fopen("/proc/hardware", "r");
	if (file == NULL)
		return "unknown";

	while (fgets(line, sizeof(line), file) != NULL) 
	{
	    if (strstr(line, "Model:") == line)
	    {
	        pos = strchr(line, ':');
		if (pos == NULL)
			   continue;
		while (*++pos && (*pos == ' ' || *pos == '\t'));

		strncpy(model, pos, sizeof(model));
		break;
	    }
	}

	fclose(file);

	for (i = 0; map_model[i].model; i++)
	{
	    if (!strncasecmp(map_model[i].model, model, 
			strlen(map_model[i].model)))
	    {
		return( map_model[i].subarch );
	    }
	}

	return "unknown";
}
