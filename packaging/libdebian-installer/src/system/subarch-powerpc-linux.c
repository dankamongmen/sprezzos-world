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

static struct map map_generation[] = {
	{ "OldWorld", "powermac_oldworld" },
	{ "NewWorld", "powermac_newworld" },
	{ "NuBus", "powermac_nubus" },
	{ NULL, NULL }
};

static struct map map_machine[] = {
	{ "PReP", "prep" },
	{ "CHRP Pegasos", "chrp_pegasos" },
	{ "EFIKA", "chrp_pegasos" },
	{ "CHRP IBM", "chrp_rs6k" },
	{ "CHRP", "chrp" },
	{ "Amiga", "amiga" },
	{ "64-bit iSeries Logical Partition", "iseries" },
	{ NULL, NULL }
};

static struct map map_platform[] = {
	{ "PS3", "ps3" },
	{ "Cell", "cell" },
	{ "PA Semi", "pasemi" },
	{ "Maple", "chrp_ibm" },
	{ "pSeries", "chrp_ibm" },
	{ NULL, NULL }
};

static char *check_map(struct map map[], const char *entry)
{
	for (; map->entry; map++)
		if (!strncasecmp(map->entry, entry, strlen(map->entry)))
			return map->ret;

	return NULL;
}

const char *di_system_subarch_analyze(void)
{
	FILE *cpuinfo;
	char line[1024];
	char cpuinfo_platform[256], cpuinfo_machine[256], cpuinfo_generation[256];
	char *ret, *pos;

	cpuinfo = fopen("/proc/cpuinfo", "r");
	if (cpuinfo == NULL)
		return "unknown";

	while (fgets(line, sizeof(line), cpuinfo) != NULL) {
		pos = strchr(line, ':');
		if (pos == NULL)
			continue;
		while (*++pos && (*pos == '\t' || *pos == ' '));

		if (strstr(line, "platform") == line)
			strncpy(cpuinfo_platform, pos, sizeof(cpuinfo_platform));

		if (strstr(line, "machine") == line)
			strncpy(cpuinfo_machine, pos, sizeof(cpuinfo_machine));

		if (strstr(line, "pmac-generation") == line)
			strncpy(cpuinfo_generation, pos, sizeof(cpuinfo_generation));
	}

	fclose(cpuinfo);

	ret = check_map(map_platform, cpuinfo_platform);
	if (ret)
		return ret;
	ret = check_map(map_machine, cpuinfo_machine);
	if (ret)
		return ret;
	ret = check_map(map_generation, cpuinfo_generation);
	if (ret)
		return ret;

	return "unknown";
}
