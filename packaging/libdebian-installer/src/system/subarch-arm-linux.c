#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <sys/utsname.h>

#include <debian-installer/system/subarch.h>

struct map {
	char *entry;
	char *ret;
};

static const char *supported_generic_subarches[] = {
    "dove",
    "omap",
    "omap4",
    "mx51",
    "mx5",
    "vexpress",
    NULL
};

static struct map map_hardware[] = {
    { "Acorn-RiscPC" , "rpc" },
    { "EBSA285" , "netwinder" },
    { "Rebel-NetWinder" , "netwinder" },
    { "Chalice-CATS" , "netwinder" },
    { "co-EBSA285" , "netwinder" },
    { "Compaq-PersonalServer" , "netwinder" },
    { "Freescale MX51 Babbage Board", "imx51" }, /* iMX51 reference hardware. */
    { "ADS" , "ads" }, /* Matches only ADS boards. Put any exceptions before. */
    { "Applied Data Systems" , "ads" }, /* More ADS boards. */
    { "HP t5325 Thin Client", "kirkwood" },
    { "Marvell DB-88F6281-BP Development Board", "kirkwood" },
    { "Marvell RD-88F6192-NAS Development Board", "kirkwood" },
    { "Marvell RD-88F6281 Reference Board", "kirkwood" },
    { "Marvell GuruPlug Reference Board", "kirkwood" },
    { "Marvell OpenRD Base Board", "kirkwood" },
    { "Marvell OpenRD Client Board", "kirkwood" },
    { "Marvell OpenRD Ultimate Board", "kirkwood" },
    { "Marvell SheevaPlug Reference Board", "kirkwood" },
    { "Marvell eSATA SheevaPlug Reference Board", "kirkwood" },
    { "Globalscale Technologies Dreamplug", "kirkwood" },
    { "QNAP TS-119/TS-219", "kirkwood" },
    { "QNAP TS-41x", "kirkwood" },
    { "Seagate FreeAgent DockStar", "kirkwood" },
    { "LaCie Network Space v2", "kirkwood" },
    { "LaCie Internet Space v2", "kirkwood" },
    { "LaCie Network Space Max v2", "kirkwood" },
    { "LaCie d2 Network v2", "kirkwood" },
    { "LaCie 2Big Network v2", "kirkwood" },
    { "LaCie 5Big Network v2", "kirkwood" },
    { "Buffalo/Revogear Kurobox Pro", "orion5x" },
    { "D-Link DNS-323", "orion5x" },
    { "QNAP TS-109/TS-209", "orion5x" },
    { "QNAP TS-409", "orion5x" },
    { "HP Media Vault mv2120", "orion5x" },
    { "Buffalo Linkstation LiveV3 (LS-CHL)", "orion5x" },
    { "Buffalo Linkstation Mini", "orion5x" },
    { "Buffalo Linkstation Pro/Live", "orion5x" },
    { "Marvell Orion-NAS Reference Design", "orion5x" },
    { "Marvell Orion-2 Development Board", "orion5x" },
    { "Intel EP80219", "iop32x" },
    { "Intel IQ31244", "iop32x" },
    { "Intel IQ80321", "iop32x" },
    { "Thecus N2100", "iop32x" },
    { "Thecus N4100", "iop32x" },
    { "GLAN Tank", "iop32x" },
    { "Lanner EM7210", "iop32x" },
    { "Intel IQ80331", "iop33x" },
    { "Intel IQ80332", "iop33x" },
    { "ADI Engineering Coyote", "ixp4xx" },
    { "Freecom Storage Gateway", "ixp4xx" },
    { "Intel IXDPG425", "ixp4xx" },
    { "Intel IXDP425 Development Platform", "ixp4xx" },
    { "Intel IXDP465 Development Platform", "ixp4xx" },
    { "Intel IXCDP1100 Development Platform", "ixp4xx" },
    { "Gateworks Avila Network Platform", "ixp4xx" },
    { "Gemtek GTWX5715 (Linksys WRV54G)", "ixp4xx" },
    { "Iomega NAS 100d", "ixp4xx" },
    { "Linksys NSLU2", "ixp4xx" },
    { "ARM-Versatile AB", "versatile" },
    { "ARM-Versatile PB", "versatile" },
    { "Genesi Efika MX (Smartbook)", "mx5" },
    { "Genesi Efika MX (Smarttop)", "mx5" },
    { "Nokia RX-51 Board", "omap" },
    { "OMAP3 Beagle Board", "omap" },
    { "OMAP4 Panda Board", "omap" },
    { "ARM-Versatile Express", "vexpress" },
    { NULL, NULL }
};

static int read_dt_model(char *entry, int entry_len)
{
	FILE *model;
	int ret;

	model = fopen("/proc/device-tree/model", "r");
	if (model == NULL)
		return 1;

	ret = fgets(entry, entry_len, model) == NULL;
	fclose(model);
	return ret;
}

static int read_cpuinfo(char *entry, int entry_len)
{
	FILE *cpuinfo;
	char line[1024];
	char *pos;
	int ret = 1;

	cpuinfo = fopen("/proc/cpuinfo", "r");
	if (cpuinfo == NULL)
		return 1;

	while (fgets(line, sizeof(line), cpuinfo) != NULL)
	{
	    if (strstr(line, "Hardware") == line)
	    {
	        pos = strchr(line, ':');
		if (pos == NULL)
			   continue;
		while (*++pos && (*pos == '\t' || *pos == ' '));

		strncpy(entry, pos, entry_len);
		ret = 0;
		break;
	    }
	}

	fclose(cpuinfo);
	return ret;
}

const char *di_system_subarch_analyze(void)
{
	char entry[256];
	int i;
	int ret;

	entry[0] = '\0';

	ret = read_dt_model(entry, sizeof(entry));
	if (ret)
		ret = read_cpuinfo(entry, sizeof(entry));
	if (ret)
		return "unknown";

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

const char *di_system_subarch_analyze_guess(void)
{
	struct utsname sysinfo;
	size_t uname_release_len, i;

	/* Attempt to determine subarch based on kernel release version */
	uname(&sysinfo);
	uname_release_len = strlen(sysinfo.release);

	for (i = 0; supported_generic_subarches[i] != NULL; i++)
	{
		size_t subarch_len = strlen (supported_generic_subarches[i]);
		if (!strncmp(sysinfo.release+uname_release_len-subarch_len,
			supported_generic_subarches[i],
			subarch_len))
		{
			return supported_generic_subarches[i];
		}
	}

	/* If we get here, try falling back on the normal detection method */
	return di_system_subarch_analyze();
}
