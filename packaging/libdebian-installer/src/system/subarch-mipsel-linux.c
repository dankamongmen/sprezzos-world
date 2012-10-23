#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <debian-installer/system/subarch.h>

struct cpu {
	char *cpu;
	char *ret;
};

struct systype {
	char *sys;
	struct cpu *cpu;
};

static struct cpu system_dec_decs_cpu[] = {
	{ "R3", "r3k-kn02" },
	{ "R4", "r4k-kn04" },
	{ NULL, "unknown" }
};

static struct cpu system_sibyte_sb1_cpu[] = {
	{ "SiByte SB1A", "sb1a-bcm91480b" },
	{ "SiByte SB1 ", "sb1-bcm91250a" },
	{ NULL, "unknown" }
};

static struct cpu system_cobalt_cpu[] = {
	{ "Nevada", "cobalt" },
	{ NULL, "unknown" }
};

static struct cpu system_bcm_bcm947xx_cpu[] = {
	{ "Broadcom BCM3302", "bcm947xx" },
	{ "Broadcom BCM4710", "bcm947xx" },
	{ NULL, "unknown" }
};

static struct cpu system_qemu_cpu[] = {
	{ "MIPS 4Kc", "qemu-mips32" },
	{ "MIPS 24Kc", "qemu-mips32" },
	{ NULL, "unknown" }
};

static struct cpu system_malta_cpu[] = {
	{ "MIPS 4K", "4kc-malta" },
	{ "MIPS 24K", "4kc-malta" },
	{ "MIPS 34K", "4kc-malta" },
	{ "MIPS 5K", "5kc-malta" },
	{ "MIPS 20K", "5kc-malta" },
	{ NULL, "unknown" }
};

static struct cpu system_loongson2_cpu[] = {
	{ "ICT Loongson-2 V0.2", "loongson-2e" },
	{ "ICT Loongson-2 V0.3", "loongson-2f" },
};

/* add new system types here */

static struct cpu system_unknown_cpu[] = {
	{ NULL, "unknown" }
};

static struct systype system_type[] = {
	/*
	 * match any of
	 *	"Digital unknown DECstation"
	 *	"Digital DECstation"
	 *	"Digital DECsystem"
	 *	"Digital Personal DECstation"
	 */
	{"Digital ", system_dec_decs_cpu },
	{"SiByte BCM9", system_sibyte_sb1_cpu }, /* match Broadcom SB1 boards */
	{"MIPS Cobalt", system_cobalt_cpu }, /* old kernels */
	{"Cobalt ", system_cobalt_cpu }, /* match any Cobalt machine; new kernels */
	{"Broadcom BCM947XX", system_bcm_bcm947xx_cpu }, /* Broadcom based APs/NAS */
	{"Qemu", system_qemu_cpu },
	{"MIPS Malta", system_malta_cpu },
	{"lemote-", system_loongson2_cpu },
	{"dexxon-gdium-2f", system_loongson2_cpu },
	{ NULL, system_unknown_cpu }
};

#define INVALID_SYS_IDX (sizeof(system_type) / sizeof(struct systype) - 1)
#define INVALID_CPU_IDX (-1)

#define BUFFER_LENGTH (1024)

static int check_system(const char *entry)
{
	int ret;

	for (ret = 0; system_type[ret].sys; ret++) {
		if (!strncmp(system_type[ret].sys, entry,
			     strlen(system_type[ret].sys)))
			break;
	}

	return ret;
}

static int check_cpu(const char *entry, int sys_idx)
{
	int ret;

	if (sys_idx == INVALID_SYS_IDX) {
		/*
		 * This means an unsupported system type, because the
		 * system type is always the first entry in /proc/cpuinfo.
		 */
		return INVALID_CPU_IDX;
	}

	for (ret = 0; system_type[sys_idx].cpu[ret].cpu; ret++) {
		if (!strncmp(system_type[sys_idx].cpu[ret].cpu, entry,
			     strlen(system_type[sys_idx].cpu[ret].cpu)))
			break;
	}

	return ret;
}

const char *di_system_subarch_analyze(void)
{
	FILE *file;
	int sys_idx = INVALID_SYS_IDX;
	int cpu_idx = INVALID_CPU_IDX;
	char buf[BUFFER_LENGTH];
        char *pos;
	size_t len;

	if (!(file = fopen("/proc/cpuinfo", "r")))
		return system_type[sys_idx].cpu[0].ret;

	while (fgets(buf, sizeof(buf), file)) {
		if (!(pos = strchr(buf, ':')))
			continue;
		if (!(len = strspn(pos, ": \t")))
			continue;
		if (!strncmp(buf, "system type", strlen("system type")))
			sys_idx = check_system(pos + len);
		else if (!strncmp(buf, "cpu model", strlen("cpu model")))
			cpu_idx = check_cpu(pos + len, sys_idx);
	}

	fclose(file);

	if (cpu_idx == INVALID_CPU_IDX) {
		sys_idx = INVALID_SYS_IDX;
		cpu_idx = 0;
	}

	return system_type[sys_idx].cpu[cpu_idx].ret;
}
