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

static struct cpu system_sgi_ind_cpu[] = {
	{ "R4", "r4k-ip22" }, /* match various R4000 variants */
	{ "R5000", "r5k-ip22" },
	{ "R8000", "r8k-ip26" },
	{ "R10000", "r10k-ip28" },
	{ NULL, "unknown" }
};

static struct cpu system_sgi_origin_cpu[] = {
	{ "R10000", "r10k-ip27" },
	{ "R12000", "r12k-ip27" },
	{ NULL, "unknown" }
};

static struct cpu system_sgi_o2_cpu[] = {
	{ "R5", "r5k-ip32" }, /* match R5000 and R5500 */
	{ "Nevada", "r5k-ip32" },
	{ "RM7000", "r5k-ip32" },
	{ "R10000", "r10k-ip32" },
	{ "R12000", "r12k-ip32" },
	{ NULL, "unknown" }
};

static struct cpu system_sibyte_sb1_cpu[] = {
	{ "SiByte SB1A", "sb1a-bcm91480b" },
	{ "SiByte SB1 ", "sb1-bcm91250a" },
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

/* add new system types here */

static struct cpu system_unknown_cpu[] = {
	{ NULL, "unknown" }
};

static struct systype system_type[] = {
	{"SGI Ind", system_sgi_ind_cpu }, /* match "SGI Indy" and "SGI Indigo2" */
	{"SGI Origin", system_sgi_origin_cpu },
	{"SGI IP32", system_sgi_o2_cpu },
	{"SGI O2", system_sgi_o2_cpu },
	{"SiByte BCM9", system_sibyte_sb1_cpu }, /* match Broadcom SB1 boards */
	{"Qemu", system_qemu_cpu },
	{"MIPS Malta", system_malta_cpu },
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
