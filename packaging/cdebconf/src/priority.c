#include "common.h"
#include "priority.h"
#include <string.h>

/**
 * @brief returns an integer value suitable for comparison priorities
 * @param const char *p - priority string
 * @return int - integer value representing priority
 */
static int priority_code(const char *p)
{
	if (p == 0) return -1;
	if (strcmp(p, "low") == 0)
		return 0;
	if (strcmp(p, "medium") == 0)
		return 1;
	if (strcmp(p, "high") == 0)
		return 2;
	if (strcmp(p, "critical") == 0)
		return 3;
	return -1;
}

int priority_compare(const char *p1, const char *p2)
{
	int i1, i2;

	i1 = priority_code(p1);
	i2 = priority_code(p2);

	INFO(INFO_VERBOSE, "Comparing priorities %s (%d) with %s (%d)",
		p1, i1, p2, i2);

	if (i1 > i2)
		return 1;
	else if (i1 == i2)
		return 0;
	else
		return -1;
}
