#include "udpkg.h"

#ifdef DODEPENDS
#include <stdio.h>
#include <stdlib.h>
#include <search.h>
#include <string.h>
#include <ctype.h>

static char **depends_split(const char *dependsstr)
{
	static char *dependsvec[DEPENDSMAX];
	char *p;
	int i = 0;

	dependsvec[0] = 0;

	if (dependsstr != 0)
	{
		p = strdup(dependsstr);
		while (*p != 0 && *p != '\n')
		{
			if (*p != ' ')
			{
				if (*p == ',')
				{
					*p = 0;
					dependsvec[++i] = 0;
				}
				else if (dependsvec[i] == 0)
					dependsvec[i] = p;
			}
			else
				*p = 0; /* eat the space... */
			p++;
		}
		*p = 0;
	}
	dependsvec[i+1] = 0;
	return dependsvec;
}

static void depends_sort_visit(struct package_t **ordered, 
	struct package_t *pkgs, struct package_t *pkg)
{
	/* Topological sort algorithm:
	 * ordered is the output list, pkgs is the dependency graph, pkg is 
	 * the current node
	 *
	 * recursively add all the adjacent nodes to the ordered list, marking
	 * each one as visited along the way
	 *
	 * yes, this algorithm looks a bit odd when all the params have the
	 * same type :-)
	 */
	unsigned short i;

	/* mark node as processing */
	pkg->color = COLOR_GRAY;

	/* visit each not-yet-visited node */
	for (i = 0; i < pkg->requiredcount; i++)
		if (pkg->requiredfor[i]->color == COLOR_WHITE)
			depends_sort_visit(ordered, pkgs, pkg->requiredfor[i]);

#if 0
	/* add it to the list */
	newnode = (struct package_t *)di_malloc(sizeof(struct package_t));
	/* make a shallow copy */
	*newnode = *pkg;
	newnode->next = *ordered;
	*ordered = newnode;
#endif
	pkg->next = *ordered;
	*ordered = pkg;

	/* mark node as done */
	pkg->color = COLOR_BLACK;
}

static struct package_t *depends_sort(struct package_t *pkgs)
{
	/* TODO: it needs to break cycles in the to-be-installed package 
	 * graph... */
	struct package_t *ordered = NULL;
	struct package_t *pkg;

	for (pkg = pkgs; pkg != 0; pkg = pkg->next)
		pkg->color = COLOR_WHITE;

	for (pkg = pkgs; pkg != 0; pkg = pkg->next)
		if (pkg->color == COLOR_WHITE)
			depends_sort_visit(&ordered, pkgs, pkg);

	/* Leaks the old list... return the new one... */
	return ordered;
}


/* resolve package dependencies -- 
 * for each package in the list of packages to be installed, we parse its 
 * dependency info to determine if the dependent packages are either 
 * already installed, or are scheduled to be installed. If both tests fail
 * than bail.
 *
 * The algorithm here is O(n^2*m) where n = number of packages to be 
 * installed and m is the # of dependencies per package. Not a terribly
 * efficient algorithm, but given that at any one time you are unlikely
 * to install a very large number of packages it doesn't really matter
 */
struct package_t *depends_resolve(struct package_t *pkgs, void *status)
{
	struct package_t *pkg, *chk;
	struct package_t dependpkg;
	char **dependsvec;
	int i;
	void *found;

	for (pkg = pkgs; pkg != 0; pkg = pkg->next)
	{
		dependsvec = depends_split(pkg->depends);
		i = 0;
		while (dependsvec[i] != 0)
		{
			/* Check for dependencies; first look for installed packages */
			dependpkg.package = dependsvec[i];
			if ((found = tfind(&dependpkg, &status, package_compare)) == 0 ||
			    ((chk = *(struct package_t **)found) &&
			     (chk->status & (STATUS_FLAGOK | STATUS_STATUSINSTALLED)) != 
			      (STATUS_FLAGOK | STATUS_STATUSINSTALLED)))
			{
				/* if it fails, we look through the list of packages we are going to 
				 * install */
				for (chk = pkgs; chk != 0; chk = chk->next)
				{
					if (strcmp(chk->package, dependsvec[i]) == 0 ||
					    (chk->provides && 
					     strncmp(chk->provides, dependsvec[i], strlen(dependsvec[i])) == 0))
					{
						if (chk->requiredcount >= DEPENDSMAX)
						{
							fprintf(stderr, "Too many dependencies for %s\n", 
								chk->package);
							return 0;
						}
						if (chk != pkg)
							chk->requiredfor[chk->requiredcount++] = pkg;
						break;
					}
				}
				if (chk == 0)
				{
					fprintf(stderr, "%s depends on %s, but it is not going to be installed\n", pkg->package, dependsvec[i]);
					return 0;
				}
			}
			i++;
		}
	}

	return depends_sort(pkgs);
}
#endif
