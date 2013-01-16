/*
 * listing.c
 *
 * Copyright 2000-2009 Werner Fink, 2000 SuSE GmbH Nuernberg, Germany,
 *				    2003 SuSE Linux AG, Germany.
 *			       2007-2009 SuSE Linux Products GmbH Nuernberg, Germany
 *
 * This source is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <limits.h>
#include <sys/types.h>
#include <ctype.h>
#include "listing.h"

int maxstart = 0;  		/* Maximum start order of runlevels 0 upto 6 and S */
int maxstop  = 0;  		/* Maximum stop  order of runlevels 0 upto 6 and S */
static int *maxorder;		/* Pointer to one of above */

/* See listing.c for list_t and list_entry() macro */
#define getdir(list)		list_entry((list), dir_t,   d_list)
#define getlink(list)		list_entry((list), link_t,  l_list)
#define getnextlink(list)	(list_empty(list) ? (dir_t*)0 : getlink((list)->next)->target)

/*
 * We handle services (aka scripts) as directories because
 * dependencies can be handels as symbolic links therein.
 * A provided service will be linked into a required service.
 * For the general type of linked lists see listing.h.
 */

typedef struct dir_struct dir_t;

typedef struct link_struct {
    list_t		  l_list;	/* The linked list of symbolic links */
    dir_t	*restrict target;
} __align link_t;			/* This is a "symbolic link" */

typedef struct handle_struct {
    list_t		    link;	/* The linked list of symbolic start/stop links in the directory */
    level_t		     run;
    ushort		   flags;
    uchar		 mindeep;	/* Default start/stop deep if any */
    uchar		    deep;	/* Current start/stop deep */
    char		  * name;
} __align handle_t;

struct dir_struct {
    list_t		  d_list;	/* The peg into linked list to other directories */
    handle_t		   start;
    handle_t		   stopp;
    service_t	  *restrict serv;
    int			     ref;
    char		* script;
    char		  * name;
} __align;				/* This is a "directory" */

#define attof(dir)	(&(dir)->serv->attr)

/*
 * The linked list off all directories, note that the s_list
 * entry within the dir_struct is used as the peg pointer.
 */
static list_t dirs = { &dirs, &dirs };
static list_t * d_start = &dirs;

#define DIR_SCAN	0x0001
#define DIR_LOOP	0x0002
#define DIR_LOOPREPORT	0x0004
#define DIR_MAXDEEP	0x0008

/*
 * The linked list off all services, note that the d_list
 * entry within the service_struct is used as the peg pointer.
 */
static list_t servs = { &servs, &servs };
list_t * s_start = &servs;

/*
 * Provide or find a service dir, set initial states and
 * link it into the maintaining if a new one.
 */

static inline dir_t * providedir(const char *restrict const name) attribute((malloc,always_inline,nonnull(1)));
static inline dir_t * providedir(const char *restrict const name)
{
    dir_t *restrict dir = (dir_t*)0;
    service_t *restrict serv;
    list_t * ptr;

    list_for_each_prev(ptr, d_start) {
	dir = getdir(ptr);
	if (!strcmp(dir->name, name))
	    goto out;
    }

    if (posix_memalign((void*)&serv, sizeof(void*), alignof(service_t)+strsize(name)) != 0)
	error("%s", strerror(errno));

    memset(serv, 0, alignof(service_t)+strsize(name));
    insert(&serv->s_list, s_start->prev);
    serv->name = ((char*)serv)+alignof(service_t);

    if (posix_memalign((void*)&dir, sizeof(void*), alignof(dir_t)) != 0)
	error("%s", strerror(errno));

    memset(dir, 0, alignof(dir_t));
    insert(&dir->d_list, d_start->prev);
    dir->ref = 1;

    serv->dir = (void*)dir;
    dir->serv = serv;

    initial(&dir->start.link);
    initial(&dir->stopp.link);

    initial(&serv->sort.req);
    initial(&serv->sort.rev);

    strcpy(serv->name, name);
    dir->name	    = serv->name;
    dir->start.name = serv->name;
    dir->stopp.name = serv->name;

    dir->start.mindeep = 1;
    dir->stopp.mindeep = 1;

    serv->start = &dir->start.run;
    serv->stopp = &dir->stopp.run;
out:
    return dir;
}

/*
 * Find or add and initialize a service
 */
service_t * addservice(const char *restrict const serv) attribute((malloc,nonnull(1)));
service_t * addservice(const char *restrict const serv)
{
    service_t * this;
    list_t * ptr;
    dir_t * dir;

    list_for_each_prev(ptr, s_start) {
	this = getservice(ptr);
	if (!strcmp(this->name, serv))
	    goto out;
    }
    dir = providedir(serv);
    this = dir->serv;
out:
    return this;
}

/*
 * Always return the address of the original service
 */
service_t * getorig(service_t *restrict const serv)
{
    dir_t *const dir = (dir_t *)serv->dir;
    return dir->serv;
}

/*
 * Find a service dir by its script name.
 */
static inline dir_t * findscript(const char *restrict const script) attribute((always_inline,nonnull(1)));
static inline dir_t * findscript(const char *restrict const script)
{
    dir_t  * ret = (dir_t*)0;
    list_t * ptr;

    list_for_each_prev(ptr, d_start) {
	dir_t * dir = getdir(ptr);

	if (!dir->script)
	    continue;

	if (!strcmp(dir->script, script)) {
	    ret = dir;
	    break;
	}
    }

    return ret;
}

/*
 * Link the current service into the required service.
 * If the services do not exist, they will be created.
 */
static void ln_sf(dir_t *restrict cur, dir_t *restrict req, const char mode) attribute((nonnull(1,2)));
static void ln_sf(dir_t *restrict cur, dir_t *restrict req, const char mode)
{
    list_t * dent, * l_list = (mode == 'K') ? &req->stopp.link : &req->start.link;
    link_t *restrict this;

    if (cur == req)
	goto out;

    list_for_each_prev(dent, l_list) {
	dir_t * target = getlink(dent)->target;
	if (!strcmp(target->name, cur->name))
	    goto out;
    }

    if (posix_memalign((void*)&this, sizeof(void*), alignof(link_t)) == 0) {
	insert(&this->l_list, l_list->prev);
	this->target = cur;
	++cur->ref;
	goto out;
    }
    error("%s", strerror(errno));
out:
    return;
}

/*
 * Remember loops to warn only once
 */
static inline boolean remembernode (handle_t *restrict const peg) attribute((always_inline,nonnull(1)));
static inline boolean remembernode (handle_t *restrict const peg)
{
    register boolean ret = true;

    if (peg->flags & DIR_LOOP)
	goto out;

    ret = false;
    peg->flags |= DIR_LOOP;
out:
    return ret;
}

/*
 * Recursively called function to follow all
 * links within a service dir.
 * Just like a `find * -follow' within a directory tree
 * of depth one with cross linked dependencies.
 *
 * Be warned: the direction is naturally reversed.  That
 * means that the most requested services have the lowest
 * order.  In other word, an empty link list of a service
 * indicates that this service has a higher order number.
 */
#if defined(DEBUG) && (DEBUG > 0)
# define loop_warn_two(a,b,o)	\
	warn("There is a loop between service %s and %s if %s (list:%d)\n", \
	(a)->name, (b)->name, o, __LINE__)
# define loop_warn_one(a,o)	\
	warn("There is a loop at service %s if %s (list:%d)\n", \
	(a)->name, o, __LINE__)
#else
# define loop_warn_two(a,b,o)	\
	warn("There is a loop between service %s and %s if %s\n", (a)->name, (b)->name, o)
# define loop_warn_one(a,o)	\
	warn("There is a loop at service %s if %s\n", (a)->name, o)
#endif
#define loop_check(a)	\
	((a) && (a)->flags & DIR_LOOP)

static void __follow (dir_t *restrict dir, dir_t *restrict skip, const int, const char, const char)
#if  __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
	attribute((noinline,flatten,nonnull(1)));
#else
	attribute((noinline,nonnull(1)));
#endif
static void __follow (dir_t *restrict dir, dir_t *restrict skip, const int level, const char mode, const char reportloop)
{
    list_t * l_list;
    dir_t * tmp;
    register int deep = level;	/* Link depth, maybe we're called recursively */
    register int loop = 0;	/* Count number of links in symbolic list */
    handle_t * peg, * pskp = (handle_t*)0;
    const char * act;

    if (mode == 'K') {
	peg = &dir->stopp;
	if (skip) pskp = &skip->stopp;
	act  = "stopped";
    } else {
	peg = &dir->start;
	if (skip) pskp = &skip->start;
	act  = "started";
    }
    l_list = &peg->link;
    prefetch(l_list->next);

    if (peg->flags & DIR_SCAN) {
	if (pskp) {
	    if (!remembernode(pskp) || !remembernode(peg))
		loop_warn_two(peg, pskp, act);
	} else {
	    /* Does this happen? */
	    if (!remembernode(pskp))
		loop_warn_one(peg, act);
	}
	goto out;
    }

    if (deep < (peg->mindeep))	/* Default deep of this tree is higher */
	deep = (peg->mindeep);

    if (deep > MAX_DEEP) {
	if ((peg->flags & DIR_MAXDEEP) == 0)
	    warn("Max recursions depth %d for %s reached\n",  MAX_DEEP, peg->name);
	peg->flags |= DIR_MAXDEEP;
	goto out;
    }

    for (tmp = dir; tmp; tmp = getnextlink(l_list)) {
	const typeof(attof(tmp)->flags) sflags = attof(tmp)->flags;
	register boolean recursion = true;
	handle_t * ptmp = (mode == 'K') ? &tmp->stopp : &tmp->start;
	uchar  * order = &ptmp->deep;
	list_t * dent;

	if (loop++ > MAX_DEEP) {
	    if (pskp) {
		if (!remembernode(pskp) || !remembernode(ptmp))
		    loop_warn_two(ptmp, pskp, act);
	    } else {
		if (!remembernode(ptmp))
		    loop_warn_one(ptmp, act);
	    }
	    break;			/* Loop detected, stop recursion */
	}
	l_list = &ptmp->link;		/* List of symbolic links for getnextlink() */
	prefetch(l_list->next);

	if (!((peg->run.lvl) & (ptmp->run.lvl)))
	     continue;			/* Not same boot level */

	if (pskp && pskp == ptmp) {
	    if (!remembernode(pskp) || !remembernode(ptmp))
		loop_warn_one(pskp, act);
	    break;			/* Loop detected, stop recursion */
	}

	/*
	 * As higher the link depth, as higher the start order.
	 */
	if (*order > deep)
	    deep = *order;
	if (*order < deep)
	    *order = deep;

	if ((ptmp->run.lvl) & LVL_ALL) {
	    if (maxorder && (*maxorder < *order))
		*maxorder = *order;
	}

	if (list_empty(l_list))
	    break;			/* No further service requires this one */

	/*
	 * Do not count the dependcy deep of the system facilities
	 * but follow them to count the replacing provides.
	 */

	if (*ptmp->name == '$')
	    warn("System facilities not fully expanded, see %s!\n", dir->name);
	else if (++deep > MAX_DEEP) {
	    if ((ptmp->flags & DIR_MAXDEEP) == 0)
		warn("Max recursions depth %d reached\n",  MAX_DEEP);
	    ptmp->flags |= DIR_MAXDEEP;
	    break;
	}

	ptmp->flags |= DIR_SCAN; 	/* Mark this service for loop detection */

	/*
	 * If there are links in the links included, follow them
	 */
	np_list_for_each(dent, l_list) {
	    dir_t * target = getlink(dent)->target;
	    handle_t * ptrg = (mode == 'K') ? &target->stopp : &target->start;
	    const typeof(attof(target)->flags) kflags = attof(target)->flags;

	    if ((peg->run.lvl & ptrg->run.lvl) == 0)
		continue;			/* Not same boot level */

	    if (target == tmp)
		break;				/* Loop avoided */
	
	    if (target == dir)
		break;				/* Loop avoided */
	
	    if (skip && skip == target) {
		if (!remembernode(pskp) || !remembernode(ptmp))
		    loop_warn_two(pskp, ptmp, act);
		recursion = false;
		break;				/* Loop detected, stop recursion */
	    }

	    if (mode == 'K') {
		if (kflags & SERV_FIRST) {
		    warn("Stopping %s depends on %s and therefore on system facility `$all' which can not be true!\n",
			 tmp->script ? tmp->script : tmp->name, target->script ? target->script : target->name);
		    continue;
		}
	    } else {
		if (sflags & SERV_ALL) {
		    warn("Starting %s depends on %s and therefore on system facility `$all' which can not be true!\n",
			 target->script ? target->script : target->name, tmp->script ? tmp->script : tmp->name);
		    continue;
		}
	    }

	    if (ptrg->deep >= deep)		/* Nothing new */
		continue;
						/* The inner recursion */
	    __follow(target, tmp, deep, mode, reportloop);
	    prefetch(dent->next);

	    /* Just for the case an inner recursion was stopped */
	    if (loop_check(ptrg) || loop_check(ptmp) || loop_check(pskp)) {
		recursion = false;
		break;				/* Loop detected, stop recursion */
	    }
	}

	ptmp->flags &= ~DIR_SCAN; 	/* Remove loop detection mark */
	prefetch(l_list->next);

	if (!recursion) {
	    if (reportloop && !(ptmp->flags & DIR_LOOPREPORT)) {
		warn(" loop involving service %s at depth %d\n", tmp->name, level);
		ptmp->flags |= DIR_LOOPREPORT;
	    }
	    break;			/* Loop detected, stop recursion */
	}
    }
out:
    return;			/* Make compiler happy */
}

#undef loop_warn_two
#undef loop_warn_one
#undef loop_check

/*
 * Helper for follow_all: start with depth one.
 */
static inline void follow(dir_t *restrict dir, const char mode, const char reportloop) attribute((always_inline,nonnull(1)));
static inline void follow(dir_t *restrict dir, const char mode, const char reportloop)
{
    const int deep = (mode == 'K') ? (dir->stopp.mindeep) : (dir->start.mindeep);
    /* Link depth starts here with one */
    __follow(dir, (dir_t*)0, deep, mode, reportloop);
}

/*
 * Put not existing services into a guessed order.
 * The maximal order of not existing services can be
 * set if they are required by existing services.
 */
static void guess_order(dir_t *restrict dir, const char mode) attribute((nonnull(1)));
static void guess_order(dir_t *restrict dir, const char mode)
{
    handle_t * peg  = (mode == 'K') ? &dir->stopp : &dir->start;
    list_t * l_list = &peg->link;
    register int min = 99;
    register int deep = 0;
    ushort lvl = 0;

    if (dir->script)	/* Skip it because we have read it */
	goto out;

    if (*dir->name == '$') {	/* Don't touch our system facilities */
	warn("System facilities not fully expanded, see %s!\n", dir->name);
	goto out;
    }

    /* No full loop required because we seek for the lowest order */
    if (!list_empty(l_list)) {
	dir_t * target = getnextlink(l_list);
	handle_t * ptrg = (mode == 'K') ? &target->stopp : &target->start;
	uchar * order = &ptrg->deep;
	list_t * dent;

	if (min > *order)
	    min = *order;

	lvl |= ptrg->run.lvl;

	list_for_each_prev(dent, l_list) {
	    dir_t * tmp = getlink(dent)->target;
	    handle_t * ptmp = (mode == 'K') ? &tmp->stopp : &tmp->start;
	    uchar * order = &ptmp->deep;

	    if (++deep > MAX_DEEP)
		break;

	    if (target == dir)
		break;		/* Loop detected */

	    if (min > *order)
		min = *order;

	    lvl |= ptmp->run.lvl;
	}
	if (min > 1) {		/* Set guessed order of this unknown script */
	    uchar * order = &peg->deep;
	    *order = min - 1;
	    peg->run.lvl |= lvl;	/* Set guessed runlevels of this unknown script */
	} else {
	    peg->run.lvl  = LVL_BOOT;
	}
    }
out:
    return;
}

/*
 * Sort linked list of provides into start or stop order
 * during this set new start or stop order of the serives.
 */
#undef SORT_REQUESTS
void lsort(const char type)
{
    list_t sort = { &sort, &sort };
#ifdef SORT_REQUESTS
    list_t * this;
#endif /* SORT_REQUESTS */
    int order;

    switch (type) {
    case 'K':
	for (order = 0; order <= maxstop; order++) {
	    list_t * ptr, * safe;
	    list_for_each_safe(ptr, safe, d_start) {
		dir_t * dir = getdir(ptr);
		if (dir->stopp.deep == order)
		    move_tail(ptr, &sort);
	    }
	}
	join(&sort, d_start);
#ifdef SORT_REQUESTS
	list_for_each(this, s_start) {
	    service_t * serv = getservice(this);
	    if (serv->attr.flags & SERV_DUPLET)
		continue;
	    initial(&sort);
	    for (order = 0; order <= maxstop; order++) {
		list_t * ptr, * safe;
		list_for_each_safe(ptr, safe, &serv->sort.rev) {
		    req_t * rev = getreq(ptr);
		    dir_t * dir = (dir_t*)rev->serv->dir;
		    if (dir->stopp.deep == order)
			move_tail(ptr, &sort);
		}
	    }
	    join(&sort, &serv->sort.rev);
	}
#endif /* SORT_REQUESTS */
	break;
    default:
	for (order = 0; order <= maxstart; order++) {
	    list_t * ptr, * safe;
	    list_for_each_safe(ptr, safe, d_start) {
		dir_t * dir = getdir(ptr);
		if (dir->start.deep == order)
		    move_tail(ptr, &sort);
	    }
	}
	join(&sort, d_start);
#ifdef SORT_REQUESTS
	list_for_each(this, s_start) {
	    service_t * serv = getservice(this);
	    if (serv->attr.flags & SERV_DUPLET)
		continue;
	    initial(&sort);
	    for (order = 0; order <= maxstart; order++) {
		list_t * ptr, * safe;
		list_for_each_safe(ptr, safe, &serv->sort.req) {
		    req_t * req = getreq(ptr);
		    dir_t * dir = (dir_t*)req->serv->dir;
		    if (dir->start.deep == order)
			move_tail(ptr, &sort);
		}
	    }
	    join(&sort, &serv->sort.req);
	}
#endif /* SORT_REQUESTS */
	break;
    }


}

/*
 * Clear out aliases of existing services, that is that for *one* script there
 * exist several provides which could have have been required different by
 * other services.  This avoids doing the same work several times.
 */ 
void nickservice(service_t *restrict orig, service_t *restrict nick)
{
    dir_t * dir = (dir_t*)orig->dir;
    dir_t * cmp = (dir_t*)nick->dir;
    list_t * dent, * safe;

    if (dir == cmp)
	return;

    if (cmp->script && cmp->script != dir->script)
	return;

    list_for_each_safe(dent, safe, &cmp->start.link) {
	link_t * link  = getlink(dent);
	dir_t * target = link->target;

	if (target == cmp)
	    continue;

	ln_sf(target, dir, 'S');

	/* remove the link from local link list but never free the target */

	delete(dent);
	free(link);
    }

    list_for_each_safe(dent, safe, &cmp->stopp.link) {
	link_t * link  = getlink(dent);
	dir_t * target = link->target;

	if (target == cmp)
	    continue;

	ln_sf(target, dir, 'K');

	/* remove the link from local link list but never free the target */

	delete(dent);
	free(link);
    }

    delete(&cmp->d_list);	/* remove alias entry from global service list */

	    			/* remember levels of old start handle */
    dir->start.run.lvl |= cmp->start.run.lvl;
    dir->start.flags   |= cmp->start.flags;

				/* remember levels of old stop handle */
    dir->stopp.run.lvl |= cmp->stopp.run.lvl;
    dir->stopp.flags   |= cmp->stopp.flags;

				/* remember global flags of old provide */
    orig->attr.flags |= nick->attr.flags;
    nick->attr.flags |= SERV_DUPLET;

    if (cmp->script && cmp->script != dir->script) {
	free(nick->attr.script);
	nick->attr.script = orig->attr.script;
    }

    nick->dir   = (void*)dir;	/* remember main provide */
    nick->start = &dir->start.run;
    nick->stopp = &dir->stopp.run;

    if (--cmp->ref <= 0) free(cmp);

    list_for_each_safe(dent, safe, &nick->sort.req) {
	req_t * this = getreq(dent);
	boolean ok = true;
	list_t * req;
	list_for_each(req, &orig->sort.req) {
	    if (!strcmp(this->serv->name,getreq(req)->serv->name)) {
		ok = false;
		break;
	    }
	}
	if (!ok) {
	    delete(dent);
	    free(this);
	} else
	    move_tail(dent, &orig->sort.req);
    }

    list_for_each_safe(dent, safe, &nick->sort.rev) {
	req_t * this = getreq(dent);
	boolean ok = true;
	list_t * rev;
	list_for_each(rev, &orig->sort.rev) {
	    if (!strcmp(this->serv->name,getreq(rev)->serv->name)) {
		ok = false;
		break;
	    }
	}
	if (!ok) {
	    delete(dent);
	    free(this);
	} else
	    move_tail(dent, &orig->sort.rev);
    }
}

void clear_all(void)
{
    list_t * this;

    /*
     * Find dangling links in global service list and remove them
     * if we by detect the remove bit from set above in the flags.
     */

    list_for_each(this, d_start) {
	dir_t *dir = getdir(this);
	list_t *dent, *hold;

	list_for_each_safe(dent, hold, &dir->start.link) {
	    link_t * link  = getlink(dent);
	    dir_t * target = link->target;

	    if (target == dir)
		continue;

	    if ((attof(target)->flags & SERV_DUPLET) == 0)
		continue;

	    /* remove the link from local link list */

	    delete(dent);
	    free(link);

 	    /* 
	     * Do not free allocated strings and structure if in use
	     * never free cmp->attr.script as this remains always in use.
	     */

	    if (--target->ref <= 0) free(target);
	}

	list_for_each_safe(dent, hold, &dir->stopp.link) {
	    link_t * link  = getlink(dent);
	    dir_t * target = link->target;

	    if (target == dir)
		continue;

	    if ((attof(target)->flags & SERV_DUPLET) == 0)
		continue;

	    /* remove the link from local link list */

	    delete(dent);
	    free(link);

 	    /* 
	     * Do not free allocated strings and structure if in use
	     * never free cmp->attr.script as this remains always in use.
	     */

	    if (--target->ref <= 0) free(target);
	}
    }
#if defined(DEBUG) && (DEBUG > 0)
    list_for_each(this, s_start) {
	service_t * srv = getservice(this);
	list_t * nxt, * hold;

	if (srv->attr.flags & SERV_DUPLET)
	    continue;

	list_for_each_safe(nxt, hold, s_start) {
	    list_t * dent, * safe;
	    service_t * orv;

	    orv = getservice(nxt);

	    if ((orv->attr.flags & SERV_DUPLET) == 0)
		continue;

	    if (srv->dir != orv->dir)
		continue;

	    srv->attr.flags |= orv->attr.flags;
	    srv->attr.flags &= ~SERV_DUPLET;

	    list_for_each_safe(dent, safe, &orv->sort.req) {
		req_t * this = getreq(dent);
		boolean ok = true;
		list_t * req;
		list_for_each(req, &srv->sort.req) {
		    if (!strcmp(this->serv->name,getreq(req)->serv->name)) {
			ok = false;
			break;
		    }
		}
		if (!ok) {
		   fprintf(stderr, "BUG: removed %s from start list of %s, missed getorig()?\n",
			   this->serv->name, orv->name);
		   delete(dent);
		   free(this);
		} else {
		   fprintf(stderr, "BUG: moved %s from start list of %s to %s, missed getorig()?\n",
			   this->serv->name, orv->name, srv->name);
		   move_tail(dent, &srv->sort.req);
		}
	    }

	    list_for_each_safe(dent, safe, &orv->sort.rev) {
		req_t * this = getreq(dent);
		boolean ok = true;
		list_t * rev;
		list_for_each(rev, &srv->sort.rev) {
		   if (!strcmp(this->serv->name,getreq(rev)->serv->name)) {
			ok = false;
			break;
		   }
		}
		if (!ok) {
		   fprintf(stderr, "BUG: removed %s from start list of %s, missed getorig()?\n",
			   this->serv->name, orv->name);
		   delete(dent);
		   free(this);
		} else {
		   fprintf(stderr, "BUG: moved %s from start list of %s to %s, missed getorig()?\n",
			   this->serv->name, orv->name, srv->name);
		   move_tail(dent, &srv->sort.rev);
		}
	    }
	}
    }
#endif
}

/*
 * Follow all services and their dependencies recursivly.
 */
void follow_all(void)
{
    list_t *tmp;

    /*
     * Follow all scripts and calculate the main ordering.
     */
    list_for_each(tmp, d_start) {
	maxorder = &maxstart;
	follow(getdir(tmp), 'S', 1);
	maxorder = &maxstop;
	follow(getdir(tmp), 'K', 1);
    }

    /*
     * Guess order of not installed scripts in comparision
     * to the well known scripts.
     */
    list_for_each(tmp, d_start) {
	maxorder = &maxstart;
	guess_order(getdir(tmp), 'S');
	maxorder = &maxstop;
	guess_order(getdir(tmp), 'K');
    }
}

boolean is_loop_detected(void)
{
    list_t *tmp;
    list_for_each(tmp, d_start) {
	dir_t * dir = getdir(tmp);
	if (dir->start.flags & DIR_LOOPREPORT)
	    return true;
	if (dir->stopp.flags  & DIR_LOOPREPORT)
	    return true;
    }
    return false;
}

/*
 * For debuging: show all services
 */
void show_all()
{
    list_t *tmp;
    if (maxstop > 0) list_for_each(tmp, d_start) {
	char * script, *name, *lvlstr;
	dir_t * dir = getdir(tmp);
	handle_t * peg;
	uchar deep;
	ushort lvl;
	if (!dir)
	    continue;
	name = dir->name;
	peg  = &dir->stopp;
	lvl  = peg->run.lvl;
	lvlstr = lvl2str(lvl);
	deep = peg->deep;
	if (attof(dir)->script)
	    script = attof(dir)->script;
#if defined(DEBUG) && (DEBUG > 0)
	else if (*name == '$')
	    script = "%system";
	else
	    script = "%guessed";
	info(1, "K%.2d %s 0x%.2x '%s' (%s)\n", deep, name, lvl, lvlstr,
	     script);
#else
	else
	    script = NULL;
	if (script && lvlstr)
	    fprintf(stdout, "K:%.2d:%s:%s\n", deep, lvlstr, script);
#endif
	xreset(lvlstr);
    }
    if (maxstart > 0) list_for_each(tmp, d_start) {
	char * script, *name, *lvlstr;
	dir_t * dir = getdir(tmp);
	handle_t * peg;
	uchar deep;
	ushort lvl;
	if (!dir)
	    continue;
	name = dir->name;
	peg  = &dir->start;
	lvl  = peg->run.lvl;
	lvlstr = lvl2str(lvl);
	deep = peg->deep;
	if (attof(dir)->script)
	    script = attof(dir)->script;
#if defined(DEBUG) && (DEBUG > 0)
	else if (*name == '$')
	    script = "%system";
	else
	    script = "%guessed";
	info(1, "S%.2d %s 0x%.2x '%s' (%s)\n", deep, name, lvl, lvlstr,
	     script);
#else
	else
	    script = NULL;
	if (script && lvlstr)
	    fprintf(stdout, "S:%.2d:%s:%s\n", deep, lvlstr, script);
#endif
	xreset(lvlstr);
    }
}

/*
 * Used within loops to get scripts not included in this runlevel
 */
boolean notincluded(const char *restrict const script, const char mode, const int runlevel)
{
    list_t *tmp;
    boolean ret = false;
    const ushort lvl = map_runlevel_to_lvl (runlevel);

    list_for_each_prev(tmp, d_start) {
	dir_t * dir = getdir(tmp);
	level_t * run = (mode == 'K') ? &dir->stopp.run : &dir->start.run;

	if (run->lvl & lvl)		/* Same runlevel */
	    continue;

	if (dir->script == (char*)0)	/* No such file */
	    continue;

	if (strcmp(script, dir->script))
	    continue;			/* Not this file */

	ret = true;			/* Not included */
	break;
    }

    return ret;
}

/*
 * Used within loops to list services an for a given runlevel bit mask.
 */
service_t * listscripts(const char **restrict script, const char mode, const ushort lvl)
{
    static list_t * tmp;
    service_t * serv;
    ushort level;
    dir_t * dir;

    if (!*script)
	tmp  = d_start->next;

    do {
	serv = (service_t*)0;
	if (tmp == d_start)
	    break;
	prefetch(tmp->next);
	dir = getdir(tmp);

        attof(dir)->korder = dir->stopp.deep;
        attof(dir)->sorder = dir->start.deep;

	serv = dir->serv;
	*script = serv->attr.script;

	switch (mode) {
	case 'X':
	    level = (dir->stopp.run.lvl|dir->start.run.lvl);
	    break;
	case 'K':
	    level = dir->stopp.run.lvl;
	    break;
	default:
	    level = dir->start.run.lvl;
	    break;
	}

	tmp = tmp->next;

    } while ((*script == (char*)0) || (level & lvl) == 0);

    return serv;
}

/*
 * THIS services DEPENDS on that service befor startup or shutdown.
 */
void requires(service_t *restrict this, service_t *restrict dep, const char mode)
{
    ln_sf((dir_t*)this->dir, (dir_t*)dep->dir, mode);
}

/*
 * Set the runlevels of a service.
 */
void runlevels(service_t *restrict serv, const char mode, const char *restrict lvl)
{
    dir_t * dir   = (dir_t *)serv->dir;
    handle_t * peg = (mode == 'K') ? &dir->stopp : &dir->start;
    peg->run.lvl |= str2lvl(lvl);
}

/*
 * Reorder all services starting with a service
 * being in same runlevels.
 */
void setorder(const char *restrict script, const char mode, const int order, const boolean recursive)
{
    dir_t * dir = findscript(script);
    handle_t * peg;
    list_t * tmp;

    if (!dir)
	goto out;

    if (mode == 'K') {
	peg = &dir->stopp;
	maxorder = &maxstop;
    } else {
	peg = &dir->start;
	maxorder = &maxstart;
    }

    if (peg->mindeep < order)
	peg->mindeep = order;		/* Remember lowest default order deep */

    if (peg->deep >= peg->mindeep)	/* Nothing to do */
	goto out;

    if (!recursive) {
	peg->deep = peg->mindeep;
	goto out;
    }

    /*
     * Follow the script and re-calculate the ordering.
     */
    __follow(dir, (dir_t*)0, peg->mindeep, mode, 0);

    /*
     * Guess order of not installed scripts in comparision
     * to the well known scripts.
     */
    list_for_each(tmp, d_start)
	guess_order(getdir(tmp), mode);
out:
    return;
}

/*
 * Get the order of a script.
 */
int getorder(const char *restrict script, const char mode)
{
    dir_t * dir = findscript(script);
    int order = 0;

    if (dir) {
	handle_t * peg = (mode == 'K') ? &dir->stopp : &dir->start;
	order = peg->deep;
    }

    return order;
}

/*
 * Provide a service if the corresponding script
 * was read and the scripts name was remembered.
 * A given script name marks a service as a readed one.
 * One script and several provided facilities leads
 * to the same order for those facilities.
 */
boolean makeprov(service_t *restrict serv, const char *restrict script)
{
    dir_t *restrict alias = findscript(script);
    dir_t *restrict dir   = (dir_t *restrict)serv->dir;
    boolean ret = true;

    if (!dir->script) {
	list_t * ptr;
	if (!alias) {
	    serv->attr.script = xstrdup(script);
	    serv->attr.flags |= SERV_SCRIPT;
	    dir->script = serv->attr.script;
	} else
	    dir->script = alias->script;

	list_for_each(ptr, s_start) {
	    service_t * tmp = getservice(ptr);
	    if (tmp == serv)
		continue;
	    if (tmp->dir != serv->dir)
		continue;
	    if (tmp->attr.script)
		continue;
	    tmp->attr.script = dir->script;
	    tmp->attr.flags |= SERV_SCRIPT;
	}

    } else if (strcmp(dir->script, script))
	ret = false;

    return ret;
}

/*
 * Find the script name of a provided feature
 */
const char * getscript(const char *restrict prov)
{
    char * script = (char*)0;
    list_t * ptr;

    list_for_each(ptr, s_start) {
	service_t * this = getservice(ptr);
	if (!strcmp(this->name, prov)) {
	    if (this->attr.script)
		script = this->attr.script;
	    break;
	}
    }
    return script;
}

/*
 * Return the provided service of a given script
 */
const char * getprovides(const char *restrict script)
{
    const dir_t * dir = findscript(script);
    const char * prov = (const char*)0;

    if (dir)
	prov = dir->name;
    return prov;
}

/*
 * Find a specific service by its name
 */
service_t * findservice(const char *restrict const name)
{
    list_t * ptr;
    service_t * ret = (service_t*)0;

    if (name == (const char*)0)
	goto out;

    list_for_each(ptr, s_start) {
	service_t * this = getservice(ptr);
	if (!strcmp(this->name, name)) {
	    ret = this;
	    break;
	}
    }
out:
    return ret;
}
