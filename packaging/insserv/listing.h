/*
 * listing.h
 *
 * Copyright 2000,2009 Werner Fink, 2000 SuSE GmbH Nuernberg, Germany.
 *				    2008,2009 SuSE Linux Products GmbH Nuernberg, Germany
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

#include <stddef.h>
#include <sys/types.h>
#include "config.h"

typedef enum _boolean {false, true} boolean;
typedef unsigned char uchar;
#ifndef __USE_MISC
typedef unsigned short ushort;
typedef unsigned int uint;
#endif

#ifndef __OPTIMIZE__
# warning This will not compile without -O at least
#endif
#if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 199901L)
# ifndef  inline
#  define inline		__inline__
# endif
# ifndef  restrict
#  define restrict		__restrict__
# endif
# ifndef  volatile
#  define volatile		__volatile__
# endif
# ifndef  asm
#  define asm			__asm__
# endif
# ifndef  extension
#  define extension		__extension__
# endif
#endif
#ifndef  attribute
# define attribute(attr)	__attribute__(attr)
#endif

/*
 * This is lent from the kernel by e.g. using
 *
 *   echo '#include <asm-i386/processor.h>\nint main () { prefetch(); return 0; }' | \
 *	gcc -I/usr/src/linux/include -D__KERNEL__ -x c -E -P - | \
 *	sed -rn '/void[[:blank:]]+prefetch[[:blank:]]*\(/,/^}/p'
 *
 * on the appropiate architecture (here on i686 for i586).
 */
static inline void prefetch(const void *restrict x) attribute((used,always_inline));
static inline void prefetch(const void *restrict x)
{
#if   defined(__x86_64__)
    asm volatile ("prefetcht0 %0"  :: "m" (*(unsigned long *)x))
#elif defined(__ia64__)
    asm volatile ("lfetch [%0]"    :: "r" (x))
#elif defined(__powerpc64__)
    asm volatile ("dcbt 0,%0"      :: "r" (x))
#elif 1 && defined(__i386__)
    asm volatile ("661:\n\t"
		  ".byte 0x8d,0x74,0x26,0x00\n"
		  "\n662:\n"
		  ".section .altinstructions,\"a\"\n"
		  "  .align 4\n"
		  "  .long 661b\n"
		  "  .long 663f\n"
		  "  .byte %c0\n"
		  "  .byte 662b-661b\n"
		  "  .byte 664f-663f\n"
		  ".previous\n"
		  ".section .altinstr_replacement,\"ax\"\n"
		  "   663:\n\t"
		  "   prefetchnta (%1)"
		  "   \n664:\n"
		  ".previous"
		  :: "i" ((0*32+25)), "r" (x))
#endif
    ;
}

#if defined(DEBUG) && (DEBUG > 0)
# define __align attribute((packed))
#else
# define __align attribute((aligned(sizeof(struct list_struct*))))
#endif
#define __packed attribute((packed))

#define alignof(type)		((sizeof(type)+(sizeof(void*)-1)) & ~(sizeof(void*)-1))
#define strsize(string)		((strlen(string)+1)*sizeof(char))

typedef struct list_struct {
    struct list_struct * next, * prev;
} __align list_t;

/*
 * Linked list handling
 * ====================
 * The structures which will be linked into such lists have to be of the
 * same type.  The structures may have alway a list identifier of the type
 * `list_t' as very first element.  With this the macro list_entry() can
 * be used to cast the memory address of a list member to the corresponding
 * allocated structure.
 */

/*
 * Insert new entry as next member.
 */
static inline void insert(list_t *restrict new, list_t *restrict here) attribute((always_inline,nonnull(1,2)));
static inline void insert(list_t *restrict new, list_t *restrict here)
{
    list_t * prev = here;
    list_t * next = here->next;

    next->prev = new;
    new->next = next;
    new->prev = prev;
    prev->next = new;
}

/*
 * Set head
 */
static inline void initial(list_t *restrict head) attribute((always_inline,nonnull(1)));
static inline void initial(list_t *restrict head)
{
    head->prev = head->next = head;
}

/*
 * Remove entries, note that the pointer its self remains.
 */
static inline void delete(list_t *restrict entry) attribute((always_inline,nonnull(1)));
static inline void delete(list_t *restrict entry)
{
    list_t * prev = entry->prev;
    list_t * next = entry->next;

    next->prev = prev;
    prev->next = next;

    initial(entry);
}

static inline void join(list_t *restrict list, list_t *restrict head) attribute((always_inline,nonnull(1,2)));
static inline void join(list_t *restrict list, list_t *restrict head)
{
    list_t * first = list->next;

    if (first != list) {
	list_t * last = list->prev;
       	list_t * at = head->next;

       	first->prev = head;
       	head->next = first;

       	last->next = at;
       	at->prev = last;
    }
}

static inline boolean list_empty(list_t *restrict head) attribute((always_inline,nonnull(1)));
static inline boolean list_empty(list_t *restrict head)
{
     return head->next == head;
}

static inline void move_tail(list_t *restrict entry, list_t *restrict head) attribute((always_inline,nonnull(1,2)));
static inline void move_tail(list_t *restrict entry, list_t *restrict head)
{
    list_t * prev = entry->prev;
    list_t * next = entry->next;

    next->prev = prev;		/* remove enty from old list */
    prev->next = next;

    prev = head->prev;
    next = head;

    next->prev = entry;		/* and add it at tail of new list */
    entry->next = next;
    entry->prev = prev;
    prev->next = entry;
}


#define list_entry(ptr, type, member)	(__extension__ ({	\
	const typeof( ((type *)0)->member ) *__mptr = (ptr);	\
	((type *)( (char *)(__mptr) - offsetof(type,member) )); }))
#define list_for_each(pos, head)	\
	for (pos = (head)->next; prefetch(pos->next), pos != (head); pos = pos->next)
#define np_list_for_each(pos, head)	\
	for (pos = (head)->next; pos != (head); pos = pos->next)
#define list_for_each_safe(pos, safe, head)	\
	for (pos = (head)->next, safe = pos->next; pos != (head); pos = safe, safe = pos->next)
#define list_for_each_prev(pos, head)	\
	for (pos = (head)->prev; prefetch(pos->prev), pos != (head); pos = pos->prev)
#define np_list_for_each_prev(pos, head)	\
	for (pos = (head)->prev; pos != (head); pos = pos->prev)

/*
 * The runlevel bits within own struct
 */
typedef struct level_struct {
    ushort		    lvl;
} __packed level_t;

/*
 * Common attributes
 */
typedef struct attr_struct {
    ushort		  flags;
    short		    ref;
    uchar		 sorder;
    uchar		 korder;
    char		*script;
} __packed attr_t;

/*
 * Linked list of required services (start/stop)
 */
typedef struct sort_struct {
    list_t	       req, rev;
} __align sort_t;

/*
 * Objects of linked list of required services
 */
typedef struct service_struct service_t;
typedef struct req_serv {
    list_t		   list;
    ushort		  flags;
    service_t	 *restrict serv;
} __align req_t;
#define getreq(arg)	list_entry((arg), struct req_serv, list)

/*
 * Used by findservice()
 */
struct service_struct {
    list_t		 s_list;
    sort_t		   sort;
    void	*restrict   dir;
    level_t	*restrict start;
    level_t	*restrict stopp;
    attr_t		   attr;
    char		 * name;
} __align;
#define getservice(list)	list_entry((list), service_t, s_list)

extern list_t * s_start;
extern int maxstart;
extern int maxstop;

extern void clear_all(void);
extern void nickservice(service_t *restrict orig, service_t *restrict nick) attribute((nonnull(1,2)));
extern void follow_all(void);
extern void show_all(void);
extern void requires(service_t *restrict this, service_t *restrict dep, const char mode) attribute((nonnull(1,2)));
extern void runlevels(service_t *restrict serv, const char mode, const char *restrict lvl) attribute((nonnull(1,3)));
extern boolean makeprov(service_t *restrict serv, const char *restrict script) attribute((nonnull(1,2)));
extern void setorder(const char *restrict script, const char mode, const int order, const boolean recursive) attribute((nonnull(1)));
extern int getorder(const char *restrict script, const char mode) attribute((nonnull(1)));
extern boolean notincluded(const char *restrict const script, const char mode, const int runlevel) attribute((nonnull(1)));
extern const char * getscript(const char *restrict prov) attribute((nonnull(1)));
extern const char * getprovides(const char *restrict script) attribute((nonnull(1)));
extern service_t * listscripts(const char **restrict script, const char mode, const ushort lvl);
extern boolean is_loop_detected(void);
extern service_t * addservice(const char *restrict const serv) attribute((malloc,nonnull(1)));
extern service_t * findservice(const char *restrict const name);
extern service_t * getorig(service_t *restrict serv) attribute((const,nonnull(1)));
extern void lsort(const char type);

/*
 * Common short cuts
 */
extern const char *const delimeter;
extern void error(const char *restrict fmt, ...) attribute((noreturn,format(printf,1,2)));
extern void warn (const char *restrict fmt, ...) attribute((format(printf,1,2)));
extern void info (int level, const char *restrict fmt, ...) attribute((format(printf,2,3)));
extern inline int map_has_runlevels(void) attribute((always_inline));
extern inline char map_runlevel_to_key(const int runlevel);
extern inline ushort map_key_to_lvl(const char key);
extern inline const char *map_runlevel_to_location(const int runlevel);
extern inline ushort map_runlevel_to_lvl(const int runlevel);
extern inline ushort map_runlevel_to_seek(const int runlevel);
extern ushort str2lvl(const char *restrict lvl) attribute((nonnull(1)));
extern char * lvl2str(const ushort lvl);

static inline char * xstrdup(const char *restrict s) attribute((always_inline,malloc));
static inline char * xstrdup(const char *restrict s)
{
    char * r;
    if (!s)
	error("%s", strerror(EINVAL));
    if (!(r = strdup(s)))
	error("%s", strerror(errno));
    return r;
} 

#define xreset(ptr)	\
	{char *restrict tmp = (char *restrict)ptr; if (ptr && *tmp) free(ptr);} ptr = NULL

#if defined(HAS_unlinkat) && defined(_ATFILE_SOURCE) && !defined(__stub_unlinkat)
# define xremove(d,x) (__extension__ ({ if ((dryrun ? 0 : \
	(unlinkat(d,x,0) != 0 && (errno != EISDIR || unlinkat(d,x,AT_REMOVEDIR) != 0)))) \
	warn ("can not remove(%s%s): %s\n", rcd, x, strerror(errno)); \
	else \
	info(1, "remove service %s/%s%s\n", path, rcd, x); }))
#else
# define xremove(d,x) (__extension__ ({ if ((dryrun ? 0 : (remove(x) != 0))) \
	warn ("can not remove(%s%s): %s\n", rcd, x, strerror(errno)); \
	else \
	info(1, "remove service %s/%s%s\n", path, rcd, x); }))
#endif
#if defined(HAS_symlinkat) && defined(_ATFILE_SOURCE) && !defined(__stub_symlinkat)
# define xsymlink(d,x,y) (__extension__ ({ if ((dryrun ? 0 : (symlinkat(x, d, y) != 0))) \
	warn ("can not symlink(%s, %s%s): %s\n", x, rcd, y, strerror(errno)); \
	else \
	info(1, "enable service %s -> %s/%s%s\n", x, path, rcd, y); }))
#else
# define xsymlink(d,x,y) (__extension__ ({ if ((dryrun ? 0 : (symlink(x, y) != 0))) \
	warn ("can not symlink(%s, %s%s): %s\n", x, rcd, y, strerror(errno)); \
	else \
	info(1, "enable service %s -> %s/%s%s\n", x, path, rcd, y); }))
#endif
#if defined(HAS_fstatat) && defined(_ATFILE_SOURCE) && !defined(__stub_fstatat)
# define xstat(d,x,s)	(__extension__ ({ fstatat(d,x,s, 0); }))
# define xlstat(d,x,s)	(__extension__ ({ fstatat(d,x,s, AT_SYMLINK_NOFOLLOW); }))
#else
# define xstat(d,x,s)	(__extension__ ({ stat(x,s); }))
# define xlstat(d,x,s)	(__extension__ ({ lstat(x,s); }))
#endif
#if defined(HAS_readlinkat) && defined(_ATFILE_SOURCE) && !defined(__stub_readlinkat)
# define xreadlink(d,x,b,l)	(__extension__ ({ readlinkat(d,x,b,l); }))
#else
# define xreadlink(d,x,b,l)	(__extension__ ({ readlink(x,b,l); }))
#endif
#if defined(HAS_openat) && defined(_ATFILE_SOURCE) && !defined(__stub_openat)
# define xopen(d,x,f)	(__extension__ ({ openat(d,x,f); }))
#else
# define xopen(d,x,f)	(__extension__ ({ open(x,f); }))
#endif

/*
 * Bits of the requests
 */
#define REQ_MUST	0x0001
#define REQ_SHLD	0x0002
#define REQ_KILL	0x0004

/*
 * Bits of the services
 */
#define SERV_KNOWN	0x0001
#define SERV_NOTLSB	0x0002
#define SERV_ALREADY	0x0004
#define SERV_INTRACT	0x0008 
#define SERV_ENABLED	0x0010
#define SERV_ALL	0x0020
#define SERV_DUPLET	0x0040
#define SERV_SCRIPT	0x0080
#define SERV_NOSTOP	0x0100
#define SERV_CMDLINE	0x0200
#define SERV_FIRST	0x0400

/*
 * Bits of the runlevels
 */
#define LVL_HALT	0x0001
#define LVL_ONE		0x0002
#define LVL_TWO		0x0004
#define LVL_THREE	0x0008
#define LVL_FOUR	0x0010
#define LVL_FIVE	0x0020
#define LVL_REBOOT	0x0040
#ifdef SUSE
# define LVL_SINGLE	0x0080
# define LVL_BOOT	0x0100
#else
# define LVL_SINGLE	0x0000
# define LVL_BOOT	0x0080
#endif

/*
 * LVL_BOOT is already done if one of the LVL_ALL will be entered.
 */
#define LVL_ALL		(LVL_HALT|LVL_ONE|LVL_TWO|LVL_THREE|LVL_FOUR|LVL_FIVE|LVL_REBOOT|LVL_SINGLE)

/*
 * Normal runlevels which are _direct_ available by shutdown/reboot/halt
 */
#define LVL_NORM	(LVL_HALT|LVL_ONE|LVL_TWO|LVL_THREE|LVL_FOUR|LVL_FIVE|LVL_REBOOT)

/*
 * Oneway runlevels at shutdown/reboot/halt/single
 */
#define LVL_ONEWAY	(LVL_HALT|LVL_ONE|LVL_REBOOT|LVL_SINGLE)
/*
 * Maximum start/stop level
 */
#define MAX_DEEP 99
