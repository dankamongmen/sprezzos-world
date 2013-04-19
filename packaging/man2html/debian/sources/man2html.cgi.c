/* vim:ts=4:et:sts=4:sw=4
**
** This program was written by Richard Verhoeven (NL:5482ZX35)
** at the Eindhoven University of Technology. Email: rcb5@win.tue.nl
**
** Permission is granted to distribute, modify and use this program as long
** as this comment is not removed or changed.
*/

/*
** This program has been modified to act as cgi-bin program
** which calls /usr/bin/man2html
** Robert Luberda <robert@debian.org>, Jan 2003
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>

#include "utils.h"

#define NULL_TERMINATED(n) ((n) + 1)

#define LARGE_STR_MAX 2000
#define MED_STR_MAX   500
#define SMALL_STR_MAX 100

#define MAX_MAN_PATHS 100	/* Max number of directories */


#ifndef TOPLEVELDOC
#define TOPLEVELDOC "/usr/share/man2html/man.aux"
#endif

#ifndef CGIBASE
#define CGIBASE "/cgi-bin/man"
#endif

#ifndef LYNXCGIBASE
#define LYNXCGIBASE "lynxcgi:/usr/lib/cgi-bin/man"
#endif


typedef struct STRDEF STRDEF;
struct STRDEF
{
    int nr;
    dev_t dev;
    ino_t inode;
    char *st;
    STRDEF *next;
};
static char *sectionname = NULL;
static STRDEF *foundpages = NULL;



static char *manpath[MAX_MAN_PATHS + 1] =
{
    "",		/* for searches when path is absolute */
    "/usr/local/share/man/",
    "/usr/share/man/",
    "/opt/man/",
    NULL
};



#define file_magic "\1f"
#define lynx_magic "\1l"
#define pipe_magic "\1p"
static char * zcats[][6] =
{
    { "", NULL },
    { ".Z", "/bin/zcat", file_magic, NULL },
    { ".gz", "/bin/gzip", "-d", "-c", file_magic, NULL },
    { ".bz2", "/usr/bin/bzip2", "-d", "-c", file_magic, NULL },
    { NULL }
};

static char * cmd_man2html[] =
{
    "/usr/bin/man2html",
    lynx_magic, "-l",
    pipe_magic, "-D",
    file_magic,
    NULL
};

static char ** get_argv(char** argv, const int islynx, const int ispipe, char * filename)
{
    int i,j;
    for (i = 0,j = 0; argv[i]; i++)
    {
        if (!strcmp(argv[i], file_magic))
        {
            if (filename)
                argv[j++] = filename;
        }
        else if (!strcmp(argv[i], lynx_magic))
        {
            if (!islynx)
                i++;
        }
        else if (!strcmp(argv[i], pipe_magic))
        {
            if (!ispipe)
                i++;
        }
        else
            argv[j++] = argv[i];
    };
    argv[j] = NULL;
    return argv;
}



/* executes /usr/bin/man2html, eventually through a pipe */
static void execute_man2html(int islynx, char* filename)
{
    int i, manpipe;
    int len;
    int filedes[2];
    char m[256];
    char ** argv;
    struct stat bstat;
    struct tm *tm;

    len = strlen(filename);
    manpipe = -1;
    for (i = 1; zcats[i][0]; i++)
    {
        if (!strcmp(filename + len - strlen(zcats[i][0]), zcats[i][0]))
        {
            manpipe = i;
            break;
        }
    }

    if (manpipe > - 1)
    {
        if (pipe(filedes) < 0)
        {
            error_page(500, "Can't create pipe",
                       "The pipe() function failed: %s", strerror(errno));
        }
        switch (fork())
        {
            case -1:
                //error;
                break;
            case 0: /* child */
                {
                    close(filedes[0]);
                    if (filedes[1] != 1)
                    {
                        close(1);
                        dup2(filedes[1], 1);
                    }
                    argv = get_argv(zcats[manpipe] + 1, islynx, 1, filename);
                    execv(argv[0], argv);
                    exit(EXIT_FAILURE);
                    break;
                }
            default: /* parent */
                {
                    close(filedes[1]);
                    if (filedes[0] != 0)
                    {
                        close(0);
                        dup2(filedes[0], 0);
                    }
                    break;
                }
        }
    }
    if ( !stat(filename, &bstat) )
    {
        tm = gmtime(&bstat.st_mtime);
        if(strftime(m, sizeof(m), "%a, %d %b %Y %H:%M:%S +0000", tm))
        {
            printf("Last-Modified: %s\n", m);
            fflush(stdout);
        }
    }

    argv = get_argv(cmd_man2html, islynx, manpipe > 0, filename);
    execv(argv[0], argv);
    error_page(500, "Can't execute man2html",
               "Can't execute %s : %s", cmd_man2html[0], strerror(errno));
}


static const char * const sections = "123456789";

static char *strmaxcpy(char * const to, const char * const from, const int n)
{
    /* Assumes space for n plus a null */
    int len = strlen(from);
    strncpy(to, from, n);
    to[(len <= n) ? len : n] = '\0';
    return to;
}

static char *strmaxcat(char * const to, const char * const from, const int n)
{
    /* Assumes space for n plus a null */
    int to_len = strlen(to);
    if (to_len < n)
    {
        int from_len = strlen(from);
        int cp = (to_len + from_len <= n) ? from_len : n - to_len;
        strncpy(to + to_len, from, cp);
        to[to_len + cp] = '\0';
    }
    return to;
}

static char *strlimitcpy(char *to, char *from, int n, int limit)
{
    /* Assumes space for limit plus a null */
    int len = n > limit ? limit : n;
    strmaxcpy(to, from, len);
    to[len] = '\0';
    return to;
}

static char *trim_compress_suffix(char *filename)
{
    static char result[NULL_TERMINATED(MED_STR_MAX)];
    int i, flen, zlen;
    int trim = -1;

    flen = strlen(filename);
    for (i = 1; zcats[i][0]; i++)
    {
        zlen = strlen(zcats[i][0]);
        if (strcmp(filename + flen - zlen, zcats[i][0]) == 0)
        {
            trim = zlen;
            break;
        }
    }

    strlimitcpy(result, filename, strlen(filename) - trim, MED_STR_MAX);
    if (trim > 0)
        result[flen - trim + 1] = '\0';
    return result;
}


static void usage(char * cgibase)
{
    char buffer[NULL_TERMINATED(LARGE_STR_MAX)];
    FILE *toplevel = fopen(TOPLEVELDOC, "r");

    if (!toplevel && errno != ENOENT)
    {
        fprintf(stderr, "man2html: error openning %s: %m\n", TOPLEVELDOC);
    }

    if (!toplevel)
    {
        error_page(0, "Manual pages",
                   "This is a HyperText interface to the UNIX man pages.\n"
                   "You can enter a program name, the section, an extra\n"
                   "directory (using -M) or a full name. For example\n"
                   "<UL><LI><TT>elm</TT>\n"
                   "<LI><TT>elm 1</TT>\n"
                   "<LI><TT>-M /usr/local/man elm</TT>\n"
                   "<LI><TT>/usr/share/man/man1/gperf.1</TT>\n"
                   "</UL>\n"
                   "<ISINDEX>\n"
                   "<P>"
                   "This man2html converter was written by \n"
                   "<A HREF=\"http://wsinwp01.win.tue.nl:1234/index.html\">"
                   "Richard Verhoeven</A>"
                  );
        exit(0);
    }

    while (fgets(buffer, LARGE_STR_MAX, toplevel))
    {
        char *p, *line;
        line = buffer;
        while ((p = strstr(line, "%cg")))
        {
            *p = 0;
            fputs(line,stdout);
            fputs(cgibase,stdout);
            line = p + 3;
        }
        fputs(line,stdout);
    }
    if (!feof(toplevel))
    {
        fprintf(stderr, "man2html: error reading %s: %m\n", TOPLEVELDOC);
        exit(EXIT_FAILURE);
    }
    fclose(toplevel);
    exit(0);
}



/* if section_idx is < -1 then searches in all sections
** otherwise it searches in section[section_idx]
*/
static int search_man_in_manpaths(const char *name, int section_idx)
{
    char smfbuf[NULL_TERMINATED(LARGE_STR_MAX)];
    char cmpbuf[NULL_TERMINATED(MED_STR_MAX)];
    int i,j,n,l,nr = 0;
    DIR *dr;
    struct dirent *de;
    STRDEF *h = NULL;

    if (strlen(name) > MED_STR_MAX)
    {
        error_page(500, "Error: name too long",
                   "man2html: search_manpath_all - name too long: \n%-80s...\n",
                   name);
    }
    strmaxcpy(cmpbuf,name, MED_STR_MAX - 2);

    n = strlen(name);
    cmpbuf[n++]='.';
    cmpbuf[n+1]='\0';
    for (i = 1; manpath[i]; i++)
    {
        strmaxcpy(smfbuf, manpath[i], LARGE_STR_MAX - 10);
        l = strlen(smfbuf);
        strcpy(smfbuf+l, "man");
        l+=3;
        smfbuf[l+1]='\0';
        for (j = 0; sections[j]; j++)
        {
            if (section_idx > -1 && section_idx != j)
                continue;
            smfbuf[l]=sections[j];
            cmpbuf[n]=sections[j];
            if ((dr = opendir(smfbuf)))
            {
                struct stat st;
                if (stat(smfbuf, &st) < 0)
                    continue;
                while ((de = readdir(dr)))
                {
                    if (!strncasecmp(de->d_name, cmpbuf, n+1))
                    {
                        int found = 0;
                        STRDEF * t = foundpages;
                        while (t && !found)
                        {
                            if (t->inode == de->d_ino && t->dev == st.st_dev)
                                found = 1;
                            t = t->next;
                        }

                        if (found)
                            continue;

                        if (h)
                        {
                            h->next = (STRDEF*) xmalloc(sizeof(STRDEF));
                            h = h->next;
                        }
                        else
                            h = foundpages = (STRDEF*) xmalloc(sizeof(STRDEF));
                        h->nr = i*256+j;
                        nr++;
                        h->st = xstrdup(de->d_name);
                        h->next = NULL;
                        h->inode = de->d_ino;
                        h->dev = st.st_dev;
                    }
                }
                closedir(dr);
            }
        }
    }
    return nr;
}
static int search_manpath_all(const char *name)
{
    return search_man_in_manpaths(name, -1);
}

static int search_manpath_section(const char *name, const char* section)
{
    if (!section) return 0;
    int j = 0;
    while (sections[j] && sections[j]!=section[0]) j++;
    if (!sections[j]) return 0;

    return search_man_in_manpaths(name, j);

}

static char smfbuf[NULL_TERMINATED(LARGE_STR_MAX)];

static char *search_manpath(const char *const name)
{
    int i, j;
    struct stat stbuf;

    /* assume manpath[0] == '/' && zcats[0][0] == "" */
    for (i =(*name == '/')?0:1; manpath[i]; i++)
    {
        for (j = 0; zcats[j][0]; j++)
        {
            if (strlen(name) + strlen(manpath[i]) + strlen(zcats[j][0])> LARGE_STR_MAX)
            {
                error_page(500, "Error: name too long",
                           "man2html: search_manpath - name too long: \n%-80s...\n",
                           name);
            }

            strmaxcpy(smfbuf, manpath[i], LARGE_STR_MAX);
            strmaxcat(smfbuf, name, LARGE_STR_MAX);
            strmaxcat(smfbuf, zcats[j][0], LARGE_STR_MAX);
            if (stat(smfbuf, &stbuf) !=-1) return smfbuf;
        }

        if (!i) /* <==> *name == '/' */
            return NULL;
    }
    return NULL;
}

static void get_man_config()
{
#ifdef MAN_CONFIG
    FILE *config = NULL;
    int num_paths = 1;			/* Zero is reserved for searches */
    char buffer[NULL_TERMINATED(MED_STR_MAX + 1)]; /* Allow for adding a "/" */

    config = fopen(MAN_CONFIG, "r");
    if (!config)
    {
        return;				/* Assume no config. */
    }

    while (fgets(buffer, MED_STR_MAX, config) && num_paths < MAX_MAN_PATHS)
    {
        char *dir, *line = buffer;
        char * end_dir = 0;
        line = line + strspn(line, " \t"); /* Skip spaces and tabs */
        if ((strncmp("MANDATORY_MANPATH", line, 17) == 0
                || strncmp("MANDB_MAP", line, 9) == 0)
                && (dir = strchr(line, '/')))  	/* dir points to start of dir-name */
        {
            end_dir = strpbrk(dir, " \t\n") ;
        }
        else
            continue;

        if (end_dir)  			/* End of dir name. */
        {
            (*end_dir) = '/';		/* Replace newline/space with "/" */
            (*(end_dir + 1)) = '\0';
        }

        int already_exists = 0;
        int i = 0;
        for (; !already_exists && i <num_paths; ++i)
        {
            if (strcmp(manpath[i], dir) == 0)
                already_exists = 1;
        }
        if (!already_exists)
        {
            manpath[num_paths] = xstrdup(dir);
            num_paths++;
        }
    }
    if (num_paths != 0)  			/* Mark new end of paths list */
    {
        manpath[num_paths] = NULL;
    }
    fclose(config);
#endif
}

static void set_user_manpaths(char *  mpath)
{
    if (!mpath)
        return;

    size_t nr_paths = 1;
    size_t i;
    char *new_manpath[sizeof(manpath)];
    char *t, *p;


    p = strtok(mpath, ":");
    while (nr_paths < sizeof(new_manpath) && p)
    {
        if (strstr(p, "..") != NULL)
        {
            /* invalid */
            error_page(400, "Error: invalid manpath",
                       "Invalid manpath given in the -M option.");
        }
        else
        {
            /* add missing slash */
            t = p;
            if (*(p + strlen(p) - 1) != '/')
            {
                t = (char *) xmalloc(strlen(p) + 2);
                strcpy(t, p);
                strcat(t, "/");
            }

#ifndef UNSECURE_MANPATH

            /* Set - see if it is a valid prefix */
            for (i = 1; manpath[i]; i++)
            {
                if (strncmp(manpath[i], t, strlen(manpath[i])) == 0)
                    break;	/* valid */
            }
            if (!manpath[i])
                error_page(400, "Error: invalid manpath",
                           "Invalid manpath given in the -M option.");

#endif
            new_manpath[nr_paths++] = t;
            p = strtok(NULL, ":");
        }
    }

    if (nr_paths > 1)
    {
        for (i = 1; i < nr_paths; i++)
            manpath[i] = new_manpath[i];
        manpath[nr_paths] = NULL;
    }
}

static void check_input(const char * arg)
{
    if (! arg)
        return;
    while (*arg)
    {
        if (*arg == '<' || *arg == '>' || *arg == '"')
        {
            error_page(403, "Invalid characters.",
                             "Request contains invalid characters.");
        }
        ++arg;
    }

}

int main(int argc, char **argv)
{
    char *t = NULL;
    int i, slsh_cnt = 0 , invalid = 0;
    char * user_manpaths = NULL;
    int insection = 0;
    char *h, *fullname;
    int argv_modif;
    int islynx;
    char * cgibase;
    char ffname[NULL_TERMINATED(LARGE_STR_MAX)];


    islynx = is_lynx();
    cgibase = islynx ? LYNXCGIBASE : CGIBASE;


    argv_modif = querystring2argv(&argc, &argv);
    t = getenv("PATH_INFO");
    if (t && *t)
        t = urldecode(t);

    if (!t || !*t) /* not :  cgi/man2html/mani/name.i */
    {
        i = 1;
        while (i<argc)
        {
            switch (argv[i][0])
            {
                case '-':
                    if (argv[i][1]=='M')  	/* Paths to search */
                    {
                        if (i+1<argc)
                        {
                            i++;
                            user_manpaths = argv[i];
                        }
                    }
                    break;
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                case 'n':
                case 'l':
                    /* Section name eg 1 or 1n */
                    if (!argv[i][1])
                    {
                        sectionname = argv[i];
                        break;
                    }
                    else if (!argv[i][2] && isalpha(argv[i][1]) &&
                             !isalpha(argv[i][0]) && islower(argv[i][1]))
                    {
                        sectionname = argv[i];
                        break;
                    }
                default:
                    if (argv[i][0])
                        t= (argv_modif) ? argv[i] : urldecode(argv[i]);
                    break;
            }
            i++;
        }
    }

    if (!t || !*t) usage(cgibase);		/* No man page requested */

    check_input(t);
    check_input(sectionname);
    check_input(user_manpaths);

    get_man_config();
    set_user_manpaths(user_manpaths);
    fullname = NULL;

    i = 0;
    slsh_cnt = 0;
    h = t;

    while (*h) slsh_cnt += (*h++ == '/');

    if (*t == '/')
    {
        /* check if it is valid manpath */
        for (i = 1; manpath[i]; i++)
            if (strncmp(manpath[i], t, strlen(manpath[i])) == 0)
                break;
        invalid = !manpath[i];
    }
    else if (slsh_cnt > 0)
    {
        h = strstr(t, "man");
        invalid = (!h || strchr(h, '/') == NULL);
    }

    if (invalid)
    {
        fprintf(stderr, "man2html: request for non man file %s\n", t);
        error_page(403, "Only man pages allowed.",
                   "Viewing the requested file is not allowed.");
        /* NOT REACHED */
    }


    if (slsh_cnt && strstr(t, "/../"))
    {
        fprintf(stderr, "man2html: attempt to use .. in man2html: %s\n", t);
        error_page(403, "Only man pages allowed.",
                   "You still try to get files which are man pages. Using the\n"
                   "<B>..</B> construction to get to a different directory will\n"
                   "<B>not</B> work either. If you try this very often, you\n"
                   "will end up in a black list.\n");
        /* NOT REACHED */
    }


    if (slsh_cnt == 0 && sectionname)
    {
        if (strlen(t) + strlen(sectionname) + 6 > LARGE_STR_MAX)
        {
            error_page(500, "Error: Name too long",
                       "man2html: error - name too long: man%c/%-80s.%-80s\n",
                       sectionname[0],t,sectionname);
        }
        snprintf(ffname, sizeof(ffname),  "man%c/%s.%s", sectionname[0],t,sectionname);
        h = search_manpath(ffname);
        if (h)
            fullname = h;

    }
    else if (slsh_cnt)
    {
        h = search_manpath(t);
        if (!h && slsh_cnt > 2)
        {
            char *g, *p;
            p = trim_compress_suffix(t);
            g = strrchr(p,'/');
            *g='.';
            p = strrchr(p,'/');
            *g = '/';
            h = search_manpath(p);
        }
        if (h)
            fullname = h;
    }

    h = t;
    if (!fullname)
    {
        if (slsh_cnt)
        {
            char * p;
            p = trim_compress_suffix(t);
            h = strrchr(p, '.');
            if (h)
            {
                *h='\0';
                sectionname = h+1;
            }
            else
                sectionname = NULL;

            h = strrchr(p, '/');
            if (!h) h = p;
            else h++;
        }

        i = 0;
        if (sectionname)
        {
            i = search_manpath_section(h, sectionname);
            insection = (i>0);
        }

        if (!i)
        {
            i = search_manpath_all(h);
        }


        /* we found some man pages ... */
        if (i == 1 && (!sectionname || insection) && !slsh_cnt)
        {
            snprintf(ffname, sizeof(ffname), "%sman%c/%s",
                     manpath[foundpages->nr/256],
                     sections[foundpages->nr%256],
                     foundpages->st);
            fullname = ffname;
        }
    }

    if (!fullname)
    {
        STRDEF *strd;
        int gen_error;
        if (!i)
        {
            error_page(404, "Man page not found",
                       "Sorry, no man pages available for %s%s%s%s%s.\n"
                       "<HR>\n"
                       "The links to other manual pages are not always correct.\n"
                       "Normally you will get a list of possible replacements,\n"
                       "but in this case the manual page just can't be found.\n",
                       h,
                       sectionname ? " in section " : " in any section" ,
                       sectionname ? sectionname : "",
                       sectionname ? ", nor in any other section" : "",
                       user_manpaths ? " in the given manpath" : "");
            /* NOT REACHED */
        }

        gen_error = slsh_cnt || (!insection && sectionname);

        if (gen_error)
            printf("Status: 404 Not found\n");
        printf(CONTENTTYPE);
        printf(DOCTYPE);
        printf("<HTML>\n<HEAD>\n<TITLE>");
        if (!gen_error)
            printf("Index to %s man pages%s%.1s", t,
                   insection ? " for section " : "",
                   insection ? sectionname : "");
        else
            printf("No man page for %s", h);
        printf("</TITLE>\n</HEAD>\n<BODY>\n<H1>");
        if (!gen_error)
            printf("Index to %s man pages%s%.1s.", t,
                   insection ? " for section " : "",
                   insection ? sectionname : "");
        else
            printf("No man page for %s", h);
        printf("</H1>\n<P>\n");

        if (gen_error)
        {
            printf("Sorry, the man page for %s does not exist%s%s",
                   h, sectionname ? " in section " : "",
                   sectionname ? sectionname : "");
            if (slsh_cnt)
                printf(" or path you specified was invalid");
            printf(".\n<BR>Maybe you can use %s instead.\n",
                   (i>1?"one of the following pages":"this page"));
        }

        printf("<UL>\n");
        strd = foundpages;
        while (strd)
        {
            printf("<LI><A HREF=\"%s/man2html%sman%c/%s\">%s</A> (%s)\n",
                   cgibase, manpath[strd->nr/256],
                   sections[strd->nr%256], strd->st,
                   trim_compress_suffix(strd->st),
                   manpath[strd->nr/256]);
            strd = strd->next;
        }
        printf("<UL>\n");
        printf("</BODY>\n</HTML>\n");
        exit(EXIT_SUCCESS);
    }


    execute_man2html(islynx, fullname);


    return EXIT_FAILURE;
}
