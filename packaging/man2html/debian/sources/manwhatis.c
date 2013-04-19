/* vim:ts=4:et:sts=4:sw=4

    Man pages CGI indexer.
    Copyright (C) 1998 Nicolás Lichtmaier <nick@debian.org>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    You may also find a copy of the license is in
    /usr/share/common-licenses/GPL in Debian systems.

    $Id: manwhatis.c 238 2011-01-09 17:05:31Z robert $
*/

#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>

#include "utils.h"

#define MAX 10000

#define CGIBASE "/cgi-bin/man"
#define LYNXCGIBASE "lynxcgi:/usr/lib/cgi-bin/man"

enum {mansec, manwhatis} called_as=manwhatis;
char manpath[200];
static char * cgibase;

const char *mandesc[11] =
{
    "",
    "User Commands",
    "System Calls",
    "Library Functions",
    "Special Files",
    "File Formats",
    "Games",
    "Miscellany",
    "Administration and Privileged Commands",
    "",
    "All available manual pages"
};

typedef struct
{
    char fl;
    char *name;
    char *section;
    char *real_name;
    char *description;
} manpage_t;


inline static int xstrcasecmp(const char *a, const char *b)
{
    while(ispunct(*a)&&*(a+1))
        a++;
    while(ispunct(*b)&&*(b+1))
        b++;
    return strcasecmp(a,b);
}

int compar(const void *a, const void *b)
{
    int x=(((manpage_t*)a)->fl - ((manpage_t*)b)->fl);
    if(x)
        return x;
    return xstrcasecmp(((manpage_t*)a)->name,((manpage_t*)b)->name);
}


static void set_privileges(int set)
{
    int i;
    static int saved_uids = 0; /* 0 if we haven't check uids yet
				      1 if process euid != uid or egid != gid
				      -1 if process is not setuid nor setgid
				    */
    static uid_t saved_euid = -1;
    static gid_t saved_egid = -1;

    if (saved_uids < 0)
        return;

    if (!saved_uids)
    {
        if (  ((saved_euid = geteuid()) != getuid())
                || ((saved_egid = getegid()) != getgid()) )
            saved_uids = 1;
        else
        {
            saved_uids = -1;
            return;
        }
    }

    if (!set)
    {
        i = setregid(-1, getgid());
        if (!i)
            i = setreuid(-1, getuid());
    }
    else
    {
        i = setregid(-1, saved_egid);
        if (!i)
            i = setreuid(-1, saved_euid);
    }

    if (i)
        error_page(500, "Can't set privileges",
                   "Can't set program privileges, setregid "
                   "or setreuid failed: %s", strerror(errno));
}



static int make_index(FILE *f, int section)
{
    char s[200],t[200],*p, last_letter='\0';
    manpage_t *m;
    char usedletters[256];
    int n=0,i;
    char * tmp;
    size_t j;

    memset(usedletters, 0, sizeof(usedletters));
    m=xmalloc(sizeof(manpage_t)*MAX);
    if(called_as==manwhatis)
    {
        FILE *g;

        g=popen("whatis -w '*'","r");
        if(!g)
        {
            error_page(500, "Can't read from pipe",
                       "Opening pipe for whatis command failed: %s", strerror(errno));
        }
        if (section!=10)
            snprintf(t, sizeof(t), "(%d",section);
        while(fgets(s, sizeof(s), g))
        {
            p = strstr(s, " - ");
            if (!p)
                continue;
            if(section!=10 && !strstr(s,t))
                continue;

            m[n].section = NULL;
            m[n].name    = NULL;
            m[n].real_name = NULL;
            m[n].description = xstrdup(p + 3);
            while ((p > s) && isspace(*p))
                p--;

            if (*p == ']')
            {
                *p = '\0';
                while (p > s && *p != '[')
                    p--;
                if (*p != '[')
                    continue; /* parse error */
                m[n].real_name = xstrdup(p+1);
                p--;
            }

            while ((p > s) && isspace(*p))
                p--;
            if (*p == ')')
            {
                *p = '\0';
                while ((p > s && *p != '('))
                    p--;
                if (*p != '(')
                    continue; /* parse error */
                m[n].section = xstrdup(p + 1);
                p--;
            }
            else
                continue; /* parse error */

            while ((p > s) && isspace(*p))
                p--;

            *(p + 1)= '\0';
            m[n].name = xstrdup(s);

            p=m[n].name;
            while(ispunct(*p) && *(p+1))
                p++;
            m[n].fl=toupper(*p);
            usedletters[(int)(m[n].fl)]++;
            n++;
            if(n%MAX==0)
                m=xrealloc(m,sizeof(manpage_t)*(n+MAX));
        }
        pclose(g);
    }
    else
    {
        strncpy(s, manpath, sizeof(s));
        s[sizeof(s)-1] = '\0';

        p=strtok(s,":\n\r");
        while(p)
        {
            for(i=1; i<10; i++) if(i==section || section==10)
                {
                    DIR *dir;
                    struct dirent *dirent;
                    snprintf(t, sizeof(t), "%s/man%d",p,i);
                    dir=opendir(t);
                    if(!dir)
                        continue;
                    while((dirent=readdir(dir)))
                    {
                        char *q;
                        if(dirent->d_name[0]=='.')
                            continue;
                        q=strrchr(dirent->d_name,'.');
                        if(!q || !*(q+1)) continue;
                        if(!strcmp(q+1,"gz"))
                        {
                            *q='\0';
                            while(q>dirent->d_name && *q!='.')
                                q--;
                            if(*q!='.')
                                continue;
                        }
                        m[n].section = xstrdup(q+1);
                        *q='\0';
                        m[n].name = xstrdup(dirent->d_name);
                        m[n].real_name = NULL;
                        m[n].description = NULL;

                        q=m[n].name;
                        while(ispunct(*q) && *(q+1))
                            q++;
                        m[n].fl=toupper(*q);
                        usedletters[(int)(m[n].fl)]++;
                        n++;
                        if(n%MAX==0)
                            m=xrealloc(m,sizeof(manpage_t)*(n+MAX));
                    }
                    closedir(dir);
                }
            p=strtok(NULL,":\n\r");
        }
    }
    qsort(m,n,sizeof(*m),&compar);
    fputs(DOCTYPE
          "<html>\n<head>\n"
          "<title>Manual Pages: ",f);
    if(section!=10)
        fprintf(f,"%d - ",section);
    fprintf(f,"%s</title>\n"
            "<link rel=Index href=\"%s/man2html\">\n"
            ,mandesc[section], cgibase);
    if(section>1)
        fprintf(f,"<link rel=Previous href=\"%s/%s?%d\">\n"
                ,cgibase, (called_as==manwhatis)?"manwhatis":"mansec"
                ,section-1);
    if(section<9)
        fprintf(f,"<link rel=Next href=\"%s/%s?%d\">\n"
                ,cgibase,(called_as==manwhatis)?"manwhatis":"mansec"
                ,section+1);
    if(section==10)
        fprintf(f,"</head>\n<body><h1>%s</h1>",mandesc[section]);
    else
        fprintf(f,"</head>\n<body><h1>Section %d: %s</h1>",section,mandesc[section]);

    fputs("<center>",f);
    for(j=33; j<sizeof(usedletters); j++)
        if(usedletters[j])
            fprintf(f,"<a href=\"#%c\">%c</a>\n",j,j);
    fprintf(f,"<br><a href=\"%s/man2html\">Section index</a>\n", cgibase);
    fputs("</center>\n<hr>\n",f);

    last_letter='\0';
    for(i=0; i<n; i++)
    {
        if(m[i].fl!=last_letter)
        {
            if(called_as==manwhatis && last_letter)
                fputs("</dl><p>\n",f);
            last_letter=m[i].fl;
            fprintf(f,"<h2><a name=\"%c\">%c</a></h2>\n",m[i].fl,m[i].fl);
            if(called_as==manwhatis)
                fputs("<dl>",f);
        }
        if(called_as==manwhatis)
            fputs("<dt>",f);
        tmp = urlencode(m[i].real_name?m[i].real_name:m[i].name);
        fprintf(f,"<a href=\"%s/man2html?%s%s+%c\">", cgibase,
                strchr(tmp, '%') ? QUERY_MAGIC_STR : "",
                tmp, m[i].section[0]);
        free(tmp);
        print_html_enc(m[i].name, f);
        fputc('(', f);
        print_html_enc(m[i].section, f);
        fputs(")</a>\n", f);
        if(called_as==manwhatis)
        {
            fputs("<dd>", f);
            print_html_enc(m[i].description, f);
            fputc('\n', f);
        }

        if(m[i].real_name) free(m[i].real_name);
        if(m[i].name) free(m[i].name);
        if(m[i].section) free(m[i].section);
        if(m[i].description) free(m[i].description);
    }
    if(called_as==manwhatis)
        fputs("</dl>\n",f);

    free(m);
    fputs("<hr>\n<center>\n",f);
    for(j=0; j< sizeof(usedletters); j++)
        if(usedletters[j])
            fprintf(f,"<a href=\"#%c\">%c</a>\n",j,j);
    fputs("</center>\n",f);

    fprintf(f,"</body>\n</html>\n");
    return 1;
}

int main(int argc, char**argv)
{
    char cache[100];
    int section;
    char fn[200],s[200],t[200],*p,*buf;
    struct stat st;
    struct tm *tm;
    int refresh=0,n,cache_exists;
    FILE *f;
    char cache_suffix;


    set_privileges(0);
    umask(022);

    if (is_lynx())
    {
        cgibase = LYNXCGIBASE;
        cache_suffix = 'l';
    }
    else
    {
        cgibase = CGIBASE;
        cache_suffix = 'h';
    }

    querystring2argv(&argc, &argv);

    if (argc >= 2)
        section=argv[1][0]-'0';
    else
    {
        error_page(400, "No section number.",
                   "No section number submitted.\n"
                   "Must be 1..9 or all\n");
        return 0;
    }
    if(strstr(argv[0],"mansec"))
        called_as=mansec;
    if (section=='a'-'0')
        section=10;

    if (section<1 || section>10)
    {
        error_page(400, "Illegal section number.",
                   "Must be 1..9 or all\n");
        /* NOT REACHED */
    }
    snprintf(cache, sizeof(cache), "/var/cache/man2html/%s%c-%d.html"
             ,(called_as==manwhatis)?"whatis":"manindex",cache_suffix, section);
    cache_exists = !stat(cache,&st);
    if (!cache_exists)
        refresh = 1;
    if(cache_exists || called_as==mansec) /* mansec always need manpath */
    {
        f=popen( (called_as==manwhatis)? "manpath -cgq" : "manpath -gq" ,"r");
        if(!f)
        {
            error_page(500, "Can't read from pipe",
                       "Opening pipe for manpath command failed: %s", strerror(errno));
            exit(1);
        }
        fgets(manpath, sizeof(manpath), f);
        pclose(f);
        if(cache_exists)
        {
            time_t ct=st.st_mtime;
            int f_exists=0; /* if does exist at least one of the whatis db files */
            strncpy(s,manpath, sizeof(s));
            s[sizeof(s)-1] = '\0';
            p=strtok(s,":\n\r");
            while(p && !refresh)
            {
                if(called_as==manwhatis)
                {
                    char* sfx[] = { "db", "bt", "dir", "pag" }; /* possible mandb index cache suffixes */
                    size_t i;
                    for (i = 0; !refresh && i < sizeof(sfx)/sizeof(char*); i++)
                    {
                        snprintf(t, sizeof(t), "%s/index.%s",p, sfx[i]);
                        if(!stat(t,&st))
                        {
                            f_exists = 1;
                            if (st.st_mtime>ct)
                            {
                                refresh=1;
                            }
                        }
                    }
                }
                else
                {
                    int i;
                    for(i=0; i<10 && !refresh; i++)
                    {
                        if(section!=10 && i!=section)
                            continue;
                        snprintf(t, sizeof(t), "%s/man%d",p,i);
                        if(!stat(t,&st) && st.st_mtime>ct)
                            refresh=1;
                    }
                }
                p=strtok(NULL,":\n\r");
            }
            if(called_as==manwhatis && !f_exists)
                refresh = 1; /* force refresh if we didn't find any whatis db file */
        }
    }


    f=NULL;

    if(refresh)
    {
        snprintf(fn, sizeof(fn), "%s_%d", cache, getpid());
        set_privileges(1);
        f=fopen(fn,"w+");
        set_privileges(0);
        if (f)
        {
            make_index(f, section);
            set_privileges(1);
            if (rename(fn, cache))
                error_page(500, "Can't rename cache file",
                           "Can't rename cache file %s to %s: %s", fn, cache,
                           strerror(errno));
            set_privileges(0);
            rewind(f);
        }
    }

    if (!f && cache_exists)
    {
        set_privileges(1);
        f=fopen(cache,"r");
        if(!f)
        {
            error_page(500, "Can't open file for reading",
                       "Can't open file %s for reading: %s", cache, strerror(errno));
            exit(1);
        }
        set_privileges(0);
    }

    if (!f)
    {
        /* if we are here, than
         * - cache file does not exist
         * - we can't new create cache file
         * so the only thing we can do is just to
         * generate needed informations on the fly
         */
        printf(CONTENTTYPE);
        make_index(stdout, section);
        exit(0);
    }

    if (!fstat(fileno(f),&st))
    {
        tm=gmtime(&st.st_mtime);
        strftime(s, sizeof(s),"%a, %02d %b %Y %02H:%02M:%02S GMT",tm);
        printf("Last-modified: %s\n",s);
        printf("Content-length: %ld\n",st.st_size);
    }
    printf(CONTENTTYPE);
    buf=xmalloc(8192);
    do
    {
        n=fread(buf,1,8192,f);
        if(n)
            fwrite(buf,1,n,stdout);
    }
    while(n==8192);
    free(buf);
    fclose(f);
    return 0;
}
