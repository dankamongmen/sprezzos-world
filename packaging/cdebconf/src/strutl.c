#include "common.h"
#include "strutl.h"
#include "question.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <wchar.h>

#ifdef HAVE_LIBTEXTWRAP
#include <textwrap.h>
#endif
#include <assert.h>

#include "debian-installer.h"

/*  Structure used to sort translated lists  */
typedef struct {
    char *string;
    int index;
} sort_str_t;

int strcountcmp(const char *s1, const char *e1, const char *s2, const char *e2)
{
	for (; s1 != e1 && s2 != e2 && *s1 == *s2; s1++, s2++) ;

	if (s1 == e1 && s2 == e2)
		return 0;

	if (s1 == e1)
		return 1;
	if (s2 == e2)
		return -1;
	if (*s1 < *s2)
		return -1;
	return 1;
}

char *strstrip(char *buf)
{
	char *end;

	/* skip initial spaces */
	for (; *buf != 0 && isspace(*buf); buf++) ;

	if (*buf == 0)
		return buf;

	end = buf + strlen(buf) - 1;
	while (end != buf - 1)
	{
		if (isspace(*end) == 0)
			break;

		*end = 0;
		end--;
	}
	return buf;
}

char *strlower(char *buf)
{
	char *p = buf;
	while (*p != 0) *p = tolower(*p);
	return buf;
}

void strvacat(char *buf, size_t maxlen, ...)
{
	va_list ap;
	char *str;
	size_t len = strlen(buf);

	va_start(ap, maxlen);
	
	while (1)
	{
		str = va_arg(ap, char *);
		if (str == NULL)
			break;
		if (len + strlen(str) > maxlen)
			break;
		strcat(buf, str);
		len += strlen(str);
	}
	va_end(ap);
}

int strparsecword(char **inbuf, char *outbuf, size_t maxlen)
{
	char buffer[maxlen];
	char *buf = buffer;
	char *c = *inbuf;
	char *start;

	for (; *c != 0 && isspace(*c); c++);

	if (*c == 0) 
		return 0;
	if (strlen(*inbuf) > maxlen)
		return 0;
	for (; *c != 0; c++)
	{
		if (*c == '"')
		{
			start = c+1;
			for (c++; *c != 0 && *c != '"'; c++)
			{
				if (*c == '\\')
				{
					c++;
					if (*c == 0)
						return 0;
				}
			}
			if (*c == 0)
				return 0;
			/* dequote the string */
			strunescape(start, buf, (int) (c - start + 1), STRESCAPE_QUOTE);
			buf += strlen(buf);
			continue;
		}
		
		if (c != *inbuf && isspace(*c) != 0 && isspace(c[-1]) != 0)
			continue;
		if (isspace(*c) == 0)
			return 0;
		*buf++ = ' ';
	}
	*buf = 0;
	strncpy(outbuf, buffer, maxlen);
	*inbuf = c;

	return 1;
}

int strparsequoteword(char **inbuf, char *outbuf, size_t maxlen)
{
	char *start = *inbuf;
	char *c;

	/* skip ws, return if empty */
	for (; *start != 0 && isspace(*start); start++); 
	if (*start == 0)
		return 0;

	c = start;
	
	for (; *c != 0 && isspace(*c) == 0; c++)
	{
		if (*c == '"')
		{
			for (c++; *c != 0 && *c != '"'; c++)
			{
				if (*c == '\\')
				{
					c++;
					if (*c == 0)
						return 0;
				}
			}
			if (*c == 0)
				return 0;
		}
		if (*c == '[')
		{
			for (c++; *c != 0 && *c != ']'; c++);
			if (*c == 0)
				return 0;
		}
	}

	/* dequote the string */
	strunescape(start, outbuf, (int) (c - start + 1), STRESCAPE_QUOTE);

	/* skip trailing spaces */
	for (; *c != 0 && isspace(*c); c++);
	*inbuf = c;

	return 1;
}

int strgetargc(const char *inbuf)
{
    int count = 1;
    const char *s;

    if (inbuf == 0 || *inbuf == 0)
        return 0;
    for (s=inbuf; *s != 0; s++)
    {
        if (*s == '\\' && *(s+1) == ',')
            s++;
        else if (*s == ',')
            count++;
    }
    return count;
}

int strchoicesplit(const char *inbuf, char **argv, size_t maxnarg)
{
    int argc = 0, i;
    const char *s = inbuf, *e, *c;
    char *p;

    if (inbuf == 0) return 0;

    INFO(INFO_VERBOSE, "Splitting [%s]", inbuf);
    while (*s != 0 && argc < maxnarg)
    {
        /* skip initial spaces */
        while (isspace(*s)) s++;

        /* find end */
        e = s;
        while (*e != 0)
        {
            if (*e == '\\' && (*(e+1) == ',' || *(e+1) == ' '))
                e += 2;
            else if (*e == ',')
                break;
            else
                e++;
        }

        argv[argc] = malloc(e-s+1);
        for (c = s, i = 0; c < e; c++, i++)
        {
            if (*c == '\\' && c < (e-1) && (*(c+1) == ',' || *(c+1) == ' '))
            {
                argv[argc][i] = *(c+1);
                c++;
            }
            else
                argv[argc][i] = *c;
        }
        argv[argc][i] = 0;
        p = &argv[argc][i-1];
        /* strip off trailing spaces */
        while (p > argv[argc] && *p == ' ') *p-- = 0;
        argc++;

        s = e;
        if (*s == ',') s++;
    }
    return argc;
}

int strchoicesplitsort(const char *origbuf, const char *transbuf, const char *indices, char **oargv, char **targv, int *oindex, size_t maxnarg)
{
    int i;
    char **cindex;
    char **sorted_targv;

    assert(oindex);
    assert(oargv);
    assert(targv);
    assert(origbuf);
    assert(transbuf);

    if (strchoicesplit(origbuf, oargv, maxnarg) != maxnarg)
        return DC_NOTOK;
    if (strchoicesplit(transbuf, targv, maxnarg) != maxnarg)
        return DC_NOTOK;
    if (indices == NULL || *indices == '\0') {
        for (i = 0; i < maxnarg; i++)
            oindex[i] = i;
    } else {
        cindex = malloc(sizeof(char *) * maxnarg);
        if (strchoicesplit(indices, cindex, maxnarg) != maxnarg) {
            INFO(INFO_WARN, "length of indices list '%s' != expected length %zd", indices, maxnarg);
            /* fall back semi-gracefully to unsorted list */
            for (i = 0; i < maxnarg; i++)
                oindex[i] = i;
            return maxnarg;
        }
        sorted_targv = malloc(sizeof(char *) * maxnarg);
        for (i = 0; i < maxnarg; i++) {
            oindex[i] = strtol(cindex[i], NULL, 10) - 1;
            if (oindex[i] < 0 || oindex[i] >= maxnarg) {
                int j;
                INFO(INFO_WARN, "index %d in indices list '%s' out of range", oindex[i] + 1, indices);
                /* fall back semi-gracefully to unsorted list */
                for (j = 0; j < maxnarg; j++)
                    oindex[j] = j;
                return maxnarg;
            }
            sorted_targv[i] = STRDUP(targv[oindex[i]]);
        }
        for (i = 0; i < maxnarg; i++) {
            free(targv[i]);
            targv[i] = sorted_targv[i];
        }
        free(sorted_targv);
        free(cindex);
    }
    return maxnarg;
}

int strcmdsplit(char *inbuf, char **argv, size_t maxnarg)
{
	int argc = 0;
	int inspace = 1;

	if (maxnarg == 0) return 0;
	
	for (; *inbuf != 0; inbuf++)
	{
		if (isspace(*inbuf) != 0)
		{
			inspace = 1;
			*inbuf = 0;
		}
		else if (inspace != 0)
		{
			inspace = 0;
			argv[argc++] = inbuf;
			if (argc >= maxnarg)
				break;
		}
	}

	return argc;
}

void strunescape(const char *inbuf, char *outbuf, const size_t maxlen, const int mode)
{
	const char *p = inbuf;
	int i = 0;
	while (*p != 0 && i < maxlen-1)
	{
		if (*p == '\\')
		{
			/*  Debconf only escapes \n by default */
			if (*(p+1) == 'n')
			{
				outbuf[i++] = '\n';
				p += 2;
			}
			/*  The configuration also escapes '"' */
			else if (mode == STRESCAPE_QUOTE && *(p+1) == '"')
			{
				outbuf[i++] = *(p+1);
				p += 2;
			}
			/*  The frontend unescapes everything */
			else if (mode == STRESCAPE_CAPB)
			{
				outbuf[i++] = *(p+1);
				p += 2;
			}
			else
				outbuf[i++] = *p++;
		}
		else
			outbuf[i++] = *p++;
	}
	outbuf[i] = 0;
}

void strescape(const char *inbuf, char *outbuf, const size_t maxlen, const int mode)
{
	const char *p = inbuf;
	int i = 0;
	while (*p != 0 && i < maxlen-1)
	{
		/*  Debconf only escapes \n by default */
		if (*p == '\n')
		{
			if (i + 2 >= maxlen) break;
			outbuf[i++] = '\\';
			outbuf[i++] = 'n';
			p++;
		}
		/* As well as '"' in configuration files */
		else if (mode == STRESCAPE_QUOTE && *p == '"')
		{
			if (i + 2 >= maxlen) break;
			outbuf[i++] = '\\';
			outbuf[i++] = *p++;
		}
		/* And '\' in the escape CAPB */
		else if (mode == STRESCAPE_CAPB && *p == '\\')
		{
			if (i + 2 >= maxlen) break;
			outbuf[i++] = '\\';
			outbuf[i++] = *p++;
		}
		else
			outbuf[i++] = *p++;
	}
	outbuf[i] = 0;
}

/* These versions allocate their own memory.
 * TODO: rename to something more self-explanatory
 */

char *unescapestr(const char *in)
{
	static size_t buflen = 0;
	static char *buf = NULL;
	size_t inlen;

	if (in == 0) return 0;

	inlen = strlen(in) + 1;
	/* This is slightly too much memory, because each escaped newline
	 * will be unescaped; but an upper bound is fine.
	 */
	if (buflen < inlen) {
		buflen = inlen;
		buf = realloc(buf, buflen * sizeof *buf);
		if (!buf)
			DIE("Out of memory");
	}

	strunescape(in, buf, buflen, STRESCAPE_822);
	return buf;
}

char *escapestr(const char *in)
{
	static size_t buflen = 0;
	static char *buf = NULL;
	size_t inlen;
	const char *p;

	if (in == 0) return 0;

	inlen = strlen(in) + 1;

	/* Each newline will consume an additional byte due to escaping. */
	for (p = in; *p; ++p)
		if (*p == '\n')
			++inlen;

	if (buflen < inlen) {
		buflen = inlen;
		buf = realloc(buf, buflen * sizeof *buf);
		if (!buf)
			DIE("Out of memory");
	}

	strescape(in, buf, buflen, STRESCAPE_822);
	return buf;
}

int strwrap(const char *str, const int width, char *lines[], int maxlines)
{
#ifdef HAVE_LIBTEXTWRAP
	textwrap_t p;
	int j;
	char *s;
	char *s0;
	char *t;

	textwrap_init(&p);
	textwrap_columns(&p, width);
	s0 = s = textwrap(&p, str);
	for (j=0; j<maxlines; j++)
	{
		t = strchr(s, '\n');
		if (t == NULL)
		{
			lines[j] = (char *)malloc(strlen(s) + 1);
			strcpy(lines[j], s);
			free(s0);
			return j + 1;
		}
		else
		{
			lines[j] = (char *)malloc(t - s + 1);
			strncpy(lines[j], s, t-s); lines[j][t-s] = 0;
			s = t + 1;
		}
	}
	return j;
#else
	/* "Simple" greedy line-wrapper */
	int len = STRLEN(str);
	int l = 0;
	const char *s, *e, *end, *lb;

	if (str == 0) return 0;

	/*
	 * s = first character of current line, 
	 * e = last character of current line
	 * end = last character of the input string (the trailing \0)
	 */
	s = e = str;
	end = str + len;
	
	while (len > 0)
	{
		/* try to fit the most characters */
		e = s + width - 1;
		
		/* simple case: we got everything */
		if (e >= end) 
		{
			e = end;
		}
		else
		{
			/* find a breaking point */
			while (e > s && !isspace(*e) && *e != '.' && *e != ',')
				e--;
		}
		/* no word-break point found, so just unconditionally break 
		 * the line */
		if (e == s) e = s + width;

		/* if there's an explicit linebreak, honor it */
		lb = strchr(s, '\n');
		if (lb != NULL && lb < e) e = lb;

		lines[l] = (char *)malloc(e-s+2);
		strncpy(lines[l], s, e-s+1);
		lines[l][e-s+1] = 0;
		CHOMP(lines[l]);

		len -= (e-s+1);
		s = e + 1;
		if (++l >= maxlines) break;
	}
	return l;
#endif /* HAVE_LIBTEXTWRAP */
}

int strlongest(char **strs, int count)
{
	int i, max = 0;
	size_t width;

	for (i = 0; i < count; i++)
	{
		width = strwidth(strs[i]);
		if (width > max)
			max = width;
	}
	return max;
}

size_t
strwidth (const char *what)
{
    size_t res;
    int k;
    const char *p;
    wchar_t c;

    for (res = 0, p = what ; (k = mbtowc (&c, p, MB_LEN_MAX)) > 0 ; p += k)
        res += wcwidth (c);

    return res;
}  

/*
 * Add spaces at the end of string so that strwidth(that) == maxsize
 * Input string must have been allocated with enough space.
 */
int
strpad (char *what, size_t maxsize)
{
    size_t pos;
    int k;
    char *p;
    wchar_t c;

    pos = 0;
    for (p = what; (k = mbtowc (&c, p, MB_LEN_MAX)) > 0; p += k)
        pos += wcwidth (c);
    if (pos > maxsize)
        return 0;
    for (k = pos; k < maxsize; k++, p++)
        *p = ' ';
    *p = '\0';
    return 1;
}

int
strtruncate (char *what, size_t maxsize)
{
    size_t pos;
    int k;
    char *p;
    wchar_t c;

    if (strwidth(what) <= maxsize)
        return 0;

    /*  Replace end of string with ellipsis "..."; as the last character
     *  may have a double width, stops 4 characters before the end
     */
    pos = 0;
    for (p = what; (k = mbtowc (&c, p, MB_LEN_MAX)) > 0 && pos < maxsize-5; p += k)
        pos += wcwidth (c);

    for (k=0; k < 3; k++, p++)
        *p = '.';
    *p = '\0';
    return 1;
}

int stralign(char **strs, int count)
{
    unsigned int i;
    unsigned int j;
    unsigned int * cells_per_line;
    unsigned int num_columns;
    unsigned int * column_widths;
    unsigned int * remaining_line_widths;
    size_t * column_sizes;
    size_t * remaining_line_sizes;
    char * next_cell;
    char * cell;
    unsigned int max_width;
    unsigned int line_width;
    size_t max_size;
    size_t line_size;
    unsigned int column_width;
    unsigned int cell_width;
    unsigned int cell_len;
    unsigned int left_pad;
    char * new_str;

    cells_per_line = malloc(sizeof (unsigned int) * count);
    memset(cells_per_line, 0, sizeof (unsigned int) * count);
    remaining_line_widths = malloc(sizeof (unsigned int) * count);
    remaining_line_sizes = malloc(sizeof (size_t) * count);

    num_columns = 0;
    column_widths = NULL;
    column_sizes = NULL;
    for (i = 0; i < count; i++) {
        next_cell = strs[i];
        for (j = 0; NULL != next_cell; j++) {
            cells_per_line[i] = j + 1;
            if (num_columns < cells_per_line[i]) {
                num_columns = j + 1;
                column_widths = realloc(column_widths,
                                        sizeof (unsigned int) * num_columns);
                column_widths[j] = 0;
                column_sizes = realloc(column_sizes,
                                        sizeof (size_t) * num_columns);
                column_sizes[j] = 0;
            }
            cell = strsep(&next_cell, STRALIGN_TAB);
            if (0 == strncmp(STRALIGN_ALIGN_CENTER, cell, 1) ||
                0 == strncmp(STRALIGN_ALIGN_RIGHT, cell, 1)) {
                cell++;
            }
            if (NULL != next_cell)
                column_widths[j] = MAX(column_widths[j], strwidth(cell));
            else
                remaining_line_widths[i] = strwidth(cell);
        }
    }
    for (i = 0; i < count; i++) {
        cell = strs[i];
        for (j = 0; j < cells_per_line[i]; j++) {
            cell_width = strwidth(cell);
            cell_len = strlen(cell);
            if (j < cells_per_line[i] - 1)
                /* one byte padding per character from maximum column width */
                column_sizes[j] = MAX(column_sizes[j],
                                      cell_len +
                                      column_widths[j] - cell_width);
            else
                remaining_line_sizes[i] = cell_len;
            cell = cell + cell_len + 1;
        }
    }
    max_width = 0;
    for (i = 0; i < count; i++) {
        line_width = remaining_line_widths[i];
        for (j = 0; j < cells_per_line[i] - 1; j++) {
            line_width += column_widths[j] + 2 /* extra spaces */;
        }
        max_width = MAX(max_width, line_width);
    }
    max_size = 0;
    for (i = 0; i < count; i++) {
        line_size = remaining_line_sizes[i];
        for (j = 0; j < cells_per_line[i] - 1; j++) {
            line_size += column_sizes[j] + 2 /* extra spaces */;
        }
        max_size = MAX(max_size, line_size);
    }
    free(column_sizes);
    for (i = 0; i < count; i++) {
        new_str = malloc(sizeof (char) * max_size + 1);
        next_cell = new_str;
        *next_cell = '\0';
        cell = strs[i];
        for (j = 0; j < cells_per_line[i]; j++) {
            column_width = j < cells_per_line[i] - 1
                           ? column_widths[j]
                           : max_width - strwidth(new_str);
            if (0 == strncmp(STRALIGN_ALIGN_CENTER, cell, 1)) {
                cell++;
                cell_width = strwidth(cell);
                left_pad = (column_width - cell_width) / 2;
            } else if (0 == strncmp(STRALIGN_ALIGN_RIGHT, cell, 1)) {
                cell++;
                cell_width = strwidth(cell);
                left_pad = column_width - cell_width;
            } else {
                left_pad = 0;
            }
            strpad(next_cell, left_pad);
            strcat(next_cell, cell);
            if (j < cells_per_line[i] - 1) {
                strpad(next_cell, column_width);
                next_cell += strlen(next_cell);
                strcpy(next_cell, "  " /* extra space */);
                next_cell += 2;
                cell += strlen(cell) + 1;
            }
        }
        free(strs[i]);
        strs[i] = new_str;
    }

    free(column_widths);
    free(cells_per_line);

    return 0;
}

#define INITIAL_ROPE_SIZE 128
#define VAR_SIZE 100

struct rope {
    const char * str;
    size_t len;
};

char * strexpand(const char * src, lookup_function func, void * user_data)
{
    struct rope * rope;
    void * tmp;
    size_t rope_size = INITIAL_ROPE_SIZE;
    unsigned int rope_index = 0;
    int i;
    int j;
    char var[VAR_SIZE];
    size_t dest_size = 1;
    char * dest = NULL;;
    char * buf;

    if (NULL == src) {
        return NULL;
    }

    if (NULL == (rope = malloc(sizeof (struct rope) * rope_size))) {
        return NULL;
    }

    /* 1. create the rope */
    rope[rope_index].str = src;
    rope[rope_index].len = 0;
    for (i = 0; '\0' != src[i]; i++) {
        if ('$' != src[i] || '{' != src[i + 1]) {
            rope[rope_index].len++;
            continue;
        }
        if (rope_index >= rope_size - 2 /* room for skips */) {
            rope_size = rope_size * 2;
            tmp = realloc(rope, sizeof (struct rope) * rope_size);
            if (NULL == tmp) {
                goto end;
            }
            rope = tmp;
        }
        i += 2; /* skip "${" */
        for (j = 0; j < VAR_SIZE && '\0' != src[i] && '}' != src[i]; j++) {
            var[j] = src[i++];
        }
        if ('\0' == src[i]) {
            rope[rope_index].len = strlen(rope[rope_index].str);
            break;
        }
        var[j] = '\0';
        dest_size += rope[rope_index++].len;
        rope[rope_index].str = func(var, user_data);
        /* Handle skips */
        if (NULL == rope[rope_index].str) {
            rope[rope_index].str = &src[i - j - 2 /* "${" */];
            rope[rope_index].len = j + 3; /* "${}" */
        } else {
            rope[rope_index].len = strlen(rope[rope_index].str);
        }
        dest_size += rope[rope_index++].len;
        /* skip '}' */
        rope[rope_index].str = &src[i + 1];
        rope[rope_index].len = 0;
    }
    dest_size += rope[rope_index].len;

    /* 2. dump rope content */
    if (NULL == (dest = malloc(dest_size))) {
        goto end;
    }
    buf = dest;
    for (i = 0; i <= rope_index; i++) {
        strncpy(buf, rope[i].str, rope[i].len);
        buf += rope[i].len;
    }
    *buf = '\0';

end:
    free(rope);
    return dest;
}

char *strjoinv(const char *sep, va_list ap)
{
    size_t sep_len = strlen(sep);
    char *str;
    size_t str_size = 0, str_alloc = 128;
    const char *arg;

    str = di_malloc(str_alloc);

#define APPEND(piece, len) do { \
    if (str_size + (len) + 1 > str_alloc) { \
        str_alloc = (str_size + (len) + 1) * 2; \
        str = di_realloc(str, str_alloc); \
    } \
    strncpy(str + str_size, (piece), (len)); \
    str_size += (len); \
} while (0)

    arg = va_arg(ap, const char *);
    while (arg) {
        size_t arg_len = strlen(arg);
        if (str_size)
            APPEND(sep, sep_len);
        APPEND(arg, arg_len);
        arg = va_arg(ap, const char *);
    }

#undef APPEND

    str[str_size] = '\0';
    return str;
}

char *strjoin(const char *sep, ...)
{
    va_list ap;
    char *str;

    va_start(ap, sep);
    str = strjoinv(sep, ap);
    va_end(ap);
    return str;
}

char *strreplace(const char *src, const char *from, const char *to)
{
    char *str;
    size_t str_size = 0, str_alloc = 128;
    char *p;
    const char *prev;
    size_t from_len = strlen(from), to_len = strlen(to);

    str = di_malloc(str_alloc);

#define APPEND(piece, len) do { \
    if (str_size + (len) + 1 > str_alloc) { \
        str_alloc = (str_size + (len) + 1) * 2; \
        str = di_realloc(str, str_alloc); \
    } \
    strncpy(str + str_size, (piece), (len)); \
    str_size += (len); \
} while (0)

    prev = src;
    for (p = strstr(prev, from); p; p = strstr(prev, from)) {
        if (p > prev)
            APPEND(prev, p - prev);
        APPEND(to, to_len);
        prev = p + from_len;
    }
    if (*prev)
        APPEND(prev, strlen(prev));

#undef APPEND

    str[str_size] = '\0';
    return str;
}
