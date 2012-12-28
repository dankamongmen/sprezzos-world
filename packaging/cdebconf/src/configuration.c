#include "common.h"
#include "configuration.h"
#include "strutl.h"

#include <stdio.h>
#include <ctype.h>

#define DELIMITER	"::"

/* private functions */

/**
 * @brief helper function for lookups
 * @param struct configitem *head - where to start looking
 * @param const char *tag - tag to look for
 * @param unsigned long len - length of tag
 * @param int create - create a new node if it doesn't exist?
 * @return struct configitem * - node of the item being sought, NULL 
 *         if not found
 */
static struct configitem *config_lookuphlp(struct configitem *head, 
	const char *tag, unsigned long len, int create)
{
	int res = 1;
	struct configitem *i = head->child;
	struct configitem **last = &head->child;
	struct configitem *newitem;

	/* empty strings match nothing; they are used for lists */
	if (len != 0)
	{
		for (; i != 0; last = &i->next, i = i->next)
			if ((res = strcountcmp(i->tag, i->tag+strlen(i->tag), tag, tag + len)) == 0)
				break;
	}
	else
		for (; i != 0; last = &i->next, i = i->next);

	if (res == 0)
		return i;
	if (create == 0)
		return NULL;

	newitem = NEW(struct configitem);
	memset(newitem, 0, sizeof(struct configitem));
	newitem->tag = malloc(len+1);
	newitem->tag[len] = 0;
	memcpy(newitem->tag, tag, len);
	newitem->value = 0;
	newitem->next = *last;
	newitem->parent = head;
	newitem->child = 0;
	*last = newitem;
	return newitem;
}

/*
 * @brief lookup a given configuration item
 * @param struct configuration *config - config object to look inside
 * @param const char *tag - tag of item to look for
 * @param int create - create if doesn't exist?
 * @return struct configitem * - item being sough, NULL if doesn't exist
 */
static struct configitem *config_lookup(struct configuration *config, 
	const char *tag, int create)
{
	const char *start, *end, *tagend;
	struct configitem *item = config->root;

	if (tag == 0)
		return config->root->child;

	start = tag;
	end = start + strlen(tag);
	tagend = tag;

	for (; end - tagend >= 2; tagend++)
	{
		if (tagend[0] == ':' && tagend[1] == ':')
		{
			item = config_lookuphlp(item, start, tagend-start, create);
			if (item == 0)
				return NULL;
			tagend = start = tagend + 2;
		}
	}

	/* trailing :: */
	if (end - start == 0)
		if (create == 0) return NULL;
	
	item = config_lookuphlp(item, start, end-start, create);
	return item;
}

/**
 * @brief recursive builds the complete tag name, seperated by :: 
 * @param const struct configitem *top - head of config tree
 * @param const struct configitem *stop - when to stop
 * @param char *tag - buffer to build the tag
 * @param const size_t maxlen - length of buffer
 * @return none
 */
void config_fulltag(const struct configitem *top, 
	const struct configitem *stop, char *tag, const size_t maxlen)
{
	char buf[maxlen];
	buf[0] = 0;

	if (top->parent == 0 || top->parent->parent == 0 ||
		top->parent == stop)
	{
		strncpy(tag, top->tag, maxlen);
		return;
	}
	config_fulltag(top->parent, stop, buf, sizeof(buf));
	strvacat(tag, maxlen, buf, DELIMITER, top->tag, (char *) 0);
}

/* public functions */
static const char *config_get(struct configuration *cfg, const char *tag, 
		const char *defaultvalue)
{
	struct configitem *item = config_lookup(cfg, tag, 0);
	if (item == 0) 
		return defaultvalue;
	else
		return item->value;
}

static int config_geti(struct configuration *cfg, const char *tag, 
		int defaultvalue)
{
	int res;
	char *end;
	struct configitem *item = config_lookup(cfg, tag, 0);
	if (item == 0) 
		return defaultvalue;

	res = strtol(item->value, &end, 0);
	if (end == item->value)
		return defaultvalue;
	else
		return res;
}

static void config_set(struct configuration *cfg, const char *tag,
		const char *value)
{
	struct configitem *item = config_lookup(cfg, tag, 1);
	if (item == 0) return;
	DELETE(item->value);
	item->value = STRDUP(value);
}

static void config_seti(struct configuration *cfg, const char *tag,
		int value)
{
	char s[50];
	snprintf(s, sizeof(s), "%i", value);
	config_set(cfg, tag, s);
}

static int config_exists(struct configuration *cfg, const char *tag)
{
	struct configitem *item = config_lookup(cfg, tag, 0);
	return (item != 0);
}

static int config_read(struct configuration *cfg, const char *filename)
{
	FILE *infp;
	char parenttag[256];
	char tag[256], value[4096], item[4096];
	char buffer[4096];
	char *buf;
	char linebuf[8192];
	char *stack[100];
	char *s, *q, *start, *stop;
	char termchar;
	unsigned int stackpos = 0;
	int curline = 0, i;
	int incomment = 0, inquote = 0;
	int ret = DC_OK;

	linebuf[0] = 0;
	parenttag[0] = 0;
	for (i = 0; i < DIM(stack); i++)
		stack[i] = NULL;

	if ((infp = fopen(filename, "r")) == NULL)
		return DC_NOTOK;

	while (fgets(buffer, sizeof(buffer), infp))
	{
		curline++;
		buf = strstrip(buffer);

		/* multiline comments -- check for end-of-comment */
		if (incomment != 0)
		{
			for (s = buf; *s != 0; s++)
			{
				if (s[0] == '*' && s[1] == '/')
				{
					memmove(buf, s+2, strlen(s+2)+1);
					incomment = 0;
					break;
				}
			}
			if (incomment != 0)
				continue;
		}

		/* discard single line comments */
		inquote = 0;
		for (s = buf; *s != 0; s++)
		{
			if (*s == '\\')
			{
				s++;
				if (*s == 0)
					break;
				continue;
			}
			if (*s == '"')
				inquote = !inquote;
			if (inquote != 0)
				continue;

			if (s[0] == '/' && s[1] == '/')
			{
				*s = 0;
				break;
			}
		}

		/* look for multiline comments */
		inquote = 0;
		for (s = buf; *s != 0; s++)
		{
			if (*s == '\\')
			{
				s++;
				if (*s == 0)
					break;
				continue;
			}
			if (*s == '"')
				inquote = !inquote;
			if (inquote != 0)
				continue;
			if (s[0] == '/' && s[1] == '*')
			{
				incomment = 1;
				for (q = buf; *q != 0; q++)
				{
					if (q[0] == '*' && q[1] == '/')
					{
						memmove(s, q+2, strlen(q+2)+1);
						incomment = 0;
						break;
					}
				}

				if (incomment != 0)
				{
					*s = 0;
					break;
				}
			}
		}

		/* skip blank lines */
		if (buf[0] == 0) 
			continue;
		
		/* valid line */
		buf = strstrip(buf);
		inquote = 0;
		for (s = buf; *s != 0;)
		{
			if (*s == '\\')
			{
				if (*(s+1) == 0)
					break;
				s += 2;
				continue;
			}
			if (*s == '"')
				inquote = !inquote;

			if (inquote == 0 && (*s == '{' || *s == ';' || *s == '}'))
			{
				/* add the last fragment to the buffer */
				start = buf;
				stop = s;
				for (; start != s && isspace(*start) != 0; start++);
				for (; stop != start && isspace(stop[-1]) != 0; stop--);
				if (linebuf[0] != 0 && stop - start != 0)
					strcat(linebuf, " ");
				strncat(linebuf, start, stop-start);

				/* remove the fragment */
				termchar = *s;
				memmove(buf, s+1, strlen(s+1)+1);
				s = buf;

				/* syntax error */
				if (termchar == '{' && linebuf[0] == 0)
				{
					INFO(INFO_ERROR, "Syntax error %s:%u: block starts with no name", filename, curline);
					ret = DC_NOTOK;
					goto out;
				}

				if (linebuf[0] == 0)
				{
					if (termchar == '}')
					{
						if (stackpos == 0)
							parenttag[0] = 0;
						else
							snprintf(parenttag, sizeof(parenttag), "%s", stack[--stackpos]);
					}
					continue;
				}

				/* parse off the tag */
				q = (char *)linebuf;
				tag[0] = 0;
				if (strparsecword(&q, tag, sizeof(tag)) == 0
				    && strparsequoteword(&q, tag, sizeof(tag)) == 0)
				{
					INFO(INFO_ERROR, "Syntax error %s:%u: Malformed tag", filename, curline);
					ret = DC_NOTOK;
					goto out;
				}

				/* parse off the value */
				value[0] = 0;
				if (strparsecword(&q, value, sizeof(value)) == 0
				    && strparsequoteword(&q, value, sizeof(value)) == 0)
				{
					if (termchar != '{')
					{
						strncpy(value, tag, sizeof(value));
						tag[0] = 0;
					}
				}

				/* extra junk */
				if (strlen(q) != 0)
				{
					INFO(INFO_ERROR, "Syntax error %s:%u: Extra junk after tag", filename, curline);
					ret = DC_NOTOK;
					goto out;
				}

				if (termchar == '{')
				{
                                        /* 99, not 100.  100 gives possible
                                           off-by-one error */
					if (stackpos <= 99) {
						DELETE(stack[stackpos]);
						stack[stackpos++] = strdup(parenttag);
					}
					if (value[0] != 0)
					{
						strvacat(tag, sizeof(tag),
							DELIMITER, value, (char *) 0);
						value[0] = 0;
					}

					if (parenttag[0] == 0)
						strncpy(parenttag, tag, 
							sizeof(parenttag));
					else
						strvacat(parenttag,
							sizeof(parenttag), 
							DELIMITER, tag, (char *) 0);
					tag[0] = 0;
				}

				/* generate the item name */
				if (parenttag[0] == 0)
					strncpy(item, tag, sizeof(item));
				else
				{
					if (termchar != '{' || tag[0] != 0)
					{
						item[0] = 0;
						strvacat(item, sizeof(item),
							parenttag, DELIMITER,
							tag, (char *) 0);
					}
					else
					{
						strncpy(item, parenttag, 
							sizeof(item));
					}
				}
				
				/* insert the data item into the tree */
				config_set(cfg, item, value);

				linebuf[0] = 0;

				if (termchar == '}')
				{
					if (stackpos == 0)
						parenttag[0] = 0;
					else
					{
						strncpy(parenttag,
							stack[--stackpos],
							sizeof(parenttag));
						DELETE(stack[stackpos]);
					}
				}
			}
			else
				s++;
		}
		
		/* store the line fragment */
		strstrip(buf);
		if (buf[0] != 0 && linebuf[0] != 0)
			strvacat(linebuf, sizeof(linebuf), " ", (char *) 0);
		strcat(linebuf, buf);
	}
out:
	fclose(infp);
	for (i = 0; i < DIM(stack); i++)
		DELETE(stack[i]);
	return ret;
}

static void config_dump(struct configuration *cfg)
{
	const struct configitem *top = cfg->tree(cfg, 0);
	char buf[512];

	while (top != 0)
	{
		buf[0] = 0;
		config_fulltag(top, 0, buf, sizeof(buf));
		printf("%s \"%s\"\n", buf, top->value);

		if (top->child != 0)
		{
			top = top->child;
			continue;
		}
		while (top != 0 && top->next == 0)
			top = top->parent;
		if (top != 0)
			top = top->next;
	}
}

static struct configitem *config_tree(struct configuration *cfg, const char *tag)
{
	return config_lookup(cfg, tag, 0);
}

struct configuration *config_new(void)
{
	struct configuration *config = NEW(struct configuration);
	memset(config, 0, sizeof(struct configuration));
	config->root = NEW(struct configitem);
	memset(config->root, 0, sizeof(struct configitem));
	config->get = config_get;
	config->geti = config_geti;
	config->set = config_set;
	config->seti = config_seti;
	config->exists = config_exists;
	config->read = config_read;
	config->dump = config_dump;
	config->tree = config_tree;

	return config;
}

void config_delete(struct configuration *config)
{
	struct configitem *next;
	struct configitem *top = config->root;

	while (top != 0)
	{
		if (top->child != 0)
		{
			top = top->child;
			continue;
		}

		while (top != 0 && top->next == 0)
		{
			next = top->parent;
			DELETE(top->tag);
			DELETE(top->value);
			DELETE(top);
			top = next;
		}

		if (top != 0)
		{
			next = top->next;
			DELETE(top->tag);
			DELETE(top->value);
			DELETE(top);
			top = next;
		}
	}

	DELETE(config);
}
