/**
 * @file debconf-escape.c
 * @brief helper when working with debconf's escape capability
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

static int escape = 0;
static int unescape = 0;

static struct option options[] = {
    { "help", 0, 0, 'h' },
    { "escape", 0, 0, 'e' },
    { "unescape", 0, 0, 'u' },
    { 0, 0, 0, 0 },
};

static void help(FILE *f, const char *exename)
{
    fprintf(f, "Usage: %s -e|-u < input-text\n", exename);
    fputs("  -e, --escape      escape text\n"
        "  -u, --unescape    unescape text\n"
        "\n"
        "Exactly one of -e or -u must be used.\n", f);
}

static void parsecmdline(int argc, char **argv)
{
    int c;

    while ((c = getopt_long(argc, argv, "euh", options, NULL)) >= 0)
    {
        switch (c)
        {
            case 'h':
                help(stdout, argv[0]);
                exit(0);
                break;
            case 'e':
                escape = 1;
                break;
            case 'u':
                unescape = 1;
                break;
            default:
                break;
        }
    }

    if (optind > argc || escape == unescape)
    {
        help(stderr, argv[0]);
        exit(1);
    }
}

int main(int argc, char **argv)
{
    char buf[1024];
    const char *p;
    size_t r;

    parsecmdline(argc, argv);

    if (escape)
    {
        while (!feof(stdin) && !ferror(stdin))
        {
            r = fread(buf, sizeof(*buf), 1024, stdin);
            for (p = buf; p < buf + r; ++p)
            {
                switch (*p)
                {
                    case '\\':
                        fputs("\\\\", stdout);
                        break;
                    case '\n':
                        fputs("\\n", stdout);
                        break;
                    default:
                        fputc(*p, stdout);
                        break;
                }
            }
        }
    }
    else
    {
        int unesc = 0;
        while (!feof(stdin) && !ferror(stdin))
        {
            r = fread(buf, sizeof(*buf), 1024, stdin);
            for (p = buf; p < buf + r; ++p)
            {
                if (unesc)
                {
                    switch (*p)
                    {
                        case 'n':
                            fputc('\n', stdout);
                            break;
                        default:
                            fputc(*p, stdout);
                            break;
                    }
                    unesc = 0;
                }
                else if (*p == '\\')
                    unesc = 1;
                else
                    fputc(*p, stdout);
            }
        }
    }

    if (ferror(stdin))
    {
        fprintf(stderr, "%s: error reading stdin\n", argv[0]);
        return 1;
    }

    return 0;
}
