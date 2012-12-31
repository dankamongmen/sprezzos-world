/*
 * Copyright (C) 2009-2012 Dr. Christoph L. Spiel
 *
 * This file is part of Enblend.
 *
 * Enblend is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Enblend is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Enblend; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */


// No test, no bug.  -- Chris Spiel


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <getopt.h>
#include <stdlib.h>
#include <string.h>

#include <cassert>
#include <iostream>
#include <string>

#include "self_test.h"


#define lengthof(m_array) (sizeof(m_array) / sizeof(m_array[0]))


extern const std::string command;


////////////////////////////////////////////////////////////////////////


// Number of arguments we pass to getopt_long in each of our tests.
#define ARG_COUNT 4


enum {a_short, b_short, c_short, a_long, b_long, c_long, FLAG_COUNT};


struct test_case {
    const char* arguments[ARG_COUNT];
    int flags[FLAG_COUNT];
};


inline static int
int_of_string(const char* s)
{
    return static_cast<int>(strtol(s, NULL, 10));
}


static void
reset_getopt_globals()
{
    opterr = 0;                 // silence getopt_long(3)
    optopt = -1;                // reset "unknown option" character
    optind = 1;                 // reset parsing index
    optarg = NULL;              // reset pointer to value of option argument
}


static int
try_out_getopt_long(int arg_count, const char* arguments[], int* flags)
{
    const char* short_options = "ab:c::";
    const struct option long_options[] = {
        {"long-a", no_argument,       NULL, 1},
        {"long-b", required_argument, NULL, 2},
        {"long-c", optional_argument, NULL, 3},
        {NULL, 0, NULL, 0}
    };

    reset_getopt_globals();
    while (true)
    {
        int option_index = 0;
        int code = getopt_long(arg_count, const_cast<char* const*>(arguments),
                               short_options, long_options,
                               &option_index);

        if (code == -1)
        {
            break;
        }

        switch (code)
        {
        case 1:
            flags[a_long] = 1;
            break;
        case 2:
            flags[b_long] = int_of_string(optarg);
            break;
        case 3:
            flags[c_long] = optarg == NULL ? 1 : int_of_string(optarg);
            break;
        case 'a':
            flags[a_short] = 1;
            break;
        case 'b':
            flags[b_short] = int_of_string(optarg);
            break;
        case 'c':
            flags[c_short] = optarg == NULL ? 1 : int_of_string(optarg);
            break;
        default:
            return -1;
        }
    }

    return optind;
}


// Write a list of elements separated by spaces to stream out.
template <typename T>
static void
write_list(std::ostream& out, unsigned size, const T list)
{
    for (unsigned i = 0U; i != size; ++i)
    {
        out << list[i];
        if (i != size - 1U)
        {
            out << ' ';
        }
    }
}


// Name of the first argument, i.e. the first non-option in the list
// of arguments.  We need to know its name so that we can check
// whether getopt_long(3) really parsed all options.
#define ARG1 "1"


// Test whether the library function getopt_long(3) works as required.
bool
getopt_long_works_ok()
{
    bool has_passed_test = true;
    struct test_case tests[] = {
        {{"p", ARG1, "2", "3"},                    {0, 0, 0, 0, 0, 0}},

        {{"p", "-a", ARG1, "2"},                   {1, 0, 0, 0, 0, 0}},
        {{"p", "-b2", ARG1, "2"},                  {0, 2, 0, 0, 0, 0}},
        {{"p", "-c", ARG1, "2"},                   {0, 0, 1, 0, 0, 0}},
        {{"p", "-c2", ARG1, "2"},                  {0, 0, 2, 0, 0, 0}},

        {{"p", "--long-a", ARG1, "2"},             {0, 0, 0, 1, 0, 0}},
        {{"p", "--long-b=2", ARG1, "2"},           {0, 0, 0, 0, 2, 0}},
        {{"p", "--long-c", ARG1, "2"},             {0, 0, 0, 0, 0, 1}},
        {{"p", "--long-c=2", ARG1, "2"},           {0, 0, 0, 0, 0, 2}},

        {{"p", "-a", "-b2", ARG1},                 {1, 2, 0, 0, 0, 0}},
        {{"p", "-a", "-b2", ARG1},                 {1, 2, 0, 0, 0, 0}},
        {{"p", "-ab2", "-c", ARG1},                {1, 2, 1, 0, 0, 0}},
        {{"p", "-ab2", "-c3", ARG1},               {1, 2, 3, 0, 0, 0}},

        {{"p", "--long-a", "--long-b=2", ARG1},    {0, 0, 0, 1, 2, 0}},
        {{"p", "--long-a", "--long-b=2", ARG1},    {0, 0, 0, 1, 2, 0}},
        {{"p", "--long-b=2", "--long-c", ARG1},    {0, 0, 0, 0, 2, 1}},
        {{"p", "--long-b=2", "--long-c=3", ARG1},  {0, 0, 0, 0, 2, 3}},

        {{"p", "-a", "--long-a", ARG1},            {1, 0, 0, 1, 0, 0}},
        {{"p", "-b2", "--long-a", ARG1},           {0, 2, 0, 1, 0, 0}},
        {{"p", "-a", "--long-b=2", ARG1},          {1, 0, 0, 0, 2, 0}},
        {{"p", "-b2", "--long-b=2", ARG1},         {0, 2, 0, 0, 2, 0}},

        {{NULL, NULL, NULL}, {0, 0, 0, 0, 0, 0}}
    };
    const unsigned arg_count = lengthof(tests->arguments);
    const unsigned flag_count = lengthof(tests->flags);

    for (struct test_case* t = tests; t->arguments[0] != NULL; ++t)
    {
        int flags[] = {0, 0, 0, 0, 0, 0};
        assert(lengthof(tests->flags) == lengthof(flags));
        const int index = try_out_getopt_long(arg_count, t->arguments, flags);

        if (index < 0 || index >= static_cast<int>(arg_count) ||
            strcmp(t->arguments[index], ARG1) != 0)
        {
            std::cerr <<
                command <<
                ": failed self test: getopt_long(3) did not parse argument list \"";
            write_list(std::cerr, arg_count, t->arguments);
            std::cerr << "\"\n";

            has_passed_test = false;
        }

        for (unsigned i = 0U; i != flag_count; ++i)
        {
            if (flags[i] != t->flags[i])
            {
                std::cerr <<
                    command <<
                    ": failed self test: getopt_long(3) incorrectly parses argument list \"";
                write_list(std::cerr, arg_count, t->arguments);
                std::cerr << "\";\n";

                std::cerr <<
                    command <<
                    ": failed self test:     expected {";
                write_list(std::cerr, flag_count, t->flags);
                std::cerr << "}, but got {";
                write_list(std::cerr, flag_count, flags);
                std::cerr << "}\n";

                has_passed_test = false;
            }
        }
    }

    reset_getopt_globals();

    return has_passed_test;
}
