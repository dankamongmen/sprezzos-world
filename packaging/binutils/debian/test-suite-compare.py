#!/usr/bin/env python

# Quick'n'dirty regression check for dejagnu testsuites
# Copyright (C) 2003, 2004, 2005, 2006, 2007  James Troup <james@nocrew.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU;5B General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

################################################################################

import optparse
import os
import sys

################################################################################

def fubar(msg, exit_code=1):
    sys.stderr.write("E: %s\n" % (msg))
    sys.exit(exit_code)

def warn(msg):
    sys.stderr.write("W: %s\n" % (msg))

def info(msg):
    sys.stderr.write("I: %s\n" % (msg))

################################################################################

def read_testsummary(filename):
    results = {}
    file = open(filename)
    for line in file.readlines():
        if not line:
            continue
        if line.startswith("Running"):
            s = line.split()
            if "/" in s[1]:
                x = s[1]
                if x.find("/testsuite/") == -1:
                    fubar("Can't find /testsuite/ in '%s'." % (x))
                # 'Running /home/james/debian/packages/binutils/binutils-2.14.90.0.7/gas/testsuite/gas/hppa/unsorted/unsorted.exp ...' -> 'gas/hppa/unsorted/unsorted.exp'
                # ... since using basename() isn't dupe safe.
                section = x[x.find("/testsuite/"):].replace("/testsuite/","").split()[0]

                # Tests can be duplicated, e.g. hppa/basic/basic.exp
                # is run twice, once for hppa-linux and once for
                # hppa64-linux.  This is of course a horrible bodge,
                # but I can't think of anything trivial and better off
                # hand.

                if results.has_key(section):
                    extra = 1
                    too_many = 10
                    while results.has_key(section) and extra < too_many:
                        section = "%s.%s" % (section, extra)
                        extra += 1
                        if extra >= too_many:
                            fubar("gave up trying to unduplicate %s." % (section))

                results[section] = {}
                continue

        got_state = 0
        for state in [ "PASS", "XPASS", "FAIL", "XFAIL", "UNRESOLVED",
                       "UNTESTED", "UNSUPPORTED" ]:
            if line.startswith(state):
                s = line.split(':')
                state = s[0]
                test = ':'.join(s[1:]).strip()
                if results.has_key(test):
                    warn("%s/%s is duplicated." % (section, test))
                results[section][test] = state
                got_state = 1
                break

        if got_state:
            continue

    return results

################################################################################

def compare_results(old, new):
    total_num = 0
    pass_count = 0
    fail_count = 0
    xfail_count = 0
    untested_count = 0
    regression_count = 0
    progression_count = 0
    change_count = 0

    for section in new.keys():
        for test in new[section].keys():
            state = new[section][test]

            # Stats pr0n
            total_num += 1
            if state == "PASS" or state == "XPASS":
                pass_count += 1
            elif state == "FAIL" or state == "UNRESOLVED":
                fail_count += 1
            elif state == "XFAIL":
                xfail_count += 1
            elif state == "UNTESTED":
                untested_count += 1

            # Compare to old
            if not old.has_key(section):
                continue
            if not old[section].has_key(test):
                continue
            old_state = old[section][test]
            if state == "PASS":
                if old_state != "PASS":
                    progression_count += 1
                    info("[%s] progression (%s -> %s): %s" % (section, old_state, state, test))
            elif state == "XPASS":
                if old_state != "XPASS" and old_state != "PASS":
                    progression_count += 1
                    warn("[%s] %s: %s" % (section, state, test))
            elif state == "FAIL":
                if old_state != "FAIL":
                    regression_count += 1
                    warn("[%s] REGRESSION (%s -> %s): %s" % (section, old_state, state, test))
            elif state == "XFAIL":
                if old_state != "XFAIL":
                    change_count += 1
                    info("[%s] change (%s -> %s): %s" % (section, old_state, state, test))
            elif state == "UNRESOLVED":
                if old_state != "UNRESOLVED" and old_state != "FAIL":
                    regression_count += 1
                    warn("[%s] REGRESSION (%s -> %s): %s" % (section, old_state, state, test))
                if old_state == "FAIL":
                    change_count += 1
                    info("[%s] change (%s -> %s): %s" % (section, old_state, state, test))
            elif state == "UNTESTED":
                if old_state != "UNTESTED":
                    change_count += 1
                    warn("[%s] REGRESSION (%s -> %s): %s" % (section, old_state, state, test))

    if regression_count:
        print "%d REGRESSIONS (%.2f%%)." % (regression_count, (float(regression_count)/total_num)*100)
    if progression_count:
        print "%d progressions (%.2f%%)." % (progression_count, (float(progression_count)/total_num)*100)

    if change_count:
        print "%d changes (%.2f%%)." % (change_count, (float(change_count)/total_num)*100)

    print "%d tests: %d pass (%.2f%%), %d fail (%.2f%%), %d xfail (%.2f%%) %d untested (%.2f%%)." \
          % (total_num, pass_count, (float(pass_count)/total_num)*100,
             fail_count, (float(fail_count)/total_num)*100,
             xfail_count, (float(xfail_count)/total_num)*100,
             untested_count, (float(untested_count)/total_num)*100)

    if regression_count:
        sys.exit(1)

################################################################################

def compare_multiple(directory, first_version, second_version):
    architectures = [ "alpha", "arm", "hppa", "i386", "ia64", "mips",
                      "m68k", "mipsel", "powerpc", "s390", "sparc" ]

    for arch in architectures:
        print "*********************************** %s ******************************" % (arch)
        second_filename = "%s/%s_%s" % (directory, second_version, arch)
        if not os.path.exists(second_filename):
            print "   -- NOT AVAILABLE --"
            continue

        new = read_testsummary(second_filename)
        first_filename = "%s/%s_%s" % (directory, first_version, arch)
        old = read_testsummary(first_filename)
        compare_results(old, new)

################################################################################

def init():
    """Initalization, including parsing of options."""

    usage = """usage: %prog [OPTIONS] <OLD> <NEW>
compare (binutils) dejagnu testsuite results.

Example usage:

  test-suite-compare.py binutils-2.17/test-summary binutils-2.18/test-summary

Or to compare across all architectures (with test results stored in a
'test-summary' directory):

  test-suite-compare.py -mtest-summary 2.17-3 2.18-1"""
    parser = optparse.OptionParser(usage)
    parser.add_option("-m", "--multiple", dest="multiple",
                      nargs=1, type="string",
                      help="compare multiple architectures")
    (options, args) = parser.parse_args()

    if len(args) > 2 or len(args) < 2:
        parser.error("takes 2 arguments (old and new)")
    (old_version, new_version) = args

    return options, old_version, new_version
            
################################################################################

def main():
    (options, old_version, new_version) = init()
    if options.multiple:
        compare_multiple(options.multiple, old_version, new_version)
    else:
        old = read_testsummary(old_version)
        new = read_testsummary(new_version)
        compare_results(old, new)

################################################################################

if __name__ == '__main__':
    main()
