# Copyright (c) 2009-2011, Benjamin Drung <bdrung@debian.org>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Reference: https://developer.mozilla.org/en/Toolkit_version_format

import sys

def decode_part(part):
    """Decodes a version part (like 5pre4) to
       <number-a><string-b><number-c><string-d>"""
    subpart = [0, "", 0, ""]

    # Split <number-a>
    length = 0
    for i in xrange(len(part)):
        if part[i].isdigit() or part[i] in ("-"):
            length += 1
        else:
            break
    if length > 0:
        subpart[0] = int(part[0:length])
    part = part[length:]

    # Split <string-b>
    length = 0
    for i in xrange(len(part)):
        if not (part[i].isdigit() or part[i] in ("-")):
            length += 1
        else:
            break
    subpart[1] = part[0:length]
    part = part[length:]

    # Split <number-c>
    length = 0
    for i in xrange(len(part)):
        if part[i].isdigit() or part[i] in ("-"):
            length += 1
        else:
            break
    if length > 0:
        subpart[2] = int(part[0:length])
    subpart[3] = part[length:]

    # if string-b is a plus sign, number-a is incremented to be compatible with
    # the Firefox 1.0.x version format: 1.0+ is the same as 1.1pre
    if subpart[1] == "+":
        subpart[0] += 1
        subpart[1] = "pre"

    # if the version part is a single asterisk, it is interpreted as an
    # infinitely-large number: 1.5.0.* is the same as 1.5.0.(infinity)
    if subpart[1] == "*":
        subpart[0] = sys.maxint
        subpart[1] = ""

    return subpart

def decode_version(version, verbose=False):
    """Decodes a version string like 1.1pre1a"""
    parts = version.split(".")
    decoded_parts = map(decode_part, parts)
    if verbose:
        print "I: Split %s up into %s." % (version, decoded_parts)
    return decoded_parts

def compare_subpart((a, b)):
    # A string-part that exists is always less-then a nonexisting string-part
    if a == "":
        if b == "":
            return 0
        else:
            return 1
    elif b == "":
        if a == "":
            return 0
        else:
            return -1
    else:
        return cmp(a, b)

def compare_part((x, y)):
    compared_subparts = filter(lambda x: x != 0,
                               map(compare_subpart, zip(x, y)))
    if compared_subparts:
        return compared_subparts[0]
    else:
        return 0

def compare_versions(version1, version2, verbose=False):
    a = decode_version(version1, verbose)
    b = decode_version(version2, verbose)

    if len(a) < len(b):
        a.extend((len(b) - len(a)) * [[0, "", 0, ""]])
    if len(b) < len(a):
        b.extend((len(a) - len(b)) * [[0, "", 0, ""]])

    result = filter(lambda x: x != 0, map(compare_part, zip(a, b)))
    if result:
        return result[0]
    else:
        return 0

def extract_upstream_version(debian_version):
    # remove last part separated by a dash (1.0-2 -> 1.0)
    parts = debian_version.split('-')
    if len(parts) > 1:
        del parts[-1]
    upstream_version = '-'.join(parts)

    # remove epoch
    parts = upstream_version.split(':')
    if len(parts) > 1:
        del parts[0]
    upstream_version = ':'.join(parts)

    return upstream_version

def _convert_part_to_debian(part):
    """Converts a Mozilla version part (like 5pre4) to a Debian version."""
    (number_a, string_b, number_c, string_d) = part
    debian_version = ""
    if string_d != "":
        debian_version = "~" + string_d
    if number_c != 0 or string_d != "":
        debian_version = str(number_c) + debian_version
    if string_b != "":
        debian_version = "~" + string_b + debian_version
    debian_version = str(number_a) + debian_version
    return debian_version

def convert_debian_to_moz_version(debian_version, verbose=False):
    upstream_version = extract_upstream_version(debian_version)

    # compatibility: strip +nobinonly and +build
    parts = upstream_version.split('+')
    if len(parts) > 1 and parts[-1] == "nobinonly":
        del parts[-1]
    if len(parts) > 1 and parts[-1].startswith("build"):
        del parts[-1]
    upstream_version = '+'.join(parts)

    moz_version = upstream_version.replace("~", "")
    return moz_version

def convert_moz_to_debian_version(moz_version, epoch=0, verbose=False):
    parts = decode_version(moz_version, verbose)
    # tranform parts
    parts = [_convert_part_to_debian(p) for p in parts]
    debian_version = ".".join(parts)
    if epoch != 0:
        debian_version = str(epoch) + ":" + debian_version
    return debian_version

def moz_to_next_debian_version(moz_version, epoch=0, verbose=False):
    """Convert a given Mozilla version to the next Debian version.

    Compared to convert_moz_to_debian_version it does following:
    * append 0 to a trailing letter, or
    * append + to a trailing number, or
    * replace a trailing * with +.

    Examples:
    9.0a => 9.0~a0
    9.0a1 => 9.0~a1+
    9.0 => 9.0+
    9.0.* => 9.0.+
    """
    parts = decode_version(moz_version, verbose)
    # tranform last parts
    (number_a, string_b, number_c, string_d) = parts[-1]
    last_part = ""
    if string_d != "":
        last_part = "~" + string_d + "0"
    if number_c != 0 or string_d != "":
        if last_part:
            last_part = str(number_c) + last_part
        else:
            if number_c == sys.maxint:
                last_part = "+"
            else:
                last_part = str(number_c) + "+"
    if string_b != "":
        if last_part:
            last_part = "~" + string_b + last_part
        else:
            last_part = "~" + string_b + "0"
    if last_part:
        last_part = str(number_a) + last_part
    else:
        if number_a == sys.maxint:
            last_part = "+"
        else:
            last_part = str(number_a) + "+"

    parts = [_convert_part_to_debian(p) for p in parts[:-1]] + [last_part]
    debian_version = ".".join(parts)
    if epoch != 0:
        debian_version = str(epoch) + ":" + debian_version
    return debian_version
