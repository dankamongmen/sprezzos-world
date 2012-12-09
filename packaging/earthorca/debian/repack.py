#!/usr/bin/python

from optparse import OptionParser
import fnmatch
import tarfile
import StringIO
import re
import os
import sys
import rfc822
import urllib2
from urlparse import urlparse

class URLFile(object):
    '''Simple proxy to urllib2.urlopen, that responds to seek only if
       it's called before read. This is enough for tarfile to be happy'''

    def __init__(self, url):
        self.file = urllib2.urlopen(url)

    def seek(self, offset, whence = os.SEEK_SET):
        if whence != os.SEEK_SET or offset != 0 or self.read == self._read:
            raise "unsupported"

    def _read(self, size = -1):
        return self.file.read(size)

    def read(self, size = -1):
        self.read = self._read
        return self._read(size)

    def close(self):
        self.file.close()

def dirname(filespec):
    '''Returns os.path.dirname if a file, and '' if an url'''
    if urlparse(filespec).scheme:
        return ''
    return os.path.dirname(filespec)

class TarFilterList(object):
    def __init__(self, filename):
        self.patterns = {}
        for filt in open(filename).readlines():
            f = filt.strip().split(None, 1)
            if len(f) == 1:
                [pat] = f
                cmd = None
            else:
                [pat, cmd] = f

            pat = pat.split(os.sep)
            self.add_pattern(pat, self.patterns, cmd)

    def add_pattern(self, pat, patterns, cmd):
        if re.search(r'[\[\?\*]', pat[0]):
            if not '*' in patterns:
                patterns['*'] = []
            patterns['*'].append([os.sep.join(pat), cmd, False])
        else:
            if not pat[0] in patterns:
                patterns[pat[0]] = {}
            if len(pat) > 2:
                self.add_pattern(pat[1:], patterns[pat[0]], cmd)
            else:
                if not '*' in patterns[pat[0]]:
                    patterns[pat[0]]['*'] = []
                patterns[pat[0]]['*'].append([os.sep.join(pat[1:]), cmd, False])

    def match(self, name):
        name = name.split(os.sep)[1:]
        if len(name) == 0:
            return False
        return self._match(name, self.patterns)

    def _match(self, name, patterns):
        if len(name) > 1 and name[0] in patterns:
            cmd = self._match(name[1:], patterns[name[0]])
            if cmd != False:
                return cmd
        if '*' in patterns:
            for pat in patterns['*']:
                if fnmatch.fnmatch(name[0], pat[0]) or fnmatch.fnmatch(os.sep.join(name), pat[0]):
                    pat[2] = True
                    return pat[1]
        return False

    def unused(self, patterns=None, root=''):
        result = []
        if root:
            root += '/'
        if not patterns:
            patterns = self.patterns
        for pat in patterns:
            if pat != '*':
                result += self.unused(patterns[pat], root + pat)
            else:
                for p in patterns[pat]:
                    if not p[2]:
                        result.append(root + p[0])
        return result

def file_extension(name):
    return os.path.splitext(name)[1][1:]

def filter_tar(orig, new, filt, topdir = None):
    filt = TarFilterList(filt)
    if urlparse(orig).scheme:
        tar = tarfile.open(orig, "r:" + file_extension(orig), URLFile(orig))
    else:
        tar = tarfile.open(orig, "r:" + file_extension(orig))
    new_tar = tarfile.open(new + ".new", "w:" + file_extension(new))

    while True:
        info = tar.next()
        if not info:
            break
        if topdir:
            info.name = "/".join([topdir] + info.name.split("/")[1:])
        do_filt = filt.match(info.name)
        if do_filt == None:
            print >> sys.stderr, "Removing %s" % (info.name)
            continue

        if info.isfile():
            file = tar.extractfile(info)
            if do_filt:
                print >> sys.stderr, "Filtering %s" % (info.name)
                orig = file
                file = StringIO.StringIO()
                the_filt = lambda l: l
                if do_filt[0].isalpha():
                    f = do_filt.split(do_filt[1])
                    if f[0] == 's':
                        the_filt = lambda l: re.sub(f[1], f[2], l)
                else:
                    f = do_filt.split(do_filt[0])
                    if f[2] == 'd':
                        the_filt = lambda l: "" if re.search(f[1], l) else l
                file.writelines(map(the_filt, orig.readlines()))
                file.seek(0);
                info.size = len(file.buf)
            new_tar.addfile(info, file)
        else:
            new_tar.addfile(info)

    tar.close()
    new_tar.close()
    os.rename(new_tar.name, new)
    unused = filt.unused()
    if unused:
        print 'Unused filters:'
        print '', '\n '.join(unused)

def get_package_name():
    control = os.path.join(os.path.dirname(__file__), "control")
    return rfc822.Message(open(control))["Source"]

def main():
    parser = OptionParser()
    parser.add_option("-u", "--upstream-version", dest="upstream_version",
        help="define upstream version number to use when creating the file",
        metavar="VERSION")
    parser.add_option("-f", "--filter", dest="filter",
        help="use the given filter list", metavar="FILE")
    parser.add_option("-p", "--package", dest="package",
        help="use the given package name", metavar="NAME")
    parser.add_option("-o", "--output", dest="new_file",
        help="save the filtered tarball as the given file name", metavar="FILE")
    parser.add_option("-t", "--topdir", dest="topdir",
        help="replace the top directory with the given name", metavar="NAME")
    (options, args) = parser.parse_args()

    if not options.upstream_version and not options.new_file:
        parser.error("Need an upstream version")
        return

    if len(args) < 1:
        parser.error("Too few arguments")
        return
    if len(args) > 1:
        parser.error("Too many arguments")
        return

    if not options.filter:
        options.filter = os.path.join(os.path.dirname(__file__), "source.filter")
    if not options.package:
        options.package = get_package_name()

    if options.new_file:
        new_file = options.new_file

    if os.path.islink(args[0]):
        orig = os.path.realpath(args[0])
        if not new_file:
            new_file = args[0]
    else:
        orig = args[0]
        compression = file_extension(orig)
        if not new_file:
            new_file = options.package + "_" + options.upstream_version + ".orig.tar." + compression
            new_file = os.path.realpath(os.path.join(dirname(orig), new_file))
    print orig, new_file
    filter_tar(orig, new_file, options.filter, options.topdir)

if __name__ == '__main__':
    main()
