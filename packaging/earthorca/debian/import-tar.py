#!/usr/bin/python

import glob
import tarfile
import sys
import StringIO
from optparse import OptionParser
import os

class GitImportTar(object):
    def __init__(self, filename, head):
        self.mark = 1
        self.git = sys.stdout
        self.files = {}
        self.name = filename
        self.mtime = 0
        self.head = head

    def addfile(self, info, prefix = '', file = None):
        if info.isdir():
            return
        self.git.write("blob\n" +
                       "mark :%d\n" % self.mark)
        mode = info.mode
        if info.issym():
            self.git.write("data %d\n" % len(info.linkname) +
                           info.linkname)
            mode = 0120000
        elif file:
            file = StringIO.StringIO(file.read(info.size))
            self.git.write("data %d\n" % (info.size) +
                           file.getvalue())

        self.git.write("\n")
        if not prefix in self.files:
            self.files[prefix] = {}
        self.files[prefix][info.name] = (self.mark, mode)
        self.mark += 1
        if info.mtime > self.mtime:
            self.mtime = info.mtime

    def close(self):
        self.git.write("commit refs/heads/%s\n" % (self.head) +
                       "author T Ar Creator' <tar@example.com> %d +0000\n" % (self.mtime) +
                       "committer T Ar Creator' <tar@example.com> %d +0000\n" % (self.mtime) +
                       "data <<EOM\n" +
                       "Imported from %s\n" % (self.name) +
                       "EOM\n\n" +
                       "from refs/heads/%s^0\n" % (self.head) +
                       "deleteall\n")
        for prefix, fileset in self.files.iteritems():
            basedir = os.path.commonprefix(fileset)
            for path, info in fileset.iteritems():
                (mark, mode) = info
                if mode != 0120000:
                    mode = 0755 if (mode & 0111) else 0644
                path = path[len(basedir):]
                if prefix != '':
                    path = prefix + '/' + path
                self.git.write("M %o :%d %s\n" % (mode, mark, path))

def main():
    parser = OptionParser()
    parser.add_option("-H", "--head", dest="head",
        help="import on the given head", metavar="NAME")
    (options, args) = parser.parse_args()

    if not options.head:
        options.head = "upstream"

    (name, ext) = os.path.splitext(args[0])
    if ext[0:2] != '.t':
        (name, ext2) = os.path.splitext(name)
        ext = ext2 + ext

    git_import = GitImportTar(os.path.basename(args[0]), options.head)
    for file in [args[0]] + glob.glob(name + "-*" + ext):
        prefix = file[len(name)+1:-len(ext)]
        tar = tarfile.open(file, "r:*")

        while True:
            info = tar.next()
            if not info:
                break
            if info.isfile():
                file = tar.extractfile(info)
                git_import.addfile(info, prefix, file)
            else:
                git_import.addfile(info, prefix)

        tar.close()
    git_import.close()

if __name__ == '__main__':
    main()
