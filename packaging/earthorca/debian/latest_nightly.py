#!/usr/bin/env python

import sys
import ftputil
from operator import itemgetter

def main():
    url = sys.argv[1]
    if not url.startswith('ftp://'):
        print >>sys.stderr, 'Expecting an ftp:// url as argument'
    (scheme, _, host, path) = url.split('/', 3)
    host = ftputil.FTPHost(host, 'Anonymous', '')
    # Get the most recent .txt file in that ftp directory.
    file = sorted([(f, host.lstat(host.path.join(path, f)).st_mtime) for f in host.listdir(path) if f.endswith('.txt')], key=itemgetter(1), reverse=True)[0][0]
    with host.open(host.path.join(path, file)) as f:
        (date, url) = [l.strip() for l in f.readlines()]
    print date, url

if __name__ == '__main__':
    main()
