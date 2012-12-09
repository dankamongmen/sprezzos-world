from optparse import OptionParser
import os
import re
import zipfile

def split_jar_path(path):
    if not path.startswith('jar:'):
        raise "Not a jar"
    (jar, path) = path.split('!', 1)
    jar = jar[4:]
    if path.startswith('/'):
        path = path[1:]
    return (jar, path)

def files_from_jar(jar, path):
    zip = zipfile.ZipFile(jar, 'r')
    if not path.endswith('/'):
        path += '/'
    return ['jar:' + jar + '!/' + file for file in zip.namelist() if file.startswith(path)]

def files_from_dir(basedir, relpath):
    if relpath.startswith('jar:'):
        (jar, path) = split_jar_path(relpath)
        return files_from_jar(os.path.join(basedir, jar), path)

    files = []
    for root, dirnames, filenames in os.walk(os.path.join(basedir, relpath)):
        for file in filenames:
            files.append(os.path.join(root, file))
    return files

def files_from_manifest(manifest):
    files = []
    basedir = os.path.dirname(manifest)
    for line in open(manifest).readlines():
        fields = line.split()
        if not fields:
            continue
        if fields[0] == 'manifest':
            files += files_from_manifest(os.path.join(basedir, fields[1]))
        if fields[0] == 'content':
            files += files_from_dir(basedir, fields[2])
        if fields[0] == 'component':
            files.append(os.path.join(basedir, fields[2]))
    return files

def files(dir):
    manifest = os.path.join(dir, 'chrome.manifest')
    return files_from_manifest(manifest) + files_from_dir(dir, 'modules')

def content(path):
    try:
        if path.startswith('jar:'):
            (jar, path) = split_jar_path(path)
            zip = zipfile.ZipFile(jar, 'r')
            file = zip.open(path, 'r')
        else:
            file = open(path, 'r')
        return file.readlines()
    except:
        return []

def resolve(uri, appdir, xredir):
    (_, _, where, path) = uri.split('/', 3)
    if not where or where == 'app':
        return os.path.join(appdir, path)
    elif where == 'gre':
        return os.path.join(xredir, path)
    else:
        return None

def main():
    parser = OptionParser()
    parser.add_option('-x', '--xre-dir', dest='xredir',
    help='XRE directory', metavar='DIR')
    parser.add_option('-a', '--app-dir', dest='appdir',
    help='Application directory', metavar='DIR')
    parser.add_option('-o', '--overrides', dest='overrides',
    help='Overrides', metavar='FILE')
    (options, args) = parser.parse_args()

    if not options.xredir or not options.appdir:
        parser.error('Both XRE and Application directory required')
        return

    if not options.overrides:
        options.overrides = os.path.splitext(__file__)[0] + '.overrides'

    try:
        overrides = dict([(l.strip(),0) for l in open(options.overrides, 'r').readlines()])
    except:
        overrides = {}
    allfiles = files(options.appdir) + files(options.xredir)

    r = re.compile(r'''resource://(?:gre|app|)/[^\s'"]*''')
    errors = False
    for file in allfiles:
        for line in content(file):
            for match in r.findall(line):
                path = resolve(match, options.appdir, options.xredir)
                if not path in allfiles and not os.path.exists(path):
                    res = match + ' ' + file
                    if not res in overrides:
                        errors = True
                        print res
                    else:
                        print res, '(ignored)'
                        overrides[res] += 1
    for key, value in overrides.items():
        if value == 0:
            errors = True
            print 'Unmatched override:', key

    if errors:
        exit(1)

if __name__ == '__main__':
    main()
