#! /usr/bin/python

import glob, logging, os, re, string, sys, time, cStringIO
from optparse import OptionParser
from ConfigParser import SafeConfigParser

sys.path[0:0] = ['/usr/share/pycentral-data', '/usr/share/python']
import pyversions

try:
    SetType = set
except NameError:
    import sets
    SetType = sets.Set
    set = sets.Set

program = os.path.basename(sys.argv[0])

shared_base = '/usr/share/pycentral/'
shared_base2 = '/usr/share/pyshared/'
pycentral_version = '0.6.16'
req_pycentral_version = '0.6.15'


def get_file_overwrite_error(existing_files):
    """
    helper that returns a error string to be passed to the user when
    local files are detected (and querries if they are local installs
    or part of another package)
    """
    from subprocess import PIPE, Popen
    l = []
    # ask dpkg what it knows about the files in question
    (stdout, stderr) = Popen(["dpkg","-S"]+existing_files,
                             env={"LANG" : "C"},
                             stdout=PIPE, stderr=PIPE).communicate()
    # this should never happen, dpkg should always give us
    # something but we deal with it for robustness
    if not stdout and not stderr:
        l.append("Error, can not overwrite existing files:")
        l.extend(existing_files)
    # stdout has files that belong to packages
    if stdout:
        l.append("Not overwriting files owned by other packages:")
        for line in map(string.strip, stdout.split("\n")):
             if line:
                 l.append("  "+line)
    # stderr has local (or maintainer script created) files
    if stderr:
        l.append("Not overwriting local files:")
        for line in map(string.strip, stderr.split("\n")):
             if line:
                 l.append("   "+line)
    return "\n".join(l)

def samefs(path1, path2):
    if not (os.path.exists(path1) and os.path.exists(path2)):
        return False
    while path1 != os.path.dirname(path1):
        if os.path.ismount(path1):
            break
        path1 = os.path.dirname(path1)
    while path2 != os.path.dirname(path2):
        if os.path.ismount(path2):
            break
        path2 = os.path.dirname(path2)
    return path1 == path2

def version2depends(vinfo):
    if isinstance(vinfo, set):
        vinfo = list(vinfo)
    if isinstance(vinfo, list):
        vinfo = vinfo[:]
        vinfo.sort()
        nv = [int(s) for s in vinfo[-1].split('.')]
        deps = 'python (>= %s), python (<< %d.%d)' % (vinfo[0], nv[0], nv[1]+1)
    elif vinfo in ('all', 'current'):
        supported = [d[6:] for d in pyversions.supported_versions()
                     if re.match(r'python\d\.\d', d)]
        supported.sort()
        deps = 'python (>= %s)' % supported[0]
    elif vinfo == 'current_ext':
        cv = pyversions.default_version(version_only=True)
        nv = [int(s) for s in cv.split('.')]
        deps = 'python (>= %s), python (<< %d.%d)' % (cv, nv[0], nv[1]+1)
    else:
        raise ValueError, 'unknown version info %s' % vinfo
    return deps + ', python-central (>= %s)' % req_pycentral_version

def third_party_dir(version):
    if version.startswith('python'):
        version = version[6:]
    if version in ('2.3', '2.4', '2.5'):
        return 'usr/lib/python' + version + '/site-packages'
    else:
        return 'usr/lib/python' + version + '/dist-packages'

def build_relative_link(tgt, link):
    t = tgt.split('/')
    l = link.split('/')
    while l[0] == t[0]:
        del l[0], t[0]
    return '/'.join(['..' for i in range(len(l)-1)] + t)

def read_dpkg_status(verbose=False):
        """Read the dpkg status file, return a list of packages depending
        on python-central and having a Python-Version information field."""
        packages = []
        rx = re.compile(r'\bpython-central\b')
        pkgname = version = None
        depends = ''
        status = []
        for line in file('/var/lib/dpkg/status'):
            if line.startswith('Package:'):
                if version != None and 'installed' in status:
                    if 'python-support' in depends:
                        pass
                    elif rx.search(depends):
                        packages.append((pkgname, version))
                        if verbose:
                            print "    %s: %s (%s)" % (pkgname, version, status)
                version = None
                status = []
                pkgname = line.split(':', 1)[1].strip()
            elif line.startswith('Python-Version:'):
                version = line.split(':', 1)[1].strip()
            elif line.startswith('Depends:'):
                depends = line.split(':', 1)[1].strip()
            elif line.startswith('Status:'):
                status = line.split(':', 1)[1].strip().split()
        if version != None and 'installed' in status:
            if rx.search(depends):
                packages.append((pkgname, version))
                if verbose:
                    print "    %s: %s (%s)" % (pkgname, version, status)
        return packages


class PyCentralConfigParser(SafeConfigParser):
    '''SafeConfigParser allowing mixed case, `:' and `=' in keys'''
    OPTCRE = re.compile(
        r'(?P<option>[^\s].*)'
        r'\s*(?P<vi>[:=])\s*'
        r'(?P<value>.*)$'
        )
    optionxform = str

class PyCentralError(Exception):
    """Python Central Exception"""
    pass

class PyCentralVersionMissingError(PyCentralError):
    """Python Central Version Missing Exception"""
    pass

class PythonRuntime:
    def __init__(self, name, version, interp, prefix):
        self.name = name
        self.version = version
        if name.startswith('python'):
            self.short_name = name[6:]
        else:
            self.short_name = name
        self.interp = interp
        if version in ('2.3', '2.4', '2.5'):
            sitedir = 'site-packages/'
        else:
            sitedir = 'dist-packages/'
        if prefix.endswith('/'):
            self.prefix = prefix + sitedir
        else:
            self.prefix = prefix + '/' + sitedir

    def byte_compile_dirs(self, dirs, bc_option, exclude=None):
        """call compileall.py -x <exclude regexp> <dirs> according
        to bc_options"""

        logging.debug('\tbyte-compile directories')
        errors = False
        cmd = [self.interp, self.prefix + '/compileall.py', '-q']
        if exclude:
            cmd.extend(['-x', exclude])
        cmd.extend(dirs)
        for opt in ('standard', 'optimize'):
            if not opt in bc_option:
                continue
            if opt == 'optimize':
                cmd[1:1] = ['-O']
            rv = os.spawnv(os.P_WAIT, self.interp, cmd[1:])
            if rv:
                raise PyCentralError

    def byte_compile(self, files, bc_option, exclude=None, ignore_errors=False, force=False):
        errors = False
        if exclude:
            rx = re.compile(exclude)
            files2 = []
            for fn in files:
                mo = rx.search(fn)
                if mo:
                    continue
                files2.append(fn)
        else:
            files2 = files
        if not files2:
            logging.info('\tno files to byte-compile')
            return
        logging.debug('\tbyte-compile files (%d/%d) %s' \
                      % (len(files), len(files2), self.name))
        debug_files = files2[:min(2, len(files2))]
        if len(files2) > 2:
            debug_files.append('...')
        logging.debug('\t    %s' % debug_files)
        if 'python3' in self.interp:
          cmd = [self.interp, '/usr/bin/py3_compilefiles', '-q']
        else:
          cmd = [self.interp, '/usr/bin/py_compilefiles', '-q']
        if ignore_errors:
            cmd.append('-i')
        if force:
            cmd.append('-f')
        cmd.append('-')
        for opt in ('standard', 'optimize'):
            if not opt in bc_option:
                continue
            if opt == 'optimize':
                cmd[1:1] = ['-O']
            try:
                import subprocess
                p = subprocess.Popen(cmd, bufsize=1,
                                     shell=False, stdin=subprocess.PIPE)
                fd = p.stdin
            except ImportError:
                p = None
                fd = os.popen(' '.join(cmd), 'w')
            for fn in files2:
                fd.write(fn + '\n')
            rv = fd.close()
            if p:
                p.wait()
                errors = p.returncode != 0
            else:
                errors = rv != None
            if errors:
                raise PyCentralError, 'error byte-compiling files (%d)' % len(files2)

    def remove_byte_code(self, files):
        errors = False
        logging.debug('\tremove byte-code files (%d)' % (len(files)))
        for ext in ('c', 'o'):
            for fn in files:
                fnc = fn + ext
                if os.path.exists(fnc):
                    try:
                        os.unlink(fnc)
                    except OSError, e:
                        print "Sorry", e
                        errors = True
        if errors:
            raise PyCentralError, 'error removing the byte-code files'

    def list_byte_code(self, files):
        logging.debug('\tlist byte-code files (%d)' % (len(files)))
        for ext in ('c', 'o'):
            for fn in files:
                fnc = fn + ext
                yield fnc

installed_runtimes = None
default_runtime = None

def get_installed_runtimes(with_unsupported=True):
    global installed_runtimes
    global default_runtime

    if not installed_runtimes:
        installed_runtimes = []
        default_version = pyversions.default_version(version_only=True)
        supported = pyversions.supported_versions()
        old = pyversions.old_versions()
        if with_unsupported:
            unsupported = pyversions.unsupported_versions()
        else:
            unsupported = []
        for interp in glob.glob('/usr/bin/python[0-9].[0-9]'):
            if old and os.path.basename(interp) in old:
                pass
                #print "INFO: using old version '%s'" % interp
            elif unsupported and os.path.basename(interp) in unsupported:
                print "INFO: using unsupported version '%s'" % interp
            if not (os.path.basename(interp) in supported
                    or (old and os.path.basename(interp) in old)
                    or (unsupported and os.path.basename(interp) in unsupported)):
                # pycentral used to ignore everything it has no explicit
                # knowledge about. this causes install errors when
                # debian_defaults is not up-to-date (LP: #354228)
                if with_unsupported:
                    if not interp.startswith('/usr/bin/python3'):
                        print "INFO: using unknown version '%s' (debian_defaults not up-to-date?)" % interp
                else:
                    print "INFO: ignoring unknown version '%s'" % interp
                    continue
            version = interp[-3:]
            rt = PythonRuntime('python' + version,
                               version,
                               '/usr/bin/python' + version,
                               '/usr/lib/python' + version)
            installed_runtimes.append(rt)
            if version == default_version:
                default_runtime = rt
    return installed_runtimes

def get_default_runtime():
    get_installed_runtimes()
    return default_runtime

def get_runtime_for_version(version):
    if version == 'current':
        return get_default_runtime()
    for rt in get_installed_runtimes():
        if rt.version == version:
            return rt
    return None

debian_config = None
def get_debian_config():
    global debian_config
    if debian_config is not None:
        return debian_config

    config = SafeConfigParser()
    fn = '/etc/python/debian_config'
    if os.path.exists(fn):
        try:
            config.readfp(open(fn))
        except:
            logging.error("error reading config file `%s'" % fn)
            sys.exit(1)
    # checks
    if not config.has_option('DEFAULT', 'byte-compile'):
        config.set('DEFAULT', 'byte-compile', 'standard')
    bc_option = config.get('DEFAULT', 'byte-compile')
    bc_values = set([v.strip() for v in bc_option.split(',')])
    bc_unknown = bc_values - set(['standard', 'optimize'])
    if bc_unknown:
        sys.stderr.write("%s: `%s': unknown values `%s'"
                         " in `byte-compile option'\n"
                         % (program, fn, ', '.join(list(bc_unknown))))
        sys.exit(1)
    config.set('DEFAULT', 'byte-compile', ', '.join(bc_values))
    if config.has_option('DEFAULT', 'overwrite-local'):
        val = config.get('DEFAULT', 'overwrite-local').strip().lower()
        overwrite_local = val in ('yes', '1', 'true')
    else:
        overwrite_local = False
    config.set('DEFAULT', 'overwrite-local', overwrite_local and '1' or '0')
    debian_config = config
    return debian_config

class DebPackage:
    def __init__(self, kind, name,
                 oldstyle=False, default_runtime=None,
                 pkgdir=None, pkgconfig=None, parse_versions=True):
        self.kind = kind
        self.name = name
        self.version_field = None
        self.oldstyle = oldstyle
        self.parse_versions = parse_versions
        self.default_runtime = default_runtime
        self.shared_prefix = None
        self.pkgdir = pkgdir
        self.pkgconfig = pkgconfig
        self.has_shared_extension = {}
        self.has_private_extension = False
        self.has_shared_module = {}
        self.has_private_module = False
        if pkgdir:
            self.read_control()
        else:
            self.read_pyfiles()
            #self.print_info()

    def read_pyfiles(self):
        self.shared_files = []
        self.pylib_files = {}
        self.other_pylib_files = []
        self.private_files = []
        self.omitted_files = []
        self.pysupport_files = []
        if self.pkgdir:
            lines = []
            for root, dirs, files in os.walk(self.pkgdir):
                if root.endswith('DEBIAN'):
                    continue
                if root != self.pkgdir:
                    d = root[len(self.pkgdir):]
                    lines.append(d)
                    for name in files:
                        lines.append(os.path.join(d, name))
                else:
                    pass # no files in /
        else:
            config_file = '/usr/share/pyshared-data/%s' % self.name
            if self.pkgconfig:
                lines = [fn for fn, t in self.pkgconfig.items('files')]
                lines.sort()
            elif os.path.isfile(config_file):
                logging.debug("reading %s" % config_file)
                self.pkgconfig = PyCentralConfigParser()
                self.pkgconfig.optionxform = str
                self.pkgconfig.readfp(file(config_file))
                if not self.pkgconfig.has_option('pycentral', 'include-links'):
                    self.pkgconfig.set('pycentral', 'include-links', '0')
                elif self.pkgconfig.get('pycentral', 'include-links') == '':
                    # LP #332532, empty value in file
                    self.pkgconfig.set('pycentral', 'include-links', '0')
                lines = [fn for fn, t in self.pkgconfig.items('files')]
                lines.sort()
            else:
                lines = self.read_preinst_pkgconfig()
            if lines:
                pass
            elif os.environ.has_key("PYCENTRAL_NO_DPKG_QUERY"):
                logging.debug("Not using dpkg-query as requested")
                lines = map(string.strip, open('/var/lib/dpkg/info/%s.list' % self.name).readlines())
            else:
                cmd = ['/usr/bin/dpkg-query', '-L', self.name]
                try:
                    import subprocess
                    p = subprocess.Popen(cmd, bufsize=1,
                                         shell=False, stdout=subprocess.PIPE)
                    fd = p.stdout
                except ImportError:
                    fd = os.popen(' '.join(cmd))
                lines = [s[:-1] for s in fd.readlines()]
            if not self.pkgconfig:
                pc = PyCentralConfigParser()
                pc.optionxform = str
                pc.add_section('python-package')
                pc.set('python-package', 'format', '1')
                pc.add_section('pycentral')
                pc.set('pycentral', 'version', req_pycentral_version)
                pc.set('pycentral', 'include-links', '0')
                pc.add_section('files')
                for line in lines:
                    if os.path.isdir(line) and not os.path.islink(line):
                        pc.set('files', line, 'd')
                    elif os.path.exists(line):
                        pc.set('files', line, 'f')
                    else:
                        pass # should not happen
                self.pkgconfig = pc

        old_shared_base = shared_base + self.name + '/site-packages/'
        found_old_base = found_base2 = False
        for line in lines:
            fn = line
            if fn.startswith(shared_base2):
                # keep _all_ files and directories
                self.shared_files.append(fn)
                if fn.endswith('.py'):
                    self.has_shared_module['all'] = True
                found_base2 = True
                self.shared_prefix = shared_base2
                continue
            elif fn.startswith(old_shared_base):
                # keep _all_ files and directories
                self.shared_files.append(fn)
                if fn.endswith('.py'):
                    self.has_shared_module['all'] = True
                found_old_base = True
                self.shared_prefix = old_shared_base
                continue
            elif fn.startswith('/usr/share/python-support') \
                     or fn.startswith('/usr/lib/python-support'):
                self.pysupport_files.append(fn)
                continue
            elif not fn.endswith('.py') and not fn.endswith('.so'):
                if re.match(r'/usr/lib/python\d\.\d/', fn):
                    self.other_pylib_files.append(fn)
                continue
            elif fn.startswith('/etc/') or fn.startswith('/usr/share/doc/'):
                # omit files in /etc and /usr/share/doc
                self.omitted_files.append(fn)
                continue
            elif re.search(r'/s?bin/', fn):
                # omit files located in directories
                self.omitted_files.append(fn)
                continue
            elif fn.startswith('/usr/lib/site-python/'):
                version = pyversions.default_version(version_only=True)
                self.pylib_files.setdefault(version, []).append(fn)
                continue
            elif re.match(r'/usr/lib/python\d\.\d/', fn):
                version = fn[15:18]
                if fn.endswith('.so'):
                    self.has_shared_extension[version] = True
                if fn.endswith('.py'):
                    self.has_shared_module[version] = True
                    self.pylib_files.setdefault(version, []).append(fn)
                continue
            else:
                self.private_files.append(fn)
                if fn.endswith('.py'):
                    self.has_private_module = True

        if found_old_base and found_base2:
            raise PyCentralError, \
                  'shared files found in old (%s) and new locations (%s)' % (old_shared_base, shared_base2)

    def read_preinst_pkgconfig(self):
        try:
            fd = open('/var/lib/dpkg/info/%s.preinst' % self.name, 'r')
        except:
            return None
        buffer = None
        for line in fd.readlines():
            if line == '[python-package]\n':
                buffer = cStringIO.StringIO()
            if line in ('PYEOF\n', 'EOF\n'):
                break
            if buffer:
                buffer.write(line)
        if buffer is None:
            return None
        self.pkgconfig = PyCentralConfigParser()
        self.pkgconfig.optionxform = str
        self.pkgconfig.readfp(cStringIO.StringIO(buffer.getvalue()))
        # never set in the preinst copy of the config file
        self.pkgconfig.set('pycentral', 'include-links', '0')
        files = [fn for fn, t in self.pkgconfig.items('files')]
        files.sort()
        return files

    def print_info(self, fd=sys.stdout):
        fd.write('Package: %s\n' % self.name)
        fd.write('    shared files  :%4d\n' % len(self.shared_files))
        fd.write('    private files :%4d\n' % len(self.private_files))
        for ver, files in self.pylib_files.items():
            fd.write('    pylib%s files:%4d\n' % (ver, len(files)))

    def read_control(self):
        """read the debian/control file, extract the XS-Python-Version
        field; check that XB-Python-Version exists for the package."""
        if not os.path.exists('debian/control'):
            raise PyCentralError, "debian/control not found"
        self.version_field = None
        self.sversion_field = None
        try:
            section = None
            for line in file('debian/control'):
                line = line.strip()
                if line == '':
                    section = None
                elif line.startswith('Source:'):
                    section = 'Source'
                elif line.startswith('Package: ' + self.name):
                    section = self.name
                elif line.startswith('XS-Python-Version:'):
                    if section != 'Source':
                        raise PyCentralError, \
                              'attribute XS-Python-Version not in Source section'
                    self.sversion_field = line.split(':', 1)[1].strip()
                elif line.startswith('XB-Python-Version:'):
                    if section == self.name:
                        self.version_field = line.split(':', 1)[1].strip()
        except:
            pass
        if self.version_field == None:
            raise PyCentralVersionMissingError, \
                  'missing XB-Python-Version attribute in package %s' % self.name
        if self.sversion_field == None:
            raise PyCentralError, 'missing XS-Python-Version attribute'
        if self.parse_versions:
            self.sversion_info = pyversions.parse_versions(self.sversion_field)
        else:
            self.sversion_info = 'all' # dummy
        self.has_private_extension = self.sversion_info == 'current_ext'

    def move_files(self):
        """move files from the installation directory to the pycentral location"""
        import shutil

        dsttop = self.pkgdir + shared_base2
        try:
            os.makedirs(dsttop)
        except OSError:
            pass
        pversions = pyversions.supported_versions() \
                    + pyversions.unsupported_versions() + pyversions.old_versions()
        pversions = list(set(pversions))

        # rename .egg-info files and dirs, remove *.py[co] files
        rename = 'norename' not in os.environ.get('DH_PYCENTRAL', '')
        vrx = re.compile(r'(.*)(-py\d\.\d)(.*)(\.egg-info|\.pth)$')
        for pversion in pversions:
            if pversion in ('python2.3', 'python2.4', 'python2.5'):
                srctop = os.path.join(self.pkgdir, 'usr/lib', pversion, 'site-packages')
            else:
                srctop = os.path.join(self.pkgdir, 'usr/lib', pversion, 'dist-packages')
                # srctop2 and srctop3 are the old/wrong locations, move these.
                srctop2 = os.path.join(self.pkgdir, 'usr/lib', pversion, 'site-packages')
                srctop3 = os.path.join(self.pkgdir, 'usr/local/lib', pversion, 'dist-packages')
                if os.path.isdir(srctop) and (os.path.isdir(srctop2) or os.path.isdir(srctop3)):
                    raise PyCentralError, 'both directories site-packages and dist-packages exist for %s.' % pversion
                if os.path.isdir(srctop2):
                    print 'renaming %s to %s' % (srctop2, srctop)
                    os.rename(srctop2, srctop)
                elif os.path.isdir(srctop3):
                    print 'renaming %s to %s' % (srctop3, srctop)
                    try:
                        os.makedirs(os.path.dirname(srctop))
                    except OSError:
                        pass
                    os.rename(srctop3, srctop)
                    while srctop3:
                        srctop3 = os.path.dirname(srctop3)
                        try:
                            os.rmdir(srctop3)
                        except OSError:
                            break
                # do the same with -dbg packages (which don't have an Python-Version attribute)
                pkgdbgdir = self.pkgdir + '-dbg'
                if os.path.isdir(pkgdbgdir):
                    srcdtop = os.path.join(pkgdbgdir, 'usr/lib', pversion, 'dist-packages')
                    srcdtop2 = os.path.join(pkgdbgdir, 'usr/lib', pversion, 'site-packages')
                    srcdtop3 = os.path.join(pkgdbgdir, 'usr/local/lib', pversion, 'dist-packages')
                    if os.path.isdir(srcdtop) and (os.path.isdir(srcdtop2) or os.path.isdir(srcdtop3)):
                        print 'both directories site-packages and dist-packages exist for %s-dbg.' % pversion
                    if os.path.isdir(srcdtop2):
                        print 'renaming %s to %s' % (srcdtop2, srcdtop)
                        os.rename(srcdtop2, srcdtop)
                    elif os.path.isdir(srcdtop3):
                        print 'renaming %s to %s' % (srcdtop3, srcdtop)
                        try:
                            os.makedirs(os.path.dirname(srcdtop))
                        except OSError:
                            pass
                        os.rename(srcdtop3, srcdtop)
                        while srcdtop3:
                            srcdtop3 = os.path.dirname(srcdtop3)
                            try:
                                os.rmdir(srcdtop3)
                            except OSError:
                                break
                    srcdtop = os.path.join(pkgdbgdir, 'usr/lib/debug/usr/lib', pversion, 'dist-packages')
                    srcdtop2 = os.path.join(pkgdbgdir, 'usr/lib/debug/usr/lib', pversion, 'site-packages')
                    srcdtop3 = os.path.join(pkgdbgdir, 'usr/lib/debug/usr/local/lib', pversion, 'dist-packages')
                    if os.path.isdir(srcdtop) and (os.path.isdir(srcdtop2) or os.path.isdir(srcdtop3)):
                        print 'both directories site-packages and dist-packages exist.'
                    if os.path.isdir(srcdtop2):
                        print 'renaming %s to %s' % (srcdtop2, srcdtop)
                        os.rename(srcdtop2, srcdtop)
                    elif os.path.isdir(srcdtop3):
                        print 'renaming %s to %s' % (srcdtop3, srcdtop)
                        try:
                            os.makedirs(os.path.dirname(srcdtop))
                        except OSError:
                            pass
                        os.rename(srcdtop3, srcdtop)
                        while srcdtop3:
                            srcdtop3 = os.path.dirname(srcdtop3)
                            try:
                                os.rmdir(srcdtop3)
                            except OSError:
                                break
            for root, dirs, files in os.walk(srctop, topdown=False):
                for name in files:
                    m = vrx.match(name)
                    if m and rename:
                        name2 = ''.join(m.group(1, 3, 4))
                        print "rename: %s -> %s" % (name, name2)
                        os.rename(os.path.join(root, name), os.path.join (root, name2))
                    elif name.endswith('.pyc') or name.endswith('.pyo'):
                        os.unlink(os.path.join(root, name))
                for name in dirs:
                    m = vrx.match(name)
                    if m and rename:
                        name2 = ''.join(m.group(1, 3, 4))
                        print "rename: %s -> %s" % (name, name2)
                        os.rename(os.path.join(root, name), os.path.join (root, name2))

        # search for differences
        import filecmp
        class MyDircmp(filecmp.dircmp):
            def report(self):
                if self.left_only or self.right_only or self.diff_files or self.funny_files:
                    self.differs = True
                    print 'diff', self.left, self.right
                if self.left_only:
                    self.left_only.sort()
                    print 'Only in', self.left, ':', self.left_only
                if self.right_only:
                    self.right_only.sort()
                    print 'Only in', self.right, ':', self.right_only
                if self.diff_files:
                    self.diff_files.sort()
                    print 'Differing files :', self.diff_files
                if self.funny_files:
                    self.funny_files.sort()
                    print 'Trouble with common files :', self.funny_files

        for pv1 in pversions:
            for pv2 in pversions[pversions.index(pv1)+1:]:
                site1 = os.path.join(self.pkgdir, third_party_dir(pv1))
                site2 = os.path.join(self.pkgdir, third_party_dir(pv2))
                if not (os.path.isdir(site1) and os.path.isdir(site2)):
                    continue
                dco = MyDircmp(site1, site2)
                dco.differs = False
                dco.report_full_closure()

        # move around
        for pversion in pversions:
            srctop = os.path.join(self.pkgdir, third_party_dir(pversion))
            for root, dirs, files in os.walk(srctop):
                if root == srctop:
                    d = '.'
                else:
                    d = root[len(srctop)+1:]
                for name in dirs:
                    src = os.path.join(root, name)
                    dst = os.path.join(dsttop, d, name)
                    try:
                        os.mkdir(dst)
                        shutil.copymode(src, dst)
                    except OSError:
                        pass
                for name in files:
                    src = os.path.join(root, name)
                    dst = os.path.join(dsttop, d, name)
                    if re.search(r'\.so(\.\d+)*?$', name):
                        continue
                    # TODO: if dst already exists, make sure, src == dst
                    os.rename(src, dst)
            # remove empty dirs in /usr/lib/pythonX.Y
            for root, dirs, files in os.walk(self.pkgdir + '/usr/lib', topdown=False):
                try:
                    if re.match("/usr/lib/python\d\.\d($|/)", root.replace(self.pkgdir, "")):
                        os.rmdir(root)
                except OSError:
                    pass
            try:
                os.rmdir(self.pkgdir + '/usr/lib')
            except OSError:
                pass
        # remove empty dirs in /usr/share/pyshared
        for root, dirs, files in os.walk(self.pkgdir + shared_base2, topdown=False):
            try:
                os.rmdir(root)
            except OSError:
                pass
        try:
            os.rmdir(self.pkgdir + shared_base2)
        except OSError:
            pass

    def add_shared_links(self):
        versions = pyversions.requested_versions(self.sversion_field, True)
        tgttop = os.path.join(self.pkgdir, shared_base2.strip('/'))
        existing_links = []
        for version in versions:
            linktop = os.path.join(self.pkgdir, third_party_dir(version))
            try:
                os.makedirs(linktop)
            except OSError:
                pass
            for root, dirs, files in os.walk(tgttop, topdown=True):
                linkroot = root.replace(tgttop, linktop, 1)
                for name in files:
                    tgt = os.path.join(root, name)
                    link = os.path.join(linkroot, name)
                    if os.path.exists(link):
                        existing_links.append(link)
                    else:
                        rel_tgt = build_relative_link(tgt, link)
                        os.symlink(rel_tgt, link)
                for name in dirs:
                    try:
                        os.makedirs(os.path.join(linkroot, name))
                    except OSError:
                        pass
        if existing_links:
            print 'Found existing files while including symlinks:'
            for link in existing_links:
                print '  ', link
            raise PyCentralError, 'Found existing files while including symlinks'

    def gen_substvars(self):
        supported = [d[6:] for d in pyversions.supported_versions()
                     if re.match(r'python\d\.\d', d)]
        versions = ''
        prversions = ''
        self.depends = None
        if len(self.has_shared_module) or len(self.has_shared_extension):
            # shared modules / extensions
            if len(self.has_shared_extension):
                versions = self.has_shared_extension.keys()
            else:
                if self.sversion_info in ('current', 'current_ext'):
                    versions = 'current'
                elif self.sversion_info == 'all':
                    versions = 'all'
                    prversions = supported
                else:
                    versions = self.sversion_field
                    prversions = list(self.sversion_info.intersection(supported))
                    self.depends = version2depends(self.sversion_info)
        elif self.has_private_module or self.has_private_extension:
            if self.sversion_info == 'all':
                versions = 'current'
            elif self.sversion_info == 'current':
                versions = 'current'
            elif self.sversion_info == 'current_ext':
                versions = [pyversions.default_version(version_only=True)]
            elif isinstance(self.sversion_info, list) or isinstance(self.sversion_info, set):
                # exact version info required, no enumeration, no relops
                if len(self.sversion_info) != 1 or not re.match(r'\d\.\d', self.sversion_info[0]):
                    raise PyCentralError, 'no exact version for package with private modules'
                versions = [list(self.sversion_info)[0]]
            else:
                raise PyCentralError, 'version error for package with private modules'
        else:
            # just "copy" it from the source field
            if self.sversion_info == 'current':
                versions = 'current'
            elif self.sversion_info == 'current_ext':
                versions = [pyversions.default_version(version_only=True)]
            elif self.sversion_info == 'all':
                versions = 'all'
                prversions = supported
            else:
                versions = self.sversion_field
                prversions = list(self.sversion_info.intersection(supported))
                self.depends = version2depends(self.sversion_info)

        if (len(self.has_shared_module) or len(self.has_shared_extension)) \
           and self.has_private_module or self.has_private_extension:
            # use case? use the information for the shared stuff
            pass
        if versions == '':
            raise PyCentralError, 'unable to determine Python-Version attribute'
        if isinstance(versions, list) or isinstance(versions, set):
            self.version_field = ', '.join(versions)
        else:
            self.version_field = versions
        if not self.depends:
            self.depends = version2depends(versions)
        if self.name.startswith('python-'):
            if prversions == '':
                prversions = versions
            self.provides = ', '.join([self.name.replace('python-', 'python%s-' % ver)
                                       for ver in prversions])

    def set_version_field(self, version_field):
        self.version_field = version_field
        if self.parse_versions:
            self.version_info = pyversions.parse_versions(version_field)

    def read_version_info(self, use_default_if_missing=False):
        """Read the Python-Version information field"""
        if self.version_field:
            return
        if self.pkgconfig and self.pkgconfig.has_option('python-package', 'python-version'):
            self.version_field = self.pkgconfig.get('python-package', 'python-version')
            logging.debug("Using python-version from pkgconfig: %s" % self.version_field)
        elif os.environ.has_key("PYCENTRAL_NO_DPKG_QUERY"):
            logging.debug("Not using dpkg-query as requested")
            needle = "Package: %s\n" % self.name
            for block in open("/var/lib/dpkg/status").read().split("\n\n"):
                if needle in block:
                    for line in block.split("\n"):
                        if line.startswith('Python-Version:'):
                            self.version_field = line.split(':', 1)[1].strip()
                            break
        else:
            logging.debug("dpkg-query -s %s" % self.name)
            cmd = ['/usr/bin/dpkg-query', '-s', self.name]
            try:
                import subprocess
                p = subprocess.Popen(cmd, bufsize=1,
                                     shell=False, stdout=subprocess.PIPE)
                fd = p.stdout
            except ImportError:
                fd = os.popen(' '.join(cmd))

            for line in fd:
                if line.startswith('Python-Version:'):
                    self.version_field = line.split(':', 1)[1].strip()
                    break
            fd.close()
        # now verify/parse it
        if not self.version_field and use_default_if_missing:
            logging.warn("%s: has no Python-Version field, assuming default runtime" % self.name)
            self.version_field = get_default_runtime().version
        if not self.version_field:
            raise PyCentralError, "package has no field Python-Version"
        if self.parse_versions:
            self.version_info = pyversions.parse_versions(self.version_field)

    def set_default_runtime_from_version_info(self):
        versions = list(pyversions.requested_versions(self.version_field, version_only=True))
        if not versions:
            #raise PyCentralError, "no matching runtime for `%s'" % self.version_field
            logging.warn("%s: no matching runtime for `%s', using default"
                         % (self.name, self.version_field))
            self.default_runtime = get_default_runtime()
        if len(versions) == 1:
            self.default_runtime = get_runtime_for_version(versions[0])
        elif pyversions.default_version(version_only=True) in versions:
            self.default_runtime = get_default_runtime()
        else:
            self.default_runtime = get_runtime_for_version(versions[0])

    def byte_compile(self, runtimes, bc_option, exclude_regex, ignore_errors=False):
        """byte compiles all files not handled by pycentral"""

        logging.debug("    byte-compile %s" % self.name)
        if self.shared_files:
            ppos = len(self.shared_prefix)
            for rt in runtimes:
                linked_files = [ rt.prefix + fn[ppos:]
                                 for fn in self.shared_files
                                 if fn[-3:] == '.py']
                rt.byte_compile(linked_files, bc_option, exclude_regex, ignore_errors)

        for pyver, files in self.pylib_files.items():
            logging.debug("bc for v%s (%d files)" % (pyver, len(files)))
            rt = get_runtime_for_version(pyver)
            if rt in runtimes:
                rt.byte_compile(files, bc_option, exclude_regex)

        if self.private_files:
            logging.debug("bc %s private (%d files)" %
                          (self.default_runtime.version, len(self.private_files)))
            rt = self.default_runtime
            rt.byte_compile(self.private_files, bc_option, exclude_regex)

    def remove_bytecode(self):
        """remove all byte-compiled files not handled by pycentral"""

        assert self.oldstyle

        logging.debug("    remove byte-code for %s" % self.name)
        pyfiles = []
        for files in self.pylib_files.values():
            pyfiles.extend(files)
        pyfiles.extend(self.private_files)

        errors = False
        for ext in ('c', 'o'):
            for fn in pyfiles:
                fnc = fn + ext
                if os.path.exists(fnc):
                    try:
                        os.unlink(fnc)
                    except OSError, e:
                        print "Sorry", e
                        errors = True
        if errors:
            raise PyCentralError

    def link_shared_files(self, rt):
        """link shared files at runtime"""
        #if samefs(rt.prefix, self.shared_files[0]):
        #    link_cmd = os.link
        #else:
        #    link_cmd = os.symlink
        logging.debug("\tlink shared files %s/%s" % (rt.name, self.name))
        if not self.shared_files:
            return []
        link_cmd = os.symlink
        ppos = len(self.shared_prefix)
        existing_files = []
        for fn in self.shared_files:
            fn2 = rt.prefix + fn[ppos:]
            if os.path.isdir(fn) and not os.path.islink(fn):
                continue
            if os.path.exists(fn2):
                link = abs_link = None
                if os.path.islink(fn2):
                    link = abs_link = os.readlink(fn2)
                    if link.startswith('../'):
                        abs_link = os.path.normpath(os.path.join(os.path.dirname(fn2), link))
                if abs_link == fn or link == fn:
                    continue
                if not link or not (abs_link.startswith(shared_base2) or abs_link.startswith(shared_base)):
                    existing_files.append(fn2)

        if existing_files:
            conf = get_debian_config()
            overwrite_local = conf.get('DEFAULT', 'overwrite-local') == '1'
            if overwrite_local:
                print '\n  '.join(["overwriting local files:"] + existing_files)

        linked_files = []
        try:
            for fn in self.shared_files:
                fn2 = rt.prefix + fn[ppos:]
                if os.path.isdir(fn) and not os.path.islink(fn):
                    if os.path.isdir(fn2):
                        continue
                    os.makedirs(fn2)
                    linked_files.append(fn2)
                else:
                    if os.path.exists(fn2):
                        msg = "already exists: %s" % fn2
                        link = abs_link = None
                        if os.path.islink(fn2):
                            link = abs_link = os.readlink(fn2)
                            if link.startswith('../'):
                                abs_link = os.path.normpath(os.path.join(os.path.dirname(fn2), link))
                        if abs_link == fn or link == fn:
                            linked_files.append(fn2)
                            continue
                        if not link or not (abs_link.startswith(shared_base2) or abs_link.startswith(shared_base)):
                            msg = msg + " -> %s" % link
                            if overwrite_local:
                                print "warning:", msg
                                os.unlink(fn2)
                            else:
                                continue # raise PyCentralError, msg at end of method
                    # make sure that fn2 really does not exist; this is a
                    # special hack to make pycentral work with fakechroot,
                    # which has a slightly weird treatment of symlinks
                    # now needed to switch between old and new prefix
                    try:
                        os.unlink(fn2)
                    except OSError:
                        pass
                    link_cmd(fn, fn2)
                    linked_files.append(fn2)
        except PyCentralError, msg:
            raise
        except Exception, msg:
            print msg
            # FIXME: undo
            linked_files.reverse()
            return []
        else:
            if existing_files and not overwrite_local:
                s = get_file_overwrite_error(existing_files)
                raise PyCentralError, s
            return linked_files

    def unlink_shared_files(self, rt):
        logging.debug('\tunlink_shared_files %s/%s' % (rt.name, self.name))
        if not self.shared_files:
            return
        ppos = len(self.shared_prefix)
        shared_files = self.shared_files[:]
        shared_files.reverse()
        for fn in shared_files:
            fn2 = rt.prefix + fn[ppos:]
            if os.path.isdir(fn2) and not os.path.islink(fn2):
                try:
                    os.removedirs(fn2)
                except OSError:
                    pass
            else:
                if os.path.exists(fn2):
                    os.unlink(fn2)

    def list_shared_files(self, rt):
        logging.debug('\tlist_shared_files %s/%s' % (rt.name, self.name))
        if not self.shared_files:
            return
        ppos = len(self.shared_prefix)
        for fn in self.shared_files:
            fn2 = rt.prefix + fn[ppos:]
            yield fn2


    def install(self, runtimes, bc_option, exclude_regex,
                byte_compile_default=True, ignore_errors=False):
        logging.debug('\tinstall package %s' % self.name)
        # install shared .py files
        symlinks_included = self.pkgconfig.getboolean('pycentral', 'include-links')
        if symlinks_included:
            pass
        else:
            if self.shared_files:
                for rt in runtimes:
                    linked_files = self.link_shared_files(rt)
                    rt.byte_compile(linked_files, bc_option, exclude_regex, ignore_errors)
        # byte compile files inside prefix
        if self.pylib_files:
            for pyver, files in self.pylib_files.items():
                rt = get_runtime_for_version(pyver)
                if rt in runtimes:
                    rt.byte_compile(files, bc_option, exclude_regex, ignore_errors)
        # byte compile with the default runtime for the package
        if byte_compile_default:
            if self.private_files:
                self.default_runtime.byte_compile(self.private_files, bc_option,
                                                  exclude_regex, ignore_errors)

    def prepare(self, runtimes, old_runtimes, old_pkg, ignore_errors=False):
        logging.debug('\tprepare package %s' % self.name)

        ppos = len(self.shared_prefix)

        if old_pkg and old_pkg.private_files:
            fs_in_old = set([fn for fn in old_pkg.private_files if fn[-3:] == '.py'])
            fs_in_new = set([fn for fn in self.private_files if fn[-3:] == '.py'])
            removed_fs = list(fs_in_old.difference(fs_in_new))
            if removed_fs:
                logging.debug_list('\t', 'removed private', removed_fs)
                default_runtime.remove_byte_code(removed_fs)

        old_pylib_fs = []
        if old_pkg and old_pkg.pylib_files:
            for pyver, files in old_pkg.pylib_files.items():
                fs_in_old = set([fn for fn in files if fn[-3:] == '.py'])
                fs_in_new = set([fn for fn in self.pylib_files.get(pyver, []) if fn[-3:] == '.py'])
                removed_fs = list(fs_in_old.difference(fs_in_new))
                if removed_fs:
                    logging.debug_list('\t', 'removed pylib', removed_fs)
                    default_runtime.remove_byte_code(removed_fs)
                old_pylib_fs += files
            old_pylib_fs += old_pkg.other_pylib_files

        if old_pkg and old_pkg.shared_files:
            for rt in old_runtimes:
                if rt in runtimes:
                    continue
                linked_files = [ rt.prefix + fn[ppos:]
                                 for fn in old_pkg.shared_files if fn[-3:] == '.py']
                if linked_files:
                    logging.debug_list('\t', 'removed runtimes', linked_files)
                    default_runtime.remove_byte_code(linked_files)
                    self.unlink_files(rt)

        if not self.shared_files:
            return

        dirs_in_new = set([fn for fn, t in self.pkgconfig.items('files')
                           if t == 'd' if fn.startswith(self.shared_prefix)])
        dirs_in_old = set()
        if old_pkg:
            dirs_in_old = set([fn for fn, t in old_pkg.pkgconfig.items('files')
                               if t == 'd' if fn.startswith(self.shared_prefix)])
        new_dirs = list(dirs_in_new.difference(dirs_in_old))
        new_dirs.sort()
        removed_dirs = list(dirs_in_old.difference(dirs_in_new))
        removed_dirs.sort()

        fs_in_new = set([fn for fn, t in self.pkgconfig.items('files')
                         if t == 'f' if fn.startswith(self.shared_prefix)])
        fs_in_old = set()
        if old_pkg:
            fs_in_old = set([fn for fn, t in old_pkg.pkgconfig.items('files')
                             if t == 'f' if fn.startswith(self.shared_prefix)])
        new_fs = list(fs_in_new.difference(fs_in_old))
        new_fs.sort()
        removed_fs = list(fs_in_old.difference(fs_in_new))
        removed_fs.sort()

        logging.debug_list('\t', 'new      dirs', new_dirs)
        logging.debug_list('\t', 'removed  dirs', removed_dirs)
        logging.debug_list('\t', 'new     files', new_fs)
        logging.debug_list('\t', 'removed files', removed_fs)

        link_cmd = os.symlink
        ppos = len(self.shared_prefix)
        existing_files = []
        for rt in runtimes:
            for f1 in new_fs:
                f2 = rt.prefix + f1[ppos:]
                if os.path.exists(f2):
                    link = abs_link = None
                    if os.path.islink(f2):
                        link = abs_link = os.readlink(f2)
                        if link.startswith('../'):
                            abs_link = os.path.normpath(os.path.join(os.path.dirname(f2), link))
                    if abs_link == f1 or link == f1:
                        continue
                    if not link or not (abs_link.startswith(shared_base2) or abs_link.startswith(shared_base)):
                        existing_files.append(f2)
        if existing_files:
            # if the current installed version does not use python-central
            # then having a file here is expected and harmless
            if not self.name in [p for (p,v) in read_dpkg_status()]:
                logging.info("%s: upgrade from package version not using python-central" % self.name)
                return
            # if all existing files are found in the old package in
            # /usr/lib/pythonX.Y/site-packages, and moved to the shared area,
            # do nothing.
            not_in_same_pkg = set(existing_files)
            if old_pkg:
                not_in_same_pkg.difference_update(old_pylib_fs)
            if not_in_same_pkg == set():
                logging.info("%s: upgrade from package version with unmoved files" % self.name)
                return
            conf = get_debian_config()
            overwrite_local = conf.get('DEFAULT', 'overwrite-local') == '1'
            if overwrite_local:
                print "overwriting local files"

        for rt in runtimes:
            for d1 in new_dirs:
                d2 = rt.prefix + d1[ppos:]
                if os.path.isdir(d2):
                    continue
                os.makedirs(d2)
            for f1 in new_fs:
                f2 = rt.prefix + f1[ppos:]
                if os.path.exists(f2):
                    msg = "already exists: %s" % f2
                    link = abs_link = None
                    if os.path.islink(f2):
                        link = abs_link = os.readlink(f2)
                        if link.startswith('../'):
                            abs_link = os.path.normpath(os.path.join(os.path.dirname(f2), link))
                    if abs_link == f1 or link == f1:
                        continue
                    if not link or not (abs_link.startswith(shared_base2) or abs_link.startswith(shared_base)):
                        msg = msg + " -> %s" % link
                        if overwrite_local:
                            print "warning:", msg
                            os.unlink(f2)
                        else:
                            continue # raise PyCentralError, msg at end of loop
                # hack to make pycentral work with fakechroot
                # now needed to switch between old and new prefix
                try:
                    os.unlink(f2)
                except OSError:
                    pass
                try:
                    d = os.path.dirname(f2)
                    if not os.path.isdir(d):
                        print "create directory %s" % d
                        os.makedirs(d)
                    link_cmd(f1, f2)
                except OSError:
                    print "unable to create symlink %s" % f2
                    raise
        if existing_files and not overwrite_local:
            raise PyCentralError, "not overwriting local files"

        for rt in runtimes:
            for f1 in removed_fs:
                f2 = rt.prefix + f1[ppos:]
                try:
                    os.unlink(f2)
                    os.unlink(f2 + 'c')
                    os.unlink(f2 + 'o')
                except OSError:
                    pass
            for d1 in removed_dirs:
                d2 = rt.prefix + d1[ppos:]
                try:
                    os.rmdir(d2)
                except OSError:
                    pass

        return

    def remove(self, runtimes, remove_script_files=True):
        logging.debug('\tremove package %s' % self.name)
        # remove shared .py files
        symlinks_included = self.pkgconfig.getboolean('pycentral', 'include-links')
        if symlinks_included:
            pass
        else:
            if self.shared_files:
                ppos = len(self.shared_prefix)
                for rt in runtimes:
                    linked_files = [ rt.prefix + fn[ppos:]
                                     for fn in self.shared_files
                                     if fn[-3:] == '.py']
                    #print self.shared_files
                    #print linked_files
                    default_runtime.remove_byte_code(linked_files)
                    self.unlink_shared_files(rt)
        # remove byte compiled files inside prefix
        if self.pylib_files:
            for pyver, files in self.pylib_files.items():
                rt = get_runtime_for_version(pyver)
                if rt in runtimes:
                    default_runtime.remove_byte_code(files)
        # remove byte code for script files
        if remove_script_files:
            if self.private_files:
                default_runtime.remove_byte_code(self.private_files)

    def list(self, runtimes, list_script_files=True):
        logging.debug('\tlist package %s' % self.name)
        # list shared .py files
        symlinks_included = self.pkgconfig.getboolean('pycentral', 'include-links')
        if not symlinks_included and self.shared_files:
            ppos = len(self.shared_prefix)
            for rt in runtimes:
                linked_files = [ rt.prefix + fn[ppos:]
                                 for fn in self.shared_files
                                 if fn[-3:] == '.py']
                for f in default_runtime.list_byte_code(linked_files):
                    yield f
                for f in self.list_shared_files(rt):
                    yield f
        # list byte compiled files inside prefix
        if self.pylib_files:
            for pyver, files in self.pylib_files.items():
                rt = get_runtime_for_version(pyver)
                if rt in runtimes:
                    for f in default_runtime.list_byte_code(files):
                        yield f
        # list byte code for script files
        if list_script_files:
            if self.private_files:
                for f in default_runtime.list_byte_code(self.private_files):
                    yield f

    def update_bytecode_files(self, runtimes, rt_default, bc_option):
        # byte-compile with default python version
        logging.debug('\tupdate byte-code for %s' % self.name)
        exclude_regex = None
        # update shared .py files
        if self.shared_files:
            ppos = len(self.shared_prefix)
            for rt in runtimes:
                if rt == rt_default:
                    linked_files = self.link_shared_files(rt)
                    rt.byte_compile(linked_files, bc_option, exclude_regex)
                else:
                    linked_files = [ rt.prefix + fn[ppos:]
                                     for fn in self.shared_files
                                     if fn[-3:] == '.py']
                    rt.remove_byte_code(linked_files)
                    self.unlink_shared_files(rt)
        # byte compile with the default runtime for the package
        if self.private_files:
            self.default_runtime.byte_compile(self.private_files,
                                              bc_option, exclude_regex, force=True)

known_actions = {}
def register_action(action_class):
    known_actions[action_class.name] = action_class

class Action:
    _option_parser = None
    name = None
    help = ""
    usage = "<options>"
    def __init__(self):
        self.errors_occured = 0
        parser = self.get_option_parser()
        parser.set_usage(
            'usage: %s [<options> ...] %s %s' % (program, self.name, self.usage))

    def get_option_parser(self):
        if not self._option_parser:
            p = OptionParser()
            self._option_parser = p
        return self._option_parser

    def info(self, msg, stream=sys.stderr):
        logging.info('%s %s: %s' % (program, self.name, msg))

    def warn(self, msg, stream=sys.stderr):
        logging.warn('%s %s: %s' % (program, self.name, msg))

    def error(self, msg, stream=sys.stderr, go_on=False):
        logging.error('%s %s: %s' % (program, self.name, msg))
        self.errors_occured += 1
        if not go_on:
            sys.exit(1)

    def parse_args(self, arguments):
        self.options, self.args = self._option_parser.parse_args(arguments)
        return self.options, self.args

    def check_args(self, global_options):
        return self.errors_occured

    def run(self, global_opts):
        pass

    def get_arch(self):
        import subprocess
        # a little convoluted, but we need to get the current architecture
        # name
        cmd = ['/usr/bin/dpkg', '--print-architecture']
        p = subprocess.Popen(cmd, bufsize=1,
                             shell=False, stdout=subprocess.PIPE)
        fd = p.stdout
        arch = fd.readline().strip()
        fd.close()
        return arch


class ActionByteCompile(Action):
    """byte compile the *.py files in <package> using the the
    default python version (or use the version specified with -v.
    Any additional directory arguments are ignored (only files
    found in the package are byte compiled. Files in
    /usr/lib/pythonX.Y are compiled with the matching python version.

    bccompile is a replacement for the current byte compilation
    generated by the dh_python debhelper script.
    """
    name = 'bccompile'
    help = 'byte compile .py files in a package'
    usage = '[<options>] <package> [<dir> ...]'

    def get_option_parser(self):
        if not self._option_parser:
            p = OptionParser()
            p.add_option('-x', '--exclude',
                         help="skip files matching the regular expression",
                         default=None, action='store', dest='exclude')
            p.add_option('-V', '--version',
                         help="byte compile using this python version",
                         default='current', action='store', dest='version')
            self._option_parser = p
        return self._option_parser

    def check_args(self, global_options):
        if len(self.args) < 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.pkgname = self.args[0]
        self.runtime = get_runtime_for_version(self.options.version)
        if not self.runtime:
            self.error("unknown runtime version %s" % self.options.version)

        arch = self.get_arch()
        if not os.path.exists('/var/lib/dpkg/info/%s.list' % self.pkgname) \
           and not os.path.exists('/var/lib/dpkg/info/%s/%s.list' % (arch, self.pkgname)):
            self.error("package %s is not installed" % self.pkgname)
        self.pkg = DebPackage('package', self.pkgname, oldstyle=False,
                              default_runtime=self.runtime)
        self.pkg.read_version_info()
        return self.errors_occured

    def run(self, global_options):
        logging.debug('bccompile %s' % self.pkgname)
        runtimes = get_installed_runtimes()
        config = get_debian_config()
        bc_option = config.get('DEFAULT', 'byte-compile')
        requested = pyversions.requested_versions_for_runtime(self.pkg.version_field, version_only=True)
        used_runtimes = [rt for rt in runtimes if rt.short_name in requested]
        # called with directories as arguments
        if 0 and self.directories:
            try:
                for version, dirs in self.pylib_dirs.items():
                    rt = get_runtime_for_version(version)
                    rt.byte_compile_dirs(dirs, bc_option, self.options.exclude)
                if self.private_dirs:
                    version = self.pkg.version_field
                    if version == 'current':
                        version = pyversions.default_version(version_only=True)
                    rt = get_runtime_for_version(version)
                    rt.byte_compile_dirs(self.private_dirs, bc_option, self.options.exclude)
            except PyCentralError:
                self.error("error byte-compiling package `%s'" % self.pkgname)
            return

        try:
            self.pkg.byte_compile(used_runtimes, bc_option, self.options.exclude)
        except PyCentralError:
            self.error("error byte-compiling package `%s'" % self.pkgname)

register_action(ActionByteCompile)

class ActionPkgInstall(Action):
    name = 'pkginstall'
    help = 'make a package available for all supported runtimes'
    usage = '[<options>] <package>'

    def get_option_parser(self):
        if not self._option_parser:
            p = OptionParser()
            p.add_option('-x', '--exclude',
                         help="skip files matching the regular expression",
                         default=None, action='store', dest='exclude')
            self._option_parser = p
        return self._option_parser

    def check_args(self, global_options):
        if len(self.args) != 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.pkgname = self.args[0]
        arch = self.get_arch()
        if not os.path.exists('/var/lib/dpkg/info/%s.list' % self.pkgname) \
           and not os.path.exists('/var/lib/dpkg/info/%s/%s.list' % (arch, self.pkgname)):
            self.error("package %s is not installed" % self.pkgname)
        return self.errors_occured

    def run(self, global_options):
        runtimes = get_installed_runtimes()
        config = get_debian_config()
        bc_option = config.get('DEFAULT', 'byte-compile')
        pkg = DebPackage('package', self.args[0], oldstyle=False)
        pkg.read_version_info()
        requested = pyversions.requested_versions_for_runtime(pkg.version_field, version_only=True)
        used_runtimes = [rt for rt in runtimes if rt.short_name in requested]
        try:
            pkg.set_default_runtime_from_version_info()
        except ValueError:
            # Package doesn't provide support for any supported runtime
            if len(used_runtimes) == 0:
                self.error('%s needs unavailable runtime (%s)'
                           % (self.pkgname, pkg.version_field))
            else:
                # Still byte compile for the available runtimes (with the
                # first matching runtime)
                pkg.default_runtime = get_runtime_for_version(used_runtimes[0])
        logging.debug('\tavail=%s, pkg=%s, install=%s'
                      % ([rt.short_name for rt in runtimes],
                         pkg.version_field,
                         [rt.short_name for rt in used_runtimes]))
        try:
            pkg.install(used_runtimes, bc_option,
                        self.options.exclude, byte_compile_default=True)
        except PyCentralError, msg:
            self.error(msg)

        # cleanup after failed pkgprepare upgrades, see #552595.
        import subprocess
        p = subprocess.Popen(['dpkg-trigger',
                   '--no-await',
                   '--by-package=%s' % os.environ.get('DPKG_MAINTSCRIPT_PACKAGE', 'python'),
                   'cleanup-pkgprepare-updates'],
                  shell=False)
        sts = os.waitpid(p.pid, 0)[1]
        if sts != 0:
            self.error("error calling dpkg-trigger")

register_action(ActionPkgInstall)


class ActionPkgPrepare(Action):
    name = 'pkgprepare'
    help = 'prepare a package for all supported runtimes'
    usage = '[<options>] <package>'

    def get_option_parser(self):
        if not self._option_parser:
            p = OptionParser()
            p.add_option('-x', '--exclude',
                         help="skip files matching the regular expression",
                         default=None, action='store', dest='exclude')
            self._option_parser = p
        return self._option_parser

    def check_args(self, global_options):
        if len(self.args) != 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.pkgname = self.args[0]
        # FIXME: run from the preinst, package may not exist
        #if not os.path.exists('/var/lib/dpkg/info/%s.list' % self.pkgname):
        #    self.error("package %s is not installed" % self.pkgname)
        return self.errors_occured

    def run(self, global_options):
        # functionality disabled starting with 0.6.9
        return

        runtimes = get_installed_runtimes()
        config = get_debian_config()
        pkgconfig = PyCentralConfigParser()
        pkgconfig.optionxform = str
        pkgconfig.readfp(sys.stdin)
        version_field = pkgconfig.get('python-package', 'python-version')
        try:
            requested = pyversions.requested_versions_for_runtime(version_field, version_only=True)
        except pyversions.PyCentralEmptyValueError, msg:
            # cannot install yet; remove the symlinked files and byte code files from the old
            # version, rely on the pkginstall in the postinst.
            print "pycentral: required runtimes not yet installed, skip pkgprepare, call pkgremove"
            runtimes = get_installed_runtimes(with_unsupported=True)
            pkg = DebPackage('package', self.args[0], oldstyle=False)
            pkg.read_version_info()
            pkg.default_runtime = get_default_runtime()
            try:
                pkg.remove(runtimes, remove_script_files=True)
            except PyCentralError, msg:
                self.warn(msg)
            return
        used_runtimes = [rt for rt in runtimes if rt.short_name in requested]
        pkg = DebPackage('package', self.args[0], oldstyle=False, pkgconfig=pkgconfig)
        pkg.set_version_field(version_field)
        try:
            pkg.set_default_runtime_from_version_info()
        except ValueError:
            # Package doesn't provide support for any supported runtime
            if len(used_runtimes) == 0:
                self.error('%s needs unavailable runtime (%s)'
                           % (self.pkgname, pkg.version_field))
            else:
                # Still byte compile for the available runtimes (with the
                # first matching runtime)
                pkg.default_runtime = get_runtime_for_version(used_runtimes[0])

        if os.path.exists('/var/lib/dpkg/info/%s.list' % self.pkgname):
            old_pkg = DebPackage('package', self.args[0], oldstyle=False)
            try:
                old_pkg.read_version_info()
            except PyCentralError:
                old_pkg.set_version_field(version_field)

            old_requested = pyversions.requested_versions_for_runtime(old_pkg.version_field, version_only=True)
            old_used_runtimes = [rt for rt in runtimes if rt.short_name in requested]
        else:
            old_pkg = None
            old_used_runtimes = []
        logging.debug('\tavail=%s, pkg=%s, prepare=%s'
                      % ([rt.short_name for rt in runtimes],
                         version_field,
                         [rt.short_name for rt in used_runtimes]))
        try:
            pkg.prepare(used_runtimes, old_used_runtimes, old_pkg)
        except PyCentralError, msg:
            self.error(msg)

register_action(ActionPkgPrepare)


class ActionBCRemove(Action):
    """remove the byte-compiled files in <package>.

    bccompile is a replacement for the current byte compilation
    generated by the dh_python debhelper script.
    """
    name = 'bcremove'
    help = 'remove the byte compiled .py files'
    usage = '<package>'

    def check_args(self, global_options):
        if len(self.args) != 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.pkgname = self.args[0]
        arch = self.get_arch()
        if not os.path.exists('/var/lib/dpkg/info/%s.list' % self.pkgname) \
           and not os.path.exists('/var/lib/dpkg/info/%s/%s.list' % (arch, self.pkgname)):
            self.error("package %s is not installed" % self.pkgname)
        return self.errors_occured

    def run(self, global_options):
        pkg = DebPackage('package', self.args[0], oldstyle=True)
        try:
            pkg.remove_bytecode()
        except PyCentralError, msg:
            self.error(msg)

register_action(ActionBCRemove)


class ActionPkgRemove(Action):
    """
    """
    name = 'pkgremove'
    help = 'remove a package installed for all supported runtimes'
    usage = '<package>'

    def check_args(self, global_options):
        if len(self.args) != 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.pkgname = self.args[0]
        arch = self.get_arch()
        if not os.path.exists('/var/lib/dpkg/info/%s.list' % self.pkgname) \
           and not os.path.exists('/var/lib/dpkg/info/%s/%s.list' % (arch, self.pkgname)):
            self.error("package %s is not installed" % self.pkgname)
        return self.errors_occured

    def run(self, global_options):
        runtimes = get_installed_runtimes(with_unsupported=True)
        pkg = DebPackage('package', self.args[0], oldstyle=False)
        pkg.read_version_info(use_default_if_missing=True)
        try:
            pkg.set_default_runtime_from_version_info()
        except ValueError:
            # original runtime is already removed, use the default for removal
            pkg.default_runtime = get_default_runtime()
        try:
            pkg.remove(runtimes, remove_script_files=True)
        except PyCentralError, msg:
            self.error(msg)

register_action(ActionPkgRemove)

class ActionPkgList(Action):
    name = 'pkglist'
    help = 'list all pycentral-managed files for <package>'
    usage = '<package>'

    def check_args(self, global_options):
        if len(self.args) != 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.pkgname = self.args[0]
        arch = self.get_arch()
        if not os.path.exists('/var/lib/dpkg/info/%s.list' % self.pkgname) \
           and not os.path.exists('/var/lib/dpkg/info/%s/%s.list' % (arch, self.pkgname)):
            self.error("package %s is not installed" % self.pkgname)
        return self.errors_occured

    def run(self, global_options):
        runtimes = get_installed_runtimes(with_unsupported=True)
        pkg = DebPackage('package', self.args[0], oldstyle=False)
        pkg.read_version_info()
        try:
            pkg.set_default_runtime_from_version_info()
        except ValueError:
            # original runtime may be removed, use the default
            pkg.default_runtime = get_default_runtime()
        try:
            for f in pkg.list(runtimes, list_script_files=True):
                print f
        except PyCentralError, msg:
            self.error(msg)

register_action(ActionPkgList)


class ActionRuntimeInstall(Action):
    name = 'rtinstall'
    help = 'make installed packages available for this runtime'

    def get_option_parser(self):
        if not self._option_parser:
            p = OptionParser()
            p.add_option('-i', '--ignore-errors',
                         help="ignore errors during byte compilations",
                         default=None, action='store', dest='ignore')
            self._option_parser = p
        return self._option_parser

    def check_args(self, global_options):
        if len(self.args) != 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.rtname = self.args[0]
        if self.rtname[-8:] == '-minimal':
            self.rtname = self.rtname[:-8]
        self.runtime = None
        for rt in get_installed_runtimes():
            if rt.name == self.rtname:
                self.runtime = rt
                break
        if not self.runtime:
            self.error('installed runtime %s not found' % self.rtname)
        self.rtname_short = self.rtname[6:]
        return self.errors_occured

    def run(self, global_options):
        packages = [(p, v) for p, v in read_dpkg_status()
                    if not p in (self.rtname, self.rtname+'-minimal')]
        needed_packages = []
        for pkgname, vstring in packages:
            try:
                requested = list(pyversions.requested_versions(vstring, version_only=True))
#                if not self.rtname_short in requested:
#                    logging.info("%s not in what requested_versions() returned, adding it anyway" % self.rtname_short)
#                    requested.append(self.rtname_short)

            except ValueError:
                logging.info('\tunsupported for %s: %s (%s)' % (self.rtname, pkgname, vstring))
                continue
            if self.runtime.short_name in requested:
                needed_packages.append((pkgname, vstring, requested))
        logging.info('\t%d packages with Python-Version info installed, %d for %s'
                     % (len(packages), len(needed_packages), self.rtname))

        # XXX not needed for an upgrade of a runtime
        byte_compile_for_default = (self.runtime == default_runtime)

        bc_option = get_debian_config().get('DEFAULT', 'byte-compile')
        for pkgname, vstring, vinfo in needed_packages:
            try:
                logging.info('\tsupport %s for %s' % (pkgname, self.rtname))
                pkg = DebPackage('package', pkgname, oldstyle=False)
                pkg.read_version_info()
                try:
                    pkg.set_default_runtime_from_version_info()
                except ValueError:
                    logging.warn('\t%s not available for %s (%s)'
                                 % (pkgname, self.rtname, pkg.version_field))
                pkg.install([self.runtime], bc_option, None,
                            byte_compile_for_default,
                            ignore_errors = self.options.ignore != None)
            except PyCentralError, msg:
                self.error('package %s: %s' % (pkgname, msg))

register_action(ActionRuntimeInstall)

class ActionRuntimeRemove(Action):
    name = 'rtremove'
    help = 'remove packages installed for this runtime'

    def check_args(self, global_options):
        if len(self.args) != 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.rtname = self.args[0]
        if self.rtname[-8:] == '-minimal':
            self.rtname = self.rtname[:-8]
        self.runtime = None
        for rt in get_installed_runtimes(with_unsupported=True):
            if rt.name == self.rtname:
                self.runtime = rt
                break
        if not self.runtime:
            self.error('installed runtime %s not found' % self.rtname)
        return self.errors_occured

    def run(self, global_options):
        packages = [(p, v) for p, v in read_dpkg_status(verbose=True)
                    if not p in (self.rtname, self.rtname+'-minimal')]
        needed_packages = []
        import subprocess
        for pkgname, vstring in packages:
            if not os.path.exists('/var/lib/dpkg/info/%s.list' % pkgname) \
               and not os.path.exists('/var/lib/dpkg/info/%s/%s.list' % (arch, pkgname)):
                # already removed, but /var/lib/dpkg/status not yet updated
                continue
            cmd = ['/usr/bin/dpkg-query', '-W', '-f', '${Status}\n', pkgname]
            p = subprocess.Popen(cmd, bufsize=1,
                                 shell=False, stdout=subprocess.PIPE)
            fd = p.stdout
            status = fd.readline().strip().split()
            fd.close()
            if not 'installed' in status:
                # already removed, but /var/lib/dpkg/status not yet updated
                continue
            try:
                requested = list(pyversions.requested_versions_for_runtime(vstring, version_only=True))
            except ValueError:
                logging.info('\tunsupported for %s: %s (%s)' % (self.rtname, pkgname, vstring))
                continue
            if self.runtime.short_name in requested:
                needed_packages.append((pkgname, vstring, requested))
        logging.info('\t%d pycentral supported packages installed, %d for %s'
                     % (len(packages), len(needed_packages), self.rtname))
        failed = []
        for pkgname, vstring, vinfo in needed_packages:
            logging.info('\trtremove: remove package %s for %s' % (pkgname, self.rtname))
            pkg = DebPackage('package', pkgname)
            pkg.set_version_field(vstring)
            try:
                pkg.set_default_runtime_from_version_info()
            except ValueError:
                # original runtime is already removed, use the default for removal
                pkg.default_runtime = get_default_runtime()
            try:
                pkg.remove([self.runtime], remove_script_files=False)
            except PyCentralError, msg:
                self.error('failed to remove %s support for package %s' % (self.rtname, pkgname), go_on=True)
                failed.append(pkgname)
        if failed:
            self.error('failed to remove %s support for %d packages' % len(failed))

register_action(ActionRuntimeRemove)


class ActionUpdateDefault(Action):
    name = 'updatedefault'
    help = 'update the default python version'
    usage = '<old runtime> <new runtime>'

    def check_args(self, global_options):
        if len(self.args) != 2:
            self._option_parser.print_help()
            sys.exit(1)
        self.oldrtname = self.args[0]
        self.rtname = self.args[1]
        packages = read_dpkg_status()
        self.needed_packages = []
        for pkgname, vstring in packages:
            if vstring.find('current') == -1:
                continue
            try:
                versions = pyversions.requested_versions(vstring, version_only=True)
            except ValueError:
                self.error("package %s is not ready to be updated for %s"
                           % (pkgname, self.rtname))
                continue
            pkg = DebPackage('package', pkgname)
            self.needed_packages.append(pkg)
        return self.errors_occured

    def run(self, global_options):
        logging.info('\tupdate default: update %d packages for %s'
                     % (len(self.needed_packages), self.rtname))
        runtimes = get_installed_runtimes()
        default_rt = get_default_runtime()
        bc_option = get_debian_config().get('DEFAULT', 'byte-compile')
        try:
            for pkg in self.needed_packages:
                pkg.read_version_info()
                pkg.set_default_runtime_from_version_info()
                if pkg.shared_files or pkg.private_files:
                    pkg.update_bytecode_files(runtimes, default_rt, bc_option)
        except PyCentralError, msg:
            self.error(msg)

register_action(ActionUpdateDefault)


class ActionCleanupPkgPrepareUpdates(Action):
    name = 'cleanup-pkgprepare-updates'
    help = 'cleanup dangling symlinks and byte code files'

    def check_args(self, global_options):
        if len(self.args) != 0:
            self._option_parser.print_help()
            sys.exit(1)
        return self.errors_occured

    def run(self, global_options):
        # needs to be done for 2.3, 2.4 and 2.5 (2.6 packages were not built
        # using python-central 0.6.8.
        dangling = self.check_dangling_links()
        if not dangling:
            return self.errors_occured
        self.warn("found %d dangling symlinks" % len(dangling))
        # check if these belong to packages
        self.warn("checking for links owned by packages (this may take some time)")
        packaged = self.links_in_packages(dangling)
        unowned = [link for link in dangling if not link in packaged]
        
        # then remove those not belonging to any package
        dirs = []
        removed = 0
        for fn in unowned:
            try:
                os.unlink(fn + 'c')
                os.unlink(fn + 'o')
            except OSError, e:
                pass
            try:
                os.unlink(fn)
                removed += 1
                if not os.path.dirname(fn) in dirs:
                    dirs.append(os.path.dirname(fn))
            except OSError, e:
                self.error("error removing dangling symlink: %s" % str(e),
                           go_on=True)
        for d in dirs:
            try:
                os.rmdir(d)
            except OSError, e:
                pass

        if len(packaged) > 0:
            for fn in packaged:
                self.warn('dangling symlink owned by package: %s' % fn)
        if removed:
            self.warn('removed %d dangling symlinks not owned by any package' % removed)
        return self.errors_occured

    def check_dangling_links(self):
        dangling = []
        for pv in ('2.3', '2.4', '2.5'):
            sdir = '/usr/lib/python%s/site-packages' % pv
            if not os.path.isdir(sdir):
                continue
            for root, dirs, files in os.walk(sdir, topdown=False):
                # remember files
                # rember dirs with removed files and try to remove these
                for name in files:
                    link = os.path.join(root, name)
                    if not os.path.islink(link):
                        continue
                    target = os.readlink(link)
                    if target.startswith('..'):
                        target = os.path.normpath(target)
                    if os.path.exists(target):
                        continue
                    if not target.startswith('/usr/share/pyshared/'):
                        continue
                    dangling.append(link)
        return dangling

    def locate(self, arg_list, cmd_list):
        from subprocess import PIPE, Popen
        (stdout, stderr) = Popen(cmd_list + arg_list,
                                 env={"LANG" : "C"},
                                 shell=False,
                                 stdout=PIPE, stderr=PIPE).communicate()
        if stdout:
            for line in map(string.strip, stdout.split("\n")):
                if line == '': continue
                fn = line.split(':', 1)[1][1:]
                if fn in arg_list:
                    self.packaged.add(fn)
        if stderr:
            for line in map(string.strip, stderr.split("\n")):
                if line == '': continue
                fn = line.split(':', 1)[1][1:-11]
                self.notfound += 1

    def links_in_packages(self, links):
        def chunks(l):
            if 'SC_ARG_MAX' in os.sysconf_names:
                chunk_max = os.sysconf('SC_ARG_MAX')
            else:
                chunk_max = 16384
            chunk_max -= reduce(lambda x,y: x+y,
                                [len(k)+len(v)+3 for k,v in os.environ.items()])
            chunk_max -= 20
            chunk = []
            lchunk = 0
            all_chunks = []
            for i in l:
                if lchunk + len(i)+1 > chunk_max:
                    all_chunks.append(chunk)
                    chunk = []
                    lchunk = 0
                chunk.append(i)
                lchunk += len(i) + 1
            if chunk:
                all_chunks.append(chunk)
            return all_chunks

        self.packaged = set()
        self.notfound = 0
        all_chunks = chunks(links)
        arg_list = all_chunks[0]
        cmd_list = ["dpkg","-S"]
        self.locate(arg_list, cmd_list)
        for arg_list in all_chunks[1:]:
            self.locate(arg_list, cmd_list)
        return self.packaged

register_action(ActionCleanupPkgPrepareUpdates)

class ActionList(Action):
    name = 'list'
    help = 'List all pycentral-managed files'

    def check_args(self, global_options):
        if len(self.args) != 0:
            self._option_parser.print_help()
            sys.exit(1)
        return self.errors_occured

    def run(self, global_options):
        runtimes = get_installed_runtimes(with_unsupported=True)
        for (p, v) in read_dpkg_status():
            pkg = DebPackage('package', p)
            pkg.read_version_info()
            for f in pkg.list(runtimes, list_script_files=True):
                print f

register_action(ActionList)


class ActionShowDefault(Action):
    name = 'showdefault'
    help = 'Show default python version number'

    def check_args(self, global_options):
        if len(self.args) != 0:
            self._option_parser.print_help()
            sys.exit(1)
        return self.errors_occured

    def run(self, global_options):
        print pyversions.default_version(version_only=True)
        sys.stderr.write("pycentral showdefault is deprecated, use `pyversions -vd'\n")

register_action(ActionShowDefault)


class ActionShowVersions(Action):
    name = 'showversions'
    help = 'Show version numbers of supported python versions'

    def check_args(self, global_options):
        if len(self.args) != 0:
            self._option_parser.print_help()
            sys.exit(1)

        return self.errors_occured

    def run(self, global_options):
        supported = pyversions.supported_versions()
        versions = [d[6:] for d in supported if re.match(r'python\d\.\d', d)]
        print ' '.join(versions)
        sys.stderr.write("pycentral showversions is deprecated, use `pyversions -vs'\n")

register_action(ActionShowVersions)

class ActionShowSupported(Action):
    name = 'showsupported'
    help = 'Show the supported python versions'

    def check_args(self, global_options):
        if len(self.args) != 0:
            self._option_parser.print_help()
            sys.exit(1)
        return self.errors_occured

    def run(self, global_options):
        supported = pyversions.supported_versions()
        print ' '.join(supported)
        sys.stderr.write("pycentral showsupported is deprecated, use `pyversions -s'\n")

register_action(ActionShowSupported)


class ActionPyCentralDir(Action):
    name = 'pycentraldir'
    help = 'Show the pycentral installation directory for the package'
    usage = '<package>'

    def check_args(self, global_options):
        if len(self.args) != 1:
            self._option_parser.print_help()
            sys.exit(1)
        self.pkgname = self.args[0]
        return self.errors_occured

    def run(self, global_options):
        if shared_base2[-1] == '/':
            print shared_base2[:-1]
        else:
            print shared_base2

register_action(ActionPyCentralDir)


class ActionVersion(Action):
    name = 'version'
    help = 'Show the pycentral version'

    def check_args(self, global_options):
        if len(self.args) != 0:
            self._option_parser.print_help()
            sys.exit(1)

        return self.errors_occured

    def run(self, global_options):
        sys.stdout.write("%s\n" % pycentral_version)

register_action(ActionVersion)


class ActionDebhelper(Action):
    name = 'debhelper'
    help = 'move files to pycentral location, variable substitutions'
    usage = '[-p|--provides] [--no-move|--include-links] <package> [<package directory>]'

    def get_option_parser(self):
        if not self._option_parser:
            envvar = os.environ.get('DH_PYCENTRAL', '')
            substvars_default = 'no'
            if 'substvars=file' in envvar:
                substvars_default = 'file'
            if 'substvars=stdout' in envvar:
                substvars_default = 'stdout'

            p = OptionParser()
            p.add_option('-p', '--provides',
                         help="generate substitution for python:Provides",
                         default='add-provides' in envvar, action='store_true', dest='provides')
            p.add_option('--no-move', '--nomove',
                         help="do not move files to pycentral location",
                         default='no-move' in envvar or 'nomove' in envvar, action='store_true', dest='nomove')
            p.add_option('--include-links',
                         help="include links to the pycentral location",
                         default='include-links' in envvar, action='store_true', dest='includelinks')
            p.add_option('--stdout',
                         help="just print substitution variables to stdout",
                         default='stdout' in envvar, action='store_true', dest='stdout')
            p.add_option('--substvars',
                         help="where to print substitution vars (no, file, stdout)",
                         default=substvars_default, dest='substvars')
            p.add_option('--no-act', '--dry-run',
                         help="dry run",
                         default=('dry-run' in envvar) or ('no-act' in envvar),
                         action='store_true', dest='dryrun')
            self._option_parser = p
        return self._option_parser

    def check_args(self, global_options):
        if not len(self.args) in (1, 2):
            self._option_parser.print_help()
            sys.exit(1)
        if 'file' in self.options.substvars:
            self.options.substvars = 'file'
        if 'stdout' in self.options.substvars:
            self.options.substvars = 'stdout'
        if self.options.nomove and self.options.includelinks:
            self.error("options `--no-move' and `--include-links' conflict")
        return self.errors_occured

    def run(self, global_options):
        if len(self.args) < 2:
            pkgdir = 'debian/' + self.args[0]
        else:
            pkgdir = self.args[1]
        try:
            pkg = DebPackage('package', self.args[0], pkgdir=pkgdir,
                             parse_versions=self.options.substvars!='no')
            if not self.options.nomove:
                pkg.move_files()
            if self.options.includelinks:
                pkg.add_shared_links()
            pkg.read_pyfiles()
            if self.options.substvars!='no':
                pkg.gen_substvars()
        except PyCentralVersionMissingError, msg:
            self.warn(msg)
            return
        except PyCentralError, msg:
            self.error(msg)

        out = None
        if self.options.stdout or self.options.substvars == 'stdout':
            out = sys.stdout
        elif self.options.substvars == 'file':
            out = file('debian/%s.substvars' % pkg.name, 'a+')
        if out:
            out.write('python:Versions=%s\n' % pkg.version_field)
            out.write('python:Depends=%s\n' % pkg.depends)
            out.write('python:Provides=%s\n' % pkg.provides)


register_action(ActionDebhelper)

# match a string with the list of available actions
def action_matches(action, actions):
    prog = re.compile('[^-]*?-'.join(action.split('-')))
    return [a for a in actions if prog.match(a)]

def usage(stream, msg=None):
    print >>stream, msg
    print >>stream, "use `%s --help' for help on actions and arguments" % program
    print >>stream
    sys.exit(1)

# parse command line arguments
def parse_options(args):
    shortusage = 'usage: %s [<option> ...] <action> <pkgname>' % program
    parser = OptionParser(usage=shortusage)
    parser.disable_interspersed_args()

    # setup the parsers object
    parser.remove_option('-h')
    parser.add_option('-h', '--help',
                      help='help screen',
                      action='store_true', dest='help')
    parser.add_option('-v', '--verbose',
                      help='verbose mode',
                      action='store_true', dest='verbose')

    global_options, args = parser.parse_args()
    # Print the help screen and exit
    if len(args) == 0 or global_options.help:
        parser.print_help()
        print "\nactions:"
        action_names = known_actions.keys()
        action_names.sort()
        for n in action_names:
            print "  %-21s %s" % (n, known_actions[n].help)
        print ""
        sys.exit(1)

    # check if the specified action really exists
    action_name = args[0]
    del args[0]
    matching_actions = action_matches(action_name, known_actions.keys())
    if len(matching_actions) == 0:
        usage(sys.stderr, "unknown action `%s'" % action_name)
    elif len(matching_actions) > 1:
        usage(sys.stderr,
              "ambiguous action `%s', matching actions: %s"
              % (action_name, str(list(matching_actions))))
    else:
        action_name = matching_actions[0]

    # instantiate an object for the action and parse the remaining arguments
    action = known_actions[action_name]()
    action_options, action_names = action.parse_args(args)

    return global_options, action

class Logging:
    DEBUG, INFO, WARN, ERROR = range(4)
    def __init__(self, level=WARN):
        self.fd = None
        self.level = level
        try:
            self.fd = file('/var/log/pycentral.log', 'a+')
        except IOError:
            self.fd = None
    def msg(self, level, s):
        if level < self.level:
            return
        d = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
        if self.fd:
            self.fd.write('%s %s %s\n' % (d, level, s))
            self.fd.flush()
        sys.stdout.write('pycentral: %s\n' % (s))
        sys.stdout.flush()
    def info(self, s):
        self.msg(self.INFO, s)
    def warn(self, s):
        self.msg(self.WARN, s)
    def error(self, s):
        self.msg(self.ERROR, s)
        sys.stderr.write('%s\n' % s)
    def debug(self, s):
        self.msg(self.DEBUG, s)

    def debug_list(self, tab, s, l, n=4):
        l2 = l[:min(n, len(l))]
        if len(l) > n:
            l2.append('...')
        self.msg(self.DEBUG, "%s%s (%s/%s)" % (tab, s, len(l2), len(l)))
        if len(l2) > 0:
            logging.debug('%s    %s' % (tab, l2))

def setup_logging(loglevel=Logging.WARN, verbose=False):
    levels = ['debug', 'info', 'warn', 'error']
    env_level = os.environ.get('PYCENTRAL', 'warn').lower()
    for i in range(len(levels)):
        if env_level.find(levels[i]) != -1:
            loglevel = i
    if verbose:
        loglevel = Logging.DEBUG
    global logging
    logging = Logging(loglevel)

def main():
    global_options, action = parse_options(sys.argv[1:])

    os.umask(0022)

    # setup logging stuff
    setup_logging(Logging.WARN, global_options.verbose)
    if action.name == 'debhelper' or action.name.startswith('show'):
        pass
    else:
        logging.debug('pycentral ' + ' '.join(sys.argv[1:]))

    # check the arguments according to the action called
    if action.check_args(global_options):
        sys.exit(1)

    # run the action and exit
    rv = action.run(global_options)
    sys.exit(rv)


# call the main routine
if __name__ == '__main__':
    main()
