# $Id$

# This is the X Strike Force shell library for X Window System package
# maintainer scripts.  It serves to define shell functions commonly used by
# such packages, and performs some error checking necessary for proper operation
# of those functions.  By itself, it does not "do" much; the maintainer scripts
# invoke the functions defined here to accomplish package installation and
# removal tasks.

# If you are reading this within a Debian package maintainer script (e.g.,
# /var/lib/dpkg)info/PACKAGE.{config,preinst,postinst,prerm,postrm}), you can
# skip past this library by scanning forward in this file to the string
# "GOBSTOPPER".

SOURCE_VERSION=@SOURCE_VERSION@
OFFICIAL_BUILD=@OFFICIAL_BUILD@

# Use special abnormal exit codes so that problems with this library are more
# easily tracked down.
SHELL_LIB_INTERNAL_ERROR=86
SHELL_LIB_THROWN_ERROR=74
SHELL_LIB_USAGE_ERROR=99

# old -> new variable names
if [ -z "$DEBUG_XORG_PACKAGE" ] && [ -n "$DEBUG_XFREE86_PACKAGE" ]; then
  DEBUG_XORG_PACKAGE="$DEBUG_XFREE86_PACKAGE"
fi
if [ -z "$DEBUG_XORG_DEBCONF" ] && [ -n "$DEBUG_XFREE86_DEBCONF" ]; then
  DEBUG_XORG_DEBCONF="$DEBUG_XFREE86_DEBCONF"
fi

# initial sanity checks
if [ -z "$THIS_PACKAGE" ]; then
  cat >&2 <<EOF
Error: package maintainer script attempted to use shell library without
definining \$THIS_PACKAGE shell variable.  Please report the package name,
version, and the text of this error message to the Debian Bug Tracking System.
Visit <http://www.debian.org/Bugs/Reporting> on the World Wide Web for
instructions, read the file /usr/share/doc/debian/bug-reporting.txt from the
"doc-debian" package, or install the "reportbug" package and use the command of
the same name to file a report against version $SOURCE_VERSION of this package.
EOF
  exit $SHELL_LIB_USAGE_ERROR
fi

if [ -z "$THIS_SCRIPT" ]; then
  cat >&2 <<EOF
Error: package maintainer script attempted to use shell library without
definining \$THIS_SCRIPT shell variable.  Please report the package name,
version, and the text of this error message to the Debian Bug Tracking System.
Visit <http://www.debian.org/Bugs/Reporting> on the World Wide Web for
instructions, read the file /usr/share/doc/debian/bug-reporting.txt from the
"doc-debian" package, or install the "reportbug" package and use the command of
the same name to file a report against version $SOURCE_VERSION of the
"$THIS_PACKAGE" package.
EOF
  exit $SHELL_LIB_USAGE_ERROR
fi

ARCHITECTURE="$(dpkg --print-installation-architecture)"

if [ "$1" = "reconfigure" ] || [ -n "$DEBCONF_RECONFIGURE" ]; then
  RECONFIGURE="true"
else
  RECONFIGURE=
fi

if ([ "$1" = "install" ] || [ "$1" = "configure" ]) && [ -z "$2" ]; then
  FIRSTINST="yes"
fi

if [ -z "$RECONFIGURE" ] && [ -z "$FIRSTINST" ]; then
  UPGRADE="yes"
fi

trap "message;\
      message \"Received signal.  Aborting $THIS_PACKAGE package $THIS_SCRIPT script.\";\
      message;\
      exit 1" HUP INT QUIT TERM

reject_nondigits () {
  # syntax: reject_nondigits [ operand ... ]
  #
  # scan operands (typically shell variables whose values cannot be trusted) for
  # characters other than decimal digits and barf if any are found
  while [ -n "$1" ]; do
    # does the operand contain anything but digits?
    if ! expr "$1" : "[[:digit:]]\+$" > /dev/null 2>&1; then
      # can't use die(), because it wraps message() which wraps this function
      echo "$THIS_PACKAGE $THIS_SCRIPT error: reject_nondigits() encountered" \
           "possibly malicious garbage \"$1\"" >&2
      exit $SHELL_LIB_THROWN_ERROR
    fi
    shift
  done
}

reject_whitespace () {
  # syntax: reject_whitespace [ operand ]
  #
  # scan operand (typically a shell variable whose value cannot be trusted) for
  # whitespace characters and barf if any are found
  if [ -n "$1" ]; then
    # does the operand contain any whitespace?
    if expr "$1" : "[[:space:]]" > /dev/null 2>&1; then
      # can't use die(), because I want to avoid forward references
      echo "$THIS_PACKAGE $THIS_SCRIPT error: reject_whitespace() encountered" \
           "possibly malicious garbage \"$1\"" >&2
      exit $SHELL_LIB_THROWN_ERROR
    fi
  fi
}

reject_unlikely_path_chars () {
  # syntax: reject_unlikely_path_chars [ operand ... ]
  #
  # scan operands (typically shell variables whose values cannot be trusted) for
  # characters unlikely to be seen in a path and which the shell might
  # interpret and barf if any are found
  while [ -n "$1" ]; do
    # does the operand contain any funny characters?
    if expr "$1" : '.*[!$&()*;<>?|].*' > /dev/null 2>&1; then
      # can't use die(), because I want to avoid forward references
      echo "$THIS_PACKAGE $THIS_SCRIPT error: reject_unlikely_path_chars()" \
           "encountered possibly malicious garbage \"$1\"" >&2
      exit $SHELL_LIB_THROWN_ERROR
    fi
    shift
  done
}

# Query the terminal to establish a default number of columns to use for
# displaying messages to the user.  This is used only as a fallback in the
# event the COLUMNS variable is not set.  ($COLUMNS can react to SIGWINCH while
# the script is running, and this cannot, only being calculated once.)
DEFCOLUMNS=$(stty size 2> /dev/null | awk '{print $2}') || true
if ! expr "$DEFCOLUMNS" : "[[:digit:]]\+$" > /dev/null 2>&1; then
  DEFCOLUMNS=80
fi

message () {
  # pretty-print messages of arbitrary length
  reject_nondigits "$COLUMNS"
  echo "$*" | fmt -t -w ${COLUMNS:-$DEFCOLUMNS} >&2
}

observe () {
  # syntax: observe message ...
  #
  # issue observational message suitable for logging someday when support for
  # it exists in dpkg
  if [ -n "$DEBUG_XORG_PACKAGE" ]; then
    message "$THIS_PACKAGE $THIS_SCRIPT note: $*"
  fi
}

warn () {
  # syntax: warn message ...
  #
  # issue warning message suitable for logging someday when support for
  # it exists in dpkg; also send to standard error
  message "$THIS_PACKAGE $THIS_SCRIPT warning: $*"
}

die () {
  # syntax: die message ...
  #
  # exit script with error message
  message "$THIS_PACKAGE $THIS_SCRIPT error: $*"
  exit $SHELL_LIB_THROWN_ERROR
}

internal_error () {
  # exit script with error; essentially a "THIS SHOULD NEVER HAPPEN" message
  message "internal error: $*"
  if [ -n "$OFFICIAL_BUILD" ]; then
    message "Please report a bug in the $THIS_SCRIPT script of the" \
            "$THIS_PACKAGE package, version $SOURCE_VERSION to the Debian Bug" \
            "Tracking System.  Include all messages above that mention the" \
            "$THIS_PACKAGE package.  Visit " \
            "<http://www.debian.org/Bugs/Reporting> on the World Wide Web for" \
            "instructions, read the file" \
            "/usr/share/doc/debian/bug-reporting.txt from the doc-debian" \
            "package, or install the reportbug package and use the command of" \
            "the same name to file a report."
  fi
  exit $SHELL_LIB_INTERNAL_ERROR
}

usage_error () {
  message "usage error: $*"
  message "Please report a bug in the $THIS_SCRIPT script of the" \
          "$THIS_PACKAGE package, version $SOURCE_VERSION to the Debian Bug" \
          "Tracking System.  Include all messages above that mention the" \
          "$THIS_PACKAGE package.  Visit " \
          "<http://www.debian.org/Bugs/Reporting> on the World Wide Web for" \
          "instructions, read the file" \
          "/usr/share/doc/debian/bug-reporting.txt from the doc-debian" \
          "package, or install the reportbug package and use the command of" \
          "the same name to file a report."
  exit $SHELL_LIB_USAGE_ERROR
}


maplink () {
  # returns what symlink should point to; i.e., what the "sane" answer is
  # Keep this in sync with the debian/*.links files.
  # This is only needed for symlinks to directories.
  #
  # XXX: Most of these look wrong in the X11R7 world and need to be fixed.
  # If we've stopped using this function, fixing it might enable us to re-enable
  # it again and catch more errors.
  case "$1" in
    /etc/X11/xkb/compiled) echo /var/lib/xkb ;;
    /etc/X11/xkb/xkbcomp) echo /usr/X11R6/bin/xkbcomp ;;
    /usr/X11R6/lib/X11/app-defaults) echo /etc/X11/app-defaults ;;
    /usr/X11R6/lib/X11/fs) echo /etc/X11/fs ;;
    /usr/X11R6/lib/X11/lbxproxy) echo /etc/X11/lbxproxy ;;
    /usr/X11R6/lib/X11/proxymngr) echo /etc/X11/proxymngr ;;
    /usr/X11R6/lib/X11/rstart) echo /etc/X11/rstart ;;
    /usr/X11R6/lib/X11/twm) echo /etc/X11/twm ;;
    /usr/X11R6/lib/X11/xdm) echo /etc/X11/xdm ;;
    /usr/X11R6/lib/X11/xinit) echo /etc/X11/xinit ;;
    /usr/X11R6/lib/X11/xkb) echo /etc/X11/xkb ;;
    /usr/X11R6/lib/X11/xserver) echo /etc/X11/xserver ;;
    /usr/X11R6/lib/X11/xsm) echo /etc/X11/xsm ;;
    /usr/bin/X11) echo ../X11R6/bin ;;
    /usr/bin/rstartd) echo ../X11R6/bin/rstartd ;;
    /usr/include/X11) echo ../X11R6/include/X11 ;;
    /usr/lib/X11) echo ../X11R6/lib/X11 ;;
    *) internal_error "maplink() called with unknown path \"$1\"" ;;
  esac
}

analyze_path () {
  # given a supplied set of pathnames, break each one up by directory and do an
  # ls -dl on each component, cumulatively; i.e.
  # analyze_path /usr/X11R6/bin -> ls -dl /usr /usr/X11R6 /usr/X11R6/bin
  # Thanks to Randolph Chung for this clever hack.

  #local f g

  while [ -n "$1" ]; do
    reject_whitespace "$1"
    _g=
    message "Analyzing $1:"
    for _f in $(echo "$1" | tr / \  ); do
      if [ -e /$_g$_f ]; then
        ls -dl /$_g$_f /$_g$_f.dpkg-* 2> /dev/null || true
        _g=$_g$_f/
      else
        message "/$_g$_f: nonexistent; directory contents of /$_g:"
        ls -l /$_g
        break
      fi
    done
    shift
  done
}

find_culprits () {
  #local f p dpkg_info_dir possible_culprits smoking_guns bad_packages package \
  #  msg

  reject_whitespace "$1"
  message "Searching for overlapping packages..."
  _dpkg_info_dir=/var/lib/dpkg/info
  if [ -d $_dpkg_info_dir ]; then
    if [ "$(echo $_dpkg_info_dir/*.list)" != "$_dpkg_info_dir/*.list" ]; then
      _possible_culprits=$(ls -1 $_dpkg_info_dir/*.list | egrep -v \
        "(xbase-clients|x11-common|xfs|xlibs)")
      if [ -n "$_possible_culprits" ]; then
        _smoking_guns=$(grep -l "$1" $_possible_culprits || true)
        if [ -n "$_smoking_guns" ]; then
          _bad_packages=$(printf "\\n")
          for f in $_smoking_guns; do
            # too bad you can't nest parameter expansion voodoo
            p=${f%*.list}      # strip off the trailing ".list"
            _package=${p##*/}   # strip off the directories
            _bad_packages=$(printf "%s\n%s" "$_bad_packages" "$_package")
          done
          _msg=$(cat <<EOF
The following packages appear to have file overlaps with the X.Org packages;
these packages are either very old, or in violation of Debian Policy.  Try
upgrading each of these packages to the latest available version if possible:
for example, with the command "apt-get install".  If no newer version of a
package is available, you will have to remove it; for example, with the command
"apt-get remove".  If even the latest available version of the package has
this file overlap, please file a bug against that package with the Debian Bug
Tracking System.  You may want to refer the package maintainer to section 12.8
of the Debian Policy manual.
EOF
)
          message "$_msg"
          message "The overlapping packages are: $_bad_packages"
        else
          message "no overlaps found."
        fi
      fi
    else
      message "cannot search; no matches for $_dpkg_info_dir/*.list."
    fi
  else
    message "cannot search; $_dpkg_info_dir does not exist."
  fi
}

# we require a readlink command or shell function
if ! which readlink > /dev/null 2>&1; then
  message "The readlink command was not found.  Please install version" \
          "1.13.1 or later of the debianutils package."
  readlink () {
    # returns what symlink in $1 actually points to
    perl -e '$l = shift; exit 1 unless -l $l; $r = readlink $l; exit 1 unless $r; print "$r\n"' "$1"
  }
fi

check_symlink () {
  # syntax: check_symlink symlink
  #
  # See if specified symlink points where it is supposed to.  Return 0 if it
  # does, and 1 if it does not.
  #
  # Primarily used by check_symlinks_and_warn() and check_symlinks_and_bomb().

  #local symlink

  # validate arguments
  if [ $# -ne 1 ]; then
    usage_error "check_symlink() called with wrong number of arguments;" \
                "expected 1, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  _symlink="$1"

  if [ "$(maplink "$_symlink")" = "$(readlink "$_symlink")" ]; then
    return 0
  else
    return 1
  fi
}

check_symlinks_and_warn () {
  # syntax: check_symlinks_and_warn symlink ...
  #
  # For each argument, check for symlink sanity, and warn if it isn't sane.
  #
  # Call this function from a preinst script in the event $1 is "upgrade" or
  # "install".

  #local errmsg symlink

  # validate arguments
  if [ $# -lt 1 ]; then
    usage_error "check_symlinks_and_warn() called with wrong number of" \
                "arguments; expected at least 1, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  while [ -n "$1" ]; do
    _symlink="$1"
    if [ -L "$_symlink" ]; then
      if ! check_symlink "$_symlink"; then
        observe "$_symlink symbolic link points to wrong location" \
                "$(readlink "$_symlink"); removing"
        rm "$_symlink"
      fi
    elif [ -e "$_symlink" ]; then
      _errmsg="$_symlink exists and is not a symbolic link; this package cannot"
      _errmsg="$_errmsg be installed until this"
      if [ -f "$_symlink" ]; then
        _errmsg="$_errmsg file"
      elif [ -d "$_symlink" ]; then
        _errmsg="$_errmsg directory"
      else
        _errmsg="$_errmsg thing"
      fi
      _errmsg="$_errmsg is removed"
      die "$_errmsg"
    fi
    shift
  done
}

check_symlinks_and_bomb () {
  # syntax: check_symlinks_and_bomb symlink ...
  #
  # For each argument, check for symlink sanity, and bomb if it isn't sane.
  #
  # Call this function from a postinst script.

  #local problem symlink

  # validate arguments
  if [ $# -lt 1 ]; then
    usage_error "check_symlinks_and_bomb() called with wrong number of"
                "arguments; expected at least 1, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  while [ -n "$1" ]; do
    _problem=
    _symlink="$1"
    if [ -L "$_symlink" ]; then
      if ! check_symlink "$_symlink"; then
        _problem=yes
        warn "$_symlink symbolic link points to wrong location" \
             "$(readlink "$_symlink")"
      fi
    elif [ -e "$_symlink" ]; then
      _problem=yes
      warn "$_symlink is not a symbolic link"
    else
      _problem=yes
      warn "$_symlink symbolic link does not exist"
    fi
    if [ -n "$_problem" ]; then
      analyze_path "$_symlink" "$(readlink "$_symlink")"
      find_culprits "$_symlink"
      die "bad symbolic links on system"
    fi
    shift
  done
}

font_update () {
  # run $UPDATECMDS in $FONTDIRS

  #local dir cmd shortcmd x_font_dir_prefix

  _x_font_dir_prefix="/usr/share/fonts/X11"

  if [ -z "$UPDATECMDS" ]; then
    usage_error "font_update() called but \$UPDATECMDS not set"
  fi
  if [ -z "$FONTDIRS" ]; then
    usage_error "font_update() called but \$FONTDIRS not set"
  fi

  reject_unlikely_path_chars "$UPDATECMDS"
  reject_unlikely_path_chars "$FONTDIRS"

  for _dir in $FONTDIRS; do
    if [ -d "$_x_font_dir_prefix/$_dir" ]; then
      for _cmd in $UPDATECMDS; do
        if which "$_cmd" > /dev/null 2>&1; then
          _shortcmd=${_cmd##*/}
          observe "running $_shortcmd in $_dir font directory"
	  _cmd_opts=
          if [ "$_shortcmd" = "update-fonts-alias" ]; then
            _cmd_opts=--x11r7-layout
          fi
          if [ "$_shortcmd" = "update-fonts-dir" ]; then
            _cmd_opts=--x11r7-layout
          fi
          if [ "$_shortcmd" = "update-fonts-scale" ]; then
            _cmd_opts=--x11r7-layout
          fi
          $_cmd $_cmd_opts $_dir || warn "$_cmd $_cmd_opts $_dir" \
                              "failed; font directory data may not" \
                              "be up to date"
        else
          warn "$_cmd not found; not updating corresponding $_dir font" \
               "directory data"
        fi
      done
    else
      warn "$_dir is not a directory; not updating font directory data"
    fi
  done
}

remove_conffile_prepare () {
  # syntax: remove_conffile_prepare filename official_md5sum ...
  #
  # Check a conffile "filename" against a list of canonical MD5 checksums.
  # If the file's current MD5 checksum matches one of the "official_md5sum"
  # operands provided, then prepare the conffile for removal from the system.
  # We defer actual deletion until the package is configured so that we can
  # roll this operation back if package installation fails.
  #
  # Call this function from a preinst script in the event $1 is "upgrade" or
  # "install" and verify $2 to ensure the package is being upgraded from a
  # version (or installed over a version removed-but-not-purged) prior to the
  # one in which the conffile was obsoleted.

  #local conffile current_checksum

  # validate arguments
  if [ $# -lt 2 ]; then
    usage_error "remove_conffile_prepare() called with wrong number of" \
                "arguments; expected at least 2, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  _conffile="$1"
  shift

  # does the _conffile even exist?
  if [ -e "$_conffile" ]; then
    # calculate its checksum
    _current_checksum=$(md5sum < "$_conffile" | sed 's/[[:space:]].*//')
    # compare it to each supplied checksum
    while [ -n "$1" ]; do
      if [ "$_current_checksum" = "$1" ]; then
        # we found a match; move the confffile and stop looking
        observe "preparing obsolete conffile $_conffile for removal"
        mv "$_conffile" "$_conffile.$THIS_PACKAGE-tmp"
        break
      fi
      shift
    done
  fi
}

remove_conffile_commit () {
  # syntax: remove_conffile_commit filename
  #
  # Complete the removal of a conffile "filename" that has become obsolete.
  #
  # Call this function from a postinst script after having used
  # remove_conffile_prepare() in the preinst.

  #local conffile

  # validate arguments
  if [ $# -ne 1 ]; then
    usage_error "remove_conffile_commit() called with wrong number of" \
                "arguments; expected 1, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  _conffile="$1"

  # if the temporary file created by remove_conffile_prepare() exists, remove it
  if [ -e "$_conffile.$THIS_PACKAGE-tmp" ]; then
    observe "committing removal of obsolete conffile $_conffile"
    rm "$_conffile.$THIS_PACKAGE-tmp"
  fi
}

remove_conffile_rollback () {
  # syntax: remove_conffile_rollback filename
  #
  # Roll back the removal of a conffile "filename".
  #
  # Call this function from a postrm script in the event $1 is "abort-upgrade"
  # or "abort-install" is  after having used remove_conffile_prepare() in the
  # preinst.

  #local conffile

  # validate arguments
  if [ $# -ne 1 ]; then
    usage_error "remove_conffile_rollback() called with wrong number of" \
                "arguments; expected 1, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  _conffile="$1"

  # if the temporary file created by remove_conffile_prepare() exists, move it
  # back
  if [ -e "$_conffile.$THIS_PACKAGE-tmp" ]; then
    observe "rolling back removal of obsolete conffile $_conffile"
    mv "$_conffile.$THIS_PACKAGE-tmp" "$_conffile"
  fi
}

replace_conffile_with_symlink_prepare () {
  # syntax: replace_conffile_with_symlink_prepare oldfilename newfilename \
  # official_md5sum ...
  #
  # Check a conffile "oldfilename" against a list of canonical MD5 checksums.
  # If the file's current MD5 checksum matches one of the "official_md5sum"
  # operands provided, then prepare the conffile for removal from the system.
  # We defer actual deletion until the package is configured so that we can
  # roll this operation back if package installation fails. Otherwise copy it
  # to newfilename and let dpkg handle it through conffiles mechanism.
  #
  # Call this function from a preinst script in the event $1 is "upgrade" or
  # "install" and verify $2 to ensure the package is being upgraded from a
  # version (or installed over a version removed-but-not-purged) prior to the
  # one in which the conffile was obsoleted.

  #local conffile current_checksum

  # validate arguments
  if [ $# -lt 3 ]; then
    usage_error "replace_conffile_with_symlink_prepare() called with wrong" \
                " number of arguments; expected at least 3, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  _oldconffile="$1"
  shift
  _newconffile="$1"
  shift

  remove_conffile_prepare "$_oldconffile" "$@"
  # If $_oldconffile still exists, then md5sums didn't match.
  # Copy it to new one.
  if [ -f "$_oldconffile" ]; then
    cp "$_oldconffile" "$_newconffile"
  fi

}

replace_conffile_with_symlink_commit () {
  # syntax: replace_conffile_with_symlink_commit oldfilename
  #
  # Complete the removal of a conffile "oldfilename" that has been
  # replaced by a symlink.
  #
  # Call this function from a postinst script after having used
  # replace_conffile_with_symlink_prepare() in the preinst.

  #local conffile

  # validate arguments
  if [ $# -ne 1 ]; then
    usage_error "replace_conffile_with_symlink_commit() called with wrong" \
                "number of arguments; expected 1, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  _conffile="$1"

  remove_conffile_commit "$_conffile"
}

replace_conffile_with_symlink_rollback () {
  # syntax: replace_conffile_with_symlink_rollback oldfilename newfilename
  #
  # Roll back the replacing of a conffile "oldfilename" with symlink to
  # "newfilename".
  #
  # Call this function from a postrm script in the event $1 is "abort-upgrade"
  # or "abort-install" and verify $2 to ensure the package failed to upgrade
  # from a version (or install over a version removed-but-not-purged) prior
  # to the one in which the conffile was obsoleted.
  # You should have  used replace_conffile_with_symlink_prepare() in the
  # preinst.

  #local conffile

  # validate arguments
  if [ $# -ne 2 ]; then
    usage_error "replace_conffile_with_symlink_rollback() called with wrong" \
                "number of arguments; expected 2, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  _oldconffile="$1"
  _newconffile="$2"

  remove_conffile_rollback "$_oldconffile"
  if [ -f "$_newconffile" ]; then
    rm "$_newconffile"
  fi
}

run () {
  # syntax: run command [ argument ... ]
  #
  # Run specified command with optional arguments and report its exit status.
  # Useful for commands whose exit status may be nonzero, but still acceptable,
  # or commands whose failure is not fatal to us.
  #
  # NOTE: Do *not* use this function with db_get or db_metaget commands; in
  # those cases the return value of the debconf command *must* be checked
  # before the string returned by debconf is used for anything.

  #local retval

  # validate arguments
  if [ $# -lt 1 ]; then
    usage_error "run() called with wrong number of arguments; expected at" \
                "least 1, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  "$@" || _retval=$?

  if [ ${_retval:-0} -ne 0 ]; then
    observe "command \"$*\" exited with status $_retval"
  fi
}

register_x_lib_dir_with_ld_so () {
  # syntax: register_x_lib_dir_with_ld_so
  #
  # Configure the dynamic loader ld.so to search /usr/X11R6/lib for shared
  # libraries.
  #
  # Call this function from the postinst script of a package that places a
  # shared library in /usr/X11R6/lib, before invoking ldconfig.

  #local dir ldsoconf

  _dir="/usr/X11R6/lib"
  _ldsoconf="/etc/ld.so.conf"

  # is the line not already present?
  if ! fgrep -qsx "$_dir" "$_ldsoconf"; then
    observe "adding $_dir directory to $_ldsoconf"
    echo "$_dir" >> "$_ldsoconf"
  fi
}

deregister_x_lib_dir_with_ld_so () {
  # syntax: deregister_x_lib_dir_with_ld_so
  #
  # Configure dynamic loader ld.so to not search /usr/X11R6/lib for shared
  # libraries, if and only if no shared libaries remain there.
  #
  # Call this function from the postrm script of a package that places a shared
  # library in /usr/X11R6/lib, in the event "$1" is "remove", and before
  # invoking ldconfig.

  #local dir ldsoconf fgrep_status cmp_status

  _dir="/usr/X11R6/lib"
  _ldsoconf="/etc/ld.so.conf"

  # is the line present?
  if fgrep -qsx "$_dir" "$_ldsoconf"; then
    # are there any shared objects in the directory?
    if [ "$(echo "$_dir"/lib*.so.*.*)" = "$_dir/lib*.so.*.*" ]; then
      # glob expansion produced nothing, so no shared libraries are present
      observe "removing $_dir directory from $_ldsoconf"
      # rewrite the file (very carefully)
      set +e
      fgrep -svx "$_dir" "$_ldsoconf" > "$_ldsoconf.dpkg-tmp"
      _fgrep_status=$?
      set -e
      case $_fgrep_status in
        0|1) ;; # we don't actually care if any lines matched or not
        *) die "error reading \"$_ldsoconf\"; fgrep exited with status" \
          "$_fgrep_status" ;;
      esac
      set +e
      cmp -s "$_ldsoconf.dpkg-tmp" "$_ldsoconf"
      _cmp_status=$?
      set -e
      case $_cmp_status in
        0) rm "$_ldsoconf.dpkg-tmp" ;; # files are identical
        1) mv "$_ldsoconf.dpkg-tmp" "$_ldsoconf" ;; # files differ
        *) die "error comparing \"$_ldsoconf.dpkg-tmp\" to \"$_ldsoconf\";" \
          "cmp exited with status $_cmp_status" ;;
      esac
    fi
  fi
}

make_symlink_sane () {
  # syntax: make_symlink_sane symlink target
  #
  # Ensure that the symbolic link symlink exists, and points to target.
  #
  # If symlink does not exist, create it and point it at target.
  #
  # If symlink exists but is not a symbolic link, back it up.
  #
  # If symlink exists, is a symbolic link, but points to the wrong location, fix
  # it.
  #
  # If symlink exists, is a symbolic link, and already points to target, do
  # nothing.
  #
  # This function wouldn't be needed if ln had an -I, --idempotent option.

  # Validate arguments.
  if [ $# -ne 2 ]; then
    usage_error "make_symlink_sane() called with wrong number of arguments;" \
      "expected 2, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  # We could just use the positional parameters as-is, but that makes things
  # harder to follow.
  #local symlink target

  _symlink="$1"
  _target="$2"

  if [ -L "$_symlink" ] && [ "$(readlink "$_symlink")" = "$_target" ]; then
      observe "link from $_symlink to $_target already exists"
  else
    observe "creating symbolic link from $_symlink to $_target"
    mkdir -p "${_target%/*}" "${_symlink%/*}"
    ln -s -b -S ".dpkg-old" "$_target" "$_symlink"
  fi
}

migrate_dir_to_symlink () {
  # syntax: migrate_dir_to_symlink old_location new_location
  #
  # Per Debian Policy section 6.5.4, "A directory will never be replaced by a
  # symbolic link to a directory or vice versa; instead, the existing state
  # (symlink or not) will be left alone and dpkg will follow the symlink if
  # there is one."
  #
  # We have to do it ourselves.
  #
  # This function moves the contents of old_location, a directory, into
  # new_location, a directory, then makes old_location a symbolic link to
  # new_location.
  #
  # old_location need not exist, but if it does, it must be a directory (or a
  # symlink to a directory).  If it is not, it is backed up.  If new_location
  # exists already and is not a directory, it is backed up.
  #
  # This function should be called from a package's preinst so that other
  # packages unpacked after this one --- but before this package's postinst runs
  # --- are unpacked into new_location even if their payloads contain
  # old_location filespecs.

  # Validate arguments.
  if [ $# -ne 2 ]; then
    usage_error "migrate_dir_to_symlink() called with wrong number of"
                "arguments; expected 2, got $#"
    exit $SHELL_LIB_USAGE_ERROR
  fi

  # We could just use the positional parameters as-is, but that makes things
  # harder to follow.
  local _new _old

  _old="$1"
  _new="$2"

  # Is old location a symlink?
  if [ -L "$_old" ]; then
    # Does it already point to new location?
    if [ "$(readlink "$_old")" = "$_new" ]; then
      # Nothing to do; migration has already been done.
      observe "migration of $_old to $_new already done"
      return 0
    else
      # Back it up.
      warn "backing up symbolic link $_old as $_old.dpkg-old"
      mv -b "$_old" "$_old.dpkg-old"
    fi
  fi

  # Does old location exist, but is not a directory?
  if [ -e "$_old" ] && ! [ -d "$_old" ]; then
      # Back it up.
      warn "backing up non-directory $_old as $_old.dpkg-old"
      mv -b "$_old" "$_old.dpkg-old"
  fi

  observe "migrating $_old to $_new"

  # Is new location a symlink?
  if [ -L "$_new" ]; then
    # Does it point the wrong way, i.e., back to where we're migrating from?
    if [ "$(readlink "$_new")" = "$_old" ]; then
      # Get rid of it.
      observe "removing symbolic link $_new which points to $_old"
      rm "$_new"
    else
      # Back it up.
      warn "backing up symbolic link $_new as $_new.dpkg-old"
      mv -b "$_new" "$_new.dpkg-old"
    fi
  fi

  # Does new location exist, but is not a directory?
  if [ -e "$_new" ] && ! [ -d "$_new" ]; then
    warn "backing up non-directory $_new as $_new.dpkg-old"
    mv -b "$_new" "$_new.dpkg-old"
  fi

  # Create new directory if it does not yet exist.
  if ! [ -e "$_new" ]; then
    observe "creating $_new"
    mkdir -p "$_new"
  fi

  # Copy files in old location to new location.  Back up any filenames that
  # already exist in the new location with the extension ".dpkg-old".
  observe "copying files from $_old to $_new"
  if ! (cd "$_old" && cp -a -b -S ".dpkg-old" . "$_new"); then
    die "error(s) encountered while copying files from $_old to $_new"
  fi

  # Remove files at old location.
  observe "removing $_old"
  rm -r "$_old"

  # Create symlink from old location to new location.
  make_symlink_sane "$_old" "$_new"
}

# vim:set ai et sw=2 ts=2 tw=80:

# GOBSTOPPER: The X Strike Force shell library ends here.
