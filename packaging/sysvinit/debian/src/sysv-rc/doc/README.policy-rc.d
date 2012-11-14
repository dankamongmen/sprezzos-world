

This is the internal documentation for policy-rc.d, as
written by Henrique M Holschuh <hmh@debian.org>

This document can be found on the web as well at
http://people.debian.org/~hmh/invokerc.d-policyrc.d-specification.txt

There is also the Debian BTS entry for the invoke-rc.d policy change at
http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=76868


POLICY-RC.D   Policy layer (/usr/sbin/policy-rc.d) interface:
=============================================================

Most Debian systems will not have this script as the need for a policy layer
is not very common. Most people using chroot jails just need an one-line
script which returns an exit status of 101 as the jailed
/usr/sbin/policy-rc.d script.

The /usr/sbin/policy-rc.d file *must* be managed through the alternatives
system (/usr/sbin/update-alternatives) by any packages providing it.

/usr/sbin/policy-rc.d [options] <initscript ID> <actions> [<runlevel>]
/usr/sbin/policy-rc.d [options] --list <initscript ID> [<runlevel> ...]

Options:
 --quiet
     no error messages are generated.

 --list
     instead of verifying policy, list (in a "human parseable" way) all
     policies defined for the given initscript id (for all runlevels if no
     runlevels are specified; otherwise, list it only for the runlevels
     specified), as well as all known actions and their fallbacks for the
     given initscript id (note that actions and fallback actions might be
     global and not particular to a single initscript id).

<actions> is a space-separated list of actions (usually only one). Note that
the list is passed in a single parameter and not as multiple parameters.

The following actions are always known (even if specifying a policy for them
is not supported by whatever policy-rc.d system is in use): start,
[force-]stop, restart, [force-]reload, status.

If an out-of-runlevel start or restart attempt is detected by invoke-rc.d,
the "start" or "restart" action will be changed to "(start)" or "(restart)"
respectively. This allows policy-rc.d to differentiate an out-of-runlevel
start/restart from a normal one.

The runlevel parameters are optional. If a runlevel is not specified, it is
considered to be unknown/undefined. Note that for sysv-like initscript
systems, an undefined runlevel is very likely to cause a 105 exit status.

A runlevel for update-rc.d is defined as a character string, of which the
usual INIT one-character runlevels are only a subset. It may contain
embedded blanks.

 stdout is used to output a single line containing fallback actions,
	   or to output --list results.
 stderr is used to output error messages
 stdin  is not to be used, this is not an interactive interface.

 Exit status codes:
  0 - action allowed
  1 - unknown action (therefore, undefined policy)
 100 - unknown initscript id
 101 - action forbidden by policy
 102 - subsystem error
 103 - syntax error
 104 - [reserved]
 105 - behaviour uncertain, policy undefined.
 106 - action not allowed. Use the returned fallback actions
       (which are implied to be "allowed") instead. 

When in doubt (policy-rc.d returned status 105 or status 1), invoke-rc.d
will assume an action is allowed, but it will warn the user of the problem.

Returning fallback information:

Fallback actions are returned in the first line sent to stdout (other lines
will be discarded). Multiple actions to be tried are allowed, and must be
separated by spaces. Multiple actions are carried out one at a time, until
one is sucessful.

e.g.: returning status 106 and "restart stop" in stdout (without
the quotes) will cause invoke-rc.d to attempt action "restart",
and then only if "restart" failed, attempt action "stop".

invoke-rc.d built-in policy rules:

To shield policy-rc.d of the underlying initscript system (file-rc, links in
/etc/rc?.d or something else), invoke-rc.d implements the following built-in
rules:

  1. action "start" out of runlevel is denied,
     (policy-rc.d receives action "(start)" instead of "start");
  2. action "restart" out of runlevel is denied,
     (policy-rc.d receives action "(restart)" instead of "restart");
  3. any action for a non-executable initscript is denied.

Rule 3 is absolute, policy-rc.d cannot override it.
