divert(-1)dnl
#-----------------------------------------------------------------------------
# $Sendmail: ./msp_nullclient.m4,v 8.12.0 2001/05/23 14:30:00 cowboy Exp $
#
# Copyright (c) 2001-2001 Richard Nelson.  All Rights Reserved.
#
# msp_nullclient.m4 m4 file for supporting a nullclient in the MSP
# environment.  That is, change the feature(msp) to feature(msp_nullclient)
# in /etc/mail/submit.mc.  An MTA listener is now optional, and you can
# run with only MSP mode (setgid mail).
#
# This file is blatantly cut&pasted from nullclient.m4 and msp.m4 !!!
#
# If you've a better idea, please let me know
#
#-----------------------------------------------------------------------------
#
# Copyright (c) 1998-2000 Sendmail, Inc. and its suppliers.
#	All rights reserved.
# Copyright (c) 1983 Eric P. Allman.  All rights reserved.
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# By using this file, you agree to the terms and conditions set
# forth in the LICENSE file which can be found at the top level of
# the sendmail distribution.
#
#
ifelse(defn(`_ARG_'), `', `errprint(`Hack "msp_nullclient" requires argument')',
	`define(`_NULL_CLIENT_', _ARG_)')

#
#  This is used only for relaying mail from a client to a hub when
#  that client does absolutely nothing else -- i.e., it is a "null
#  mailer".  In this sense, it acts like the "R" option in Sun
#  sendmail.
#

divert(0)dnl
VERSIONID(`$Id: msp_nullclient.m4,v 8.12.0 2001/05/23 14:30:00 cowboy Exp $')
divert(-1)dnl

dnl #
dnl # This *MUST* be used within the context of MSP
ifdef(`DEBIAN_MSP', `dnl', `DOMAIN(`debian-msp')')
dnl #
dnl # Restrict listening to localhost (to increase security)
ifelse(defn(`_DPO_'), `',
`DAEMON_OPTIONS(`Name=NoMTA, Addr=127.0.0.1, M=E')dnl')

dnl #-------------------- Now the Nullclient carp ---------------------------
undefine(`ALIAS_FILE')
define(`MAIL_HUB', _NULL_CLIENT_)
define(`SMART_HOST', _NULL_CLIENT_)
define(`confFORWARD_PATH', `')
ifdef(`confFROM_HEADER',, `define(`confFROM_HEADER', `<$g>')')
dnl #-------------------- Now the MSP carp ----------------------------------
ifdef(`STATUS_FILE',
`define(`_F_',
`define(`_b_', index(STATUS_FILE, `sendmail.st'))ifelse(_b_, `-1', `STATUS_FILE', `substr(STATUS_FILE, 0, _b_)sm-client.st')')
define(`STATUS_FILE', _F_)
undefine(`_b_') undefine(`_F_')',
`define(`STATUS_FILE', `/var/run/sm-client.st')')
define(`confUSE_MSP', `True')dnl
define(`confFORWARD_PATH', `')dnl
define(`confPRIVACY_FLAGS', `goaway,noetrn')dnl
dnl ---------------------------------------------
dnl run as this user (even if called by root)
define(`confRUN_AS_USER', `smmsp')dnl
define(`confTRUSTED_USER', `confRUN_AS_USER')dnl
dnl ---------------------------------------------
dnl This queue directory must have the same group
dnl as sendmail and it must be group-writable.
dnl notice: do not test for QUEUE_DIR, it is set in some ostype/*.m4 files
ifdef(`MSP_QUEUE_DIR',
`define(`QUEUE_DIR', `MSP_QUEUE_DIR')',
`define(`QUEUE_DIR', `/var/spool/clientmqueue')')dnl
dnl ---------------------------------------------
ifdef(`confPID_FILE', `dnl',
`define(`confPID_FILE', QUEUE_DIR`/sm-client.pid')')
define(`confQUEUE_FILE_MODE', `0660')dnl
FEATURE(`no_default_msa')dnl
ifelse(defn(`_DPO_'), `',
`DAEMON_OPTIONS(`Name=NoMTA, Addr=127.0.0.1, M=E')dnl')
dnl #---------------------- Debian MSP fixup -------------------------------
dnl define(`confHOST_STATUS_DIRECTORY', `/var/lib/sendmail/host_status')dnl
define(`confRUN_AS_USER', `mail')dnl
define(`confTRUSTED_USER', `confRUN_AS_USER')dnl
dnl #---------------------- back to nullclient carp ------------------------
define(`_DEF_LOCAL_MAILER_FLAGS', `lsDFM5q')
MASQUERADE_AS(_NULL_CLIENT_)
FEATURE(`allmasquerade')
FEATURE(`masquerade_envelope')
MAILER(`local')
MAILER(`smtp')
