PUSHDIVERT(-1)
dnl
dnl By using this file, you agree to the terms and conditions set
dnl forth in the LICENSE file which can be found at the top level of
dnl the sendmail distribution.
dnl
dnl	Original version contributed by Andrzej Filip.
dnl
dnl $Log: ssh.m4,v $
dnl Revision 8.1  2004/03/27 12:54:53  anfi
dnl *** empty log message ***
dnl
VERSIONID(`$Id: ssh.m4,v 8.1 2004/03/27 12:54:53 anfi Exp $')

ifdef(`SSH_MAILER_PATH',,
	`ifdef(`SSH_PATH',
		`define(`SSH_MAILER_PATH', SSH_PATH)',
		`define(`SSH_MAILER_PATH', `/usr/bin/ssh')')')
ifdef(`SSH_REMOTE_COMMAND',,
		`define(`SSH_REMOTE_COMMAND', `/usr/sbin/sendmail -bs')')
define(`_SSH_QGRP', `ifelse(defn(`SSH_MAILER_QGRP'),`',`', ` Q=SSH_MAILER_QGRP,')')dnl
_DEFIFNOT(`_DEF_SSH_MAILER_FLAGS', `mDFMuX')
_DEFIFNOT(`SSH_MAILER_FLAGS',`')
ifdef(`SSH_MAILER_ARGS',, `define(`SSH_MAILER_ARGS', 
  `ssh -o BatchMode=yes -o EscapeChar=none  -- 'SSH_MAILER_HOST` 'SSH_REMOTE_COMMAND)')
ifdef(`SSH_MAILER_HOST',, `define(`SSH_MAILER_HOST',`$h')')
ifdef(`SSH_MAILER_USER',`',
`errprint(`*** SSH_MAILER_USER must be defined before MAILER(`ssh').
')')
ifdef(`SSH_MAILER_DIR',`',
`errprint(`*** SSH_MAILER_DIR must be defined before MAILER(`ssh').
')')

POPDIVERT
######################*****##############
###   SSH Mailer specification   ###
##################*****##################

Mssh,	P=SSH_MAILER_PATH, F=_MODMF_(CONCAT(_DEF_SSH_MAILER_FLAGS, SSH_MAILER_FLAGS), `SSH'), S=EnvFromSMTP/HdrFromSMTP, R=ifdef(`_ALL_MASQUERADE_', `EnvToSMTP/HdrFromSMTP', `EnvToSMTP'), E=\r\n, L=990,
	_OPTINS(`SSH_MAILER_MAX', `M=', `, ')_OPTINS(`SSH_MAILER_MAXMSGS', `m=', `, ')_OPTINS(`SSH_MAILER_MAXRCPTS', `r=', `, ')_OPTINS(`SSH_MAILER_CHARSET', `C=', `, ')T=DNS/RFC822/SMTP,_SSH_QGRP
	U=SSH_MAILER_USER, D=SSH_MAILER_DIR,
	A=SSH_MAILER_ARGS
