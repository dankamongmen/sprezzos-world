PUSHDIVERT(-1)
#
# Copyright (c) 1998 Sendmail, Inc.  All rights reserved.
# Copyright (c) 1983 Eric P. Allman.  All rights reserved.
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# By using this file, you agree to the terms and conditions set
# forth in the LICENSE file which can be found at the top level of
# the sendmail distribution.
#
#

ifdef(`XAGENT_MAILER_PATH',, `define(`XAGENT_MAILER_PATH', /usr/lib/xagent)')
ifdef(`XAGENT_MAILER_FLAGS',, `define(`XAGENT_MAILER_FLAGS', `DFMueXLn')')
ifdef(`XAGENT_MAILER_ARGS',, `define(`XAGENT_MAILER_ARGS', `xagent $h $u')')
POPDIVERT
####################################
### XAGENT Mailer specification  ### 
####################################

VERSIONID(`@(#)xagent.m4	0.1 (ISSC) 4/19/1996')

Mxagent,	P=XAGENT_MAILER_PATH, F=XAGENT_MAILER_FLAGS, S=11/31, R=21/31,
		_OPTINS(`XAGENT_MAILER_MAX', `M=', `, ')T=DNS/RFC822/X-Unix,
		A=XAGENT_MAILER_ARGS, E=\n
