divert(-1)dnl
#-----------------------------------------------------------------------------
# $Sendmail: ./vnet.m4,v 8.12.0 2001/09/24 12:00:00 cowboy Exp $
#
# Copyright (c) 2001-2004 Richard Nelson.  All Rights Reserved.
#
# feature(vnet) config file for building Sendmail
# 
#-----------------------------------------------------------------------------
#
divert(0)dnl
dnl
dnl
dnl
LOCAL_CONFIG
######################################################################
### vnet: support .rscs domain and XAGENT
###
######################################################################
#
# Support routing of .rscs nodes via XAGENT
#
# Define vnet/rscs node hlq
CAvnet.ibm.com vnet rscs ipnet
#
# Note: you'll have to define your XAGENT node:
#DAlexvmk.lexington.ibm.com
######################################################################
dnl
dnl
dnl
LOCAL_RULE_3
######################################################################
### vnet: support .rscs domain, XAGENT, and Lotus Notes
###
######################################################################
#
# Support Lotus Notes and VM TCP/IP MTAs
#
# Remove stupid route addressing (%hack) added by VM TCP/IP
R$* % ibmus.rscs < @ $+> $*	$: $1 < @ us.ibm.com > $3	# Deprecated
R$* % ibmca.rscs < @ $+> $*	$: $1 < @ ca.ibm.com > $3   # Deprecated
R$* % ibmuk.rscs < @ $+> $*	$: $1 < @ uk.ibm.com > $3	# Deprecated
R$* % $-.rscs    < @ $+> $*	$: $1 < @ $2.rscs > $4		# Deprecated
# Now, rewrite those address to user@<node>.vnet
R$* < @ $-.$=A > $*			$: $1 < @ $2.vnet . >
# Now, try the domaintable yet again...
#R$* < @ $-.vnet > $*		$: $1 < @ $(domaintable $2.vnet $) > $3
# Finally, allow an override on simply the rscs domain...
#R$* < @ $-.vnet > $*		$: $1 < @ $2.$(domaintable vnet $) > $3
######################################################################
dnl
dnl
dnl
LOCAL_NET_CONFIG
######################################################################
### vnet: Handle .rscs domain, deliver to *.ibm.com, etc...
###
######################################################################
# Skip any local addresses
# (or we'll get a "mail loops back to itself" error
R$* < @ $=w . > $*		$@ $1 < @ $2 . > $3	regular local name
R$* < @ $=w > $*		$@ $1 < @ $2 . > $3	regular local name

# Recipients of the form user@node.{vnet,rscs,ipnet} get rewritten to
# user@node.vnet, the message itself is forwarded to 
# the VM TCP/IP to RSCS gateway node specified in $A.
R$* < @ $- . vnet . > $*		$#relay $@ $A $: $1 < @$2.VNET. > $3

# Recipients inside IBM--transfer the mail directly.
R$* < @ $+.ibm.com  > $*	$#relay $@ $2.ibm.com $: $1 < @$2.ibm.com > $3
R$* < @ $+.ibm.com. > $*	$#relay $@ $2.ibm.com $: $1 < @$2.ibm.com > $3
######################################################################
LOCAL_CONFIG
