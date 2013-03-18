#! /usr/bin/perl

# Gateway script for postfix to send to LDAP mail-enabled groups.
# $Id: postfix_groups.pl,v 1.6 2007/01/29 16:11:07 subbarao Exp $

#++
# NAME
#   postfix_groups.pl 8
# SUMMARY
#   Pipe mailer program for postfix to send to LDAP mail-enabled groups.
# SYNOPSIS
#   postfix_groups.pl <sender> <nexthop> <recipient>
# DESCRIPTION
#   postfix_groups.pl delivers mail to LDAP mail-enabled groups. It is
#   intended to be invoked by \fBpipe\fR(8). Here is an example of a
#   simple mail-enabled LDAP group:
#
#   dn: cn=postfix-hackers, ou=Groups, o=hp.com
#   .br
#   objectClass: top
#   .br
#   objectClass: groupOfNames
#   .br
#   objectClass: mailGroup
#   .br
#   cn: postfix-hackers
#   .br
#   mail: postfix-hackers@groups.hp.com
#   .br
#   member: uid=lamont.jones@hp.com, ou=People, o=hp.com
#   .br
#   member: uid=kartik.subbarao@hp.com, ou=People, o=hp.com
#
#   Here are excerpts from the people entries who are members of this group:
#
#   dn: uid=lamont.jones@hp.com, ou=People, o=hp.com
#   .br
#   mailRoutingAddress: lamont@cranston.fc.hp.com
#
#   dn: uid=kartik.subbarao@hp.com, ou=People, o=hp.com
#   .br
#   mailRoutingAddress: subbarao@quest.lf.hp.com
#
#   postfix_groups.pl expands the incoming address
#   postfix-hackers@groups.hp.com to the destination addresses
#   lamont@cranston.fc.hp.com and subbarao@quest.lf.hp.com.
#
# CONFIGURATION
#   To configure postfix_groups.pl to handle addresses of the form
#   groupaddr@groups.mycompany.com, specify the following in \fBmaster.cf\fR:
#
# groups   unix  -   n   n   -   -   pipe
# flags=q user=uucp argv=/path/to/postfix_groups.pl ${sender} ${nexthop} ${recipient}
#
#   And the following in the \fBtransport\fR file:
#
# groups.mycompany.com	groups:groups
#
#   And the following in \fBmain.cf\fR (assuming an LDAP server
#   ldap.mycompany.com with the root DN of o=mycompany.com):
#
# groups_destination_recipient_limit = 1
# groups_server_host = ldap.mycompany.com
# groups_search_base = o=mycompany.com
# groups_query_filter = (mail=%u@groups.mycompany.com)
# groups_result_attribute = mailRoutingAddress mgrpRFC822MailMember
# groups_special_result_attribute = member memberURL mgrpDeliverTo
# groups_domain = groups.mycompany.com
# groups_bind = no
#
#   Note: The groups_* map should not be referenced in virtual_maps or
#   elsewhere. Also note that the groups_destination_recipient_limit
#   should be set to 1.
#
## [Describe main.cf parameters]
## TBD
#
## [Describe LDAP attributes that govern mail-enabled groups]
## TBD
#
# AUTHOR
#   Kartik Subbarao <subbarao@computer.org>
#
# SEE ALSO
#   \fBpipe\fR(8)
#   http://www.watersprings.org/pub/id/draft-steinback-ldap-mailgroups-00.txt
#   http://playground.sun.com/laser/0066.html
#   RFC 2919
## TODO: Document implementation differences/enhancements by this script,
## compared to the draft spec.
#--

use Mail::Internet;
use Mail::Address;
use Net::SMTP;
use Net::LDAP qw (:all);
use URI;
use File::Basename;
use Sys::Syslog qw(:DEFAULT setlogsock);

use strict;

$ENV{'PATH'} = '/usr/local/bin:/usr/sbin:/usr/bin:/bin';
# Exit values from /usr/include/sysexits.h
my $DATAERR=65;
my $NOUSER=67;
my $SOFTWARE=70;
my $OSFILE=72;
my $NOPERM=77;
my $TEMPFAIL=75; # Most LDAP-related errors are transient failures

my $sender = shift(@ARGV); 		# ${sender}
my $map = shift(@ARGV); 		# ${nexthop} == map name
my $recipient = shift(@ARGV);	# ${recipient}
my $debug = 0;

if ($debug && ! -t STDIN) {
	open(STDOUT, "> /tmp/postfix_groups.stdout");
	open(STDERR, "> /tmp/postfix_groups.stderr");
}

setlogsock('unix');
openlog(basename($0), 'pid', 'mail');

# Read postfix configuration from main.cf into %postconf hash
my $maincf_file = '/etc/postfix/main.cf';
my %postconf;
get_postfix_params($maincf_file, \%postconf);

my $ldap_host = $postconf{"${map}_server_host"} || 'localhost';
my $ldap_port = $postconf{"${map}_server_port"} || 389;
my $ldap_timeout = $postconf{"${map}_timeout"} || 120;
my $basedn = $postconf{"${map}_search_base"};
my @excluded_resolved_domains =
	split(" ", $postconf{"${map}_excluded_resolved_domains"});

my $mail_attr = 'mail';
my $member_attr = 'member';
my @group_objectclasses = qw(groupOfNames groupOfURLs);
my @ldapurl_attrs = qw(memberURL mgrpDeliverTo);
my @default_result_attrs = split(" ", $postconf{"${map}_result_attribute"});
my @allowed_broadcaster_result_attrs = ($mail_attr, @default_result_attrs);
my $allowed_broadcaster_attr = 'mgrpAllowedBroadcaster';
my $errorsto_attr = 'mgrpErrorsTo';
my $addheader_attr = 'mgrpAddHeader';
my $removeheader_attr = 'mgrpRemoveHeader';

my $smtp_host = 'localhost'; # Use smtpd running on localhost
my $smtpd_recipient_limit = $postconf{smtpd_recipient_limit}
							|| `postconf -h smtpd_recipient_limit`
							|| 1000;
chomp $smtpd_recipient_limit;


# Read message from STDIN
my $message = Mail::Internet->new(\*STDIN, Modify => 0);

my $ldap = Net::LDAP->new($ldap_host,
						  port => $ldap_port,
						  timeout => $ldap_timeout)
	or warn("$ldap_host: $@\n"), exit $TEMPFAIL;

## TODO: Explicitly specify the attributes retrieved by the first search, so
## that it can match the specific names as defined (e.g. member, etc)
my $ldapmesg = $ldap->search(base => $basedn,
							 filter => "(mail=$recipient)");
if ($ldapmesg->code) {
	warn("$ldap_host: ", $ldapmesg->error, "\n");
	exit $TEMPFAIL;
}
# $mail_attr is assumed to be a unique attribute
my $entry = $ldapmesg->entry(0)
	or warn("Couldn't find entry for $recipient in $ldap_host\n"), exit $NOUSER;

my @allowed_broadcasters = $entry->get_value($allowed_broadcaster_attr);
my $errorsto = $entry->get_value($errorsto_attr);
my @addheaders = $entry->get_value($addheader_attr);
my @removeheaders = $entry->get_value($removeheader_attr);

my $header = $message->head;
$header->add('X-postfixgroups-version', ('$Revision: 1.6 $' =~ /\$Revision:\s+([^\s]+)/)[0]);

# Check if user is allowed to send to this list
my $from = (Mail::Address->parse($header->get('From:')))[0];
my $fromaddr = $from->address;
my @allowed_fromaddrs;
foreach my $allowed_broadcaster (@allowed_broadcasters) {
	# Allowed broadcasters can be specified as a mailto: or ldap: URL.
	my $uri = URI->new($allowed_broadcaster);
	if ($uri->scheme eq 'ldap') {
		# Expand the LDAP entry for all appropriate mail addresses,
		my @attrs = $uri->attributes;
		if (@attrs) {
			# If attributes are specified in the LDAP URL,
			# their values are expanded as DNs, instead of expanding
			# the LDAP entry itself.
			my $mesg = $ldap->search(base => $uri->dn,
						  scope => 'base',
						  filter => "(objectclass=*)",
						  attrs => \@attrs);
			if ($mesg->code) {
				if ($mesg->code == LDAP_NO_SUCH_OBJECT) {
					syslog('warning', "Could not find allowed broadcaster " . $uri->dn);
					next;
				}
				warn("$ldap_host: ", $mesg->error, "\n");
				exit $TEMPFAIL;
			}

			my $entry = $mesg->entry(0) or next;

			foreach my $dnval (map { $entry->get_value($_) } @attrs) {
				push(@allowed_fromaddrs,
					expand_entry(dn => $dnval,
						resultattrs => \@allowed_broadcaster_result_attrs));
			}
		}
		else {
			# Expand the entry, and append to the list of allowed
			# broadcaster addresses.
			push(@allowed_fromaddrs,
				expand_entry(dn => $uri->dn,
					resultattrs => \@allowed_broadcaster_result_attrs));
		}
	}
	elsif ($uri->scheme eq 'mailto') {
		push(@allowed_fromaddrs, $uri->to);
	}
	else {
		# Unknown scheme, treat it as an RFC 822 mail address
		push(@allowed_fromaddrs, $allowed_broadcaster);
	}
}

if (@allowed_fromaddrs) {
	if (!grep(/^$fromaddr$/i, @allowed_fromaddrs)) {
		warn("$fromaddr is not allowed to send to $recipient\n");
		exit $NOPERM;
	}
}

# Populate Errors-To: header if requested. Also adjust envelope sender.
if ($errorsto) {
	# Only supports RFC 822 mail address specification for now
	$errorsto =~ s/^mailto://;
	$header->add(undef, "Errors-To: $errorsto");
	$sender = $errorsto;
}

# Add List-Id header (defined in RFC 2919)
(my $listid = $recipient) =~ s/@/.list-id./;
$header->add('List-Id', $listid);

# Adjust message headers as appropriate
foreach my $addh (@addheaders) { $header->add(undef, $addh) }
foreach my $remh (@removeheaders) { $header->delete($remh) }

if ($debug) {
	open(DEBUGMESSAGE, "> /tmp/postfix_groups.message");
	$message->print(\*DEBUGMESSAGE);
	close DEBUGMESSAGE;
}

# Get target addresses
my @alladdrs = expand_entry(entry => $entry);

# Exclude specified domains
my (@addrs, @excluded_addrs);
foreach my $addr (@alladdrs) {
	my $excluded;
	foreach my $domain (@excluded_resolved_domains) {
		grep(/$domain$/, $addr) and $excluded = 1, last;
	}
	if ($excluded) { push(@excluded_addrs, $addr) }
	else { push(@addrs, $addr) }
}
syslog('warning', "The following addresses were explicitly excluded from $recipient: " .  join(',', @excluded_addrs))
	if @excluded_addrs;

# Sort list of addresses by reversed domain name, to assist with bundling mail
@addrs = sort { lc(reverse($a)) cmp lc(reverse($b)) } @addrs;
$debug and print join("\n", @addrs) . "\n";
## TODO: do some more enhanced sorting to better collapse addresses,
## to minimize the number of messages sent out.

# Send the message
my $smtp = Net::SMTP->new($smtp_host, Debug => $debug)
	or warn("Could not contact SMTP server on $smtp_host\n"), exit $TEMPFAIL;
my @badaddrs;
while (@addrs) {
	my (@rcpt_to, @goodaddrs, %seen);

	$smtp->mail($sender);
	# Break up recipients based on $smtpd_recipient_limit
	@rcpt_to = splice(@addrs, 0, $smtpd_recipient_limit);
	@goodaddrs = $smtp->to(@rcpt_to, { SkipBad => 1 });
	@seen{@goodaddrs} = ();
	foreach my $addr (@rcpt_to) {
		push(@badaddrs, $addr) unless exists $seen{$addr};
	}
	unless ($smtp->data(split(/^/m, $message->as_string))) {
		warn("Message not accepted by SMTP server $smtp_host\n");
		exit $SOFTWARE;
	}
}
syslog('warning', "The following addresses were not accepted by the SMTP server on $smtp_host: " . join(',', @badaddrs))
	if @badaddrs;

exit;


# Read postfix configuration from main.cf into a hash
sub get_postfix_params
{
	my ($maincf_file, $hashref) = @_;
	local $/ = undef;

	open(MAINCF, $maincf_file) or warn("$maincf_file: $!\n"), exit $OSFILE;
	my $maincfstr = <MAINCF>; close MAINCF;
	$maincfstr =~ s/^#.*?\n//mg;  # Get rid of comments
	$maincfstr =~ s/\n[ \t]+/ /mg; # Collapse line continuation
	foreach (split(/\n/, $maincfstr)) {
		my ($param, $val);
		($param, $val) = /\s*(\S+)\s*=\s*(.*?)\s*$/;
		$hashref->{$param} = $val;
	}
}


# Expand an LDAP entry, returning a list of results (culled for duplicates)
sub expand_entry
{
	my %arg = (@_);
	my (%results, @result_attrs);
	my ($dn, $mesg, $entry, @entries, %seen);

	@result_attrs = $arg{resultattrs}
					? @{$arg{resultattrs}} : @default_result_attrs;

	push(@entries, $arg{entry}) if $arg{entry};	# Passed as entry
	push(@entries, $arg{dn}) if $arg{dn}; 		# Passed as DN

	while (my $entry = shift(@entries)) {
		unless (ref $entry) { # Actually a DN, get corresponding entry
			my $dn = $entry;
			$mesg = $ldap->search(base => $dn,
						  		  scope => 'base',
							  	  filter => "(objectclass=*)",
							  	  attrs => [ $mail_attr,
								  			 $member_attr,
											 @ldapurl_attrs,
											 @result_attrs ]);
			if ($mesg->code) {
				if ($mesg->code == LDAP_NO_SUCH_OBJECT) {
					syslog('warning', "Could not find entry $dn");
					next;
				}
				warn("$ldap_host: ", $mesg->error, "\n");
				exit $TEMPFAIL;
			}

			$entry = $mesg->entry(0) or next;
		}

		# Add any result attributes of the entry itself to the results hash
		foreach my $value (map { $entry->get_value($_) } @result_attrs) {
			$results{$value} = 1;
		}

		# Add any static group member DNs to the list of entries
		if ($entry->exists($member_attr)) {
			# Break infinite loops from malformed nested groups
			push(@entries, $entry->get_value($member_attr))
				unless $seen{$entry->dn};
		}

		# Perform any specified dynamic queries
		foreach my $query (map { $entry->get_value($_) } @ldapurl_attrs) {
			my $uri = URI->new($query);
			$mesg = $ldap->search(base => $uri->dn,
								  scope => $uri->scope,
								  filter => $uri->filter,
								  attrs => ['objectclass', @result_attrs]);
			if ($mesg->code) {
				if ($mesg->code == LDAP_NO_SUCH_OBJECT) {
					syslog('warning', "Invalid base DN in $query\n");
					next;
				}
				warn("$ldap_host: ", $mesg->error, "\n");
				exit $TEMPFAIL;
			}

			# Add the result attributes of each group member to the results hash
			foreach my $memberentry ($mesg->entries) {
				foreach my $value (map { $memberentry->get_value($_) }
										@result_attrs) {
					$results{$value} = 1;
				}
				# Add any nested groups to the list of entries to be expanded
				my $isgroup;
				foreach my $oc (@group_objectclasses) {
					grep(/^$oc$/i, $memberentry->get_value('objectClass'))
						and $isgroup = 1, last;
				}
				if ($isgroup) {
					# Check for infinite loops in nested groups
					push(@entries, $memberentry->dn)
						unless $seen{$memberentry->dn};
				}
			}
		}

		# Mark that we saw this entry's dn
		$seen{$entry->dn} = 1;
	}

	return keys %results;
}
