#!/usr/bin/perl

use strict;
use MIME::Base64;
use Getopt::Std;

my($p_name)   = $0 =~ m|/?([^/]+)$|;
my $p_version = "20060620.0";
my $p_usage   = "Usage: $p_name [--help|--version] | <type> ...";
my $p_cp      = <<EOM;
        Copyright (c) 2002-2006 John Jetmore <jj33\@pobox.com>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
EOM
ext_usage();

my %O    = ();
getopts('s', \%O);

my $type = get_input(\@ARGV, "encryption type: ");

if ($type =~ /^plain$/i) {
  my $user = get_input(\@ARGV, "username: ", $O{s}||0);
  my $pass = get_input(\@ARGV, "password: ", $O{s}||1);
  print "Auth String: ", encode_base64("\0$user\0$pass", ''), "\n";

} elsif ($type =~ /^decode$/i) {
  my $user = get_input(\@ARGV, "string: ", $O{s}||0);
  print decode_base64($user), "\n";

} elsif ($type =~ /^encode$/i) {
  my $user = get_input(\@ARGV, "string: ", $O{s}||0);
  print encode_base64($user, ""), "\n";

} elsif ($type =~ /^rot13$/i) {
  my $str = get_input(\@ARGV, "string: ", $O{s}||0);
  my @c = unpack("c*", $str);
  foreach my $c (@c) {
    if    ($c <= 123 && $c >= 97) { $c = ((($c - 97 + 13) % 26) + 97); }
    elsif ($c <= 90  && $c >= 65) { $c = ((($c - 65 + 13) % 26) + 65); }
  }
  print pack("c*", @c), "\n";

} elsif ($type =~ /^atbash$/i) {
  my $str = get_input(\@ARGV, "string: ", $O{s}||0);
  my @c = unpack("c*", $str);
  foreach my $c (@c) {
    if    ($c <= 123 && $c >= 97) { $c = (25 - ($c - 97)) + 97; }
    elsif ($c <= 90  && $c >= 65) { $c = (25 - ($c - 65)) + 65; }
  }
  print pack("c*", @c), "\n";

} elsif ($type =~ /^http(-basic)?$/i) {
  my $user = get_input(\@ARGV, "username: ", $O{s}||0);
  my $pass = get_input(\@ARGV, "password: ", $O{s}||1);
  print "Auth String: ", encode_base64("${user}:$pass", ''), "\n";

} elsif ($type =~ /^wcsencode$/i) {
  try_load("WCS::Encode") || die "WCS::Encode required for rce\n";
  my $user = get_input(\@ARGV, "string: ", $O{s}||0);
  chomp($user = WCS::Encode::encode($user));
  print $user, "\n";

} elsif ($type =~ /^wcsdecode$/i) {
  try_load("WCS::Encode") || die "WCS::Encode required for rce\n";
  my $user = get_input(\@ARGV, "string: ", $O{s}||0);
  print WCS::Encode::decode($user), "\n";

} elsif ($type =~ /^rce$/i) {
  try_load("WCS::Passwd") || die "WCS::Passwd required for rce\n";
  my $user = get_input(\@ARGV, "string: ", $O{s}||0);
  print WCS::Passwd::rce($user), "\n";

} elsif ($type =~ /^rcd$/i) {
  try_load("WCS::Passwd") || die "WCS::Passwd required for rce\n";
  my $user = get_input(\@ARGV, "string: ", $O{s}||0);
  print WCS::Passwd::rcd($user), "\n";

} elsif ($type =~ /^(salt)?encrypt$/i) {
  my $user = get_input(\@ARGV, "string: ", $O{s}||1);
  my $salt = join('', ('.', '/', 0..9, 'A'..'Z', 'a'..'z')[rand 64, rand 64]);
  $salt    = get_input(\@ARGV, "salt: ", $O{s}||0) if ($type =~ /^saltencrypt/i);
  print crypt($user, $salt), "\n";

} elsif ($type =~ /^login$/i) {
  my $user = get_input(\@ARGV, "username: ", $O{s}||0);
  my $pass = get_input(\@ARGV, "password: ", $O{s}||1);
  print "Username: ", encode_base64($user, ""), "\n",
        "Password: ", encode_base64($pass, ""), "\n";

} elsif ($type =~ /^md5-(base)?64$/i) {
  try_load("Digest::MD5") || die "Digest::MD5 required for md5\n";
  my $string = get_input(\@ARGV, "string: ", $O{s}||0);
  print Digest::MD5::md5_base64($string), "\n";

} elsif ($type =~ /^md5(-hex)?$/i) {
  try_load("Digest::MD5") || die "Digest::MD5 required for md5\n";
  my $string = get_input(\@ARGV, "string: ", $O{s}||0);
  print Digest::MD5::md5_hex($string), "\n";

} elsif ($type =~ /^cram(-(md5|sha1))?$/i) {
  my $digest_type = lc($2) || 'md5';
  if ($digest_type eq 'md5') {
    try_load("Digest::MD5") || die "Digest::MD5 required for CRAM-MD5\n";
  } elsif ($digest_type eq 'sha1') {
    try_load("Digest::SHA1") || die "Digest::SHA1 required for CRAM-SHA1\n";
  }
  my $user = get_input(\@ARGV, "username: ", $O{s}||0);
  my $pass = get_input(\@ARGV, "password: ", $O{s}||1);
  my $chal = get_input(\@ARGV, "challenge: ", $O{s}||0);
  if ($chal !~ /^</) {
    chomp($chal = decode_base64($chal));
  }
  my $digest = get_digest($pass, $chal, $digest_type);
  print encode_base64("$user $digest", ""), "\n";

} elsif ($type =~ /^(ntlm|spa|msn)$/i) {
  try_load("Authen::NTLM") || die "Authen::NTLM required for $type\n";
  my $user = get_input(\@ARGV, "username: ", $O{s}||0);
  my $pass = get_input(\@ARGV, "password: ", $O{s}||1);
  my $domn = get_input(\@ARGV, "domain: ", $O{s}||0);
  print "Auth Request: ", Authen::NTLM::ntlm(), "\n";
  Authen::NTLM::ntlm_user($user);
  Authen::NTLM::ntlm_password($pass);
  Authen::NTLM::ntlm_domain($domn);
  my $chal = get_input(\@ARGV, "challenge: ", $O{s}||0);
  print "Auth Response: ", Authen::NTLM::ntlm($chal), "\n";

} elsif ($type =~ /^apop$/i) {
  try_load("Digest::MD5") || die "Digest::MD5 required for APOP\n";
  my $chal = get_input(\@ARGV, "challenge: ");
  my $pass = get_input(\@ARGV, "password: ", 1);
  my $ctx = Digest::MD5->new;
  $ctx->add($chal . $pass);
  print $ctx->hexdigest, "\n";

} else {
  print STDERR "I don't speak $type\n";
  exit 1;
}

exit 0;

sub get_input {
  my $a = shift; # command line array
  my $s = shift; # prompt string
  my $q = shift; # quiet
  my $r;         # response

  if (scalar(@$a) > 0) {
    $r = shift(@$a);
  } else {
    print $s;
    system('stty', '-echo') if ($q);
    $r = <>;
    system('stty', 'echo') if ($q);
    print "\n" if ($q);
    chomp($r);
  }

  $r = '' if ($r eq '<>');
  return($r);
}

sub get_digest {
  my $secr = shift;
  my $chal = shift;
  my $type = shift;
  my $ipad = chr(0x36) x 64;
  my $opad = chr(0x5c) x 64;

  if (length($secr) > 64) {
    if ($type eq 'md5') {
      $secr = Digest::MD5::md5($secr);
    } elsif ($type eq 'sha1') {
      $secr = Digest::SHA1::sha1($secr);
    } else {
      # unknown digest type
      return;
    }
  } else {
    $secr .= chr(0) x (64 - length($secr));
  }

  my $digest = $type eq 'md5'
               ? Digest::MD5::md5_hex(($secr ^ $opad),
                 Digest::MD5::md5(($secr ^ $ipad), $chal))
               : Digest::SHA1::sha1_hex(($secr ^ $opad),
                 Digest::SHA1::sha1(($secr ^ $ipad), $chal));
  return($digest);
}

sub try_load {
  my $mod = shift;

  eval("use $mod");
  return $@ ? 0 : 1;
}

sub ext_usage {
  if ($ARGV[0] =~ /^--help$/i) {
    require Config;
    $ENV{PATH} .= ":" unless $ENV{PATH} eq "";
    $ENV{PATH} = "$ENV{PATH}$Config::Config{'installscript'}";
    exec("perldoc", "-F", "-U", $0) || exit 1;
    # make parser happy
    %Config::Config = ();
  } elsif ($ARGV[0] =~ /^--version$/i) {
    print "$p_name version $p_version\n\n$p_cp\n";
  } else {
    return;
  }

  exit(0);
}

__END__

=head1 NAME

gen-auth - generate various authentication strings

=head1 USAGE

gen-auth [--help|--version] | <type> ...

=head1 DESCRIPTION

gen-auth is tool to assist in all kinds of authentication / encoding / decoding / encrypting tasks.  It began life as an smtp-specific tool, but has drifted in functionality over time.

The program actions are broken down into types of encoding to generate.
Each <type> then takes its own specific args.  The arguments are expected
in a specific order on the command line.  Every argument that isn't
available on the command line will be prompted for.  One benefit to this is
arguments corresponding to passwords will not be echoed to the terminal when
prompted for.

=head1 TYPES

The program action is controlled by the first argument.  The meaning of the
following arguments is specified by this type

=over 4

=item PLAIN <username> <password>

This type generates a PLAIN (RFC 2595) authentication string.  It accepts
supplemental arguments of username and password.  It generates a Base64
encoded string "\0<username>\0<password>".

=item LOGIN <username> <password>

This method accepts username and password as supplemental args.  It simply
returns each string Base64 encoded.  This provides only minimal advantages
over using ENCODE twice.  One advantage is hiding the password if you
provide it on STDIN

=item CRAM-MD5 <username> <password> <challenge>

CRAM-MD5 (RFC 2195) accepts three supplemental arguments.  The first is the username and
the second is the password.  The third is the challenge string provided
by the server.  This string can be either Base64 encoded or not.  The RFC states
that all (unencoded) challenge strings must start w/ '<'.  This is used to
whether the string is Base64 encoded or not.

CRAM-MD5 uses the challenge and the supplied password to generate a digest.
it then returns the Base64 encoded version of the string md5("<username> <challenge>")

This authentication method requires the Digest::MD5 perl module to be installed.

=item CRAM-SHA1 <username> <password> <challenge>

This behaves the same as CRAM-MD5 but uses SHA1 digesting rather than MD5.

This authentication method requires the Digest::SHA1 perl module to be installed.

=item NTLM/SPA/MSN <username> <password> <domain> <challenge>

Although it may be advertised as one of the above types, this method of authentication if refered to singularly as NTLM.  This is a multi-step authentication type.  The first 3 arguments must be supplied up front.  They are username, password, and domain, in that order.  These three strings are used to generate an "Auth Request" string.  This string should be passed verbatim to the server.  The server will then respond with a challenge.  This challenge is the fourth argument.  After receiving the server challenge, gen-auth will produce an "Auth Response".  Posting this response to the server completes the NTLM authentication transaction.

This authentication method requires the Authen::NTLM perl module to be installed.  See EXAMPLES for an example of this transaction.  Note also that 'domain' is often blank from client or ignored by server.

=item HTTP-BASIC <username> <password>

Returns the value base64("<username>:<password>").  Used for HTTP Basic authentication (RFC 2617).  Used by adding a header "Authorization: Basic <string>" to a HTTP request where <string> is the output of this command.

=item APOP <challenge> <password>

This implements the APOP authentication for the POP3 protocol as described in RFC 1939.  <challenge> is the challenge string presented by the POP3 server in the greeting banner.  <password> is the "secret" (usually a password) used to authenticate the user.  This method returns a digest md5("<challenge><password>").  This can be used to authenticate to a POP3 server in a string like "APOP <user> <digest>" where <digest> is the string generated by this command.

APOP required the Digest::MD5 perl module.

=item ENCODE <string>

Simply Base64 encodes a plaintext string.  Provided as a convenience function.

=item DECODE <string>

Decodes a Base64 encoded string.  Provided as a convenience function.

=item MD5/MD5-HEX <string>

Provides an MD5 digest of the supplied string in hex.

=item MD5-BASE64 <string>

Provides an MD5 digest of the supplied string in Base64.

=item ENCRYPT <string>

Returns a crypt(3) string generated from the input string.

=item SALTENCRYPT <string> <salt>

Same as ENCRYPT but you provide the salt as the second argument.  See crypt(3) man page for details.

=item ROT13 <string>

This performs a rot13 action on <string>.  This implementation only performs the action on ASCII 65-90,97-123.  Any other character value is left untouched.  Therefore this method is primarily for LOCALE=C, ASCII only.  Feel free to send patches if you care to have it work in another setting.

=item ATBASH <string>

This performs an atbash action on <string>.  Atbash mirrors a string such that 'a'=='z', 'b'=='y', etc.  See the comments on locale and character set under ROT13.

=back

=head1 OPTIONS

=item -s

Supresses echo on all input fields read from standard input.  If this option is not used, echo is suppressed on fields which are known to be password fields but this may not be secure enough.

=item --help

this screen.

=item --version

version info.

=head1 EXAMPLES

=over 4

=item generate a PLAIN AUTH string for user 'tim', password 'tanstaaftanstaaf'

  > gen-auth plain tim tanstaaftanstaaf
  Auth String: AHRpbQB0YW5zdGFhZnRhbnN0YWFm

=item generate a CRAM-MD5 string for user 'tim', password 'tanstaaftanstaaf', 
challenge '<1896.697170952@postoffice.reston.mci.net>', using prompt to 
hide password

  > gen-auth cram-md5                  
  username: tim
  password: 
  challenge: PDE4OTYuNjk3MTcwOTUyQHBvc3RvZmZpY2UucmVzdG9uLm1jaS5uZXQ+
  dGltIGI5MTNhNjAyYzdlZGE3YTQ5NWI0ZTZlNzMzNGQzODkw

=item use the DECODE method to ensure we provided the correct output in our last
example

  > gen-auth decode dGltIGI5MTNhNjAyYzdlZGE3YTQ5NWI0ZTZlNzMzNGQzODkw
  tim b913a602c7eda7a495b4e6e7334d3890

=item use the NTLM (MSN) method to authenticate to a mail server using user 'tim', password 'tanstaaftanstaaf', and domain MAIL.  Both the gen-auth transaction and SMTP transaction are shown to demonstrate the interaction between the two.

  AUTH MSN
  334 NTLM supported
  TlRMTVNTUAABAAAAB7IAAAMAAwAgAAAABAAEACMAAAB0aW1NQUlM
  334 TlRMTVNTUAACAAAAAAAAAAAoAAABggAA9RH5KZlXvygAAACAAAAAZL//4sQAAAAC
  TlRMTVNTUAADAAAAGAAYAEAAAAAYABgAWAAAAAAAAAAwAAAABgAGAHAAAAAGAAYAdgAAAAAAAAA8AAAAAYIAAK3lcO8PldNxIrkbvgKGJRR5owQePUtYaTtLVgfQiVQBywW2yZKyp+VFGqYfgDtdEHQAaQBtAHQAaQBtAA==
  235 Authentication succeeded

  > gen-auth spa
  username: tim
  password: 
  domain: MAIL
  Auth Request: TlRMTVNTUAABAAAAB7IAAAMAAwAgAAAABAAEACMAAAB0aW1NQUlM
  challenge: TlRMTVNTUAACAAAAAAAAAAAoAAABggAA9RH5KZlXvygAAACAAAAAZL//4sQAAAAC
  Auth Response: TlRMTVNTUAADAAAAGAAYAEAAAAAYABgAWAAAAAAAAAAwAAAABgAGAHAAAAAGAAYAdgAAAAAAAAA8AAAAAYIAAK3lcO8PldNxIrkbvgKGJRR5owQePUtYaTtLVgfQiVQBywW2yZKyp+VFGqYfgDtdEHQAaQBtAHQAaQBtAA==

=back

=head1 REQUIRES

=item MIME::Base64

Required for all functionality

=item Digest::MD5

Required for MD5, MD5-BASE64, CRAM-MD5, APOP

=item Digest::SHA1

Required for CRAM-SHA1

=item Authen::NTLM

Required for NTLM/MSN/SPA

=head1 EXIT CODES

=item 0 - no errors occurred

=item 1 - unrecognized type specified

=head1 CONTACT

=item proj-gen-auth@jetmore.net
