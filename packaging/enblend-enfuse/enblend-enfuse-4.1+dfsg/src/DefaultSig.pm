# name:         DefaultSig.pm
# synopsis:     base class for signature generation
# author:       Dr. Christoph L. Spiel
# perl version: 5.10.0


# This file is part of Enblend.
# Licence details can be found in the file COPYING.


package DefaultSig;


use strict;
use warnings;

use English;
use Sys::Hostname;

our %HAVE_MODULE;
BEGIN {
    foreach my $module (qw(Time::Zone)) {
        eval "use $module ()"; # import, but keep module's original name space for clarity
        $HAVE_MODULE{$module} = $EVAL_ERROR eq '';
    }
}


sub new {
    my ($class, $do_use_gmt) = @_;

    my $self = {};
    bless $self, $class;
    $self->_initialize($do_use_gmt);

    return $self;
}


sub _initialize {
    my ($self, $do_use_gmt) = @_;

    $self->{USERNAME} = $self->_real_user_name();
    $self->{HOSTNAME} = $self->_clean_hostname();

    $self->use_gmt($do_use_gmt);
    $self->update_date_and_time();
}


sub get_username {my $self = shift;  return $self->{USERNAME}}
sub get_hostname {my $self = shift;  return $self->{HOSTNAME}}
sub get_date {my $self = shift;  return $self->{DATE}}
sub get_time {my $self = shift;  return $self->{TIME}}

sub is_using_gmt {my $self = shift;  return $self->{USE_GMT} != 0}

sub use_gmt {
    my ($self, $x) = @_;
    $self->{USE_GMT} = $x ? ($x == 0 ? 0 : 1) : 0;
}


# Answer a formatted date based on the current date.
#
# 1 <= $day_of_month <= 31
# 1 <= $month <= 12
# 1900 <= $year
# 0 <= $day_of_week <= 7
# $weekday: Mon, Tue, ...
# $monthname: Jan, Feb, ...
sub format_date {
    my ($self,
        $day_of_month, $month, $year, $day_of_week,
        $weekday, $monthname) = @_;

    return sprintf("%s, %s %02u %u", $weekday, $monthname, $day_of_month, $year);
}


# Answer a formatted time based on the current time.
sub format_time {
    my ($self,
        $second, $minute, $hour) = @_;

    my $time = sprintf("%02u:%02u:%02u", $hour, $minute, $second);

    if ($self->is_using_gmt()) {
        $time .= " GMT";
    } else {
        if ($HAVE_MODULE{'Time::Zone'}) {
            $time .= sprintf(" GMT%+d", Time::Zone::tz_offset($ENV{'TZ'}) / 3600.0);
            # alternatively:
            #     $time .= " " . Time::Zone::tz_name($ENV{'TZ'});
        }
    }

    return $time;
}


sub weekdays {my $self = shift;  return [qw(Sun Mon Tue Wed Thu Fri Sat)]}
sub monthnames {my $self = shift;  return [qw(Jan Feb Mar May Jun Jul Aug Sep Oct Nov Dec)]}


# Update date and time fields simultaneously to get a consistent time
# stamp.  If is_using_gmt() use GMT instead of the local time (maybe
# defined by the TZ environment variable).
sub update_date_and_time {
    my $self = shift;

    my ($second, $minute, $hour,
        $day_of_month, $month, $year,
        $day_of_week) = $self->is_using_gmt() ? gmtime : localtime;

    $self->{DATE} = $self->format_date($day_of_month, $month, $year + 1900, $day_of_week,
                                       $self->weekdays->[$day_of_week],
                                       $self->monthnames->[$month - 1]);
    $self->{TIME} = $self->format_time($second, $minute, $hour);
}


sub signature {
    my $self = shift;

    return sprintf("Compiled on %s by %s on %s, %s.",
                   $self->get_hostname(),
                   $self->get_username(),
                   $self->get_date(),
                   $self->get_time());
}


sub _login_name {
    my $self = shift;

    return getlogin or 'anonymous';
}


# Try to derive the real user name from Perl's built-in user id.
# Answer the user's login name if we do not find a real name.
sub _real_user_name {
    my $self = shift;

    if ($OSNAME eq 'MSWin32') {
        return $self->_login_name();
    } else {
        my ($login_name, undef, undef, undef,
            undef, undef, $gcos) = getpwuid $REAL_USER_ID;
        if ($gcos) {
            my ($real_user_name) = split m/,/, $gcos;
            return $real_user_name if $real_user_name;
        }
        if ($login_name) {
            return $login_name;
        } else {
            return $self->_login_name();
        }
    }
}


sub _clean_hostname {
    my $self = shift;

    my $hostname = hostname();

    if ($OSNAME =~ m/MacOS/) {
        $hostname =~ s/^(.*?)[-.].*$/$1/;
    }

    return $hostname;
}


1;
