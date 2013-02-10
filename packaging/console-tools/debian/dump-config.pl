#!/usr/bin/perl
use Debian::DebConf::Client::ConfModule ':all';
use Data::Dumper;

sub print_get {
    my ($confvar) = @_;
    my ($code, $ret) = get ($confvar);
    return $ret;
}

sub print_metaget {
    my ($confvar, $var) = @_;
    my ($code, $ret) = metaget ($confvar, $var);
    return $ret;
}

print STDERR "Layout: ", print_get ('shared/keymap/layout'), "\n";
print STDERR "Country: ", print_get ('shared/keymap/country'), "\n";
print STDERR "Variant: ", print_get ('shared/keymap/variant'), "\n";
print STDERR "Filename: ", print_get ('shared/keymap/keymap_filename'), "\n";
print STDERR "Keymaps: ", print_metaget('shared/keymap/keymap', 'choices'), "\n";
