#
# Module Boinc::Config - Configuration values
#
# Author: Gabor Gombas <gombasg@sztaki.hu>
#
# This is free software; you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 2.1 of the License, or (at your option)
# any later version.

package Boinc::Config;

use vars qw($project_dir);
use strict;

BEGIN {
	use Exporter();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

	$VERSION = 1.0;
	@ISA = qw(Exporter);
	@EXPORT = qw(username dbname homedir projectroot);

	%EXPORT_TAGS = ();
	@EXPORT_OK = qw();
}

#######################################################################
# System configuration variables

$project_dir = "/var/lib/boinc";

#######################################################################
# Helper functions

sub homedir($) {
	my $name = shift;

	return $project_dir . '/'. $name;
}

sub username($) {
	my $name = shift;

	return 'boinc-' . $name;
}

sub dbname($) {
	my $name = shift;

	return 'boinc_' . $name;
}

sub projectroot($) {
	my $name = shift;

	return homedir($name) . '/project';
}

# Signal that the module was parsed successfully
1;
