#
# Module Boinc::Application - application management
#
# Author: Gabor Gombas <gombasg@sztaki.hu>
#
# This is free software; you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 2.1 of the License, or (at your option)
# any later version.

package Boinc::Application;

use Boinc::Config;
use Boinc::Common;

use IO::File;
use IO::Dir;
use XML::Simple qw(:strict);
use Sys::Hostname;
use File::Temp qw(tempdir);
use Cwd;
use DBI;
use UUID;

use strict;

BEGIN {
	use Exporter();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

	$VERSION = 1.0;
	@ISA = qw(Exporter);
	@EXPORT = qw(list_client_apps list_master_apps
		add_client_app add_master_app
		delete_client_app delete_master_app
		process_archive);

	%EXPORT_TAGS = ();
	@EXPORT_OK = qw();
}

#######################################################################
# Database helper functions

# Extract the database connection parameters from config.xml and connect to the
# database
sub connect_db() {
	my $dbname = `confmgr get db_name`;
	my $dbhost = `confmgr get db_host`;
	my $dbuser = `confmgr get db_user`;
	my $dbpassword = `confmgr get db_passwd`;
	chomp ($dbname, $dbhost, $dbuser, $dbpassword);

	my $dbh = DBI->connect("dbi:mysql:database=$dbname;host=$dbhost",
		$dbuser, $dbpassword, {RaiseError => 1, PrintError => 0});
	return $dbh;
}

#######################################################################
# XML input/output functions

sub expand_base($$) {
	my ($basepath, $file) = @_;
	$file = $basepath . '/' . $file unless $file =~ m!^/!;
	return $file;
}

sub check_master_table($) {
	my $db = shift;

	eval {
		my $res = $db->selectall_arrayref("SELECT COUNT(*) FROM szdg_masterapp");
	};
	if ($@) {
		$db->do("CREATE TABLE szdg_masterapp (" .
			"id		INTEGER		NOT NULL AUTO_INCREMENT, " .
			"name		VARCHAR(254)	NOT NULL, " .
			"version	INTEGER		NOT NULL, " .
			"instance	VARCHAR(254)	NOT NULL," .
			"PRIMARY KEY(id) " .
			") engine=InnoDB");
	}
}

sub check_file_exists($$) {
	my ($basepath, $file) = @_;

	die("Required file '$file' is missing\n")
		unless -f expand_base($basepath, $file);
}

sub chk_scalar($$) {
	my ($hash, $key) = @_;

	return exists $hash->{$key} && $hash->{$key} && !ref($hash->{$key});
}

sub read_client_desc($) {
	my $file = shift;

	my ($basepath) = $file =~ m!^(.*)/!;
	$basepath = './' unless $basepath;

	debug("Reading $file");

	my $client = XMLin($file,
		ForceArray => ['platform', 'lib', 'extra_binary'],
		KeyAttr => [],
		KeepRoot => 1,
		SuppressEmpty => 1);

	# We want to check the root element but do not want to retain it for
	# the rest of the processing
	die("Bad root element in $file\n") unless $client->{'client'};
	$client = $client->{'client'};

	foreach my $key ('name', 'user_friendly_name', 'version') {
		die("Incomplete client specification: missing <$key>\n")
			unless chk_scalar($client, $key);
	}

	die("Incomplete client specification: no platform definitions\n")
		unless exists $client->{'platform'} && ref($client->{'platform'}) eq 'ARRAY';

	foreach my $platform (@{$client->{'platform'}}) {
		die("Missing platform name\n")
			unless chk_scalar($platform, 'name');
		die("No binary specified for platform " . $platform->{'name'} . "\n")
			unless chk_scalar($platform, 'binary');

		check_file_exists($basepath, $platform->{'binary'});
		foreach my $lib (@{$platform->{'lib'}}) {
			check_file_exists($basepath, $lib);
		}
		foreach my $exe (@{$platform->{'extra_binary'}}) {
			check_file_exists($basepath, $exe);
		}
	}

	return $client;
}

sub read_master_desc($$$) {
	my ($project, $file, $instance) = @_;

	my ($basepath) = $file =~ m!^(.*)/!;
	$basepath = './' unless $basepath;

	debug("Reading $file");

	# We will read the master definition a second time below to resolve
	# variable references. Make sure the XMLin attribures are consistent.
	my $master = XMLin($file,
		ForceArray => ['daemon', 'file', 'arg'],
		GroupTags => {files => 'file', arguments => 'arg'},
		KeyAttr => [],
		KeepRoot => 1,
		SuppressEmpty => 1);

	# We want to check the root element but do not want to retain it for
	# the rest of the processing
	die("Bad root element in $file\n") unless $master->{'master'};
	$master = $master->{'master'};

	foreach my $key ('name', 'version') {
		die("Incomplete master specification: missing <$key>\n")
			unless chk_scalar($master, $key);
	}

	die("Incomplete master specification: no daemon definitions\n")
		unless exists $master->{'daemon'} && ref($master->{'daemon'}) eq 'ARRAY';

	foreach my $daemon (@{$master->{'daemon'}}) {
		die("Daemon needs either <binary> or <name>\n")
			unless exists $daemon->{'binary'} || exists $daemon->{'name'};
		die("<binary> and <name> cannot be specified together\n")
			if exists $daemon->{'binary'} && exists $daemon->{'name'};

		check_file_exists($basepath, $daemon->{'binary'})
			if exists $daemon->{'binary'};
		check_file_exists('/usr/lib/boinc-server/sched', $daemon->{'name'})
			if exists $daemon->{'name'};
	}

	if (exists $master->{'configure'}) {
		die("<binary> is missing from <configure>\n")
			unless exists $master->{'configure'}->{'binary'};
		check_file_exists($basepath, $master->{'configure'}->{'binary'});
	}

	foreach my $fname (@{$master->{'files'}}) {
		check_file_exists($basepath, $fname);
	}

	# We have to parse the XML file twice for the workdir substitution
	my $workdir = homedir($project) . '/master/' . $master->{'name'};
	$workdir .= '_' . $instance if $instance;

	$master = XMLin($file,
		ForceArray => ['daemon', 'file', 'arg'],
		GroupTags => {files => 'file', arguments => 'arg'},
		KeyAttr => [],
		SuppressEmpty => 1,
		Variables => {workdir => $workdir,
			project => $project,
			projectroot => projectroot($project),
			basedir => $basepath});

	$master->{'-workdir'} = $workdir;
	$master->{'-instance'} = $instance;

	return $master;
}

#######################################################################
# Application listing

sub list_client_apps($) {
	spawn('appmgr', 'list');
}

sub list_master_apps($) {
	my $project = shift;

	print "Installed master applications:\n";
	print "------------------------------\n";

	my $db = connect_db();
	check_master_table($db);

	my $sth = $db->prepare("SELECT * FROM szdg_masterapp ORDER BY name, instance");
	$sth->execute();
	while (my $row = $sth->fetchrow_hashref()) {
		my $appname = $row->{'name'};
		my $inst = $row->{'instance'};
		$appname .= " [$inst]" if $inst;
		my $ver = sprintf("%.2f", $row->{'version'} / 100);
		print "\t$appname\t$ver\n";
	}
}

#######################################################################
# Install client application

sub signit($$) {
	my ($project, $file) = @_;

	my $keyfile = projectroot($project) . '/keys/code_sign_private';
	return unless -r $keyfile;

	my $ret = spawn("sign_executable '$file' '$keyfile' > '$file.sig'");
	die("sign_executable exited with code $ret\n") if $ret > 0;
}

sub install_file($$$$) {
	my ($project, $src, $dst, $exec) = @_;

	die("Missing client input file $src\n") unless -r $src;

	my $keyfile = projectroot($project) . '/keys/code_sign_private';

	# Do not install signature files if the project has a readable
	# code signing key. Otherwise we could overwrite the output
	# of sign_executable or vice versa.
	return if $dst =~ m/\.sig$/ && -r $keyfile;

	spawn('/bin/cp', '-p', $src, $dst);

	# Only the binaries should be marked as executable
	chmod $exec ? 0755: 0644, $dst;

	# If the file has a signature, copy that too
	if (-r $src . '.sig' && ! -r $keyfile) {
		spawn('/bin/cp', '-p', $src . '.sig', $dst . '.sig');
		chmod 0644, $dst;
	}

	# .file_ref_info files should not be signed
	signit($project, $dst) unless($dst =~ m/\.file_ref_info$/);
}

# Emit a .file_ref_info file that will cause the lib/additional exe to be
# copied under it's original name to the slot directory
sub emit_file_ref_info($$$) {
	my ($project, $orig_name, $dst_prefix) = @_;

	my $outf = new IO::File $dst_prefix . '.file_ref_info', O_WRONLY | O_CREAT | O_TRUNC, 0644
		or die("Failed to create $dst_prefix.file_ref_info\n");
	$outf->print("<open_name>" . $orig_name . "</open_name>\n");
	$outf->print("<copy_file/>");
}

sub add_client_app($$) {
	my ($project, $file) = @_;

	my ($basepath) = $file =~ m!^(.*)/!;
	$basepath = './' unless $basepath;

	my $client = read_client_desc($file);

	# update_versions is picky about the version syntax
	die("Invalid application version\n")
		unless $client->{'version'} =~ m/([0-9]+)\.([0-9]+)/;

	notice("Installing client application " . $client->{'name'} .
		" version " . $client->{'version'});

	my $appdir = projectroot($project) . '/apps/' . $client->{'name'};
	mkdir $appdir, 0775 or die("Failed to create directory $appdir\n")
		unless -d $appdir;

	my $sig_warned = 0;
	foreach my $platform (@{$client->{'platform'}}) {
		die("Bad platform specification\n")
			unless exists $platform->{'name'} &&
				exists $platform->{'binary'};

		# Create the versioned directory
		my $vername = $client->{'name'} . '_' . $client->{'version'} .
			'_' . $platform->{'name'};
		mkdir $appdir . '/' . $vername, 0775
			or die("Failed to create directory $appdir/$vername\n")
			unless -d $appdir . '/' . $vername;

		# Copy the binary to it's destination name. Preserve the extension
		# if the source name has one.
		my $dst = $appdir . '/' . $vername . '/' . $vername;

		my ($ext) = $platform->{'binary'} =~ m:\.([^./])$:;
		$dst .= $ext if $ext;

		install_file($project, expand_base($basepath, $platform->{'binary'}), $dst, 1);

		# Copy additional libraries if there are any
		foreach my $lib (@{$platform->{'lib'}}) {
			my $filename = $lib;
			$filename =~ s!^.*/!!;
			$dst = $appdir . '/' . $vername . '/' . $vername . '_lib_' . $filename;

			install_file($project, expand_base($basepath, $lib), $dst, 0);
			emit_file_ref_info($project, $filename, $dst);
		}

		# Copy additional executables if there are any
		foreach my $exe (@{$platform->{'extra_binary'}}) {
			my $filename = $exe;
			$filename =~ s!^.*/!!;
			$dst = $appdir . '/' . $vername . '/' . $vername . '_exe_' . $filename;

			install_file($project, expand_base($basepath, $exe), $dst, 1);
			emit_file_ref_info($project, $filename, $dst);
		}

		# Check that all signatures are present
		my $dh = new IO::Dir $appdir . '/' . $vername;
		while (my $name = $dh->read) {
			next if $name eq '.' || $name eq '..';
			next if $name =~ m/\.(sig|file_ref_info)$/;
			next if -f $appdir . '/' . $vername . '/' . $name . '.sig';

			unless ($sig_warned) {
				print STDERR <<"EOF";
The following files are not signed. You must manually sign them before they
can be used:
EOF
				$sig_warned = 1;
			}

			print STDERR $appdir . '/' . $vername . '/' . $name . "\n";
		}
	}

	notice("Running appmgr");
	spawn('appmgr', 'add', $client->{'name'}, $client->{'user_friendly_name'});

	notice("Running update_versions");
	spawn('update_versions', '--sign', '--force') == 0
		or die("update_versions have failed, check the log for details. " .
			"You may need to manually clean up the database.\n");
}

#######################################################################
# Delete client application

sub delete_client_app($$$) {
	my ($project, $name, $version) = @_;

	my @cmd = ('appmgr', 'delete', $name);
	if ($version) {
		push @cmd, ('--version', $version);
	}

	spawn(@cmd);
}

#######################################################################
# Install master application

sub copy_or_link($$$) {
	my ($src, $dst, $needcopy) = @_;

	if ($needcopy) {
		spawn('/bin/cp', '-p', $src, $dst);
	}
	else {
		symlink($src, $dst);
	}
}

sub install_daemons($$$$) {
	my ($project, $master, $basepath, $needcopy) = @_;

	my $appname = $master->{'name'};
	$appname .= '_' . $master->{'-instance'} if $master->{'-instance'};

	my $daemon_cnt = 1;
	foreach my $daemon (@{$master->{'daemon'}}) {

		# Include the original name in the symlinked name for easier
		# identification
		my $name = $daemon->{'binary'} if exists $daemon->{'binary'};
		$name = $daemon->{'name'} if exists $daemon->{'name'};
		$name =~ s!^.*/!!;

		# This name must be unique for all installed applications
		my $daemon_name = 'app_' . $appname . '_' . $name . '_' . $daemon_cnt;

		copy_or_link(expand_base($basepath, $daemon->{'binary'}),
				projectroot($project) . '/bin/' . $daemon_name, $needcopy)
			if exists $daemon->{'binary'};
		copy_or_link(expand_base('/usr/lib/boinc-server/sched', $daemon->{'name'}),
				projectroot($project) . '/bin/' . $daemon_name, $needcopy)
			if exists $daemon->{'name'};

		my $cmdline = $daemon_name;
		$cmdline .= ' ' . join(' ', @{$daemon->{'arguments'}})
			if exists $daemon->{'arguments'};

		spawn('confmgr', 'add_daemon', $cmdline);
		$daemon_cnt++;
	}
}

sub print_dcapi_missing($$$$) {
	my ($project, $master, $flags, $outf) = @_;

	$outf->print("WorkingDirectory = " . $master->{'-workdir'} . "\n")
		unless exists $flags->{'saw_workingdirectory'};
	unless (exists $flags->{'saw_uuid'}) {
		my ($uuid, $uuid_str);

		UUID::generate($uuid);
		UUID::unparse($uuid, $uuid_str);
		$outf->print("InstanceUUID = $uuid_str\n");
	}
	$outf->print("BoincConfigXML = " . projectroot($project) . "/config.xml\n")
		unless exists $flags->{'saw_uuid'};
	$outf->print("ProjectRootDir = " . projectroot($project) . "\n")
		unless exists $flags->{'saw_uuid'};
}

sub process_dcapi_conf($$$) {
	my ($project, $master, $file) = @_;

	my $inf = new IO::File $file, O_RDONLY
		or die("Failed to open DC-API configuration file $file\n");
	my $outf = new IO::File $file . '.tmp', O_WRONLY | O_CREAT, 0644
		or die("Failed to create $file.tmp\n");

	my $flags = {};
	while (my $line = $inf->getline) {
		if ($line =~ m/^\s*\[Master\]/i) {
			$flags->{'in_master'} = 1;
			$flags->{'saw_master'} = 1;
		}
		elsif ($line =~ m/^\s*\[/) {
			delete $flags->{'in_master'};
			print_dcapi_missing($project, $master, $flags, $outf);
		}
		if (exists($flags->{'in_master'})) {
			if ($line =~ m/^\s*WorkingDirectory\s*=/) {
				my $val = $master->{'-workdir'};

				$line =~ s/=.*$/= $val/;
				$flags->{'saw_workingdirectory'} = 1;
			}
			if ($line =~ m/^\s*InstanceUUID\s*=/) {
				my ($uuid, $uuid_str);

				UUID::generate($uuid);
				UUID::unparse($uuid, $uuid_str);

				$line =~ s/=.*$/= $uuid_str/;
				$flags->{'saw_uuid'} = 1;
			}
			if ($line =~ m/^\s*BoincConfigXML\s*=/) {
				my $val = projectroot($project) . '/config.xml';

				$line =~ s/=.*$/= $val/;
				$flags->{'saw_configxml'} = 1;
			}
			if ($line =~ m/^\s*ProjectRootDir\s*=/) {
				my $val = projectroot($project);

				$line =~ s/=.*$/= $val/;
				$flags->{'saw_projectroot'} = 1;
			}
		}

		# General substitutions
		my $val = $master->{'-workdir'};
		$line =~ s/\@workdir\@/$val/;

		$outf->print($line);
	}

	unless (exists($flags->{'saw_master'})) {
		$outf->print("[Master]\n");
		print_dcapi_missing($project, $master, $flags, $outf);
	}

	$inf->close;
	$outf->close;
	rename $file . '.tmp', $file;
}

sub add_master_app($$$$) {
	my ($project, $file, $instance, $is_archive) = @_;

	my ($basepath) = $file =~ m!^(.*)/!;
	$basepath = '' unless $basepath;
	$basepath = getcwd() . $basepath unless $basepath =~ m!^/!;

	my $master = read_master_desc($project, $file, $instance);
	my $db = connect_db();
	check_master_table($db);

	# Convenience, dealing with NULL is harder
	$instance = '' unless $instance;

	my $row = $db->selectrow_arrayref("SELECT COUNT(*) FROM szdg_masterapp " .
		"WHERE name = ? AND instance = ?", {}, $master->{'name'}, $instance);
	die("Master application " . $master->{'name'} . " (instance: '$instance') " .
		"is already installed\n") if $row->[0] > 0;

	notice("Installing master application " . $master->{'name'} .
		" version " . $master->{'version'});

	mkdir homedir($project) . '/master', 0755
		unless -d homedir($project) . '/master';
	mkdir $master->{'-workdir'}, 0755;

	# Copy the needed files to the work directory
	foreach my $file (@{$master->{'files'}}) {
		spawn('/bin/cp', '-p', expand_base($basepath, $file),
			$master->{'-workdir'});
	}

	# Run the configuration script
	if (exists $master->{'configure'}) {
		notice('Running the configuraton script');
		my $bin = $master->{'configure'}->{'binary'};
		my $pwd = getcwd;
		chdir($master->{'-workdir'});
		my $ret = spawn(expand_base($basepath, $bin),
			@{$master->{'configure'}->{'arguments'}});
		chdir($pwd);
		if ($ret > 0) {
			spawn('/bin/rm', '-rf', $master->{'-workdir'});
			die("The configuration script failed with code $ret\n");
		}
	}

	# Modify the DC-API configuration file if exists
	if (exists $master->{'dcapi-conf'})
	{
		notice('Fixing up the DC-API configuration');

		my $file = $master->{'dcapi-conf'};
		# Copy it to the working directory first
		spawn('/bin/cp', '-p', expand_base($basepath, $file), $master->{'-workdir'});

		$file =~ s!^.*/!!;
		$file = $master->{'-workdir'} . '/' . $file;

		process_dcapi_conf($project, $master, $file);
	}

	install_daemons($project, $master, $basepath, $is_archive);

	# Register the application in the database
	my $major = int($master->{'version'});
	my $minor = ($master->{'version'} - int($master->{'version'})) * 100;
	my $vernum = $major * 100 + $minor;

	$db->do("INSERT INTO szdg_masterapp (name, version, instance) VALUES (?, ?, ?)",
		{}, $master->{'name'}, $vernum, $instance);

	# Leave an opportunity for the administrator to verify if everything
	# went smoothly, so do not restart the project automatically
	my $appname = $master->{'name'};
	$appname .= '_' . $instance if $instance;
	print(<<"EOF");
Application $appname has been installed. Please review the configuration,
and if it is correct, restart the project by issuing the commands
	stop
	start
EOF
}

#######################################################################
# Delete master application

sub is_enabled($) {
	my $project = shift;

	my $state = XMLin(projectroot($project) . '/run_state_' . hostname . '.xml',
		ForceArray => [],
		KeyAttr => [],
		NormaliseSpace => 2,
		SuppressEmpty => 1);
	return $state->{'enabled'} + 0;
}

sub delete_master_app($$$) {
	my ($project, $name, $instance) = @_;

	$instance = '' unless defined $instance;

	my $db = connect_db();
	my $row = $db->selectrow_arrayref("SELECT COUNT(*) FROM szdg_masterapp " .
		"WHERE name = ? AND instance = ?", {}, $name, $instance);
	die("Master application $name (instance: '$instance') is not installed\n")
		unless $row->[0] > 0;

	my $appname = $name;
	$appname .= '_' . $instance if $instance;

	notice("Removing master application $appname");

	# We have to shut down the daemons first
	my $enabled = is_enabled($project);
	if ($enabled) {
		notice("Stopping project");
		spawn('stop');
	}

	spawn('confmgr', 'del_daemon', '--regexp', '^app_' . $appname . '_[^_]+_[0-9]+');

	# Run the uninstall script if it exists
	my $workdir = homedir($project) . '/master/' . $appname;
	if (-x $workdir . '/uninstall') {
		my $pwd = getcwd;
		notice('Running the uninstall script');
		chdir($workdir);
		spawn($workdir . '/uninstall');
		chdir ($pwd);
	}

	notice("Removing daemon symlinks");
	spawn("/bin/rm " . projectroot($project) . '/bin/app_' . $appname . '_*');

	notice("Removing daemon files");
	spawn('/bin/rm', '-rf', $workdir);

	# Update the database
	$db->do("DELETE FROM szdg_masterapp WHERE name = ? AND instance = ?",
		{}, $name, $instance);

	# Restart the project if it was enabled before
	if ($enabled) {
		notice("Restarting project");
		spawn('start');
	}
}

#######################################################################
# Process an archive

sub process_archive($$$) {
	my ($project, $file, $options) = @_;

	my $tarflags;
	$tarflags = '--gzip' if $file =~ m/\.gz$/;
	$tarflags = '--bzip2' if $file =~ m/\.bz2$/;

	my $dir = tempdir('appmgr_XXXXXX', {TMPDIR => 1, CLEANUP => 1})
		or die("Failed to create temporary directory\n");

	my @cmd = ('/bin/tar', '--directory', $dir, '--extract');
	push @cmd, $tarflags if $tarflags;
	push @cmd, ('--file', $file);
	my $ret = spawn(@cmd);
	die("Failed to extract archive $file\n") if $ret > 0;

	eval {
		add_client_app($project, $dir . '/client.xml')
			if -f $dir . '/client.xml' && exists $options->{'CLIENT'};
		add_master_app($project, $dir . '/master.xml', $options->{'instance'}, 1)
			if -f $dir . '/master.xml' && exists $options->{'MASTER'};
	};
	spawn('/bin/rm', '-rf', $dir);
	die($@) if $@;
}

# Signal that the module was parsed successfully
1;
