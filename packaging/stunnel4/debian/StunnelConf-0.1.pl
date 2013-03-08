#!/usr/bin/perl

# Copyright (C) 2004 Sergio Rua <srua@debian.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
#
# On Debian GNU/Linux systems, the complete text of the GNU General
# Public License can be found in `/usr/share/common-licenses/GPL'.

use strict;
use Gtk2;
use Gnome2;
use Gtk2::SimpleList;

use constant TRUE => 1;
use constant FALSE => 0;
# Please configure if necessary!
my $cfgfile = "/etc/stunnel/stunnel.conf";
my $backup_cfg = 1;
my $base_cfg_dir = $cfgfile;$base_cfg_dir=~s/\/stunnel\.conf//g;

# global variables
my $ekey;
my $ecert;
my $verify;
my $app;
my $elog;
my $clientmode;
my $debuglevel;
my $capath;
my $list;


sub mydie
{
	my ($msg)=@_;

	print "$msg\n";
	Gtk2->main_quit;
	exit (-1);
}


sub sel_file
{
	my ($title,$entry,$isfile)=@_;

	my $fsel=Gtk2::FileSelection->new($title);
	$fsel->ok_button->signal_connect("clicked",sub { 
			print "OK: ". $fsel->get_filename."\n"; 
			$entry->set_text($fsel->get_filename); 
			$fsel->destroy;
		});
	$fsel->cancel_button->signal_connect("clicked",sub { $fsel->destroy; });

	$fsel->show;
}

sub add_connection
{
	my $win = new Gtk2::Window("toplevel");
	$win->set_position("center");

	my $vbox = new Gtk2::VBox( 0, 2 );
	$win->add($vbox);
	$vbox->show;
	my $druid = new Gnome2::Druid;
	$druid->signal_connect("cancel", sub { $win->destroy; } );
	$vbox->pack_start($druid,0,0,0);
	my $druid_start = new Gnome2::DruidPageEdge("GNOME_EDGE_START");
	$druid_start->set_title("Connections setup");
	$druid_start->set_text("Please follow this configuration wizard to ".
				"configure your connections\n");
#	$druid_start->set_watermark($logo);
	$druid_start->show;
	$druid->append_page($druid_start);

# Second Step: accepting connections
	my $druid_name = new Gnome2::DruidPageStandard();
	$druid_name->set_title("Connection name");
	my $dvbox=new Gtk2::VBox(2,2);
	my $dtable=new Gtk2::Table(2,2,FALSE);	
	$dvbox->pack_start($dtable,FALSE,FALSE,0);

	my $label=new Gtk2::Label("Enter this connection name");
	$dtable->attach($label,0,1,0,1,["fill"],["fill"],0,0);
	my $ename=new Gtk2::Entry();
	$dtable->attach($ename,1,2,0,1,["fill"],["fill"],0,0);
	$druid_name->append_item("",$dvbox,"");
	$druid_name->show_all;
	# add page to the druid
	$druid->append_page($druid_name);


# Second Step: accepting connections
	my $druid_accept = new Gnome2::DruidPageStandard();
	$druid_accept->set_title("Accepting connections");
	my $dvbox=new Gtk2::VBox(2,2);
	my $dtable=new Gtk2::Table(2,2,FALSE);	
	$dvbox->pack_start($dtable,FALSE,FALSE,0);

	my $accept_error=new Gtk2::Label("");
	$dtable->attach($accept_error,0,1,0,1,["fill"],["fill"],0,0);
	my $label=new Gtk2::Label("IP or hostname");
	$dtable->attach($label,0,1,1,2,["fill"],["fill"],0,0);
	my $eip=new Gtk2::Entry();
	$dtable->attach($eip,1,2,1,2,["fill"],["fill"],0,0);

	my $label=new Gtk2::Label("Port number");
	$dtable->attach($label,0,1,2,3,["fill"],["fill"],0,0);
	my $eport=new Gtk2::Entry();
	$dtable->attach($eport,1,2,2,3,["fill"],["fill"],0,0);

	$druid_accept->append_item("",$dvbox,"");
	$druid_accept->show_all;
	# add page to the druid
	$druid->append_page($druid_accept);

# Third Step: connecting to...
	my $druid_connect = new Gnome2::DruidPageStandard();
	$druid_connect->set_title("Connection To...");
	my $dvbox=new Gtk2::VBox(2,2);
	my $dtable=new Gtk2::Table(2,2,FALSE);	
	$dvbox->pack_start($dtable,FALSE,FALSE,0);

	my $label=new Gtk2::Label("IP or hostname");
	$dtable->attach($label,0,1,0,1,["fill"],["fill"],0,0);
	my $etoip=new Gtk2::Entry();
	$dtable->attach($etoip,1,2,0,1,["fill"],["fill"],0,0);

	my $label=new Gtk2::Label("Port number");
	$dtable->attach($label,0,1,1,2,["fill"],["fill"],0,0);
	my $etoport=new Gtk2::Entry();
	$dtable->attach($etoport,1,2,1,2,["fill"],["fill"],0,0);

	$druid_connect->append_item("",$dvbox,"");
	$druid_connect->show_all;
	# add page to the druid
	$druid->append_page($druid_connect);


# Finishing and adding connection
	my $druid_finish = new Gnome2::DruidPageEdge("GNOME_EDGE_FINISH");
	$druid_finish->set_title("Configuration Finished.");
	$druid_finish->set_text("The configuration has been finished. Click to either save or cancel");
#	$druid_finish->set_logo($logo2);
	$druid_finish->signal_connect("finish", sub { 
			my $acip=$eip->get_text();
			my $acport=$eport->get_text();
			my $coip=$etoip->get_text();
			my $coport=$etoport->get_text();
			
			my $dslist = $list->{data};
			push @$dslist, [ $ename->get_text(), $acip.":".$acport, $coip.":".$coport ];

		
			$win->destroy;
		});
	$druid_finish->show;
	$druid->append_page($druid_finish);
	$druid->show;
	$win->show;
}

sub load_config_file
{
	my $con=$list->{data};
	my $name="";
	my $accept="";
	my $connect="";

	if (! -s $cfgfile) {
		print "Config file not found. Starting from scratch!\n";
		return (0);
	}

	open F, "<$cfgfile" or die "$cfgfile: $!\n";

	while (<F>) {
		$_=~s/\n//g;
		if ($_=~/^cert.*=.*/) {
			(undef,my $value) = split "=",$_;
			$value=~s/(\ |\t)//g;
			$ecert->set_text($value);
		} elsif ($_=~/^key.*=.*/) {
			(undef,my $value) = split "=",$_;
			$value=~s/(\ |\t)//g;
			$ekey->set_text($value);
		} elsif ($_=~/^verify.*=.*/) {
			(undef,my $value) = split "=",$_;
			$value=~s/(\ |\t)//g;
			if ($value==1) {
				$verify->entry->set_text("verify peer certificate if present");
			} elsif ($value==2) {
				$verify->entry->set_text("verify peer certificate");
			} elsif ($value==3) {
				$verify->entry->set_text("verify peer with locally installed certificate");
			} else {
				$verify->entry->set_text("no verify");
			}
		} elsif ($_=~/^client.*=.*/) {
			(undef,my $value) = split "=",$_;
			$value=~s/(\ |\t)//g;
			$clientmode->entry->set_text($value);
		} elsif ($_=~/^(capath|CApath).*=.*/) {
			(undef,my $value) = split "=",$_;
			$value=~s/(\ |\t)//g;
			$capath->set_text($value);
		} elsif ($_=~/^debug.*=.*/) {
			(undef,my $value) = split "=",$_;
			$value=~s/(\ |\t)//g;
			$debuglevel->entry->set_text($value);
		} elsif ($_=~/^output.*=.*/) {
			(undef,my $value) = split "=",$_;
			$value=~s/(\ |\t)//g;
			$elog->set_text($value);
		} elsif ($_=~/^\[.*/) {
			$_=~s/\[//g;
			$_=~s/\]//g;
			$name=$_;
		} elsif ($_=~/^accept.*=.*/) {
			(undef,$accept) = split "=",$_;
			$accept=~s/(\ |\t)//g;
		} elsif ($_=~/^connect.*=.*/) {
			(undef,$connect) = split "=",$_;
			$connect=~s/(\ |\t)//g;
		}

		# load connection
		if (($accept) && ($name) && ($connect)) {
			push @$con, [ $name, $accept, $connect ];	
			$name=$connect=$accept="";
		}
	}
	close F;

}

sub save_config_file
{
	if ($backup_cfg) {
		chdir ($base_cfg_dir);
		rename($cfgfile,$cfgfile.".$$") or 
			print "Error at \n$cfgfile: $!\nNo backup made!\n";
	}
	open O, ">$cfgfile" or 
		mydie "Cannot open config file: $!\n";

	print "Saving $cfgfile\n\n\n";
	print O "# Configuration file created by \"stunnelconf\" by ".
		"Sergio Rua <srua\@debian.org>\n\n";
	if ($ekey->get_text()) {
		print O "key = ".$ekey->get_text()."\n";
	}
	if ($ecert->get_text()) {
		print O "cert = ".$ecert->get_text()."\n";
	}
	print O "verify = ".$verify->entry->get_text()."\n";
	print O "output = ".$elog->get_text()."\n";
	print O "client = ".$clientmode->entry->get_text()."\n";
	print O "debug = ".$debuglevel->entry->get_text()."\n";
	print O "CApath = ".$capath->get_text()."\n";
	print O "\n\n"; # just some spaces

	my @rowref = @{$list->{data}};
	my $i=0;

	for $i (0 .. $#rowref) {
		print O "[".$rowref[$i][0] . "]\n";
		# if no hostname, ugly ":" to be removed
		$rowref[$i][1]=~s/^://g;
		$rowref[$i][2]=~s/^://g;
		print O "accept = ".$rowref[$i][1] . "\n";
		print O "connect = ".$rowref[$i][2] . "\n";
		print O "\n"; # just some spaces
	}

	close O;
	Gtk2->main_quit;
	return 0;
}


sub create_main_win
{
	$app = Gnome2::App->new ("stunnel-conf");
	$app->set_default_size(470,410);
	$app->signal_connect( 'destroy' => sub { Gtk2->main_quit; } );
	$app->set_title("Stunnel Configuration");

	my $vbox=Gtk2::VBox->new(FALSE,0);
	my $frame=Gtk2::Frame->new("Common options");
	$vbox->pack_start($frame,TRUE, TRUE, 0);

	my $table=Gtk2::Table->new(6, 2, FALSE);
	$frame->add($table);

	my $label0=Gtk2::Label->new("Private Key");
	$table->attach($label0,0,1,0,1,["fill"],["fill"],0,0);
	my $label1=Gtk2::Label->new("Certificate");
	$table->attach($label1,0,1,1,2,["fill"],["fill"],0,0);
	my $label2=Gtk2::Label->new("Verify level");
	$table->attach($label2,0,1,2,3,["fill"],["fill"],0,0);
	my $label3=Gtk2::Label->new("Log output");
	$table->attach($label3,0,1,3,4,["fill"],["fill"],0,0);
	my $label4=Gtk2::Label->new("Client mode");
	$table->attach($label4,0,1,4,5,["fill"],["fill"],0,0);
	my $label5=Gtk2::Label->new("Debug level");
	$table->attach($label5,0,1,5,6,["fill"],["fill"],0,0);
	my $label6=Gtk2::Label->new("Certificates path");
	$table->attach($label6,0,1,6,7,["fill"],["fill"],0,0);

	# Private Key
	my $hbox0=Gtk2::HBox->new(FALSE,0);
	$table->attach($hbox0,1,2,0,1,["fill"],["fill"],0,0);

	$ekey=Gtk2::Entry->new();
	$hbox0->pack_start($ekey,TRUE,TRUE,0);

	my $bkey=Gtk2::Button->new_from_stock("gtk-open");
	$bkey->signal_connect("clicked",sub {
			sel_file("Select private key",$ekey);
		});
	$hbox0->pack_start($bkey,FALSE,FALSE,0);

	# Certificate
	my $hbox1=Gtk2::HBox->new(FALSE,0);
	$table->attach($hbox1,1,2,1,2,["fill"],["fill"],0,0);

	$ecert=Gtk2::Entry->new();
	$hbox1->pack_start($ecert,TRUE,TRUE,0);

	my $bcert=Gtk2::Button->new_from_stock("gtk-open");
	$bcert->signal_connect("clicked",sub {
			sel_file("Select certificate",$ecert);
		});
	$hbox1->pack_start($bcert,FALSE,FALSE,0);

	# Auth level - verify
	$verify = Gtk2::Combo->new();
	$verify->entry->set_text("no verify");
	$verify->set_popdown_strings(("no verify",
			"verify peer certificate if present",
			"verify peer certificate",
			"verify peer with locally installed certificate"));
	$table->attach($verify,1,2,2,3,["fill"],["fill"],0,0);

	# Log output
	my $hbox2=Gtk2::HBox->new(FALSE,0);
	$table->attach($hbox2,1,2,3,4,["fill"],["fill"],0,0);

	$elog=Gtk2::Entry->new();
	$hbox2->pack_start($elog,TRUE,TRUE,0);

	my $blog=Gtk2::Button->new_from_stock("gtk-open");
	$blog->signal_connect("clicked",sub {
			sel_file("Select log file",$elog);
		});
	$hbox2->pack_start($blog,FALSE,FALSE,0);

	# Client mode
	$clientmode = Gtk2::Combo->new();
	$clientmode->entry->set_text("no verify");
	$clientmode->set_popdown_strings(("yes","no"));
	$table->attach($clientmode,1,2,4,5,["fill"],["fill"],0,0);

	# Debug level
	$debuglevel = Gtk2::Combo->new();
	$debuglevel->entry->set_text("no verify");
	$debuglevel->set_popdown_strings(("0","1","5","7"));
	$table->attach($debuglevel,1,2,5,6,["fill"],["fill"],0,0);

	# CA path
	my $hbox3=Gtk2::HBox->new(FALSE,0);
	$table->attach($hbox3,1,2,6,7,["fill"],["fill"],0,0);

	$capath=Gtk2::Entry->new();
	$hbox3->pack_start($capath,TRUE,TRUE,0);

#	my $bcapath=Gtk2::Button->new_from_stock("gtk-open");
#	$bcapath->signal_connect("clicked",sub {
#			sel_file("Select Certificates Path",$capath);
#		});
#	$hbox3->pack_start($bcapath,FALSE,FALSE,0);

	# connections section
	my $frame2=Gtk2::Frame->new("Connections");
	$vbox->pack_start($frame2,TRUE, TRUE, 0);

	my $hbox4=Gtk2::HBox->new(FALSE,0);
	$list=Gtk2::SimpleList->new (
			'Name'    => 'text',
			'Accept'  => 'text',
			'Connect' => 'text',
	);
#	$list->get_selection->set_mode ('multiple');
	my $scwin = Gtk2::ScrolledWindow->new;
	$scwin->set_policy (qw/automatic automatic/);
	$scwin->add($list);

	$hbox4->pack_start($scwin,TRUE,TRUE,0);
	
	# list buttons
	my $vbbox=Gtk2::VButtonBox->new();
	$vbbox->set_layout('spread');
	my $badd = Gtk2::Button->new_from_stock('gtk-add');
	$badd->signal_connect( 'clicked' => sub { add_connection; } );
	$vbbox->add($badd);


#	my $bedit = Gtk2::Button->new_from_stock('gtk-properties');
#	$bedit->signal_connect( 'clicked' => sub { 
#			print "Edit\n";
#		 } );
#	$vbbox->add($bedit);


	my $brem = Gtk2::Button->new_from_stock('gtk-remove');
	$brem->signal_connect( 'clicked' => sub { 
			my @sel = $list->get_selected_indices;
			print @sel;
			foreach my $i (@sel) {
				delete $list->{data}[$i];
			}
		 } );
	$vbbox->add($brem);

	$hbox4->pack_start($vbbox,FALSE,FALSE,0);

	# main buttons!!!
	my $bbox=Gtk2::HButtonBox->new();
	$bbox->set_layout('spread');

	my $bok = Gtk2::Button->new_from_stock('gtk-ok');
	$bok->signal_connect( 'clicked' => sub { save_config_file; } );
	$bbox->add($bok);

	my $bcancel = Gtk2::Button->new_from_stock('gtk-cancel');
	$bcancel->signal_connect( 'clicked' => sub { Gtk2->main_quit;} );
	$bbox->add($bcancel);

	$vbox->pack_start($bbox,FALSE,FALSE,0);
	$frame2->add($hbox4);
	
	
# App contents and show them
	$app->set_contents($vbox);
	$app->show_all;
}

#
# MAIN MAIN MAIN
#


#
Gnome2::Program->init ("stunnelconf", "0.1");
$app=create_main_win;
load_config_file;

Gtk2->main;

exit 0; 

