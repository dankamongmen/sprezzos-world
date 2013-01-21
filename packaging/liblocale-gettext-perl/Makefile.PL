use ExtUtils::MakeMaker;
use Config;

my $cc;
if (defined($ENV{'CC'})) {
	$cc = $ENV{'CC'};
} else {
	$cc = $Config{'cc'};
}
my $libs = '';

unless (conftest("char *x = gettext(\"foo\");", "gettext", 0)) {
	# try with -lintl
	$libs = "-lintl";
	unless (conftest("char *x = gettext(\"foo\");", "gettext", 0)) {
		unlink("conftest.c");
		unlink("conftest");
		die "gettext function not found. Please install libintl";
	}
}

open(CONFIG, ">config.h");
print CONFIG "/* Generated automatically by ", $0, ". Do not edit */\n";

conftest("char *x = dcgettext(\"foo\", \"bar\", 0);", "dgettext", 1);
conftest("char *x = ngettext(\"foo\", \"foos\", 1);", "ngettext", 1);
conftest("char *x = bind_textdomain_codeset(\"foo\", \"UTF-8\");", "bind_textdomain_codeset", 1);

close CONFIG;

unlink("conftest.c");
unlink("conftest");

WriteMakefile(
    NAME => "Locale::gettext",
    LIBS => ($libs eq '') ? [] : [$libs],
    VERSION_FROM => 'gettext.pm', 
);

sub conftest {
	my ($testcode, $func, $record) = @_;

	print "checking for ", $func;
	print(" in ", $libs) if ($libs ne '');
	print "...";
	open(TEST, ">conftest.c");
	print TEST "#include <libintl.h>\n\nint main(int argc, char **argv)\n{\n";
	print TEST $testcode;
	print TEST "return 0;}\n";
	close TEST;
	open(SAVE, ">&STDERR");
	open(STDERR, ">/dev/null");
	system($cc . " -o conftest " . $libs . " conftest.c");
	my $exitstatus = $?;
	open(STDERR, ">&SAVE");
	if ($exitstatus != 0) {
		print " no\n";
		return 0;
	} else {
		print " yes\n";
		if ($record) {
			print CONFIG "#define HAVE_", uc($func), "\n";
		}
		return 1;
	}
}
