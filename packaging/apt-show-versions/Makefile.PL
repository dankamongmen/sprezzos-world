use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

# List here scripts that should be installed 
my @scripts = ("./apt-show-versions");

WriteMakefile(
    'NAME'	=> 'apt-show-versions',
    'VERSION_FROM' => './apt-show-versions',
    'EXE_FILES' => \@scripts,
    'MAN3PODS' => { },
);
