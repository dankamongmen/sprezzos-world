use ExtUtils::MakeMaker;

my %changes = map /^([^:]*):\s+(.*)/, split /\n(?!\s)/, `dpkg-parsechangelog`;

WriteMakefile(
    NAME	 => 'AptPkg',
    VERSION_FROM => 'AptPkg.pm',
    CC		 => 'c++',
    LD		 => 'c++',
    OBJECT	 => '$(O_FILES)',
    LIBS	 => ['-lapt-pkg'],
    XSOPT	 => '-C++',
    depend	 => { Makefile => 'AptPkg.pm debian/changelog' }, # for VERSION
    clean	 => { FILES => 't/cache/var/*.bin'
			    . " $changes{Source}-$changes{Version}"
			    . " $changes{Source}_$changes{Version}.*"
			    . " $changes{Source}_$changes{Version}_*" },
);

sub MY::postamble
{
    <<EOT . <<'EOT';
DEBNAME = $changes{Source}
DEBVER = $changes{Version}
DPKG_BUILDPACKAGE_OPTS = -rfakeroot
EOT

deb:
	$(RM_RF) $(DEBNAME)-$(DEBVER)
	$(PERL) -I$(PERL_ARCHLIB) -I$(PERL_LIB) \
	    -MExtUtils::Manifest=manicopy,maniread \
	    -e "manicopy(maniread(),'$(DEBNAME)-$(DEBVER)', '$(DIST_CP)');"
	cd $(DEBNAME)-$(DEBVER); dpkg-buildpackage $(DPKG_BUILDPACKAGE_OPTS)
	$(RM_RF) $(DEBNAME)-$(DEBVER)
EOT
}
