//#include <ept/core/apt.h>
#include <ept/config.h>
#include <ept/debtags/maint/path.h>

#include <wibble/test.h>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/error.h>
#include <apt-pkg/policy.h>
#include <apt-pkg/cachefile.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/pkgcachegen.h>
#include <apt-pkg/init.h>


#ifndef EPT_TEST_H
#define EPT_TEST_H

struct AptTestEnvironment {
    //ept::core::AptDatabase db;
    AptTestEnvironment() {
        pkgInitConfig (*_config);
        _config->Set("Initialized", 1);
        _config->Set("Dir", TEST_ENV_DIR);
        _config->Set("Dir::Cache", "cache");
        _config->Set("Dir::State", "state");
        _config->Set("Dir::Etc", "etc");
        _config->Set("Dir::Etc::sourcelist", "sources.list");
        _config->Set("Dir::State::status", TEST_ENV_DIR "dpkg-status");
        pkgInitSystem (*_config, _system);
    }
};

struct DebtagsTestEnvironment : AptTestEnvironment {
    ept::debtags::Path::OverrideDebtagsSourceDir odsd;
    ept::debtags::Path::OverrideDebtagsIndexDir odid;
    ept::debtags::Path::OverrideDebtagsUserSourceDir odusd;
    ept::debtags::Path::OverrideDebtagsUserIndexDir oduid;

    DebtagsTestEnvironment()
        : odsd( TEST_ENV_DIR "debtags/"),
          odid( TEST_ENV_DIR "debtags/"),
          odusd( TEST_ENV_DIR "debtags/"),
          oduid( TEST_ENV_DIR "debtags/")
    {}
};

#endif
