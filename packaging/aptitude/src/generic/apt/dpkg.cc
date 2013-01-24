// dpkg.cc
//
//  Copyright 2012 Daniel Hartwig
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#include "dpkg.h"

#include <aptitude.h>

#include <apt-pkg/configuration.h>
#include <apt-pkg/error.h>
#include <apt-pkg/fileutl.h>
#include <apt-pkg/pkgcache.h>

#include <boost/optional.hpp>

#include <cerrno>
#include <string>
#include <vector>

#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>

using namespace std;

namespace aptitude {
namespace apt {

std::vector<const char *> dpkg_base_args(const bool cached)
{
   static std::vector<const char *> Args;

   if (cached == true && Args.empty() == false)
      return Args;

   // Generate the base argument list for dpkg
   Args.clear();
   string Tmp = _config->Find("Dir::Bin::dpkg","dpkg");
   {
      string const dpkgChrootDir = _config->FindDir("DPkg::Chroot-Directory", "/");
      size_t dpkgChrootLen = dpkgChrootDir.length();
      if (dpkgChrootDir != "/" && Tmp.find(dpkgChrootDir) == 0)
      {
	 if (dpkgChrootDir[dpkgChrootLen - 1] == '/')
	    --dpkgChrootLen;
	 Tmp = Tmp.substr(dpkgChrootLen);
      }
   }
   Args.push_back(Tmp.c_str());

   // Stick in any custom dpkg options
   Configuration::Item const *Opts = _config->Tree("DPkg::Options");
   if (Opts != 0)
   {
      Opts = Opts->Child;
      for (; Opts != 0; Opts = Opts->Next)
      {
	 if (Opts->Value.empty() == true)
	    continue;
	 Args.push_back(Opts->Value.c_str());
      }
   }

   return Args;
}

bool dpkg_multi_arch(const bool cached)
{
   static boost::optional<bool> dpkgMultiArch;

   if (cached == true && dpkgMultiArch)
      return *dpkgMultiArch;

   std::vector<const char *> Args(dpkg_base_args(cached));

   // we need to detect if we can qualify packages with the architecture or not
   Args.push_back("--assert-multi-arch");
   Args.push_back(NULL);


   pid_t dpkgAssertMultiArch = ExecFork();
   if (dpkgAssertMultiArch == 0)
   {
      std::string const chrootDir = _config->FindDir("DPkg::Chroot-Directory");
      if (chrootDir != "/" && chroot(chrootDir.c_str()) != 0)
	 _error->WarningE("dpkg_multi_arch",
                          _("Couldn't chroot into %s for %s"),
                          chrootDir.c_str(),
                          "dpkg --assert-multi-arch");
      // redirect everything to the ultimate sink as we only need the exit-status
      int const nullfd = open("/dev/null", O_RDONLY);
      dup2(nullfd, STDIN_FILENO);
      dup2(nullfd, STDOUT_FILENO);
      dup2(nullfd, STDERR_FILENO);
      execvp(Args[0], (char**) &Args[0]);
      _error->WarningE("dpkg_multi_arch", "Can't detect if dpkg supports multi-arch!");
      _exit(2);
   }

   dpkgMultiArch.reset(false);
   if (dpkgAssertMultiArch > 0)
   {
      int Status = 0;
      while (waitpid(dpkgAssertMultiArch, &Status, 0) != dpkgAssertMultiArch)
      {
	 if (errno == EINTR)
	    continue;
	 _error->WarningE("dpkg_multi_arch",
                          _("Waited for %s but it wasn't there"),
                          "dpkg --assert-multi-arch");
	 break;
      }
      if (WIFEXITED(Status) == true && WEXITSTATUS(Status) == 0)
         dpkgMultiArch.reset(true);
   }

   return *dpkgMultiArch;
}

std::string dpkg_package_name(const pkgCache::PkgIterator &pkg)
{
  if(dpkg_multi_arch() == false)
    return pkg.FullName(true);
  else
    {
      if(pkg->CurrentVer != 0)
        return string(pkg.Name()).append(":").append(pkg.CurrentVer().Arch());
      else if(pkg.VersionList().end() == false)
        return string(pkg.Name()).append(":").append(pkg.VersionList().Arch());
      else
        return pkg.FullName(false);
    }
}

}} // namespace aptitude { namespace apt {
