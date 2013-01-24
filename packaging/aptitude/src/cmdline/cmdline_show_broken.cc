// cmdline_show_broken.cc
//
//   Copyright 2004 Daniel Burrows

#include "cmdline_show_broken.h"

#include "cmdline_common.h"

#include <aptitude.h>

#include <generic/apt/apt.h>

#include <apt-pkg/pkgcache.h>

#include <stdio.h>
#include <string.h>

using namespace std;

void show_broken_deps(pkgCache::PkgIterator pkg)
{
  const unsigned int indent=pkg.FullName(true).size() + 3;
  bool is_first_dep=true;
  pkgCache::VerIterator ver=(*apt_cache_file)[pkg].InstVerIter(*apt_cache_file);

  cout << " " << pkg.FullName(true) << " :";

  if(ver.end() == true)
    {
      cout << endl;
      return;
    }

  for(pkgCache::DepIterator dep=ver.DependsList(); !dep.end(); ++dep)
    {
      pkgCache::DepIterator first=dep, prev=dep;

      while(dep->CompareOp & pkgCache::Dep::Or)
	++dep;

      // Yep, it's broken.
      if(dep.IsCritical() &&
	 !((*apt_cache_file)[dep]&pkgDepCache::DepGInstall))
	{		  
	  bool is_first_of_or=true;
	  // Iterate over the OR group, print out the information.

	  do
	    {
	      if(!is_first_dep)
		for(unsigned int i=0; i<indent; ++i)
		  printf(" ");

	      is_first_dep=false;

	      if(!is_first_of_or)
		for(unsigned int i=0; i<strlen(dep.DepType())+3; ++i)
		  printf(" ");
	      else
		printf(" %s: ", first.DepType());

	      is_first_of_or=false;

              cout << first.TargetPkg().FullName(true);

	      if(first.TargetVer())
		printf(" (%s %s)", first.CompType(), first.TargetVer());

	      // FIXME: handle virtual packages sanely.
	      pkgCache::PkgIterator target=first.TargetPkg();
	      // Don't skip real packages which are provided.
	      if(!target.VersionList().end())
		{
		  printf(" ");

		  pkgCache::VerIterator ver=(*apt_cache_file)[target].InstVerIter(*apt_cache_file);

		  if(!ver.end()) // ok, it's installable.
		    {
		      if((*apt_cache_file)[target].Install())
			printf(_("but %s is to be installed."),
			       ver.VerStr());
		      else if((*apt_cache_file)[target].Upgradable())
			printf(_("but %s is installed and it is kept back."),
			       target.CurrentVer().VerStr());
		      else
			printf(_("but %s is installed."),
			       target.CurrentVer().VerStr());
		    }
		  else
		    {
		      pkgCache::VerIterator cand = (*apt_cache_file)[target].CandidateVerIter(*apt_cache_file);
		      if(cand.end())
			printf(_("but it is not installable."));
		      else
			printf(_("but it is not going to be installed."));
		    }
		}
	      else
		// FIXME: do something sensible here!
		printf(_(" which is a virtual package."));

	      if(first!=dep)
		printf(_(" or"));

	      printf("\n");

	      prev=first;
	      ++first;
	    } while(prev!=dep);
	}
    }
}

bool show_broken()
{
  pkgvector broken;
  for(pkgCache::PkgIterator i=(*apt_cache_file)->PkgBegin();
      !i.end(); ++i)
    {
      if((*apt_cache_file)[i].InstBroken())
	broken.push_back(i);
    }

  if(!broken.empty())
    {
      printf(_("The following packages have unmet dependencies:\n"));

      for(pkgvector::iterator pkg=broken.begin(); pkg!=broken.end(); ++pkg)
	show_broken_deps(*pkg);
      return false;
    }

  return true;
}
