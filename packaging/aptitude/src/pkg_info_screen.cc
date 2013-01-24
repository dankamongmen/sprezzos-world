// pkg_info_screen.cc
//
//  Copyright 2000-2002, 2004-2005, 2007-2008 Daniel Burrows
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
//
//  Gathers information about a package into one
// spot (pkg_grouppolicy_info*) and provides dedicated code to display
// it (pkg_info_screen)

#include "aptitude.h"

#include "pkg_info_screen.h"

#include <cwidget/fragment.h>
#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/widgets/layout_item.h>

#include "dep_item.h"
#include "desc_render.h"
#include "pkg_item_with_subtree.h"
#include "pkg_subtree.h"
#include "pkg_ver_item.h"
#include "trust.h"

#include <generic/apt/apt.h>

#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/strutl.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

class pkg_grouppolicy_info:public pkg_grouppolicy
{
public:
  pkg_grouppolicy_info(pkg_signal *_sig,
		       desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig) {}

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root);

  static void setup_package_info(const pkgCache::PkgIterator &pkg,
				 const pkgCache::VerIterator &ver,
				 pkg_item_with_generic_subtree *tree,
				 pkg_signal *sig);
};

void pkg_grouppolicy_info::add_package(const pkgCache::PkgIterator &pkg,
				       pkg_subtree *root)
{
  pkg_item_with_generic_subtree *newtree=new pkg_item_with_generic_subtree(pkg,
									   get_sig(),
									   true);

  setup_package_info(pkg, pkg_item::visible_version(pkg), newtree, get_sig());

  root->add_child(newtree);
}

void pkg_grouppolicy_info::setup_package_info(const pkgCache::PkgIterator &pkg,
					      const pkgCache::VerIterator &ver,
					      pkg_item_with_generic_subtree *tree,
					      pkg_signal *sig)
{
  char buf[256];
  
  if(!ver.end())
    {
      pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

      std::wstring desc(get_long_description(ver, apt_package_records));
      std::wstring shortdesc(desc, 0, desc.find(L'\n'));

      vector<cw::fragment*> frags;

      cw::fragment *untrusted_warning=make_untrusted_warning(ver);
      if(untrusted_warning != NULL)
	{
	  frags.push_back(untrusted_warning);
	  frags.push_back(cw::newline_fragment());
	}

      // Avoid creating new strings to translate.
      frags.push_back(clipbox(cw::fragf("%B%s%b%ls%n",
				    _("Description: "), shortdesc.c_str())));
      frags.push_back(indentbox(2, 2, make_desc_fragment(desc)));

#ifdef APT_HAS_HOMEPAGE
      if(rec.Homepage() != "")
	frags.push_back(cw::dropbox(cw::fragf("%B%s%b", _("Homepage: ")),
				cw::hardwrapbox(cw::text_fragment(rec.Homepage()))));
#endif

      cw::fragment *tags = make_tags_fragment(pkg);
      if(tags != NULL)
	frags.push_back(cw::fragf("%n%F", tags));

      // Can I use something other than a clipbox below?

      if((pkg->Flags&pkgCache::Flag::Essential)==pkgCache::Flag::Essential ||
	 (pkg->Flags&pkgCache::Flag::Important)==pkgCache::Flag::Important)
	frags.push_back(clipbox(cw::fragf("%B%s%b%s",
				      _("Essential: "), _("yes"))));

      const string multiarch(multiarch_type(ver->MultiArch));
      if(!multiarch.empty())
        frags.push_back(clipbox(cw::fragf("%B%s%b%s",
                                          _("Multi-Arch: "), multiarch.c_str())));

      frags.push_back(clipbox(cw::fragf("%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n"
				    "%B%s%b%s%n",
				    _("Priority: "),pkgCache::VerIterator(ver).PriorityType()?pkgCache::VerIterator(ver).PriorityType():_("Unknown"),
				    _("Section: "),pkg.Section()?pkg.Section():_("Unknown"),
				    _("Maintainer: "),rec.Maintainer().c_str(),
				    _("Architecture: "),pkgCache::VerIterator(ver).Arch(),
				    _("Compressed Size: "), SizeToStr(ver->Size).c_str(),
				    _("Uncompressed Size: "), SizeToStr(ver->InstalledSize).c_str(),
				    _("Source Package: "),
				    rec.SourcePkg().empty()?pkg.Name():rec.SourcePkg().c_str())));

      tree->add_child(new cw::layout_item(cw::sequence_fragment(frags)));

      setup_package_deps<pkg_item_with_generic_subtree>(pkg, ver, tree, sig);

      // Note: reverse provides show up in the version list
      if(!ver.ProvidesList().end())
	{
	  snprintf(buf, 256, _("Package names provided by %s"), pkg.FullName(true).c_str());
	  pkg_subtree *prvtree=new pkg_subtree(cw::util::transcode(buf));

	  for(pkgCache::PrvIterator prv=ver.ProvidesList(); !prv.end(); ++prv)
	    {
	      prvtree->add_child(new pkg_item(prv.ParentPkg(), sig));
	      prvtree->inc_num_packages();
	    }

	  tree->add_child(prvtree);
	}
    }

  snprintf(buf, 256, _("Packages which depend on %s"), pkg.FullName(true).c_str());
  pkg_subtree *revtree=new pkg_subtree(cw::util::transcode(buf));
  setup_package_deps<pkg_subtree>(pkg, ver, revtree, sig, true);
  tree->add_child(revtree);

  pkg_vertree_generic *newtree =
    new pkg_vertree_generic(cw::util::swsprintf(W_("Versions of %s").c_str(),
						pkg.FullName(true).c_str()), true);
  setup_package_versions(pkg, newtree, sig);
  tree->add_child(newtree);
}

pkg_info_screen::pkg_info_screen(const pkgCache::PkgIterator &pkg,
				 const pkgCache::VerIterator &ver)
  :apt_info_tree(pkg.FullName(true), ver.end()?"":ver.VerStr())
{
  set_root(setup_new_root(pkg, ver), true);
}

cw::treeitem *pkg_info_screen::setup_new_root(const pkgCache::PkgIterator &pkg,
					     const pkgCache::VerIterator &ver)
{
  pkg_item_with_generic_subtree *tree=new pkg_item_with_generic_subtree(pkg,
									get_sig(),
									true);
  pkg_grouppolicy_info::setup_package_info(pkg, ver, tree, get_sig());
  return tree;
}
