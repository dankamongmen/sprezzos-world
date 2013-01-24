// pkg_sortpolicy.cc
//
//   Copyright (C) 2001, 2005 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "pkg_sortpolicy.h"
#include "pkg_item.h"
#include "pkg_ver_item.h"

#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <cwidget/widgets/subtree.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

// Blah, this is the easiest way to define trivial subclasses:
// (not that far from lambda, actually)
// Yes, I hate typing more than I have to.
#define PKG_SORTPOLICY_SUBCLASS(name,code)	\
class name##_impl:public pkg_sortpolicy		\
{						\
public:						\
  name##_impl(pkg_sortpolicy *_chain, bool _reversed)\
  :pkg_sortpolicy(_chain, _reversed) {}		\
						\
						\
  inline int do_compare(const pkgCache::PkgIterator &pkg1, const pkgCache::VerIterator &ver1, \
	                const pkgCache::PkgIterator &pkg2, const pkgCache::VerIterator &ver2) const \
  {						\
    code					\
  }						\
						\
  int compare(const pkgCache::PkgIterator &pkg1, const pkgCache::VerIterator &ver1, \
	      const pkgCache::PkgIterator &pkg2, const pkgCache::VerIterator &ver2) const \
  {						\
    int rval=do_compare(pkg1, ver1, pkg2, ver2); \
    if(rval==0 && get_chain())			\
      return get_chain()->compare(pkg1, ver1, pkg2, ver2);\
    else					\
      return get_reversed()?-rval:rval;		\
  }						\
};						\
						\
pkg_sortpolicy *name(pkg_sortpolicy *chain, bool reversed)	\
{						\
  return new name##_impl(chain, reversed);	\
}						\

/** Convert the given tree item to a package/version pair using
 *  dynamic_cast to either pkg_item or pkg_ver_item.
 *
 *  \param item the item to convert
 *  \param[out] pkg the package of this item
 *  \param[out] ver the version of this item
 *
 *  \return \b true if the conversion was successful.
 */
static
bool find_package_and_ver(cw::treeitem *item,
			  pkgCache::PkgIterator &pkg,
			  pkgCache::VerIterator &ver)
{
  const pkg_item *pitem = dynamic_cast<const pkg_item *>(item);
  if(pitem != NULL)
    {
      pkg = pitem->get_package();
      ver = pitem->visible_version();
      return true;
    }

  const pkg_ver_item *vitem = dynamic_cast<const pkg_ver_item *>(item);
  if(vitem != NULL)
    {
      pkg = vitem->get_package();
      ver = vitem->get_version();
      return true;
    }

  return false;
}

int pkg_sortpolicy_wrapper::compare(cw::treeitem *item1,
				    cw::treeitem *item2) const
{
  pkgCache::PkgIterator pkg1, pkg2;
  pkgCache::VerIterator ver1, ver2;

  // To ensure that the sort is sane, sort non-package stuff above all package stuff.
  if(!find_package_and_ver(item1, pkg1, ver1))
    {
      if(!find_package_and_ver(item2, pkg2, ver2))
	return wcscmp(item1->tag(), item2->tag());
      else
	return -1;
    }
  else if(!find_package_and_ver(item2, pkg2, ver2))
    return 1;
  else if(chain)
    return chain->compare(pkg1, ver1, pkg2, ver2);
  else
    return 0; // punt!
}

// TODO: Make these compare functions a simple less-than.  Only a
// couple of places rely on these being able to do 3-way compare.
// By-name sorting could then reuse pkg_name_lt.

// by-name sorting (including architecture)
PKG_SORTPOLICY_SUBCLASS(pkg_sortpolicy_name,
                        int cmp = strcmp(pkg1.Name(), pkg2.Name());
                        if(cmp == 0)
                          {
                            cmp = get_arch_order(pkg1.Arch()) - get_arch_order(pkg2.Arch());
                            if(cmp == 0)
                              cmp = strcmp(pkg1.Arch(), pkg2.Arch());
                          }
                        return cmp;);

// installed-size-sorting, treats virtual packages as 0-size
PKG_SORTPOLICY_SUBCLASS(pkg_sortpolicy_installed_size,
			if(ver1.end() && ver2.end())
			  return 0;
			else if(ver1.end())
			  return 1;
			else if(ver2.end())
			  return -1;
			else if(ver1->InstalledSize<ver2->InstalledSize)
			  return -1;
			else if(ver1->InstalledSize>ver2->InstalledSize)
			  return 1;
			else
			  return 0;);

// Priority sorting
PKG_SORTPOLICY_SUBCLASS(pkg_sortpolicy_priority,
			int pri1=ver1.end()?0:ver1->Priority;
			int pri2=ver2.end()?0:ver2->Priority;
			if(pri1<pri2)
			  return -1;
			else if(pri1==pri2)
			  return 0;
			else // if(pri1>pri2)
			  return 1;);

// debsize-sorting, treats virtual packages as 0-size
PKG_SORTPOLICY_SUBCLASS(pkg_sortpolicy_debsize,
			if(ver1.end() && ver2.end())
			  return 0;
			else if(ver1.end())
			  return 1;
			else if(ver2.end())
			  return -1;
			else if(ver1->Size<ver2->Size)
			  return -1;
			else if(ver1->Size>ver2->Size)
			  return 1;
			else
			  return 0;);

// Sort by version number
PKG_SORTPOLICY_SUBCLASS(pkg_sortpolicy_ver,
			if(ver1.end())
			  return -1;
			else if(ver2.end())
			  return 1;
			else
			  return _system->VS->CmpVersion(ver1.VerStr(),
							 ver2.VerStr()););
