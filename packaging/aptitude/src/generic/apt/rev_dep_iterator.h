// rev_dep_iterator.h               -*-c++-*-
//
// Copyright 2004 Daniel Burrows

#ifndef REV_DEP_ITERATOR_H
#define REV_DEP_ITERATOR_H

#include <apt-pkg/cacheiterators.h>

#include "apt.h"

/** \file rev_dep_iterator.h
 */

/** An iterator that iterates over all reverse deps of a package,
 *  including those that are via a Provided package.
 */
class rev_dep_iterator
{
  pkgCache::DepIterator rev_dep_lst;
  pkgCache::PrvIterator prv_lst;
  pkgCache::VerIterator ver_lst;

  /** Moves the internal DepIterator to the start of the next
   *  dependency list, if necessary.
   */
  void normalize()
  {
    while(rev_dep_lst.end() && prv_lst.end() && !ver_lst.end())
      {
	prv_lst = ver_lst.ProvidesList();
	++ver_lst;
      }

    while(rev_dep_lst.end() && !prv_lst.end())
      {
	rev_dep_lst=prv_lst.ParentPkg().RevDependsList();
	++prv_lst;

	// If we're out of provides, try to snarf another version off
	// the version list.
	while(prv_lst.end() && !ver_lst.end())
	  {
	    prv_lst=ver_lst.ProvidesList();
	    ++ver_lst;
	  }
      }
  }
public:
  /** Create a new rev_dep_iterator.
   *
   *  \param ver the version whose reverse dependencies are to be
   *  enumerated.
   */
  rev_dep_iterator(pkgCache::VerIterator ver)
    :rev_dep_lst(*apt_cache_file, NULL, (pkgCache::Version *) NULL),
     prv_lst(*apt_cache_file, NULL, (pkgCache::Version *) NULL),
     ver_lst(*apt_cache_file, (pkgCache::Version *) NULL)
  {
    if(!ver.end())
      {
	rev_dep_lst=ver.ParentPkg().RevDependsList();
	prv_lst=ver.ProvidesList();
      }

    normalize();
  }

  /** Create a new rev_dep_iterator.
   *
   *  \param pkg the package whose reverse dependencies are to be
   *  enumerated.  (without respect to a particular version, but
   *  including provides)
   */
  rev_dep_iterator(pkgCache::PkgIterator pkg)
    :rev_dep_lst(*apt_cache_file, NULL, (pkgCache::Version *) NULL),
     prv_lst(*apt_cache_file, NULL, (pkgCache::Version *) NULL),
     ver_lst(*apt_cache_file, (pkgCache::Version *) NULL)
  {
    if(!pkg.end())
      {
	rev_dep_lst=pkg.RevDependsList();

	ver_lst=pkg.VersionList();
      }

    normalize();
  }

  /** \return the dependency to which this iterator currently points. */
  pkgCache::DepIterator operator*() const {return rev_dep_lst;}

  /** \return \b true iff the iterator is at the end of its list. */
  bool end() const {return rev_dep_lst.end();}

  /** Advance the iterator to the next element of the list. */
  void operator++() {++rev_dep_lst; normalize();}
};

#endif // REV_DEP_ITERATOR_H
