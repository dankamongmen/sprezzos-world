// aptitude_resolver_universe.h                     -*-c++-*-
//
//   Copyright (C) 2005, 2007, 2009-2010 Daniel Burrows
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

#ifndef APTITUDE_RESOLVER_UNIVERSE_H
#define APTITUDE_RESOLVER_UNIVERSE_H

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/util/eassert.h>

#include <boost/functional/hash.hpp>

#include "apt.h"
#include "aptcache.h"

#include <generic/problemresolver/cost.h>

#include <limits.h>

/** \file aptitude_resolver_universe.h
 */

class aptitude_resolver_version;

/** \brief Translates an APT package into the abstract realm.
 *
 *  This class is a model of the \ref universe_package "Package concept".
 *
 *  \sa \ref universe_package
 */
class aptitude_resolver_package
{
  pkgDepCache *cache;
  const pkgCache::Package *pkg;
  std::string name;
public:
  /** \brief Create an invalid package object. */
  aptitude_resolver_package()
    : cache(0), pkg(0), name()
  {
  }

  /** \brief Create a package object corresponding to the given APT
   *  package.
   *
   *  \param _pkg The package to be represented by the new object.
   *  \param _cache The cache with which the new object is to be associated.
   */
  aptitude_resolver_package(const pkgCache::Package *_pkg,
			    pkgDepCache *_cache)
    :cache(_cache), pkg(_pkg)
  {
    eassert(cache!=0);
    eassert(pkg != 0);
    name = get_pkg().FullName(false);
  }

  /** \brief Generate a unique ID for this package.
   *
   *  \return The APT ID of the package.
   */
  unsigned int get_id() const
  {
    return pkg->ID;
  }

  std::size_t get_hash_value() const
  {
    boost::hash<const pkgCache::Package *> hasher;
    return hasher(pkg);
  }

  /** \return The name of the package. */
  const std::string get_name() const
  {
    return name;
  }

  /** \return The underlying APT package wrapped by this object. */
  pkgCache::PkgIterator get_pkg() const
  {
    if(cache == NULL)
      return pkgCache::PkgIterator();
    else
      return pkgCache::PkgIterator(*cache, const_cast<pkgCache::Package *>(pkg));
  }

  // Note that it's not necessary to compare the cache along with the
  // package: two separate caches won't share the same package.

  /** \return \b true if other is the same package as this. */
  bool operator==(const aptitude_resolver_package &other) const
  {
    return pkg == other.pkg;
  }

  /** \return \b true if other is not the same package as this. */
  bool operator!=(const aptitude_resolver_package &other) const
  {
    return pkg != other.pkg;
  }

  /** \brief Order packages by their memory location. */
  bool operator<(const aptitude_resolver_package &other) const
  {
    return pkg < other.pkg;
  }

  /** \return The to-be-installed version of this package. */
  aptitude_resolver_version current_version() const;

  class version_iterator;

  /** \return The first entry in the list of versions of this package. */
  version_iterator versions_begin() const;
};

inline std::size_t hash_value(const aptitude_resolver_package &p)
{
  return p.get_hash_value();
}

/** \brief Translates a version of an apt package into the abstract
 *  realm.
 *
 *  This class is a model of the \ref universe_version "Version concept".
 *
 *  The version in question may be either a real version of the
 *  package, or the "not-installed" version, which indicates that the
 *  package is removed.
 *
 *  \sa \ref universe_version
 */
class aptitude_resolver_version
{
  pkgDepCache *cache;

  // True if we store a version below; false if it's a package.
  bool is_version : 1;
  // 'Compressed' storage of a pointer to either the package or the
  // version.  This is an offset into one of the arrays stored by the
  // cache, either PkgP or VerP; the preceding boolean tells us which.
  int offset : (8*sizeof(int) - 1);

  aptitude_resolver_version(pkgDepCache *_cache,
			    bool _is_version,
			    int _offset)
    : cache(_cache), is_version(_is_version),
      offset(_offset)
  {
  }

public:
  /** \brief Create an invalid version object. */
  aptitude_resolver_version()
    : cache(0), is_version(false), offset(0)
  {
  }

  /** \brief Create a version wrapper that represents removing the
   *  given package.
   *
   *  \param _pkg The package of which this is a version.  Must not be
   *  an end iterator.
   *
   *  \param _cache  The cache associated with this object.
   */
  static aptitude_resolver_version
  make_removal(const pkgCache::Package *pkg,
	       pkgDepCache *cache)
  {
    eassert(pkg != NULL);
    return aptitude_resolver_version(cache, false, pkg - cache->GetCache().PkgP);
  }

  static aptitude_resolver_version
  make_install(const pkgCache::Version *ver,
	       pkgDepCache *cache)
  {
    eassert(ver != NULL);
    return aptitude_resolver_version(cache, true, ver - cache->GetCache().VerP);
  }

  /** \return The APT package of which this is a version.
   *
   *  \sa get_package()
   */
  pkgCache::PkgIterator get_pkg() const
  {
    if(cache == NULL)
      return pkgCache::PkgIterator();
    else
      {
	pkgCache &pcache(cache->GetCache());

	if(is_version)
	  return pkgCache::PkgIterator(pcache,
				       pcache.PkgP + (pcache.VerP[offset].ParentPkg));
	else
	  return pkgCache::PkgIterator(pcache, pcache.PkgP + offset);
      }
  }

  /** \return The APT version wrapped by this object, or an end
   *  iterator if this is a "removal version".
   */
  pkgCache::VerIterator get_ver() const
  {
    if(cache == NULL)
      return pkgCache::VerIterator();
    else
      {
	pkgCache &pcache(cache->GetCache());

	if(!is_version)
	  return pkgCache::VerIterator(pcache, 0);
	else
	  return pkgCache::VerIterator(pcache, pcache.VerP + offset);
      }
  }

  /** \return The APT ID of this version if it is a real version,
   *  or a fake ID if it is a "removal version".
   */
  unsigned int get_id() const
  {
    pkgCache &pcache(cache->GetCache());

    // Could I just use the array offset instead?  Unsure.
    if(is_version)
      return pcache.VerP[offset].ID;
    else
      // non-installed versions are faked.
      //
      // If this eats a lot of time, it could be memoized..but then
      // there's more to copy.  I could also teach the resolver about
      // "null" versions...but that would mean a bunch of pointless
      // special-casing caller-side anyway.
      return cache->Head().VersionCount + pcache.PkgP[offset].ID;
  }

  /** \return The version string of this version, mangled if multiple
   *  distinct APT versions exist with identical version strings.
   */
  std::string get_name() const;

  /** \return An abstract wrapper of the package with which this
   *  version is associated.
   *
   *  \sa get_pkg()
   */
  aptitude_resolver_package get_package() const
  {
    pkgCache &pcache(cache->GetCache());


    if(is_version)
      return aptitude_resolver_package(pcache.PkgP + (pcache.VerP[offset].ParentPkg), cache);
    else
      return aptitude_resolver_package(pcache.PkgP + offset, cache);
  }

  std::size_t get_hash_value() const
  {
    std::size_t rval = 0;
    boost::hash_combine(rval, is_version);
    boost::hash_combine(rval, offset);

    return rval;
  }

  /** \return \b true if this is the same version as other. */
  bool operator==(const aptitude_resolver_version &other) const
  {
    return is_version == other.is_version && offset == other.offset;
  }

  /** \return \b true if this is not the same version as other. */
  bool operator!=(const aptitude_resolver_version &other) const
  {
    return is_version != other.is_version || offset != other.offset;
  }

  /** \brief Order versions according to their memory location. */
  bool operator<(const aptitude_resolver_version &other) const
  {
    if(is_version < other.is_version)
      return true;
    else if(other.is_version < is_version)
      return false;
    else if(offset < other.offset)
      return true;
    else if(other.offset < offset)
      return false;
    else
      return false;
  }

  class revdep_iterator;
  class dep_iterator;

  /** \brief Return the first entry in the list of reverse
   *  dependencies for this version.
   */
  revdep_iterator revdeps_begin() const;

  /** \brief Return the first entry in the list of forward
   *  dependencies for this version.
   */
  dep_iterator deps_begin() const;
};

inline std::size_t hash_value(const aptitude_resolver_version &v)
{
  return v.get_hash_value();
}

inline aptitude_resolver_version aptitude_resolver_package::current_version() const
{
  pkgCache::PkgIterator pkg(get_pkg());

  // Transmute removed-with-config-files packages into not-installed
  // packages.
  if((*cache)[pkg].Keep() &&
     pkg->CurrentState == pkgCache::State::ConfigFiles)
    return aptitude_resolver_version::make_removal(pkg, cache);
  else
    {
      pkgCache::VerIterator instver((*cache)[pkg].InstVerIter(*cache));
      if(instver.end())
	return aptitude_resolver_version::make_removal(pkg, cache);
      else
	return aptitude_resolver_version::make_install(instver, cache);
    }
}

/** \brief Translates an apt dependency into the abstract realm.
 *
 *  This class is a model of the \ref universe_dep "Dependency concept".
 *
 *  Dependency relationships other than Conflicts/Breaks are translated in a
 *  very straightforward manner: unversioned dependencies collect all
 *  the versions of the target package and are pushed backwards
 *  through Provides, while versioned dependencies collect all
 *  matching versions.  ORed dependencies collect all the versions
 *  targeted by their subcomponents.
 *
 *  Conflicts/Breaks relationships are handled by generating one abstract
 *  dependency for the immediate conflict, and then a separate one for
 *  \e each provider of the conflicted name (if the conflict is
 *  unversioned, of course).  The solvers of these conflicts are the
 *  non-conflicted versions of the conflicted package (including the
 *  non-installed version), or the versions of the providing package
 *  other than the immediate provider, respectively.
 *
 *  \sa \ref universe_dep
 */
class aptitude_resolver_dep
{
  pkgDepCache *cache;
  const pkgCache::Dependency *start;
  /** If start is a Conflicts/Breaks and prv is not NULL, then the
   *  object represents "V -> {V'_1 V'_2 ..} where the V'-s are
   *  versions of prv.OwnerPkg() that do *not* provide V.ParentPkg().
   *  Otherwise, if start is a Conflicts/Breaks and prv is NULL, the
   *  object represents the non-virtual part of the Conflicts/Breaks;
   *  if start is not a Conflicts/Breaks, prv is unused.
   *
   *  All that discussion is mainly important when checking if the dep
   *  is broken and/or when finding its solvers.
   */
  const pkgCache::Provides *prv;
public:
  /** \brief Generate an invalid dependency object.
   */
  aptitude_resolver_dep()
    :cache(0), prv()
  {
  }

  /** \brief Generate a new dependency.
   *
   *  \param dep The APT dependency to represent.
   *
   *  \param _prv If dep is a Conflicts/Breaks, then this is either an end
   *  iterator (indicating that this object represents the conflict on
   *  the real target package), or the Provides through which the
   *  conflict should be projected.
   *
   *  \param _cache The package cache in which this dependency exists.
   */
  aptitude_resolver_dep(const pkgCache::Dependency *dep,
			const pkgCache::Provides *_prv,
			pkgDepCache *_cache)
    :cache(_cache), prv(_prv)
  {
    eassert(cache!=0);
    eassert(dep != NULL);
    if(!is_conflict(dep->Type))
      {
	// If it's not a conflict, back up to the start of the OR.
	pkgCache::DepIterator new_start, end;
	surrounding_or(pkgCache::DepIterator(cache->GetCache(),
					     const_cast<pkgCache::Dependency *>(dep)),
		       new_start, end, &cache->GetCache());

	start = new_start;
      }
    else
      // Ignore ORs and just use the selected conflict.
      //
      //  NOTE: as of this writing, no known package does something as
      // stupid as ORed conflicts.
      start=dep;
  }

  /** \brief Test whether the encapsulated dependency is a
   *   Recommends.
   */
  bool is_soft() const
  {
    return start->Type == pkgCache::Dep::Recommends;
  }

  std::size_t get_hash_value() const
  {
    std::size_t rval = 0;
    boost::hash_combine(rval, start);
    if(is_conflict(start->Type))
      boost::hash_combine(rval, prv);

    return rval;
  }

  /** \brief Compare two dependencies for equality. */
  bool operator==(const aptitude_resolver_dep &other) const
  {
    return start == other.start &&
      (!is_conflict(start->Type) || prv == other.prv);
  }

  /** \brief Compare two dependencies for equality. */
  bool operator!=(const aptitude_resolver_dep &other) const
  {
    return start != other.start ||
      (is_conflict(start->Type) && prv != other.prv);
  }

  /** \brief Orders dependencies according to their memory
   *  location.
   */
  bool operator<(const aptitude_resolver_dep &other) const
  {
    if(start < other.start)
      return true;
    else if(start > other.start)
      return false;
    else if(!is_conflict(start->Type))
      return false;
    else if(prv < other.prv)
      return true;
    else
      return false;
  }

  /** \brief Test whether a given solution breaks this dependency.
   *
   *  \param InstallationType A model of \ref universe_installation Installation.
   *
   *  \param I An installation to test against.
   *
   *  \return \b true if this dependency is not satisfied by I.
   */
  template<typename InstallationType>
  bool broken_under(const InstallationType &I) const;

  /** \return The APT dependency associated with this abstract dependency. */
  pkgCache::DepIterator get_dep() const
  {
    if(cache == NULL)
      return pkgCache::DepIterator();
    else
      return pkgCache::DepIterator(cache->GetCache(), const_cast<pkgCache::Dependency *>(start));
  }

  /** \return The APT Provides relationship associated with this
   *  abstract dependency.
   */
  pkgCache::PrvIterator get_prv() const
  {
    if(cache == NULL)
      return pkgCache::PrvIterator();
    else
      return pkgCache::PrvIterator(cache->GetCache(), const_cast<pkgCache::Provides *>(prv),
				   (pkgCache::Version *)0);
  }

  /** \return \b true if the given version will resolve this dependency. */
  bool solved_by(const aptitude_resolver_version &v) const;

  /** \return The source version of this dependency. */
  aptitude_resolver_version get_source() const
  {
    pkgCache::DepIterator dep(get_dep());
    eassert(!dep.ParentPkg().end());
    return aptitude_resolver_version::make_install(dep.ParentVer(),
						   cache);
  }

  class solver_iterator;

  /** \return The head of the target list for this dependency. */
  solver_iterator solvers_begin() const;
};

inline std::size_t hash_value(const aptitude_resolver_dep &d)
{
  return d.get_hash_value();
}

/** \brief Iterate over the versions of a package.
 *
 *  \sa aptitude_resolver_package, aptitude_resolver_version
 */
class aptitude_resolver_package::version_iterator
{
  pkgDepCache *cache;
  pkgCache::PkgIterator pkg;
  pkgCache::VerIterator ver;

  void normalize()
  {
    // This loop should only trigger once.
    while(!ver.end() &&
	  !ver.Downloadable() &&
	  (ver != pkg.CurrentVer() || pkg->CurrentState == pkgCache::State::ConfigFiles))
      ++ver;
  }
public:
  /** \brief Create an invalid version_iterator. */
  version_iterator()
    :cache(0)
  {
  }

  /** \brief Create a version_iterator pointing at the first version of the given package.
   */
  version_iterator(pkgCache::PkgIterator _pkg,
		   pkgDepCache *_cache)
    :cache(_cache), pkg(_pkg), ver(_pkg.VersionList())
  {
    normalize();
  }

  /** \return The APT package corresponding to this abstract package. */
  pkgCache::PkgIterator get_pkg() {return pkg;}

  /** \return The APT version corresponding to this abstract version.
   *  If this is an end iterator, then this version corresponds to
   *  removing the package.
   */
  pkgCache::VerIterator get_ver() {return ver;}

  /** \return \b true if this iterator is identical to other. */
  bool operator==(const version_iterator &other) const
  {
    return pkg == other.pkg && ver == other.ver;
  }

  /** \return \b true if this iterator differs from other. */
  bool operator!=(const version_iterator &other) const
  {
    return pkg != other.pkg || ver != other.ver;
  }

  /** \return The version at which this iterator currently points. */
  aptitude_resolver_version operator *() const
  {
    if(ver.end())
      return aptitude_resolver_version::make_removal(pkg, cache);
    else
      return aptitude_resolver_version::make_install(ver, cache);
  }

  /** \brief Advance to the next version in the list.
   *
   *  \return A reference to this iterator.
   */
  version_iterator &operator++()
  {
    if(!ver.end())
      {
	++ver;
	normalize();
      }
    else
      pkg=pkgCache::PkgIterator();
    return *this;
  }

  /** \return \b true if this is an end iterator. */
  bool end() const
  {
    return pkg.end();
  }
};

inline aptitude_resolver_package::version_iterator aptitude_resolver_package::versions_begin() const
{
  return version_iterator(pkgCache::PkgIterator(cache->GetCache(), const_cast<pkgCache::Package *>(pkg)), cache);
}

/** \brief Iterates over the reverse dependencies of a version.
 *
 *  As explained in the definition of the \ref universe_version
 *  "Version concept", this is not necessarily the set of dependencies
 *  that impinge on the version.
 *
 *  \sa aptitude_resolver_version, aptitude_resolver_dep
 */
class aptitude_resolver_version::revdep_iterator
{
  pkgDepCache *cache;
  /** The Depends which is currently being tried. */
  pkgCache::DepIterator dep_lst;
  /** The Provides which is currently being tried. */
  pkgCache::PrvIterator prv_lst;
  /** The package version to which this dep should apply (used
   *  to check versioned deps).
   */
  pkgCache::VerIterator ver;
  /** Whether we've started looking at provides yet. */
  bool provides_open;

  /** Advance to the next valid iterator. */
  void normalize();

  /** \return true if dep_lst applies to ver: this is, if it is
   *               a strong dependency on ver.
   */
  bool applicable() const;
public:
#if 0
  revdep_iterator()
    :cache(0)
  {
  }
#endif

  /** \brief Generate a revdep_iterator to cover the reverse deps of
   *  the given version.
   *
   *  \param v The version whose reverse dependencies are to be
   *  enumerated.  If this is an end iterator, the resulting list will
   *  be empty.
   *
   *  \param _cache The cache in which to operate.
   */
  revdep_iterator(const pkgCache::VerIterator &v,
		  pkgDepCache *_cache)
    :cache(_cache),
     prv_lst(*_cache, 0, (pkgCache::Package *) 0), ver(v),
     provides_open(false)
  {
    // Note that if v is an end iterator, we present an empty list and
    // hence don't need to know its package.  This is safe because the
    // [UNINST] version has no reverse dependencies (except conflicts,
    // but those are handled in the usual way).
    if(!v.end())
      dep_lst=v.ParentPkg().RevDependsList();
    else
      // Immediately flag this as an end iterator, and avoid crashing
      // in normalize() when we look at v.ProvidesList().
      provides_open=true;
    normalize();
  }

//   bool operator==(const revdep_iterator &other) const
//   {
//     return dep == other.dep && ver == other.ver;
//   }

//   bool operator!=(const revdep_iterator &other) const
//   {
//     return dep != other.dep || ver != other.ver;
//   }

  /** \brief Test whether this is an end iterator. */
  bool end() const
  {
    return dep_lst.end();
  }

  /** \return The dependency at which this iterator currently
   *  points.
   */
  aptitude_resolver_dep operator*() const
  {
    return aptitude_resolver_dep(dep_lst, prv_lst, cache);
  }

  /** \brief Advance to the next entry in the list.
   *
   *  \return A reference to this iterator.
   */
  revdep_iterator &operator++()
  {
    ++dep_lst;
    normalize();

    return *this;
  }
};

/** \brief Iterates over the distinct dependencies of a version.
 *
 *  \sa aptitude_resolver_version, aptitude_resolver_dep
 */
class aptitude_resolver_version::dep_iterator
{
  pkgDepCache *cache;
  pkgCache::DepIterator dep;
  pkgCache::PrvIterator prv;
  /** If \b true, then dep is a Conflicts/Breaks and we are iterating over
   *  the packages providing its target.
   */
  bool prv_open;

  /** \brief Walk forward on the full dependency graph (including
   *  things that we filter out at the high level, like self-depends)
   */
  void advance();

  static bool applicable(const pkgCache::DepIterator &dep,
			 const pkgCache::PrvIterator &prv,
			 bool prv_open,
			 pkgDepCache *cache);

  /** \return \b true if this dependency is one that should be made
   *  visible in the dep graph.
   *
   *  Specifically, the dependency should not be a (direct or
   *  indirect) self-depends, a non-interesting dependency, or
   *  a conflict that can't be triggered.
   */
  bool applicable();

  void normalize();

public:
  /** \brief Create an invalid dep iterator for the given cache.
   *
   *  \param cache The cache in which to create a dep iterator.
   */
  dep_iterator(pkgDepCache *_cache)
    :cache(_cache),
     prv(*_cache, 0, (pkgCache::Package *) 0),
     prv_open(false)
  {
  }

  /** \brief Create an iterator for the given version's dependencies in the given cache.
   *
   *  \param ver The version whose dependencies should be iterated over.
   *
   *  \param _cache The cache in which to operate.
   */
  dep_iterator(const pkgCache::VerIterator &ver,
	       pkgDepCache *_cache)
    :cache(_cache),
     dep(ver.DependsList()),
     prv(*_cache, 0, (pkgCache::Package *) 0),
     prv_open(false)
  {
    normalize();
  }

  /** \brief Assignment operator. */
  dep_iterator &operator=(const dep_iterator &other)
  {
    cache=other.cache;
    dep=other.dep;
    prv=other.prv;
    prv_open=other.prv_open;

    return *this;
  }

  /** \return The dependency at which this iterator currently points. */
  aptitude_resolver_dep operator*() const
  {
    return aptitude_resolver_dep(dep, prv, cache);
  }

  /** \brief Test whether this is an end iterator. */
  bool end() const
  {
    return dep.end();
  }

  /** \brief Advance to the next dependency of this version.
   *
   *  \return A reference to this iterator.
   */
  dep_iterator &operator++();
};

inline aptitude_resolver_version::revdep_iterator aptitude_resolver_version::revdeps_begin() const
{
  return revdep_iterator(get_ver(), cache);
}

inline aptitude_resolver_version::dep_iterator aptitude_resolver_version::deps_begin() const
{
  if(!is_version)
    return dep_iterator(cache);
  else
    return dep_iterator(get_ver(), cache);
}

/** \brief Iterates over the targets of a dependency.
 *
 *  \sa aptitude_resolver_dep
 */
class aptitude_resolver_dep::solver_iterator
{
  pkgDepCache *cache;

  pkgCache::DepIterator dep_lst;
  pkgCache::VerIterator ver_lst;
  pkgCache::PrvIterator prv_lst;
  /** \b true if we exhausted all options; needed because
   *          dep_lst might not be an end iterator (since it'll
   *          move to the next OR group)
   */
  bool finished;

  /** Advance to the next interesting version/provides -- i.e., skip
   *  uninteresting ones.
   */
  void normalize();

public:
  /** \brief Initialize a solution iterator for a dependency that is
   *  not a Conflicts/Breaks.
   *
   *  \param start The dependency whose targets should be enumerated.
   *
   *  \param _cache The package cache in which this dependency is
   *  located.
   */
  solver_iterator(const pkgCache::Dependency *start,
		  pkgDepCache *_cache)
    :cache(_cache),
     dep_lst(*cache, const_cast<pkgCache::Dependency *>(start)),
     prv_lst(*cache, 0, (pkgCache::Package *) 0),
     finished(dep_lst.end())
  {
    if(!dep_lst.end())
      {
	eassert(!is_conflict(dep_lst->Type));

	ver_lst = dep_lst.TargetPkg().VersionList();
	prv_lst = dep_lst.TargetPkg().ProvidesList();
      }

    normalize();
  }

  /** \brief Initialize a solution iterator for a Conflicts/Breaks.
   *
   *  \param d The conflict that we should iterate over solutions to.
   *
   *  \param p The Provides through which the Conflicts/Breaks is
   *  being projected, or an end iterator if we are handling a
   *  straight Conflicts/Breaks.
   *
   *  \param _cache The package cache in which to work.
   */
  solver_iterator(const pkgCache::Dependency *d,
		  const pkgCache::Provides *p,
		  pkgDepCache *_cache)
    :cache(_cache),
     dep_lst(_cache->GetCache(), const_cast<pkgCache::Dependency *>(d)),
     prv_lst(_cache->GetCache(), const_cast<pkgCache::Provides *>(p), (pkgCache::Package *)0),
     finished(dep_lst.end())
  {
    if(!dep_lst.end())
      {
	eassert(is_conflict(d->Type));
	// Either we're looking at all versions of the named dep, or
	// at all versions of the providing package.
	if(prv_lst.end())
	  ver_lst=const_cast<pkgCache::DepIterator &>(dep_lst).TargetPkg().VersionList();
	else
	  ver_lst = prv_lst.OwnerPkg().VersionList();
      }

    normalize();
  }

#if 0
  solver_iterator()
    :cache(0),
     // shouldn't do this, but otherwise we crash!!
     prv_lst(*apt_cache_file, 0, (pkgCache::Package *) 0), finished(true)
  {
  }
#endif

  /** \brief Compare two solver iterators for equality. */
  bool operator==(const solver_iterator &other) const
  {
    return dep_lst == other.dep_lst &&
      ver_lst == other.ver_lst &&
      prv_lst == other.prv_lst &&
      finished == other.finished;
  }

  /** \brief Compare two solver iterators for equality. */
  bool operator!=(const solver_iterator &other) const
  {
    return dep_lst != other.dep_lst ||
      ver_lst != other.ver_lst ||
      prv_lst != other.prv_lst ||
      finished != other.finished;
  }

  /** \brief Advance to the next solution.
   *
   *  \return a reference to this iterator.
   */
  solver_iterator &operator++();

  /** \return The version at which this iterator currently points. */
  aptitude_resolver_version operator*() const;

  /** \brief Test whether this is an end iterator. */
  bool end() const
  {
    return finished;
  }
};

inline aptitude_resolver_dep::solver_iterator aptitude_resolver_dep::solvers_begin() const
{
  if(!is_conflict(start->Type))
    return solver_iterator(start, cache);
  else
    return solver_iterator(start, prv, cache);
}

template<typename InstallationType>
bool aptitude_resolver_dep::broken_under(const InstallationType &I) const
{
  pkgCache::DepIterator start_iter(cache->GetCache(), const_cast<pkgCache::Dependency *>(start));
  // First, check that the solution actually installs the source.
  if(start_iter.ParentVer() != I.version_of(aptitude_resolver_package(start_iter.ParentPkg(), cache)).get_ver())
    return false;

  if(!is_conflict(start->Type))
    {
      pkgCache::DepIterator dep = start_iter;

      while(!dep.end())
	{
	  pkgCache::VerIterator direct_ver=I.version_of(aptitude_resolver_package(dep.TargetPkg(), cache)).get_ver();
	  if(!direct_ver.end())
	    {
	      const char * const direct_verstr = direct_ver.VerStr();
	      const char * const dep_targetstr = dep.TargetVer();
	      if(!direct_ver.end() &&
		 _system->VS->CheckDep(direct_verstr,
				       dep->CompareOp,
				       dep_targetstr))
		return false;
	    }

	  if(!dep.TargetVer())
	    {
	      for(pkgCache::PrvIterator prv=dep.TargetPkg().ProvidesList();
		  !prv.end(); ++prv)
		if(prv.OwnerVer() == I.version_of(aptitude_resolver_package(prv.OwnerPkg(), cache)).get_ver())
		  return false;
	    }

	  if(!(dep->CompareOp & pkgCache::Dep::Or))
	    break;
	  ++dep;
	}

      return true;
    }
  else
    {
      // Recall that a Conflicts/Breaks dep iterator is looking at a
      // single element of the Conflicts/Breaks: either a direct
      // conflict or an indirect conflict (i.e., via a virtual pkg).

      if(prv == NULL)
	{
	  if(start_iter.TargetPkg() == start_iter.ParentPkg())
	    return false;

	  pkgCache::VerIterator direct_ver=I.version_of(aptitude_resolver_package(start_iter.TargetPkg(), cache)).get_ver();

	  if(!direct_ver.end() &&
	     _system->VS->CheckDep(direct_ver.VerStr(),
				   start->CompareOp,
				   start_iter.TargetVer()))
	    return true;
	  else
	    return false;
	}
      else
	{
	  pkgCache::PrvIterator prv_iter(cache->GetCache(), const_cast<pkgCache::Provides *>(prv), (pkgCache::Package *)0);

	  if(prv_iter.OwnerPkg() == start_iter.ParentPkg())
	    return false;

	  if(start_iter.TargetVer() != NULL)
	    return false;

	  return I.version_of(aptitude_resolver_package(prv_iter.OwnerPkg(), cache)).get_ver() == prv_iter.OwnerVer();
	}
    }
}

/** \brief Representation of a level value as stored in the
 *  configuration.
 *
 *  All levels are integers ... except for the special level "discard"
 *  (formerly "conflict"), which is a separate entity.
 */
class cfg_level
{
  int level;
  bool is_discard;

  cfg_level(int _level, bool _is_discard)
    : level(_level), is_discard(_is_discard)
  {
  }

public:
  /** \brief Create a cfg_level that has no effect. */
  cfg_level()
    : level(INT_MIN), is_discard(false)
  {
  }

  static cfg_level make_level(int level)
  {
    return cfg_level(level, false);
  }

  static cfg_level make_conflict()
  {
    return cfg_level(INT_MAX, true);
  }

  bool get_is_discard() const { return is_discard; }
  int get_level() const { return level; }

  bool operator<(const cfg_level &other) const
  {
    if(is_discard)
      return false;
    else if(other.is_discard)
      return true;
    else
      return level < other.level;
  }
};

std::ostream &operator<<(std::ostream &out, const cfg_level &level);

/** \brief This class translates an APT package system into the
 *  abstract package system as described in \ref abstract_universe.
 *
 *  \sa \ref universe_universe
 */
class aptitude_universe
{
  aptitudeDepCache *cache;

  aptitude_universe();
public:
  typedef aptitude_resolver_package package;
  typedef aptitude_resolver_version version;
  typedef aptitude_resolver_dep dep;

  aptitude_universe(aptitudeDepCache *_cache)
    :cache(_cache)
  {
  }

  aptitudeDepCache *get_cache() const {return cache;}

  /** \brief Iterate over all the packages in the universe. */
  class package_iterator
  {
    pkgDepCache *cache;
    pkgCache::PkgIterator realiter;
  public:
    /** \brief Create an invalid package iterator. */
    package_iterator()
      :cache(0)
    {
    }

    /** \brief Create an iterator pointing at the first package in the
     *  cache.
     *
     *  \param _cache The package cache to iterate over.
     */
    package_iterator(pkgDepCache *_cache)
      :cache(_cache), realiter(_cache->PkgBegin())
    {
    }

    /** \brief Compare two package iterators for equality. */
    bool operator==(const package_iterator &other) const
    {
      return realiter==other.realiter;
    }

    /** \brief Compare two package iterators for equality. */
    bool operator!=(const package_iterator &other) const
    {
      return realiter!=other.realiter;
    }

    /** \brief Retrieve the underlying apt iterator. */
    pkgCache::PkgIterator get_pkg() const
    {
      return realiter;
    }

    /** \brief Extract the package at which this iterator currently points. */
    package operator*() const
    {
      return package(realiter, cache);
    }

    /** \brief Advance to the next package.
     *
     *  \return A reference to this iterator.
     */
    package_iterator &operator++()
    {
      ++realiter;
      return *this;
    }

    /** \brief Test whether this is an end iterator. */
    bool end() const
    {
      return realiter.end();
    }
  };

  /** \brief Iterate over all the interesting dependencies in the apt
   *  cache.
   */
  class dep_iterator
  {
    pkgDepCache *cache;

    pkgCache::PkgIterator pkg;
    pkgCache::VerIterator ver;
    aptitude_resolver_version::dep_iterator dep;

    /** \brief Advance to the earliest interesting dependency that is
     *  no earlier than the current dependency.
     */
    void normalize();

  public:
    /** \brief Create a dep_iterator for the given cache.
     *
     *  \param _cache The package cache whose dependencies should be
     *  iterated over.
     */
    dep_iterator(pkgDepCache *_cache)
      :cache(_cache),
       pkg(_cache->PkgBegin()),
       ver(),
       dep(_cache)
    {
      if(!pkg.end())
	ver=pkg.VersionList();
      if(!ver.end())
	dep=aptitude_resolver_version::dep_iterator(ver, _cache);

      normalize();
    }

    /** \return the dependency at which this iterator currently points. */
    aptitude_universe::dep operator*() const
    {
      return *dep;
    }

    /** \brief Advance to the next dependency in the cache.
     *
     *  \return a reference to this iterator.
     */
    dep_iterator &operator++()
    {
      eassert(!dep.end());

      ++dep;

      normalize();

      return *this;
    }

    /** \brief Test whether this is an end iterator. */
    bool end() const
    {
      return pkg.end();
    }
  };

  /** \brief Iterate over the broken interesting dependencies in an
   *  apt cache.
   *
   *  A bit like dep_iterator, but skips non-broken packages and deps.
   *  Since the "exposed version" of a package is its InstVersion, we
   *  need to test at most one version per package, so no need to keep
   *  a version iterator here.
   *
   *  Note on OR groups: DepGInstall is only set on the last entry in
   *  an OR group.  But Conflicts/Breaks should be handled individually.  As
   *  I'm not even sure ORed conflicts are valid, none exist in the
   *  wild, and ORed conflicts are a Pointless Idea[tm] anyway, THIS
   *  WILL NOT PRODUCE CORRECT OUTPUT for ORed conflicts.  \todo try
   *  to find a way to get correct output without compromising in the
   *  main codepath.
   */
  class broken_dep_iterator
  {
    pkgDepCache *cache;

    class pkgCache::PkgIterator pkg;
    class pkgCache::DepIterator the_dep;
    /** If the_dep is a Conflicts/Breaks, then the following keep track
     *  of which sub-relationship is being examined.
     */
    class pkgCache::PrvIterator prv;
    bool prv_open;

    /** \return \b true if the given non-end dep is InstBroken. */
    bool dep_is_inst_broken(const pkgCache::DepIterator &d) const;

    // Push forward to the next interesting point.
    void normalize();

  public:
#if 0
    broken_dep_iterator()
      :cache(0)
    {
    }
#endif

    /** \brief Create a broken_dep_iterator for the given package cache. */
    broken_dep_iterator(pkgDepCache *_cache)
      :cache(_cache),
       pkg(_cache->PkgBegin()), prv(*_cache, 0, (pkgCache::Package *) 0),
       prv_open(false)
    {
      if(!pkg.end())
	{
	  pkgCache::VerIterator ver=(*cache)[pkg].InstVerIter(*cache);

	  if(!ver.end())
	    the_dep=ver.DependsList();
	}

      normalize();
    }

    /** \return the dependency at which this iterator currently points. */
    aptitude_universe::dep operator*() const
    {
      return aptitude_universe::dep(the_dep, prv, cache);
    }

    /** \brief Advance to the next broken dependency.
     *
     *  \return a reference to this iterator.
     */
    broken_dep_iterator &operator++();

    /** \brief Test whether this is an end iterator. */
    bool end() const
    {
      return pkg.end();
    }
  };

  package_iterator packages_begin() const
  {
    return package_iterator(cache);
  }

  dep_iterator deps_begin() const
  {
    return dep_iterator(cache);
  }

  broken_dep_iterator broken_begin() const
  {
    return broken_dep_iterator(cache);
  }

  bool is_candidate_for_initial_set(const dep &d) const;

  unsigned long get_version_count() const
  {
    // PackageCount is added to make room for the UNINST versions.
    return cache->Head().VersionCount+cache->Head().PackageCount;
  }

  unsigned long get_package_count() const
  {
    return cache->Head().PackageCount;
  }

  // Configuration helper -- should this be somewhere better?
  static cfg_level parse_level(const std::string &s);
  /** \brief Parse two level strings and combine them.
   *
   *   - If neither is set, the given default level is used.
   *   - If only one is set, its value is used.
   *   - If both are set, the higher value is used (where "conflict"
   *     is taken to be higher than any numeric level).
   */
  static cfg_level parse_levels(const std::string &level1,
                                const std::string &level2,
                                cfg_level default_level);

  // Configuration fetchers.
  static cfg_level get_safe_level();
  static cfg_level get_keep_all_level();
  static cfg_level get_remove_level();
  static cfg_level get_break_hold_level();
  static cfg_level get_non_default_level();
  static cfg_level get_remove_essential_level();
};

/** \brief Write an aptitude_resolver_package to the given stream. */
std::ostream &operator<<(ostream &out, const aptitude_resolver_package &p);

/** \brief Write an aptitude_resolver_dep to the given stream. */
std::ostream &operator<<(ostream &out, const aptitude_resolver_dep &d);

/** \brief Write an aptitude_resolver_version to the given stream. */
std::ostream &operator<<(ostream &out, const aptitude_resolver_version &d);

#endif
