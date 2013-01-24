// dummy_universe.h                               -*-c++-*-
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
//

#ifndef DUMMY_UNIVERSE_H
#define DUMMY_UNIVERSE_H

#include <iostream>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <cwidget/generic/util/eassert.h>
#include <cwidget/generic/util/exception.h>

#include <boost/functional/hash.hpp>

/** \brief A package dependency universe
 *
 * 
 *  A package dependency universe that's not terribly efficient, but
 *  easy to set up for testing.
 * 
 *  \file dummy_universe.h
 */

// An error type that's thrown when a name lookup fails.
class NoSuchNameError : public cwidget::util::Exception
{
  std::string name;
  std::string type;
public:
  NoSuchNameError(const std::string &_type, const std::string &_name)
    :name(_name), type(_type) {}

  std::string errmsg() const {return "No "+type+" named "+name;}
};

template<class T1, class T2>
class wrap_ptr_iter
{
  typename std::vector<T1 *>::const_iterator realiter, realend;
public:
  wrap_ptr_iter(const std::vector<T1 *> &parent)
    :realiter(parent.begin()), realend(parent.end())
  {
  }

  wrap_ptr_iter(const typename std::vector<T1 *>::const_iterator &begin,
		const typename std::vector<T1 *>::const_iterator &end)
    :realiter(begin), realend(end)
  {
  }

  bool operator==(const wrap_ptr_iter &other) const
  {
    return realiter==other.realiter;
  }

  bool operator!=(const wrap_ptr_iter &other) const
  {
    return realiter!=other.realiter;
  }

  bool end() const
  {
    return realiter == realend;
  }

  wrap_ptr_iter &operator++()
  {
    ++realiter;
    return *this;
  }

  const T2 operator*() const
  {
    return T2(*realiter);
  }
};

class dummy_universe;
class dummy_version;

/** A dummy package type */
class dummy_package
{
  /** The name of the package is not used by the generic algorithm,
   *  but is used as helpful output from the test cases.
   */
  std::string name;

  /** The versions of this package.  The first entry in this list is
   *  the current version.
   */
  std::vector<dummy_version *> versions;

  /** The current version of this package; defaults to the first
   *  element of "versions".
   */
  dummy_version *cur;

  /** The ID of this version. */
  unsigned int ID;

  dummy_package(const dummy_package &other);
public:
  typedef std::vector<dummy_version *>::const_iterator version_iterator;

  /** Creates a dummy package. */
  dummy_package(const std::string &_name, unsigned int id);

  virtual ~dummy_package();

  std::string get_name() const {return name;}
  unsigned int get_id() const {return ID;}

  /** Dummy packages are equivalent iff they are the same object. */
  bool operator==(const dummy_package &other) const
  {
    return this == &other;
  }

  bool operator!=(const dummy_package &other) const
  {
    return this != &other;
  }

  bool operator<(const dummy_package &other) const
  {
    return ID < other.ID;
  }

  void add_version(dummy_version *version)
  {
    if(versions.empty())
      cur=version;
    versions.push_back(version);
  }

  const dummy_version &current_version() const {return *cur;}
  version_iterator versions_begin() const {return versions.begin();}
  version_iterator versions_end() const {return versions.end();}

  /** Returns the version corresponding to the given name or aborts */
  dummy_version *version_from_name(const std::string &the_name) const;

  /** Sets the current version to the given version. */
  void set_current_version(dummy_version *v)
  {
    cur=v;
  }
};

class dummy_dep;

/** A dummy version type */
class dummy_version
{
  std::string name;

  const dummy_package *package;

  std::vector<dummy_dep *> revdeps;
  std::vector<dummy_dep *> deps;

  /** The numerical ID of this version. */
  int ID;

  dummy_version(const dummy_version &other);
public:
  typedef std::vector<dummy_dep *>::const_iterator revdep_iterator;
  typedef std::vector<dummy_dep *>::const_iterator dep_iterator;

  dummy_version(const std::string &_name, const dummy_package *_package,
		unsigned int id)
    :name(_name), package(_package), ID(id)
  {
  }

  std::string get_name() const {return name;}
  unsigned int get_id() const {return ID;}

  bool operator==(const dummy_version &other) const
  {
    return this == &other;
  }

  bool operator!=(const dummy_version &other) const
  {
    return this != &other;
  }

  bool operator<(const dummy_version &other) const
  {
    return ID < other.ID;
  }

  void add_revdep(dummy_dep *dep)
  {
    revdeps.push_back(dep);
  }

  void add_dep(dummy_dep *dep)
  {
    deps.push_back(dep);
  }

  const dummy_package &get_package() const {return *package;}

  revdep_iterator revdeps_begin() const {return revdeps.begin();}
  revdep_iterator revdeps_end() const {return revdeps.end();}

  dep_iterator deps_begin() const {return deps.begin();}
  dep_iterator deps_end() const {return deps.end();}
};

/** Indicates that either package_1 depends upon package_2 or
 *  package_1 conflicts with package_2.
 */
class dummy_dep
{
  dummy_version *source;
  std::vector<dummy_version *> target_set;

  dummy_dep(const dummy_dep &other);

  unsigned int ID;

  bool soft;
  bool candidate_for_initial_set;
public:
  typedef std::vector<dummy_version *>::const_iterator solver_iterator;

  dummy_dep(dummy_version *_source,
	    const std::vector<dummy_version *> &_target_set,
	    unsigned int _ID, bool _soft, bool _candidate_for_initial_set);

  bool is_soft() const
  {
    return soft;
  }

  bool is_candidate_for_initial_set() const
  {
    return candidate_for_initial_set;
  }

  bool operator==(const dummy_dep &other) const
  {
    return this==&other;
  }

  bool operator!=(const dummy_dep &other) const
  {
    return this!=&other;
  }

  bool operator<(const dummy_dep &other) const
  {
    return ID<other.ID;
  }

  dummy_version &get_source() const {return *source;}

  solver_iterator solvers_begin() const
  {
    return target_set.begin();
  }

  solver_iterator solvers_end() const
  {
    return target_set.end();
  }

  /** Not part of the generic interface (although it could be);
   *  returns \b true if the dep is not satisfied in the global
   *  state (i.e., ignores any solution computation in progress).
   */
  bool broken() const;
};

/** \brief Represents the world of all packages and dependencies.
 *
 *  This implements the abstract universe interface as described in
 *  \ref abstract_universe.
 *
 *  \sa universe_universe
 */
class dummy_universe
{
public:
  class version;
  class dep;

  class package
  {
    const dummy_package *real_package;
  public:
    package():real_package(0) {}
    package(const dummy_package *_real_package)
      :real_package(_real_package)
    {
    }

    typedef wrap_ptr_iter<dummy_version, version> version_iterator;

    std::size_t get_hash_value() const
    {
      boost::hash<const dummy_package *> hasher;
      return hasher(real_package);
    }

    bool operator==(const package &other) const
    {
      return real_package==other.real_package;
    }

    bool operator!=(const package &other) const
    {
      return real_package!=other.real_package;
    }

    bool operator<(const package &other) const
    {
      return (*real_package)<(*other.real_package);
    }

    std::string get_name() const
    {
      return real_package->get_name();
    }

    unsigned int get_id() const
    {
      return real_package->get_id();
    }

    version current_version() const
    {
      return version(&real_package->current_version());
    }

    version version_from_name(const std::string &name) const
    {
      return version(real_package->version_from_name(name));
    }

    wrap_ptr_iter<dummy_version, version> versions_begin() const
    {
      return wrap_ptr_iter<dummy_version, version>(real_package->versions_begin(), real_package->versions_end());
    }
  };

  class version
  {
    const dummy_version *real_version;
  public:
    typedef wrap_ptr_iter<dummy_dep, dep> revdep_iterator;
    typedef wrap_ptr_iter<dummy_dep, dep> dep_iterator;

    version():real_version(0) {}
    version(const dummy_version *_real_version)
      :real_version(_real_version)
    {
    }

    std::size_t get_hash_value() const
    {
      boost::hash<const dummy_version *> hasher;
      return hasher(real_version);
    }

    bool operator==(const version &other) const
    {
      return real_version==other.real_version;
    }

    bool operator!=(const version &other) const
    {
      return real_version!=other.real_version;
    }

    bool operator<(const version &other) const
    {
      return (*real_version)<(*other.real_version);
    }

    package get_package() const
    {
      return package(&real_version->get_package());
    }

    std::string get_name() const
    {
      return real_version->get_name();
    }

    unsigned int get_id() const
    {
      return real_version->get_id();
    }

    wrap_ptr_iter<dummy_dep, dep> revdeps_begin() const
    {
      return wrap_ptr_iter<dummy_dep, dep>(real_version->revdeps_begin(),
					   real_version->revdeps_end());
    }

    wrap_ptr_iter<dummy_dep, dep> deps_begin() const
    {
      return wrap_ptr_iter<dummy_dep, dep>(real_version->deps_begin(),
					   real_version->deps_end());
    }
  };


  class dep
  {
    const dummy_dep *real_dep;
  public:
    dep():real_dep(0) {}
    dep(const dummy_dep *_real_dep)
      :real_dep(_real_dep)
    {
    }
    dep(const dep &other)
      :real_dep(other.real_dep)
    {
    }

    typedef wrap_ptr_iter<dummy_version, version> solver_iterator;

    bool is_soft() const
    {
      return real_dep->is_soft();
    }

    bool is_candidate_for_initial_set() const
    {
      return real_dep->is_candidate_for_initial_set();
    }

    std::size_t get_hash_value() const
    {
      boost::hash<const dummy_dep *> hasher;
      return hasher(real_dep);
    }

    bool operator==(const dep &other) const
    {
      return real_dep==other.real_dep;
    }

    bool operator!=(const dep &other) const
    {
      return real_dep!=other.real_dep;
    }

    bool operator<(const dep &other) const
    {
      return (*real_dep)<(*other.real_dep);
    }

    version get_source() const
    {
      return version(&real_dep->get_source());
    }

    wrap_ptr_iter<dummy_version, version> solvers_begin() const
    {
      return wrap_ptr_iter<dummy_version, version>(real_dep->solvers_begin(),
						   real_dep->solvers_end());
    }

    template<class Sol>
    bool broken_under(const Sol& s) const
    {
      if(s.version_of(get_source().get_package()) != get_source())
	return false;

      for(dummy_dep::solver_iterator i=real_dep->solvers_begin();
	  i!=real_dep->solvers_end(); ++i)
	if(s.version_of(version(*i).get_package()) == version(*i))
	  return false;

      return true;
    }

    bool solved_by(const version &other) const;
  };

  typedef wrap_ptr_iter<dummy_package, package> package_iterator;
  typedef wrap_ptr_iter<dummy_dep, dep> dep_iterator;

  /** Finds broken dependencies. */
  class broken_dep_iterator
  {
    std::vector<dummy_dep *>::const_iterator realiter;
    std::vector<dummy_dep *>::const_iterator realend;
  public:
    broken_dep_iterator(const std::vector<dummy_dep *>::const_iterator &_realiter,
			const std::vector<dummy_dep *>::const_iterator &_realend)
      :realiter(_realiter), realend(_realend)
    {
      while(realiter!=realend && !(*realiter)->broken())
	++realiter;
    }

    bool operator==(const broken_dep_iterator &other) const
    {
      return realiter==other.realiter;
    }

    bool operator!=(const broken_dep_iterator &other) const
    {
      return realiter!=other.realiter;
    }

    bool end() const
    {
      return realiter == realend;
    }

    const dep operator*() const
    {
      eassert(realiter!=realend);
      return *realiter;
    }

    broken_dep_iterator &operator++()
    {
      if(realiter!=realend)
	++realiter;

      while(realiter!=realend && !(*realiter)->broken())
	++realiter;

      return *this;
    }
  };

private:
  /** All the packages in the universe. */
  std::vector<dummy_package *> packages;
  /** All the dependencies in the universe. */
  std::vector<dummy_dep *> deps;
  /** All the versions in the universe. */
  std::vector<dummy_version *> versions;

  /** Indexes packages by name. */
  std::map<std::string, dummy_package *> packages_by_name;

  struct compare_dummy_packages
  {
    bool operator()(dummy_package *p1, dummy_package *p2)
    {
      return p1->get_id() < p2->get_id();
    }
  };

  struct compare_dummy_versions
  {
    bool operator()(dummy_version *v1, dummy_version *v2)
    {
      return v1->get_id() < v2->get_id();
    }
  };

  dummy_package *find_package_internal(const std::string &pkg_name)
  {
    std::map<std::string, dummy_package *>::const_iterator pfound=packages_by_name.find(pkg_name);

    if(pfound==packages_by_name.end())
      throw NoSuchNameError("package", pkg_name);

    return pfound->second;
  }

public:
  virtual ~dummy_universe();

  /** Lo, and it was a void, without form. */
  dummy_universe() {}

  /** Add a package to the universe.
   *
   *  \param name the name of the new package.
   *  \param the_versions the names of the versions of that package.
   *         The first element of the list is the current version.
   */
  void add_package(const std::string &name,
		   std::vector<std::string> the_versions,
		   const std::string &curname);

  /** Set the current version of the given package to the given version. */
  void set_current_version(const std::string &pkg_name,
			   const std::string &ver_name)
  {
    dummy_package *p = find_package_internal(pkg_name);
    p->set_current_version(p->version_from_name(ver_name));
  }

  /** Find a package by name. */
  package find_package(const std::string &pkg_name)
  {
    return find_package_internal(pkg_name);
  }

  /** Add a dependency to the universe.  For convenience
   *  this is std::string-based.
   */
  void add_dep(const std::string &pkg_name, const std::string &pkg_ver,
	       const std::vector<std::pair<std::string, std::string> > &target_names,
	       bool is_conflict, bool is_soft, bool candidate_for_initial_set);

  std::vector<package>::size_type get_package_count() const
  {
    return packages.size();
  }

  std::vector<version>::size_type get_version_count() const
  {
    return versions.size();
  }

  package_iterator packages_begin() const
  {
    return packages;
  }

  dep_iterator deps_begin() const
  {
    return deps;
  }

  broken_dep_iterator broken_begin() const
  {
    return broken_dep_iterator(deps.begin(), deps.end());
  }

  bool is_candidate_for_initial_set(const dep &d) const
  {
    return d.is_candidate_for_initial_set();
  }
};

inline std::size_t hash_value(const dummy_universe::package &p)
{
  return p.get_hash_value();
}

inline std::size_t hash_value(const dummy_universe::version &v)
{
  return v.get_hash_value();
}

inline std::size_t hash_value(const dummy_universe::dep &d)
{
  return d.get_hash_value();
}

// A refcounting wrapper for a dummy_universe; used to sanitize memory
// management without copying all over (and because the resolver
// expects to be able to have a full copy of its argument type)
class dummy_universe_ref
{
  struct _rep
  {
    int refcount;
    dummy_universe *universe;

    /** Start with 1 ref since our creator holds a ref. */
    _rep(dummy_universe *_universe)
      :refcount(1), universe(_universe)
    {
    }

    void incref() {++refcount;}
    void decref() {--refcount; if(refcount==0) delete this;}
  };

  _rep *rep;
public:
  typedef dummy_universe::package package;
  typedef dummy_universe::version version;
  typedef dummy_universe::dep dep;
  typedef dummy_universe::package_iterator package_iterator;
  typedef dummy_universe::dep_iterator dep_iterator;
  typedef dummy_universe::broken_dep_iterator broken_dep_iterator;

  dummy_universe_ref()
    :rep(NULL)
  {
  }

  dummy_universe_ref(const dummy_universe_ref &other)
    :rep(other.rep)
  {
    if(rep)
      rep->incref();
  }

  /** Assumes this is the first reference to the universe. */
  dummy_universe_ref(dummy_universe *universe)
    :rep(new _rep(universe))
  {
  }

  ~dummy_universe_ref()
  {
    if(rep)
      rep->decref();
  }

  dummy_universe_ref &operator=(const dummy_universe_ref &other)
  {
    if(other.rep)
      other.rep->incref();
    if(rep)
      rep->decref();
    rep=other.rep;

    return *this;
  }

  operator void*() const
  {
    return (void *) (rep && rep->universe);
  }

  void add_package(const std::string &name,
		   const std::vector<std::string> &the_versions,
		   const std::string &curname) const
  {
    rep->universe->add_package(name, the_versions, curname);
  }

  void set_current_version(const std::string &pkg_name,
			   const std::string &ver_name)
  {
    rep->universe->set_current_version(pkg_name, ver_name);
  }

  void add_dep(std::string pkg_name, std::string pkg_ver,
	       const std::vector<std::pair<std::string, std::string> > &target_names,
	       bool is_conflict, bool is_soft, bool candidate_for_initial_set)
  {
    rep->universe->add_dep(pkg_name, pkg_ver,
			   target_names, is_conflict, is_soft, candidate_for_initial_set);
  }

  package find_package(const std::string &pkg_name) const
  {
    return rep->universe->find_package(pkg_name);
  }

  std::vector<package>::size_type get_package_count() const
  {
    return rep->universe->get_package_count();
  }

  std::vector<version>::size_type get_version_count() const
  {
    return rep->universe->get_version_count();
  }

  package_iterator packages_begin() const
  {
    return rep->universe->packages_begin();
  }

  dep_iterator deps_begin() const
  {
    return rep->universe->deps_begin();
  }

  broken_dep_iterator broken_begin() const
  {
    return rep->universe->broken_begin();
  }

  bool is_candidate_for_initial_set(const dep &d) const
  {
    return rep->universe->is_candidate_for_initial_set(d);
  }
};

template<class T>
class generic_problem_resolver;
typedef generic_problem_resolver<dummy_universe_ref> dummy_resolver;

std::ostream &operator<<(std::ostream &out, const dummy_universe::package &p);

std::ostream &operator<<(std::ostream &out, const dummy_universe::version &v);

std::ostream &operator<<(std::ostream &out, const dummy_universe::dep &d);

class ParseError : public cwidget::util::Exception
{
  std::string msg;
public:
  ParseError(const std::string &_msg):msg(_msg) {}

  std::string errmsg() const {return msg;}
};

/** Read a (package, version) pair of strings from the given stream.
 *
 *  \throws ParseError
 */
std::pair<std::string, std::string> read_pkgverpair(std::istream &in);


/** Parses a universe from the given stream. \throws ParseError */
dummy_universe_ref parse_universe(std::istream &in);

/** Parses a universe to the closing ']'.  Meant for use as a subroutine
 *  to be called after the opening "UNIVERSE [" has been stripped.
 *
 *  \throws ParseError
 */
dummy_universe_ref parse_universe_tail(std::istream &in);

#endif // DUMMY_UNIVERSE_H
