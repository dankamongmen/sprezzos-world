// aptitude_resolver_universe.cc                       -*-c++-*-

#include "aptitude_resolver_universe.h"

#include "config_signal.h"

#include <generic/problemresolver/problemresolver.h>
#include <generic/problemresolver/solution.h>

#include <algorithm>
#include <sstream>

#include <apt-pkg/error.h>

#include <cwidget/generic/util/ssprintf.h>

#include <aptitude.h>
#include <loggers.h>

using namespace std;
using aptitude::Loggers;
using cwidget::util::ssprintf;

static inline
bool ver_disappeared(const pkgCache::VerIterator ver)
{
  return
    !ver.Downloadable() &&
    (ver != ver.ParentPkg().CurrentVer() ||
     ver.ParentPkg()->CurrentState == pkgCache::State::ConfigFiles);
}

/** Detects dependencies that (provides ignored) are conflicts that
 *  can't be triggered ever.
 *
 *  We want to eliminate these dependencies because they are an
 *  inconsistency (albiet spurious) in aptitude's model: they don't
 *  appear as reverse dependencies since there is no package that they
 *  would be followed backwards from.
 *
 *  Ignoring provides is safe since conflicts as seen in the model are
 *  specific to a particular provides link, and
 *  conflicts-through-provides can't be empty (true?  maybe not: what
 *  about disappeared providers?).
 */
static bool empty_conflict(const pkgCache::DepIterator &dep,
			   const pkgCache::PrvIterator &prv)
{
  if(!is_conflict(dep->Type))
    return false;

  if(prv.end())
    {
      bool found = false;
      for(pkgCache::VerIterator ver =
	    const_cast<pkgCache::DepIterator &>(dep).TargetPkg().VersionList();
	  !ver.end() && !found; ++ver)
	{
	  if(!ver_disappeared(ver) &&
	     (dep.TargetVer() == NULL ||
	      _system->VS->CheckDep(ver.VerStr(),
				    dep->CompareOp,
				    dep.TargetVer())))
	    found = true;
	}

      // If we found no matching non-disappeared version, then this
      // conflict is irrelevant.
      return !found;
    }
  else
    {
      if(ver_disappeared(const_cast<pkgCache::PrvIterator &>(prv).OwnerVer()))
	return true;

      // If the provider doesn't match the dependency, then this is an
      // irrelevant conflict.
      return !_system->VS->CheckDep(prv.ProvideVersion(),
				    dep->CompareOp, dep.TargetVer());
    }
}

string aptitude_resolver_version::get_name() const
{
  pkgCache::VerIterator ver(get_ver());
  pkgCache::PkgIterator pkg(get_pkg());
  if(!ver.end())
    {
      // If there are two distinct package files with the same
      // Version, apt will give them the same VerStr.  Detect and
      // compensate for this.
      int count=0;
      int idx=-1;

      for(pkgCache::VerIterator i=pkg.VersionList(); !i.end(); ++i)
	{
	  if(i==ver)
	    {
	      idx=count;
	      ++count;
	    }
	  else if(!strcmp(i.VerStr(), ver.VerStr()))
	    ++count;
	}

      // if not, we have a "version" of this package that's not in
      // its list of versions!
      eassert(idx>=0);

      if(count>1)
	{
	  ostringstream s;
	  s << ver.VerStr() << "<" << idx+1 << ">";
	  return s.str();
	}
      else
	return ver.VerStr();
    }
  else
    // Note that this is an invalid version string for apt, so we
    // can't clash with real versions.
    return "[UNINST]";
}

bool aptitude_resolver_version::revdep_iterator::applicable() const
{
  if(!is_interesting_dep(dep_lst, cache))
    return false;

  // Screen out self-conflicts from the reverse dependency list.  Note
  // that we can screen out *any* conflict on a version of the same
  // package: if a version conflicts with itself it's ignored, and if
  // a version conflicts with a different version of the same package,
  // that's just a redundant expression of the rule that no two
  // versions of the same package can be simultaneously installed.
  //
  // As a bonus, this lets us match what gets generated for forward
  // deps.
  if(is_conflict(dep_lst->Type) &&
     !prv_lst.end() &&
     const_cast<pkgCache::PrvIterator &>(prv_lst).OwnerPkg() == const_cast<pkgCache::DepIterator &>(dep_lst).ParentPkg())
    return false;
  // Self-deps are always irrelevant regardless of whether we're in a
  // Provides: or not.
  if(prv_lst.end() &&
     const_cast<pkgCache::DepIterator &>(dep_lst).ParentPkg() == const_cast<pkgCache::DepIterator &>(dep_lst).TargetPkg())
    return false;

  // Drop irrelevant conflicts; that is, conflicts that are never
  // satisfied.  This improves the connection consistency of the
  // abstract model.
  if(empty_conflict(dep_lst, prv_lst))
    return false;


  // Unversioned deps always apply unless they are self-deps or
  // empty conflicts.
  if(!dep_lst.TargetVer())
    return true;

  if(provides_open)
    return _system->VS->CheckDep(prv_lst.ProvideVersion(),
				 dep_lst->CompareOp, dep_lst.TargetVer());
  else
    return _system->VS->CheckDep(ver.VerStr(),
				 dep_lst->CompareOp, dep_lst.TargetVer());
}

void aptitude_resolver_version::revdep_iterator::normalize()
{
  while(!dep_lst.end() && !applicable())
    ++dep_lst;

  if(dep_lst.end() && !provides_open)
    {
      eassert(prv_lst.end());
      prv_lst=ver.ProvidesList();
      provides_open=true;
      if(!prv_lst.end())
	{
	  dep_lst=prv_lst.ParentPkg().RevDependsList();
	  while(!dep_lst.end() && !applicable())
	    ++dep_lst;
	}
    }

  // When we've run out of provides, give up..
  while(dep_lst.end() && !prv_lst.end())
    {
      eassert(provides_open);
      ++prv_lst;

      if(!prv_lst.end())
	{
	  eassert(!prv_lst.ParentPkg().end());
	  dep_lst=prv_lst.ParentPkg().RevDependsList();

	  while(!dep_lst.end() && !applicable())
	    ++dep_lst;
	}
    }
}

inline void aptitude_resolver_version::dep_iterator::advance()
{
  bool move_to_next_dep = true;

  eassert(prv_open == !prv.end());

  // If the Provides list is nonempty, advance it.
  if(!prv.end())
    {
      eassert(prv_open);

      ++prv;
      if(prv.end())
	prv_open = false;
      else
	move_to_next_dep = false;
    }
  // If we weren't trying to iterate over a Provides list *and* the
  // current dep is a non-versioned Conflicts, start such an
  // iteration.
  else if(!prv_open && is_conflict(dep->Type) &&
	  !dep.TargetVer())
    {
      prv = dep.TargetPkg().ProvidesList();
      if(!prv.end()) // otherwise we should advance to the next dep.
	{
	  prv_open = true;
	  move_to_next_dep = false;
	}
    }

  eassert(prv_open == !prv.end());

  if(move_to_next_dep)
    {
      if(!dep.end() && is_conflict(dep->Type))
	++dep;
      else
	{
	  // If it's not a conflict, skip a whole OR group.
	  while(!dep.end() && (dep->CompareOp & pkgCache::Dep::Or))
	    ++dep;

	  // Now we're on the last element of the OR group, push
	  // forward.
	  if(!dep.end())
	    ++dep;
	}
    }
}

inline bool
aptitude_resolver_version::dep_iterator::applicable(const pkgCache::DepIterator &dep,
						    const pkgCache::PrvIterator &prv,
						    bool prv_open,
						    pkgDepCache *cache)
{
  if(prv_open)
    {
      eassert(!dep.end());
      eassert(!prv.end());
      eassert(is_conflict(dep->Type));

      if(const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg() == const_cast<pkgCache::DepIterator &>(dep).ParentPkg())
	return false;
    }
  else
    if(const_cast<pkgCache::DepIterator &>(dep).ParentPkg() == const_cast<pkgCache::DepIterator &>(dep).TargetPkg())
      return false;

  if(!is_interesting_dep(dep, cache))
    return false;
  if(empty_conflict(dep, prv))
    return false;

  return true;
}

bool aptitude_resolver_version::dep_iterator::applicable()
{
  if(is_conflict(dep->Type))
    // In this case the current dependency is represented completely
    // by the depends and provides iterators; no need to step.
    return applicable(dep, prv, prv_open, cache);
  else
    {
      // We need to check everything in the current OR block.
      pkgCache::DepIterator tmpdep = dep;

      if(!tmpdep.end() && applicable(tmpdep, prv, prv_open, cache))
	return true;
      while(!tmpdep.end() && (tmpdep->CompareOp & pkgCache::Dep::Or))
	{
	  ++tmpdep;
	  if(!tmpdep.end() && applicable(tmpdep, prv, prv_open, cache))
	    return true;
	}
    }

  return false;
}

void aptitude_resolver_version::dep_iterator::normalize()
{
  while(!dep.end() && !applicable())
    advance();
  // If we ran out of deps, we're done!
}

void aptitude_resolver_dep::solver_iterator::normalize()
{
  if(!is_conflict(dep_lst->Type))
    {
      while(!end())
	{
	  while(!ver_lst.end())
	    {
	      bool ver_matches =
		!dep_lst.TargetVer() ||
		_system->VS->CheckDep(ver_lst.VerStr(),
				      dep_lst->CompareOp,
				      dep_lst.TargetVer());

	      if(ver_matches && ver_disappeared(ver_lst))
		ver_matches = false;

	      if(ver_matches)
		// Found the next entry; drop out.
		return;
	      else
		++ver_lst;
	    }

	  // If we ran out of versions, try provides instead.
	  while(!prv_lst.end())
	    {
	      bool prv_matches=(!dep_lst.TargetVer()) ||
		(prv_lst.ProvideVersion() &&
		 _system->VS->CheckDep(prv_lst.ProvideVersion(),
				       dep_lst->CompareOp,
				       dep_lst.TargetVer()));

	      if(prv_matches &&
		 !ver_disappeared(prv_lst.OwnerVer()))
		return;
	      else
		++prv_lst;
	    }

	  // No more target versions or providers of the target;
	  // increment the dependency list if we aren't yet at the
	  // end of the OR group.

	  if(!(dep_lst->CompareOp & pkgCache::Dep::Or))
	    finished=true;
	  else
	    {
	      ++dep_lst;
	      // Since we aren't finished, dep_lst should still be
	      // valid.
	      eassert(!dep_lst.end());
	      ver_lst=dep_lst.TargetPkg().VersionList();

	      // Only set the prv_lst to non-end if there is no target
	      // version.
	      prv_lst=dep_lst.TargetPkg().ProvidesList();
	    }
	}
    }
  else
    {
      // For Conflicts/Breaks, we're iterating over all the versions of
      // *one* package for *one* dep, either the owner of the
      // dep or a provided package.  (prv_lst is mostly
      // unnecessary, but it makes it simple to remember whether
      // we have a provides).  Note that the efficiency of this
      // stanza is based on the *assumption* that most packages
      // only Provide a few things.

      // For provided packages, return exactly those packages
      // that *don't* have a matching Provides.
      if(!prv_lst.end())
	{
	  while(!ver_lst.end())
	    {
	      if(ver_lst != prv_lst.OwnerVer() &&
		 !ver_disappeared(ver_lst))
		return;

	      ++ver_lst;
	    }
	  // Important point: end version iterators always match
	  // a Conflicts/Breaks! (i.e., those can always be resolved
	  // by removing the conflicted package)
	  return;
	}
      else
	{
	  while(!ver_lst.end())
	    {
	      bool ver_matches=(!dep_lst.TargetVer()) ||
		(ver_lst.VerStr() &&
		 _system->VS->CheckDep(ver_lst.VerStr(),
				       dep_lst->CompareOp,
				       dep_lst.TargetVer()));

	      if(!ver_matches && !ver_disappeared(ver_lst))
		// This version resolves the conflict.
		return;
	      else
		++ver_lst;
	    }

	  // Ignore provides; as above, end versions are A-OK.
	  return;
	}
    }
}

bool aptitude_resolver_dep::solved_by(const aptitude_resolver_version &v) const
{
  pkgCache::DepIterator start_iter(cache->GetCache(), const_cast<pkgCache::Dependency *>(start));

  // First check for moving the source.
  if(v.get_pkg() == start_iter.ParentPkg() && v.get_ver() != start_iter.ParentVer())
    return true;

  // Now check each of the members of the OR group.
  pkgCache::DepIterator d = start_iter;

  if(!is_conflict(start->Type))
    {
      // Of course, installing an end version never fixes a
      // non-conflict unless it removes the source (tested for above).
      if(v.get_ver().end())
	return false;

      while(1)
	{
	  if(d.TargetPkg() == v.get_pkg() &&
	     (!d.TargetVer() || _system->VS->CheckDep(v.get_ver().VerStr(),
						      d->CompareOp,
						      d.TargetVer())))
	    return true;

	  // Check for a resolution via Provides.
	  if(!d.TargetVer())
	    for(pkgCache::PrvIterator p2 = v.get_ver().ProvidesList();
		!p2.end(); ++p2)
	      if(const_cast<pkgCache::PrvIterator &>(p2).ParentPkg() == d.TargetPkg())
		return true;

	  if((d->CompareOp & pkgCache::Dep::Or) != 0)
	    ++d;
	  else
	    return false;
	}
    }
  else if(prv == NULL)
    {
      if(d.TargetPkg() != v.get_pkg())
	return false;

      if(v.get_ver().end())
	return true;

      // Check the non-virtual part of the conflict: the package is
      // the same and the version **doesn't** match.
      return !(!d.TargetVer() || _system->VS->CheckDep(v.get_ver().VerStr(),
						       d->CompareOp,
						       d.TargetVer()));
    }
  else
    {
      pkgCache::PrvIterator prv_iter(cache->GetCache(), const_cast<pkgCache::Provides *>(prv),
				     (pkgCache::Package *)0);

      // Only other versions of the provider can solve this.
      if(v.get_pkg() != prv_iter.OwnerPkg())
	return false;
      else
	return v.get_ver() != prv_iter.OwnerVer();
    }
}

aptitude_resolver_dep::solver_iterator &aptitude_resolver_dep::solver_iterator::operator++()
{
  eassert(!end());

  // Advance whatever needs to be advanced next in the
  // sub-list.

  if(!ver_lst.end())
    ++ver_lst;
  else if(!is_conflict(dep_lst->Type))
    {
      if(!prv_lst.end())
	++prv_lst;
    }
  else
    finished=true;

  normalize();

  return *this;
}

aptitude_resolver_version::dep_iterator &aptitude_resolver_version::dep_iterator::operator++()
{
  eassert(!dep.end());

  advance();
  normalize();

  return *this;
}

aptitude_resolver_version aptitude_resolver_dep::solver_iterator::operator*() const
{
  eassert(!end());

  if(!ver_lst.end())
    return aptitude_resolver_version::make_install(ver_lst, cache);
  else // In this case we're trying to remove some package or other.
    {
      if(!is_conflict(dep_lst->Type))
	{
	  // Assume this because otherwise end() should be true.
	  eassert(!prv_lst.end());

	  return aptitude_resolver_version::make_install(const_cast<pkgCache::PrvIterator &>(prv_lst).OwnerVer(), cache);
	}
      else if(!prv_lst.end())
	return aptitude_resolver_version::make_removal(const_cast<pkgCache::PrvIterator &>(prv_lst).OwnerPkg(), cache);
      else
	return aptitude_resolver_version::make_removal(const_cast<pkgCache::DepIterator &>(dep_lst).TargetPkg(), cache);
    }
}

void aptitude_universe::dep_iterator::normalize()
{
  while(dep.end() && !pkg.end())
    {
      while(dep.end() && !ver.end())
	{
	  ++ver;
	  if(!ver.end())
	    dep=aptitude_resolver_version::dep_iterator(ver, cache);
	}

      if(dep.end())
	{
	  ++pkg;
	  if(!pkg.end())
	    {
	      ver=pkg.VersionList();
	      if(!ver.end())
		dep=aptitude_resolver_version::dep_iterator(ver, cache);
	    }
	}
    }
}

bool aptitude_universe::broken_dep_iterator::dep_is_inst_broken(const pkgCache::DepIterator &d) const
{
  pkgCache::DepIterator d2=d;

  while(d2->CompareOp & pkgCache::Dep::Or)
    ++d2;

  return ((*cache)[d2] & pkgDepCache::DepGInstall)==0;
}

struct DummyEmptySolution
{
  aptitude_resolver_version version_of(const aptitude_resolver_package &p) const
  {
    return p.current_version();
  }
};

void dumpDep(std::ostream &out, pkgCache::DepIterator &dep,
	     pkgDepCache *cache)
{
  out << dep.ParentPkg().FullName(false) << " ("
      << dep.ParentVer().VerStr() << ") "
      << dep.DepType()
      << " "
      << dep.TargetPkg().FullName(false);
  if(dep.TargetVer() != NULL)
    {
      out << " (" << dep.CompType() << " " << dep.TargetVer() << ")";
    }
  std::vector<std::string> flags;
  if(dep->Type & pkgCache::Dep::Or)
    flags.push_back("OR");

  unsigned char dep_state = (*cache)[dep];
  if(dep_state & pkgDepCache::DepNow)
    flags.push_back("DEPNOW");
  if(dep_state & pkgDepCache::DepInstall)
    flags.push_back("DEPINSTALL");
  if(dep_state & pkgDepCache::DepCVer)
    flags.push_back("DEPCVER");
  if(dep_state & pkgDepCache::DepGNow)
    flags.push_back("DEPGNOW");
  if(dep_state & pkgDepCache::DepGInstall)
    flags.push_back("DEPGINSTALL");
  if(dep_state & pkgDepCache::DepGCVer)
    flags.push_back("DEPGCVER");

  out << "[";
  bool first = true;
  for(std::vector<std::string>::const_iterator it =
	flags.begin(); it != flags.end(); ++it)
    {
      if(first)
	first = false;
      else
	out << " ";

      out << *it;
    }
  out << "]";
}

void aptitude_universe::broken_dep_iterator::normalize()
{
  while(!the_dep.end() &&
	!(is_interesting_dep(the_dep, cache) &&
	  dep_is_inst_broken(the_dep)))
    ++the_dep;

  while(the_dep.end() && !pkg.end())
    {
      // Make sure we move at least one package forward!
      // Otherwise we just spin on the same package over and over,
      // since it's still broken..
      ++pkg;

      if(!pkg.end())
	{
	  // Examine just the InstVer of the package.
	  pkgCache::VerIterator ver=(*cache)[pkg].InstVerIter(*cache);

	  if(!ver.end())
	    the_dep=ver.DependsList();

	  while(!the_dep.end() &&
		!(is_interesting_dep(the_dep, cache) &&
		  dep_is_inst_broken(the_dep)))
	    ++the_dep;

	  if(the_dep.end())
	    ++ver;
	}
    }

  eassert(the_dep.end() || is_interesting_dep(the_dep, cache));

  // Now dep is a broken critical dep or an end dep.  If it is a
  // conflicts, we might need to push down into Provides...
  if(!the_dep.end() && is_conflict(the_dep->Type))
    {
      // If we aren't in provides, check whether the dep is
      // trivially broken (i.e., without following provides).
      if(!prv_open)
	{
	  // If it's a direct self-conflict, jump into provides
	  // right away.
	  if(the_dep.TargetPkg() != the_dep.ParentPkg())
	    {
	      pkgCache::VerIterator ver=(*cache)[the_dep.TargetPkg()].InstVerIter(*cache);

	      if(!ver.end() &&
		 !ver_disappeared(ver) &&
		 (!the_dep.TargetVer() ||
		  (ver.VerStr() &&
		   _system->VS->CheckDep(ver.VerStr(),
					 the_dep->CompareOp,
					 the_dep.TargetVer()))))
		// OK, the dep is broken without provides, no need
		// to descend.
		return;
	    }

	  prv_open=true;
	  prv=the_dep.TargetPkg().ProvidesList();
	}

      // Ok, we have found something that causes breakage.  The
      // provides-list is a list of all the package versions that
      // provide this package name; move forward until we find one
      // that matches.
      while(!prv.end())
	{
	  // Ignore indirect self-conflicts.
	  if(prv.OwnerPkg() != the_dep.ParentPkg())
	    {
	      // First, is the providing version going to be
	      // installed?
	      if((*cache)[prv.OwnerPkg()].InstVerIter(*cache) == prv.OwnerVer())
		{
		  // Ok, does it match the version string?
		  bool matches =
		    !the_dep.TargetVer() ||
		    (prv.ProvideVersion() &&
		     _system->VS->CheckDep(prv.ProvideVersion(),
					   the_dep->CompareOp,
					   the_dep.TargetVer()));

		  if(ver_disappeared(prv.OwnerVer()))
		    matches = false;

		  if(matches)
		    return;
		}
	    }

	  ++prv;
	}

      // if all provides are exhausted, increment the dep and try
      // again.  (probably this was only a self-conflict and
      // nothing else)
      ++the_dep;
      prv_open=false;
      normalize();
      // hopefully g++ is smart enough to optimize this into a
      // tail call.
      return;
    }

  if(!end() && !(**this).broken_under(DummyEmptySolution()))
    {
      std::ostringstream s;
      s << **this;
      // Not translated because this is an internal error that users
      // should report to me verbatim.
      _error->Warning("apt thinks that %s is broken, but I don't.",
		      s.str().c_str());
      ++(*this);
      return;
    }
}

aptitude_universe::broken_dep_iterator &aptitude_universe::broken_dep_iterator::operator++()
{
  eassert(!pkg.end());
  // If the_dep.end() we have pkg.end().
  eassert(!the_dep.end());

  if(!prv_open && is_conflict(the_dep->Type))
    {
      prv_open = true;
      prv = the_dep.TargetPkg().ProvidesList();
    }
  else if(prv_open && !prv.end())
    ++prv;
  else
    ++the_dep;

  normalize();
  return *this;
}

std::ostream &operator<<(ostream &out, const aptitude_resolver_package &p)
{
  return out << p.get_name();
}

std::ostream &operator<<(ostream &out, const aptitude_resolver_version &v)
{
  return out << v.get_package().get_name() << " " << v.get_name();
}

std::ostream &operator<<(ostream &out, const aptitude_resolver_dep &d)
{
  std::vector<aptitude_resolver_version> solvers;
  for(aptitude_resolver_dep::solver_iterator i=d.solvers_begin(); !i.end(); ++i)
    solvers.push_back(*i);

  generic_solution<aptitude_universe>::ver_name_lt lt;
  sort(solvers.begin(), solvers.end(), lt);

  out << d.get_source() << (d.is_soft() ? " -S> {" : " -> {");

  for(std::vector<aptitude_resolver_version>::const_iterator i = solvers.begin();
      i != solvers.end(); ++i)
    {
      if(i != solvers.begin())
	out << " ";
      out << *i;
    }
  out << "}";

  return out;
}

std::ostream &operator<<(std::ostream &out, const cfg_level &level)
{
  if(level.get_is_discard())
    out << "discard";
  else
    out << level.get_level();

  return out;
}

cfg_level aptitude_universe::parse_level(const std::string &s)
{
  typedef generic_problem_resolver<aptitude_universe> aptitude_resolver;
  if(s == "maximum")
    return cfg_level::make_level(cost_limits::maximum_level);
  else if(s == "minimum" || s == "")
    return cfg_level::make_level(cost_limits::minimum_level);
  else if(s == "conflict" || s == "discard")
    return cfg_level::make_conflict();
  else
    {
      char *endptr;
      int n = static_cast<int>(strtol(s.c_str(), &endptr, 0));
      if(*endptr != '\0')
	{
	  std::string msg(ssprintf(N_("Invalid safety level \"%s\" (not \"discard\", \"maximum\", \"minimum\", or an integer)."), s.c_str()));
	  LOG_ERROR(Loggers::getAptitudeResolverCosts(), msg);
	  _error->Error("%s", _(msg.c_str()));
	  return cfg_level::make_level(cost_limits::minimum_level);
	}
      else
	return cfg_level::make_level(n);
    }
}

cfg_level aptitude_universe::parse_levels(const std::string &level1,
                                          const std::string &level2,
                                          cfg_level default_level)
{
  if(level1.empty() && level2.empty())
    return default_level;
  else if(level1.empty())
    return parse_level(level2);
  else if(level2.empty())
    return parse_level(level1);
  else
    return std::max<cfg_level>(parse_level(level1), parse_level(level2));
}

cfg_level aptitude_universe::get_safe_level()
{
  return
    parse_levels(aptcfg->Find(PACKAGE "::ProblemResolver::Safe-Level", ""),
                 aptcfg->Find(PACKAGE "::ProblemResolver::Safe-Tier", ""),
                 cfg_level::make_level(10000));
}

cfg_level aptitude_universe::get_keep_all_level()
{
  return parse_levels(aptcfg->Find(PACKAGE "::ProblemResolver::Keep-All-Level", ""),
                      aptcfg->Find(PACKAGE "::ProblemResolver::Keep-All-Tier", ""),
                      cfg_level::make_level(20000));
}

cfg_level aptitude_universe::get_remove_level()
{
  return parse_levels(aptcfg->Find(PACKAGE "::ProblemResolver::Remove-Level", ""),
                      aptcfg->Find(PACKAGE "::ProblemResolver::Remove-Tier", ""),
                      cfg_level::make_level(10000));
}

cfg_level aptitude_universe::get_break_hold_level()
{
  return parse_levels(aptcfg->Find(PACKAGE "::ProblemResolver::Break-Hold-Level", ""),
                      aptcfg->Find(PACKAGE "::ProblemResolver::Break-Hold-Tier", ""),
                      cfg_level::make_level(40000));
}

cfg_level aptitude_universe::get_non_default_level()
{
  return parse_levels(aptcfg->Find(PACKAGE "::ProblemResolver::Non-Default-Level", ""),
                      aptcfg->Find(PACKAGE "::ProblemResolver::Non-Default-Tier", ""),
                      cfg_level::make_level(50000));
}

cfg_level aptitude_universe::get_remove_essential_level()
{
  return parse_levels(aptcfg->Find(PACKAGE "::ProblemResolver::Remove-Essential-Level", ""),
                      aptcfg->Find(PACKAGE "::ProblemResolver::Remove-Essential-Tier", ""),
                      cfg_level::make_level(60000));
}

bool aptitude_universe::is_candidate_for_initial_set(const aptitude_resolver_dep &d) const
{
  if(!d.is_soft())
    return true;

  if(cache == NULL)
    return false;

  pkgCache::DepIterator d2(d.get_dep());
  while(!d2.end() && (d2->CompareOp & pkgCache::Dep::Or))
    ++d2;

  if(d2.end())
    return true;

  // Return true only if the dependency is *currently* not broken.
  return (*cache)[d2] & pkgDepCache::DepGNow;
}
