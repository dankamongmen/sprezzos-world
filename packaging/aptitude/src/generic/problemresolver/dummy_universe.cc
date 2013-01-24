// dummy_universe.cc
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

#include "dummy_universe.h"

#include <algorithm>

using namespace std;

dummy_package::dummy_package(const string &_name, unsigned int id)
  :name(_name), ID(id)
{
}

dummy_package::~dummy_package()
{
  for(version_iterator i=versions.begin(); i!=versions.end(); ++i)
    delete *i;
}

dummy_version *dummy_package::version_from_name(const string &the_name) const
{
  for(version_iterator i=versions.begin(); i!=versions.end(); ++i)
    if((*i)->get_name()==the_name)
      return *i;

  throw NoSuchNameError("version of package "+name, the_name);
}

struct ver_ptr_name_lt
{
public:
  bool operator()(const dummy_version *v1,
		  const dummy_version *v2) const
  {
    int pncmp = v1->get_package().get_name().compare(v2->get_package().get_name());

    if(pncmp != 0)
      return pncmp < 0;
    else
      return v1->get_name() < v2->get_name();
  }
};

dummy_dep::dummy_dep(dummy_version *_source,
		     const std::vector<dummy_version *> &_target_set,
		     unsigned int _ID, bool _soft,
		     bool _candidate_for_initial_set)
  :source(_source), target_set(_target_set), ID(_ID), soft(_soft),
   candidate_for_initial_set(_candidate_for_initial_set)
{

  sort(target_set.begin(), target_set.end(),
       ver_ptr_name_lt());
}

bool dummy_dep::broken() const
{
  if(source->get_package().current_version() != *source)
    return false;

  for(solver_iterator i=target_set.begin();
      i!=target_set.end(); ++i)
    if((*i)->get_package().current_version() == **i)
      return false;

  return true;
}

bool dummy_universe::dep::solved_by(const version &ver) const
{
  if(ver.get_package() == get_source().get_package() &&
     ver != get_source())
    return true;

  for(dummy_dep::solver_iterator i = real_dep->solvers_begin();
      i != real_dep->solvers_end(); ++i)
    if(version(*i) == ver)
      return true;

  return false;
}

dummy_universe::~dummy_universe()
{
  for(vector<dummy_package *>::const_iterator i=packages.begin();
      i!=packages.end(); ++i)
    delete *i;

  for(vector<dummy_dep *>::const_iterator i=deps.begin();
      i!=deps.end(); ++i)
    delete *i;
}


void dummy_universe::add_package(const string &name,
				 vector<string> the_versions,
				 const string &curname)
{
  eassert(!the_versions.empty());

  packages.push_back(new dummy_package(name, packages.size()));

  for(vector<string>::const_iterator i=the_versions.begin();
      i!=the_versions.end(); ++i)
    {
      versions.push_back(new dummy_version(*i, packages.back(), versions.size()));
      packages.back()->add_version(versions.back());
    }

  packages.back()->set_current_version(packages.back()->version_from_name(curname));

  packages_by_name[name]=packages.back();
}

void dummy_universe::add_dep(const string &pkg_name, const string &pkg_ver,
			     const vector<pair<string, string> > &target_names,
			     bool is_conflict, bool is_soft,
			     bool is_candidate_for_initial_set)
{
  dummy_package *pkg=find_package_internal(pkg_name);

  set<dummy_version *, compare_dummy_versions> targets;
  set<dummy_package *, compare_dummy_packages> packages;

  for(vector<pair<string, string> >::const_iterator i=target_names.begin();
      i!=target_names.end(); ++i)
    {
      dummy_package *pkg=find_package_internal(i->first);

      packages.insert(pkg);
      targets.insert(pkg->version_from_name(i->second));
    }

  if(!is_conflict)
    deps.push_back(new dummy_dep(pkg->version_from_name(pkg_ver),
				 vector<dummy_version *>(targets.begin(), targets.end()),
				 deps.size(),
				 is_soft,
				 is_candidate_for_initial_set));
  else
    {
      set<dummy_version *> targets2;

      for(set<dummy_package *, compare_dummy_packages>::const_iterator i=packages.begin();
	  i!=packages.end(); ++i)
	for(dummy_package::version_iterator j=(*i)->versions_begin();
	    j!=(*i)->versions_end(); ++j)
	  if(targets.find(*j)==targets.end())
	    targets2.insert(*j);

      deps.push_back(new dummy_dep(pkg->version_from_name(pkg_ver),
				   vector<dummy_version *>(targets2.begin(), targets2.end()),
				   deps.size(),
				   is_soft,
				   is_candidate_for_initial_set));
    }

  dummy_dep *newdep=deps.back();

  newdep->get_source().add_dep(newdep);

  for(dummy_dep::solver_iterator i=newdep->solvers_begin();
      i!=newdep->solvers_end(); ++i)
    (*i)->add_revdep(newdep);
}

ostream &operator<<(ostream &out, const dummy_universe::package &p)
{
  return out << p.get_name();
}

ostream &operator<<(ostream &out, const dummy_universe::version &v)
{
  return out << v.get_package().get_name() << " " << v.get_name();
}

ostream &operator<<(ostream &out, const dummy_universe::dep &d)
{
  out << d.get_source() << " -";

  if(d.is_soft())
    out << "S";
  if(!d.is_candidate_for_initial_set())
    out << "?";
  out << "> {";

  for(dummy_universe::dep::solver_iterator i=d.solvers_begin();
      !i.end(); ++i)
    {
      if(i!=d.solvers_begin())
	out << " ";
      out << *i;
    }
  out << "}";

  return out;
}

pair<string, string> read_pkgverpair(istream &in)
{
  in >> ws;

  string pkgname,vername;

  in >> pkgname >> ws;

  if(in.eof())
    throw ParseError("Expected version name after package name "+pkgname+", got EOF");

  in >> vername >> ws;

  return pair<string, string>(pkgname, vername);
}

dummy_universe_ref parse_universe(istream &in)
{
  string s;

  in >> ws;
  if(in.eof())
    throw ParseError("Expected \"UNIVERSE\"; got EOF");

  in >> s >> ws;

  if(s != "UNIVERSE")
    throw ParseError("Expected \"UNIVERSE\"; got \"" + s + "\"");

  if(in.eof())
    throw ParseError("Expected \"[\"; got EOF");

  in >> s >> ws;

  if(s != "[")
    throw ParseError("Expected \"[\" following UNIVERSE; got \"" + s + "\"");

  if(in.eof())
    throw ParseError("Unexpected EOF after \"UNIVERSE [\"");

  return parse_universe_tail(in);
}

dummy_universe_ref parse_universe_tail(istream &in)
{
  dummy_universe_ref rval=new dummy_universe;

  in >> ws;
  while(in)
    {
      string s;

      if(in.eof())
	throw ParseError("Expected ']', 'PACKAGE', or 'DEP'; got EOF");

      in >> s >> ws;

      if(s == "]")
	break;
      else if(s == "PACKAGE")
	{
	  string pkgname;

	  in >> pkgname >> ws;

	  if(in.eof())
	    throw ParseError("Unexpected EOF after PACKAGE "+pkgname);

	  in >> s >> ws;

	  if(s != "<")
	    throw ParseError("Expected '<', got "+s);

	  if(in.eof())
	    throw ParseError("Unexpected EOF after PACKAGE "+pkgname+" <");

	  vector<string> vernames;

	  while(in)
	    {
	      string vername;

	      if(in.eof())
		throw ParseError("Expected version name or '>', got EOF");

	      in >> vername >> ws;

	      if(vername == ">")
		break;

	      vernames.push_back(vername);
	    }

	  if(in.eof())
	    throw ParseError("Expected a definition of the current version of "+pkgname+", got EOF");

	  string curname;

	  in >> curname >> ws;

	  if(vernames.empty())
	    throw ParseError("Package "+pkgname+" has no versions");

	  rval.add_package(pkgname, vernames, curname);
	}
      else if(s == "DEP" || s == "SOFTDEP")
	{
	  pair<string, string> source=read_pkgverpair(in);
	  bool is_conflict=false;
	  bool is_soft = (s == "SOFTDEP");
	  bool is_candidate_for_initial_set = true;

	  in >> s >> ws;

	  if(s == "!!")
	    is_conflict=true;
	  else if(s == "-?>")
	    is_candidate_for_initial_set = false;
	  else if(s != "->")
	    throw ParseError("Expected '->', '-?>', or '!!', got "+s);

	  if(in.eof())
	    throw ParseError("Expected '<', got EOF");

	  in >> s >> ws;

	  if(s != "<")
	    throw ParseError("Expected '<', got "+s);

	  if(in.eof())
	    throw ParseError("Expected package-version pair, got EOF");

	  vector<pair<string, string> > targets;

	  while(in)
	    {
	      string pkgname,vername;

	      in >> pkgname >> ws;

	      if(pkgname == ">")
		break;

	      if(in.eof())
		throw ParseError("Expected version name after package name "+pkgname+", got EOF");

	      in >> vername >> ws;

	      if(vername == ">")
		throw ParseError("Expected version name after package name "+pkgname+", got end-of-list instead");

	      targets.push_back(pair<string,string>(pkgname,vername));

	      if(in.eof())
		throw ParseError("Unexpected EOF in dependency target list following package "+pkgname+" version "+vername);
	    }

	  rval.add_dep(source.first, source.second, targets,
		       is_conflict, is_soft,
		       is_candidate_for_initial_set);
	}
      else
	throw ParseError("Expected PACKAGE, DEP, or SOFTDEP, got "+s);

      if(in.eof())
	throw ParseError("Expected ']' following universe declaration, got EOF.");
    }

  return rval;
}
