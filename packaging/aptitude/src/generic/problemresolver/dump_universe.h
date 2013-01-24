// dump_universe.h                                      -*-c++-*-
//
//   Copyright (C) 2005, 2009 Daniel Burrows
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

#ifndef DUMP_UNIVERSE_H
#define DUMP_UNIVERSE_H

#include <iostream>

/** \file dump_universe.h
 */

template<class PackageUniverse>
void dump_universe(const PackageUniverse &world, std::ostream &out)
{
  out << "UNIVERSE [";
  for(typename PackageUniverse::package_iterator p=world.packages_begin();
      !p.end(); ++p)
    {
      out << "  PACKAGE " << (*p).get_name() << " < ";
      for(typename PackageUniverse::package::version_iterator v=(*p).versions_begin();
	  !v.end(); ++v)
	out << (*v).get_name() << " ";
      out << ">" << " " << (*p).current_version().get_name() << std::endl;
    }

  for(typename PackageUniverse::dep_iterator d=world.deps_begin();
      !d.end(); ++d)
    {
      const typename PackageUniverse::version &sv=(*d).get_source();
      const typename PackageUniverse::package &sp=sv.get_package();

      if((*d).is_soft())
	out << "  SOFTDEP ";
      else
	out << "  DEP ";

      out << sp.get_name() << " " << sv.get_name() << " "
	  << (world.is_candidate_for_initial_set(*d)
	      ? "->" : "-?>")
	  << " < ";

      for(typename PackageUniverse::dep::solver_iterator t=(*d).solvers_begin();
	  !t.end(); ++t)
	{
	  out << " " << (*t).get_package().get_name() << " " << (*t).get_name() << " ";
	}
      out << " > " << std::endl;
    }
  out << "]" << std::endl;
}

#endif // DUMP_UNIVERSE_H
