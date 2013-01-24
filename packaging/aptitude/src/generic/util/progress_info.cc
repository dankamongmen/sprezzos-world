/** \file progress_info.cc */

// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "progress_info.h"

namespace aptitude
{
  namespace util
  {
    bool progress_info::operator==(const progress_info &other) const
    {
      if(type != other.type)
        return false;

      switch(type)
        {
        case progress_type_pulse:
          return progress_status == other.progress_status;

        case progress_type_bar:
          return
            progress_fraction == other.progress_fraction &&
            progress_status == other.progress_status;

        default:
          return true;
        }
    }

    std::ostream &operator<<(std::ostream &out, const progress_info &info)
    {
      switch(info.get_type())
        {
        case progress_type_none: return out << "none()";

        case progress_type_pulse:
          return out << "pulse(" << info.get_progress_status() << ")";

        case progress_type_bar:
          return out << "bar("
                     << info.get_progress_fraction()
                     << ", "
                     << info.get_progress_status()
                     << ")";

        default:
          return out << "error(" << info.get_type() << ")";
        }
    }
  }
}
