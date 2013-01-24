// compare_patterns.cc    -*-c++-*-
//
//   Copyright (C) 2008-2009 Daniel Burrows
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

#include "compare_patterns.h"

#include "pattern.h"

using cwidget::util::ref_ptr;

namespace aptitude
{
  namespace matching
  {
    inline
    int compare_regex_info(const pattern::regex_info &r1,
			   const pattern::regex_info &r2)
    {
      return r1.get_regex_string().compare(r2.get_regex_string());
    }

    template<typename I>
    inline int compare_int(I i1, I i2)
    {
      return i1 - i2;
    }

    int compare_pattern_lists(const std::vector<ref_ptr<pattern> > &l1,
			      const std::vector<ref_ptr<pattern> > &l2)
    {
      std::vector<ref_ptr<pattern> >::const_iterator
	begin1 = l1.begin(), end1 = l1.end(),
	begin2 = l2.begin(), end2 = l2.end();

      while(begin1 != end1 && begin2 != end2)
	{
	  int cmp = compare_patterns(*begin1, *begin2);
	  if(cmp != 0)
	    return cmp;

	  ++begin1;
	  ++begin2;
	}

      if(begin1 != end1)
	return 1;
      else if(begin2 != end2)
	return -1;
      else
	return 0;
    }

    int compare_patterns(const ref_ptr<pattern> &p1,
			 const ref_ptr<pattern> &p2)
    {
      // If the patterns have different constructors, order them
      // according to their constructors.
      if(p1->get_type() < p2->get_type())
	return -1;
      else if(p1->get_type() > p2->get_type())
	return 1;
      else
	switch(p1->get_type())
	  {
	  case pattern::archive:
	    return compare_regex_info(p1->get_archive_regex_info(),
				      p2->get_archive_regex_info());

	  case pattern::action:
	    return compare_int(p1->get_action_action_type(), p2->get_action_action_type());
	    if(p1->get_action_action_type() < p2->get_action_action_type())
	      return -1;
	    else if(p1->get_action_action_type() > p2->get_action_action_type())
	      return 1;
	    else
	      return 0;

	  case pattern::all_versions:
	    return compare_patterns(p1->get_all_versions_pattern(),
				    p2->get_all_versions_pattern());

	  case pattern::any_version:
	    return compare_patterns(p1->get_any_version_pattern(),
				    p2->get_any_version_pattern());

	  case pattern::architecture:
	    return p1->get_architecture_arch_specification()->get_specification()
              .compare(p2->get_architecture_arch_specification()->get_specification());

	  case pattern::automatic:
	    return 0;

	  case pattern::and_tp:
	    return compare_pattern_lists(p1->get_and_patterns(),
					 p2->get_and_patterns());

	  case pattern::bind:
	    {
	      const int name_cmp = compare_int(p1->get_bind_variable_index(), p2->get_bind_variable_index());

	      if(name_cmp != 0)
		return name_cmp;

	      return compare_patterns(p1->get_bind_pattern(),
				      p2->get_bind_pattern());
	    }

	  case pattern::broken:
	    return 0;

	  case pattern::broken_type:
	    return compare_int(p1->get_broken_type_depends_type(),
			       p2->get_broken_type_depends_type());

	  case pattern::candidate_version:
	    return 0;

	  case pattern::config_files:
	    return 0;

	  case pattern::current_version:
	    return 0;

	  case pattern::depends:
	    {
	      const int type_cmp = compare_int(p1->get_depends_depends_type(),
					       p2->get_depends_depends_type());
	      if(type_cmp != 0)
		return type_cmp;

	      const int broken_cmp = compare_int(p1->get_depends_broken() ? 1 : 0,
						 p2->get_depends_broken() ? 1 : 0);
	      if(broken_cmp != 0)
		return broken_cmp;

	      return compare_patterns(p1->get_depends_pattern(),
				      p2->get_depends_pattern());
	    }

	  case pattern::description:
	    return compare_regex_info(p1->get_description_regex_info(),
				      p2->get_description_regex_info());

	  case pattern::essential:
	    return 0;

	  case pattern::equal:
	    return compare_int(p1->get_equal_stack_position(),
			       p2->get_equal_stack_position());

	  case pattern::exact_name:
	    return p1->get_exact_name_name().compare(p2->get_exact_name_name());

	  case pattern::false_tp:
	    return 0;

	  case pattern::for_tp:
	    // Correct comparison here relies on the fact that
	    // variable names are lower-case.
	    {
	      const int variable_name_cmp =
		p1->get_for_variable_name().compare(p2->get_for_variable_name());

	      if(variable_name_cmp != 0)
		return variable_name_cmp;

	      return compare_patterns(p1->get_for_pattern(),
				      p2->get_for_pattern());
	    }

	  case pattern::foreign_architecture:
	    return 0;

	  case pattern::garbage:
	    return 0;

	  case pattern::install_version:
	    return 0;

	  case pattern::installed:
	    return 0;

	  case pattern::maintainer:
	    return compare_regex_info(p1->get_maintainer_regex_info(),
				      p2->get_maintainer_regex_info());

	  case pattern::multiarch:
	    return compare_int(p1->get_multiarch_multiarch_type(),
			       p2->get_multiarch_multiarch_type());

	  case pattern::name:
	    return compare_regex_info(p1->get_name_regex_info(),
				      p2->get_name_regex_info());

	  case pattern::narrow:
	    {
	      const int filter_cmp =
		compare_patterns(p1->get_narrow_filter(),
				 p2->get_narrow_filter());

	      if(filter_cmp != 0)
		return filter_cmp;

	      return compare_patterns(p1->get_narrow_pattern(),
				      p2->get_narrow_pattern());
	    }

	  case pattern::native_architecture:
	    return 0;

	  case pattern::new_tp:
	    return 0;

	  case pattern::not_tp:
	    return compare_patterns(p1->get_not_pattern(),
				    p2->get_not_pattern());

	  case pattern::obsolete:
	    return 0;

	  case pattern::or_tp:
	    return compare_pattern_lists(p1->get_or_patterns(),
					 p2->get_or_patterns());

	  case pattern::origin:
	    return compare_regex_info(p1->get_origin_regex_info(),
				      p2->get_origin_regex_info());

	  case pattern::priority:
	    return compare_int(p1->get_priority_priority(),
			       p2->get_priority_priority());

	  case pattern::provides:
	    return compare_patterns(p1->get_provides_pattern(),
				    p2->get_provides_pattern());

	  case pattern::reverse_depends:
	    {
	      const int type_cmp = compare_int(p1->get_reverse_depends_depends_type(),
					       p2->get_reverse_depends_depends_type());
	      if(type_cmp != 0)
		return type_cmp;

	      const int broken_cmp = compare_int(p1->get_reverse_depends_broken() ? 1 : 0,
						 p2->get_reverse_depends_broken() ? 1 : 0);
	      if(broken_cmp != 0)
		return broken_cmp;

	      return compare_patterns(p1->get_reverse_depends_pattern(),
				      p2->get_reverse_depends_pattern());
	    }

	  case pattern::reverse_provides:
	    return compare_patterns(p1->get_reverse_provides_pattern(),
				    p2->get_reverse_provides_pattern());

	  case pattern::section:
	    return compare_regex_info(p1->get_section_regex_info(),
				      p2->get_section_regex_info());

	  case pattern::source_package:
	    return compare_regex_info(p1->get_source_package_regex_info(),
				      p2->get_source_package_regex_info());

	  case pattern::source_version:
	    return compare_regex_info(p1->get_source_version_regex_info(),
				      p2->get_source_version_regex_info());

	  case pattern::tag:
	    return compare_regex_info(p1->get_tag_regex_info(),
				      p2->get_tag_regex_info());

	  case pattern::task:
	    return compare_regex_info(p1->get_task_regex_info(),
				      p2->get_task_regex_info());

	  case pattern::term:
	    return p1->get_term_term().compare(p2->get_term_term());

	  case pattern::term_prefix:
	    return p1->get_term_prefix_term().compare(p2->get_term_prefix_term());

	  case pattern::true_tp:
	    return 0;

	  case pattern::upgradable:
	    return 0;

	  case pattern::user_tag:
	    return compare_regex_info(p1->get_user_tag_regex_info(),
				      p2->get_user_tag_regex_info());

	  case pattern::version:
	    return compare_regex_info(p1->get_version_regex_info(),
				      p2->get_version_regex_info());

	  case pattern::virtual_tp:
	    return 0;

	  case pattern::widen:
	    return compare_patterns(p1->get_widen_pattern(),
				    p2->get_widen_pattern());
	  }

      eassert(!"Internal error: we should never get here.");
    }
  }
}
