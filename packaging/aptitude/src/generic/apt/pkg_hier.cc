// pkg_hier.cc
//
//  Implementation of pkg_hier.h

#include "pkg_hier.h"

#include <aptitude.h>

#include "apt.h"

#include <apt-pkg/error.h>
#include <apt-pkg/tagfile.h>

using namespace std;

void pkg_hier::visit_item(pkg_hier::item *item, void *parent_data,
			  pkg_hier::hierarchy_realizer *realizer)
{
  realizer->realize_item(item, parent_data);
}

void pkg_hier::visit_group(pkg_hier::group *group, void *parent_data,
			   pkg_hier::hierarchy_realizer *realizer)
{
  group::build_info *info=realizer->get_build_info(group->id);

  if(info->active)
    _error->Warning(_("Warning: group %s is involved in a cycle"), group->name.c_str());
  else
    {
      info->active=true;

      void *curr_data=realizer->realize_group(group, parent_data);

      for(vector<item *>::iterator i=group->children.begin();
	  i!=group->children.end(); ++i)
	(*i)->realize_me(this, curr_data, realizer);

      info->active=false;
    }
}

void pkg_hier::realize_group_up(pkg_hier::group *group,
				pkg_hier::hierarchy_realizer *realizer)
{
  group::build_info *info=realizer->get_build_info(group->id);

  if(info->active)
    _error->Warning(_("Warning: group %s is involved in a cycle"), group->name.c_str());
  else
    {
      info->active=true;

      if(!info->seen)
	{
	  if(group->parents.empty())
	    info->node_data.push_back(realizer->realize_group(group, NULL));
	  else
	    for(std::set<string>::iterator i=group->parents.begin();
		i!=group->parents.end(); ++i)
	      {
		groupmap::iterator found=groups.find(*i);

		if(found!=groups.end())
		  {
		    // The group exists; realize it and then realize
		    // ourselves based on it.
		    realize_group_up(&found->second, realizer);

		    group::build_info *parent_info=realizer->get_build_info(found->second.id);

		    for(vector<void *>::iterator j=parent_info->node_data.begin();
			j!=parent_info->node_data.end(); ++j)
		      info->node_data.push_back(realizer->realize_group(group, *j));
		  }
	      }

	  info->seen=true;
	}

      info->active=false;
    }
}

void pkg_hier::realize_item_up(pkg_hier::item *item,
			       pkg_hier::hierarchy_realizer *realizer)
{
  if(item->parents.empty())
    realizer->realize_item(item, NULL);
  else
    for(std::set<string>::iterator i=item->parents.begin();
	i!=item->parents.end(); ++i)
      {
	groupmap::iterator found=groups.find(*i);

	if(found!=groups.end())
	  {
	    realize_group_up(&found->second, realizer);

	    group::build_info *info=realizer->get_build_info(found->second.id);

	    for(vector<void *>::iterator j=info->node_data.begin();
		j!=info->node_data.end(); ++j)
	      realizer->realize_item(item, *j);
	  }
      }
}

bool pkg_hier::realize_item_up(string item, hierarchy_realizer *realizer)
{
  pkgmap::iterator found=pkgs.find(item);

  if(found!=pkgs.end())
    {
      realize_item_up(&found->second, realizer);
      return true;
    }
  else
    return false;
}

void pkg_hier::item::realize_me(pkg_hier *hier, void *parent_data,
				hierarchy_realizer *realizer)
{
  hier->visit_item(this, parent_data, realizer);
}

void pkg_hier::group::realize_me(pkg_hier *hier, void *parent_data,
				 hierarchy_realizer *realizer)
{
  hier->visit_group(this, parent_data, realizer);
}

// TODO: display a progress meter?
void pkg_hier::input_file(string fn)
{
  FileFd f;

  f.Open(fn, FileFd::ReadOnly);

  if(!f.IsOpen())
    {
      _error->Error(_("Cannot open package hierarchy file %s"), fn.c_str());
      return;
    }

  pkgTagFile tagfile(&f);
  pkgTagSection section;

  bool first=true;
  string realm;

  while(tagfile.Step(section))
    {
      unsigned long tmp=0;
      section.FindFlag("Global", tmp, 1);

      if(tmp)
	{
	  if(first)
	    realm=section.FindS("Realm");
	  else
	    {
	      _error->Warning(_("Global block encountered after first record, ignoring"));
	    }

	  first=false;
	}
      else
	{
	  first=false;

	  string pkgname=section.FindS("Package");
	  string groupname=section.FindS("Group");

	  if(pkgname=="" && groupname=="")
	    _error->Warning(_("Bad record encountered (no Package or Group entry), skipping"));
	  else if(pkgname!="" && groupname!="")
	    _error->Warning(_("Bad record encountered (Package=%s, Group=%s), skipping"), pkgname.c_str(), groupname.c_str());
	  else
	    {
	      string parents=section.FindS("Parents");
	      vector<string> parent_list;

	      string::size_type start=0, firstcomma=0;

	      while(1)
		{
		  while(firstcomma<parents.size() && parents[firstcomma]!=',')
		    ++firstcomma;

		  if(firstcomma!=start)
		    {
		      if(firstcomma==parents.size())
			{
			  parent_list.push_back(string(parents, start));
			  break;
			}
		      else
			parent_list.push_back(string(parents, start,
						     firstcomma-start));
		    }
		  else if(firstcomma==parents.size())
		    break;

		  while(firstcomma<parents.size() &&
			(parents[firstcomma]==' ' ||
			 parents[firstcomma]=='\t' ||
			 parents[firstcomma]==','))
		    ++firstcomma;

		  start=firstcomma;
		}

	      // Here we actually insert the package into our map
	      if(pkgname!="")
		{
		  pkgmap::iterator found=pkgs.find(pkgname);

		  if(found!=pkgs.end())
		    for(vector<string>::iterator i=parent_list.begin();
			i!=parent_list.end(); ++i)
		      found->second.parents.insert(*i);

		  else
		    pkgs[pkgname]=item(pkgname, parent_list);
		}
	      else
		{
		  string description=section.FindS("Description");

		  groupmap::iterator found=groups.find(groupname);

		  if(found!=groups.end())
		    {
		      for(vector<string>::iterator i=parent_list.begin();
			  i!=parent_list.end(); ++i)
			if(found->second.parents.find(*i)==found->second.parents.end())
			  found->second.parents.insert(*i);

		      if(description!="" && found->second.description!="")
			_error->Warning(_("Multiple descriptions found for group %s, ignoring one"), groupname.c_str());
		      else if(description!="")
			found->second.description=description;
		    }
		  else
		    groups[groupname]=group(groupname, max_group_id++,
					    parent_list, description);
		}
	    }
	}
    }
}

void pkg_hier::realize(string grp, void *init_parent_data,
		       hierarchy_realizer *realizer)
{
  groupmap::iterator found=groups.find(grp);

  if(found!=groups.end())
    {
      // Resolve all dangling references (and bail out if we have loops?)

      // First, resolve references between groups.
      for(groupmap::iterator i=groups.begin(); i!=groups.end(); ++i)
	i->second.children.clear();

      for(groupmap::iterator i=groups.begin(); i!=groups.end(); ++i)
	for(std::set<string>::iterator j=i->second.parents.begin();
	    j!=i->second.parents.end();
	    ++j)
	  {
	    groupmap::iterator target=groups.find(*j);

	    if(target!=groups.end())
	      target->second.children.push_back(&i->second);
	  }

      // Now, resolve references from packages to groups.
      for(pkgmap::iterator i=pkgs.begin(); i!=pkgs.end(); ++i)
	for(std::set<string>::iterator j=i->second.parents.begin();
	    j!=i->second.parents.end();
	    ++j)
	  {
	    groupmap::iterator target=groups.find(*j);

	    if(target!=groups.end())
	      target->second.children.push_back(&i->second);
	  }

      // Now, visit stuff.
      found->second.realize_me(this, init_parent_data, realizer);
    }
}

void pkg_hier::clear()
{
  pkgs.clear();
  groups.clear();

  max_group_id=0;
}

pkg_hier::~pkg_hier()
{
}
