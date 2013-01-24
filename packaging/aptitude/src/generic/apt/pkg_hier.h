// pkg_hier.h               -*-c++-*-
//
//  Copyright 2001 Daniel Burrows
//


#ifndef PKG_HIER_H
#define PKG_HIER_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <map>
#include <set>

#include <string>
#include <vector>

/** \brief This file allows a "package hierarchy" as described in README.hier to
 *  be loaded and constructed.
 *
 * 
 *  This file allows a "package hierarchy" as described in README.hier to
 *  be loaded and constructed.  Any number of files can be read into the
 *  database; then, the database is "realized" as a tree using two virtual
 *  callback routines.  (you should override them in a subclass to get
 *  any specific behavior you need)
 *  
 *  Important note: once you start "realizing" the database, adding new
 *  nodes via input_file results in undefined behavior.
 *  
 *  Second important note: Mixing top-down and bottom-up realization results
 *  in undefined behavior.
 *  
 *  With the bottom-up realization, the caller may manually set a "seen"
 *  flag on a group to make it appear to be a "root".  (the caller should
 *  also set the node_data values if this is doine)  Also, for root nodes,
 *  realize_* will be called with a NULL parent_data argument.
 * 
 *  \file pkg_hier.h
 */

class pkg_hier
{
public:
  class hierarchy_realizer;

  struct group;

  struct item
  {
    std::string name;
    std::set<std::string> parents;

    // HACK: Used to build the hierarchy top-down.  Calls an appropriate
    // routine in the given hierarchy class.  parent_data is an opaque
    // value which stores data used to build the UI representation of
    // the tree.  It is associated with the parent of this node.
    virtual void realize_me(pkg_hier *hier, void *parent_data,
			    pkg_hier::hierarchy_realizer *realizer);

    item(std::string _name, std::vector<std::string> &_parents)
      :name(_name)
    {
      for(std::vector<std::string>::iterator i=_parents.begin();
	  i!=_parents.end();
	  ++i)
	parents.insert(*i);
    }

    item(std::string _name)
      :name(_name)
    {
    }

    item() {}

    virtual ~item() {}
  };

  struct group:public item
  {
    std::string description;

    // Stores this group's integer ID.
    int id;

    // Used in the final stage of building the hierarchy:
    std::vector<item *> children;

    // This structure stores dynamic information about the group which is
    // used while building a hierarchy.  The realizer stores one of these
    // for each group.
    struct build_info
    {
      // Similarly -- used to avoid cycles
      bool active;

      // Similarly -- allows values to be cached.  (there may be multiple
      // values here if there are multiple parents)
      std::vector<void *> node_data;

      // This is true iff node_data has already been calculated.
      bool seen;

      build_info():active(false), seen(false) {}
    };

    void realize_me(pkg_hier *hier, void *parent_data,
			    pkg_hier::hierarchy_realizer *realizer);

    group(std::string _name, int _id, std::vector<std::string> &_parents,
	  std::string _description)
      :item(_name, _parents), description(_description), id(_id)
    {
    }

    group():item(), id(0)
    {
    }
  };

  friend struct item;
  friend struct group;

  // Used so that group numbers are contiguous and unique
  int max_group_id;

  typedef std::map<std::string, item> pkgmap;
  typedef std::map<std::string, group> groupmap;

  pkgmap pkgs;
  groupmap groups;

  class hierarchy_realizer
  {
  protected:
    pkg_hier::group::build_info *groupinfo;
    pkg_hier *hier;

    void reset_groupinfo()
      // To be used if, eg, the realization needs to be performed again
      // with the same realizer.
    {
      delete[] groupinfo;
      groupinfo=new pkg_hier::group::build_info[hier->groups.size()];
    }
  public:
    hierarchy_realizer(pkg_hier *_hier):hier(_hier)
    {
      groupinfo=new pkg_hier::group::build_info[hier->groups.size()];
    }

    pkg_hier::group::build_info *get_build_info(int group_num)
    {
      return groupinfo+group_num;
    }

    // These routines should construct any UI state associated with the
    // given item or group.  realize_group returns the value which will
    // be used as parent_data for that group's children.
    virtual void realize_item(item *item, void *parent_data)=0;
    virtual void *realize_group(group *group, void *parent_data)=0;

    virtual ~hierarchy_realizer()
    {
      delete[] groupinfo;
    }
  };

private:
  // HACK: I don't like Visitor setups, but I'm in a hurry and it'll
  // be good enough..
  void visit_item(item *item, void *parent_data, hierarchy_realizer *realizer);
  void visit_group(group *group, void *parent_data, hierarchy_realizer *realizer);

  // Helper routines for bottom-up realization
  void realize_group_up(group *group, hierarchy_realizer *realizer);
  void realize_item_up(item *item, hierarchy_realizer *realizer);
public:
  pkg_hier():max_group_id(0) {}

  // Reads the given file and adds it to our database.
  void input_file(std::string fn);

  // Generates a top-down postfix hierarchy beginning at the given group.
  void realize(std::string grp, void *init_parent_data,
	       hierarchy_realizer *_realizer);

  // Generates part of a bottom-up hierarchy beginning at the given item.
  // May be called successively to add more items to the hierarchy.
  //
  // Returns false if the item is not in the database at all.
  bool realize_item_up(std::string item, hierarchy_realizer *_realizer);

  // Clears all information stored in this object (invalidates group * and
  // item * pointers!)
  void clear();

  virtual ~pkg_hier();
};

#endif
