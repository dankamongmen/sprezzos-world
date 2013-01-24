// pkg_hier_dump.cc
//
//  A test program for the hierarchy stuff.

#include "pkg_hier.h"

#include <stdio.h>
#include <string.h>

#include <apt-pkg/error.h>

#include <algorithm>

using namespace std;

class text_hier_realizer:public pkg_hier::hierarchy_realizer
{
public:
  struct node
  {
    string text;
    int depth;

    node(string _text, int _depth):text(_text), depth(_depth) {}

    virtual void print(bool do_sort)
    {
      for(int lvl=depth; lvl>0; --lvl)
	printf("|   ");

      printf("| %s\n", text.c_str());
    }

    virtual ~node() {}
  };

  struct node_cmp
  {
  public:
    bool operator()(node *a, node *b)
    {
      return a->text<b->text;
    }
  };

  struct group_node:public node
  {
    vector<node *> children;
    string description;

    group_node(string _text, string _description, int _depth)
      :node(_text, _depth), description(_description) {}

    virtual void print(bool do_sort)
    {
      for(int lvl=depth; lvl>0; --lvl)
	printf("|   ");

      printf("+-%s: %s\n", text.c_str(), description.c_str());

      if(do_sort)
	sort(children.begin(), children.end(), node_cmp());

      for(vector<node *>::iterator i=children.begin(); i!=children.end(); ++i)
	(*i)->print(do_sort);
    }

    ~group_node()
    {
      for(vector<node *>::iterator i=children.begin(); i!=children.end(); ++i)
	delete *i;
    }
  };
private:
  vector<node *> roots;
  bool verbose;
  bool do_sort;
protected:
  void realize_item(pkg_hier::item *item, void *parent_data)
  {
    group_node *parent=(group_node *) parent_data;

    if(verbose)
      printf("Making %sitem '%s'\n", parent?"":"root ", item->name.c_str());

    if(parent)
      parent->children.push_back(new node(item->name, parent->depth+1));
    else
      roots.push_back(new node(item->name, 0));
  }

  void *realize_group(pkg_hier::group *group, void *parent_data)
  {
    group_node *parent=(group_node *) parent_data;

    if(verbose)
      printf("Making %sgroup '%s'\n", parent?"":"root ", group->name.c_str());

    group_node *rval=new group_node(group->name, group->description,
				    parent?parent->depth+1:0);

    if(parent)
      parent->children.push_back(rval);
    else
      roots.push_back(rval);

    return rval;
  }

public:
  text_hier_realizer(pkg_hier *_hier, bool _verbose, bool _do_sort)
    :hierarchy_realizer(_hier), verbose(_verbose), do_sort(_do_sort) {}

  void print()
  {
    if(do_sort)
      sort(roots.begin(), roots.end(), node_cmp());

    for(vector<node *>::iterator i=roots.begin(); i!=roots.end(); ++i)
      (*i)->print(do_sort);
  }

  virtual ~text_hier_realizer()
  {
    for(vector<node *>::iterator i=roots.begin(); i!=roots.end(); ++i)
      delete *i;
  }
};

int main(int argc, char *argv[])
{
  pkg_hier hier;
  bool verbose=false;
  bool do_sort=false;

  if(argc<2)
    fprintf(stderr, "pkg_hier_dump: no arguments!\n");
  else if(!strcmp(argv[1], "-b"))
    {
      vector<string> items;

      if(argc<4)
	fprintf(stderr, "Not enough arguments; need at least 3\n");

      hier.input_file(argv[2]);

      for(int i=3; i<argc; ++i)
	{
	  // -f file for more input files
	  if(!strcmp(argv[i], "-f") && i<argc-1)
	    {
	      ++i;
	      hier.input_file(argv[i]);
	    }
	  else if(!strcmp(argv[i], "-v"))
	    verbose=true;
	  else if(!strcmp(argv[i], "-s"))
	    do_sort=true;
	  else
	    items.push_back(argv[i]);
	}

      text_hier_realizer realizer(&hier, verbose, do_sort);

      for(vector<string>::size_type i=0; i<items.size(); ++i)
	hier.realize_item_up(items[i], &realizer);

      realizer.print();
    }
  else
    {
      vector<string> roots;

      if(argc<2)
	{
	  fprintf(stderr, "Not enough arguments; need at least 1\n");
	  exit(-1);
	}

      hier.input_file(argv[1]);

      for(int i=2; i<argc; ++i)
	{
	  if(!strcmp(argv[i], "-f") && i<argc-1)
	    {
	      hier.input_file(argv[i+1]);
	      ++i;
	    }
	  else if(!strcmp(argv[i], "-v"))
	    verbose=true;
	  else if(!strcmp(argv[i], "-s"))
	    do_sort=true;
	  else
	    roots.push_back(argv[i]);
	}

      text_hier_realizer realizer(&hier, verbose, do_sort);

      if(!roots.empty())
	for(vector<string>::size_type i=0; i<roots.size(); ++i)
	  hier.realize(roots[i], NULL, &realizer);
      else
	for(pkg_hier::groupmap::iterator i=hier.groups.begin();
	    i!=hier.groups.end(); ++i)
	  if(i->second.parents.empty())
	    hier.realize(i->second.name, NULL, &realizer);

      realizer.print();
    }

  _error->DumpErrors();
}
