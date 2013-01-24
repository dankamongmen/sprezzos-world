// undo.cc
//
//  Copyright 2000 Daniel Burrows
//
//  Implementations of stuff for undo.h

#include "undo.h"

#include <cwidget/generic/util/eassert.h>

using namespace std;

void undo_list::undo()
{
  if(items.size()>floors.back())
    {
      items.front()->undo();
      delete items.front();
      items.pop_front();
    }

  changed();
}

void undo_list::clear_items()
{
  for(list<undoable *>::iterator i=items.begin(); i!=items.end(); i++)
    delete(*i);

  // FIXME: these tests on the size shouldn't be necessary..I'm trying to
  //       debug a weird problem.
  if(items.size()!=0)
    items.erase(items.begin(), items.end());
  if(floors.size()!=0)
    floors.erase(floors.begin(), floors.end());

  floors.push_back(0);

  eassert(items.size()==0);
  eassert(floors.size()==1);

  changed();
}

void undo_list::collapse_to(unsigned int prev_size)
{
  if(items.size()>prev_size && prev_size>=0)
    {
      eassert(prev_size>=floors.back());

      undo_group *new_item=new undo_group;

      while(items.size()>prev_size)
	{
	  new_item->add_item(items.front());
	  items.pop_front();
	}

      items.push_front(new_item);
    }

  changed();
}

void undo_list::revert_to(unsigned int prev_size)
{
  if(prev_size>=0)
    {
      eassert(prev_size>=floors.back());

      while(items.size()>prev_size)
	undo();
    }

  changed();
}

void undo_list::push_floor(unsigned int floor)
{
  eassert(floor<=items.size());
  floors.push_back(floor);
}

void undo_list::pop_floor()
{
  floors.pop_back();
}
