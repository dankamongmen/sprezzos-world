// undo.h       -*-c++-*-
//
//  Copyright 2000 Daniel Burrows
//
//  

#ifndef UNDO_H
#define UNDO_H

#include <list>

#include <sigc++/signal.h>

/** \brief A generic structure for undo information.
 * 
 *  \file undo.h
 */

class undoable
// represents an action which can be undone.
{
public:
  virtual void undo()=0;
  // Undoes the action (doh! :) )

  virtual ~undoable() {}
};

class undo_group:public undoable
//  Groups several undo actions into a single shot -- does them from first to
// last..
{
  std::list<undoable *> items;
public:
  virtual void undo()
  {
    for(std::list<undoable *>::iterator i=items.begin(); i!=items.end(); i++)
      (*i)->undo();
  }

  void add_item(undoable *item)
  {
    items.push_front(item);
  }

  bool empty() {return items.empty();}

  virtual ~undo_group()
  {
    for(std::list<undoable *>::iterator i=items.begin(); i!=items.end(); i++)
      delete *i;
  }
};

class undo_list
//  A very very simple way to allow things to be undone.  It takes 
// responsibility for deleting pointers that it's given.
{
  std::list<undoable *> items;
  std::list<unsigned int> floors;
public:
  undo_list() {floors.push_back(0);}

  void undo();

  void add_item(undoable *item)
    // Inserts an item into the stack of undoable actions
  {
    items.push_front(item);
    changed();
  }

  void clear_items();

  unsigned int size() {return items.size();}
  void collapse_to(unsigned int prev_size);
  void revert_to(unsigned int prev_size);
  // These can be used to place a 'mark' in the undo stack at a given point,
  // then either collapse actions after that point to a single action
  // (collapse_to) or revert them altogether (revert_to)

  void push_floor(unsigned int floor);
  void pop_floor();

  /** \brief Emitted after a new entry is added to the list and after
   *  an entry is removed from the list.
   */
  sigc::signal0<void> changed;

  virtual ~undo_list()
  {
    for(std::list<undoable *>::iterator i=items.begin(); i!=items.end(); i++)
      delete (*i);
  }
};

#endif
