// apt_undo_group.h                          -*-c++-*-
//
// Copyright 2004 Daniel Burrows

#ifndef APT_UNDO_GROUP_H
#define APT_UNDO_GROUP_H

#include <generic/util/undo.h>

/** \file apt_undo_group.h
 */

/** An undo group that will call begin_action_group() and
 *  end_action_group() on the apt cache.  The apt cache is accessed
 *  via the global variable apt_cache_file.
 */
class apt_undo_group:public undo_group
{
public:
  void undo();
};

#endif // APT_UNDO_GROUP_H
