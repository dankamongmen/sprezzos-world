// apt_undo_group.cc

#include "apt_undo_group.h"

#include "apt.h"

void apt_undo_group::undo()
{
  aptitudeDepCache::action_group group(*apt_cache_file, NULL);

  undo_group::undo();
}
