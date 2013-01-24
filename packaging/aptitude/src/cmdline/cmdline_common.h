// cmdline_common.h                               -*-c++-*-
//
//   Copyright 2004 Daniel Burrows
//

#ifndef CMDLINE_COMMON_H
#define CMDLINE_COMMON_H

#include <apt-pkg/pkgcache.h>

#include <set>
#include <string>
#include <vector>

/** \brief Stuff that doesn't particularly belong anywhere.
 * 
 *  \file cmdline_common.h
 */

typedef std::vector<pkgCache::PkgIterator> pkgvector;
typedef std::vector<pkgCache::PrvIterator> prvvector;
typedef std::set<pkgCache::PkgIterator> pkgset;
typedef std::vector<string> strvector;

enum cmdline_pkgaction_type
  {cmdline_install, cmdline_installauto, cmdline_remove,
   cmdline_purge, cmdline_hold, cmdline_unhold, cmdline_markauto,
   cmdline_unmarkauto, cmdline_forbid_version, cmdline_reinstall,
   cmdline_keep, cmdline_build_depends, cmdline_upgrade};

enum cmdline_version_source {cmdline_version_cand,
			     /** \brief The current version, if any;
			      *  otherwise the candidate version.
			      */
			     cmdline_version_curr_or_cand,
			     cmdline_version_archive,
			     cmdline_version_version};

#endif // CMDLINE_COMMON_H
