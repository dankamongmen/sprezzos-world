// load_pkgview.h          -*-c++-*-
//


#ifndef LOAD_PKGVIEW_H
#define LOAD_PKGVIEW_H

#include "pkg_view.h"

/** \brief Lets a package-view layout be loaded from a config file
 *
 * 
 *  Lets a package-view layout be loaded from a config file.  It's not as
 *  nice as it could be, but it's enough for real hardcore people like me to
 *  customize things.
 * 
 *  \file load_pkgview.h
 */

// The caller should delete the returned list, of course.
std::list<package_view_item> *load_pkgview(std::string cfggroup);

#endif
