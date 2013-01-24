// load_sortpolicy.h
//
//  Copyright 2001 Daniel Burrows

#ifndef LOAD_SORTPOLICY_H
#define LOAD_SORTPOLICY_H

#include <string>

/** \file load_sortpolicy.h
 */

class pkg_sortpolicy;

pkg_sortpolicy *parse_sortpolicy(std::string s);

#endif
