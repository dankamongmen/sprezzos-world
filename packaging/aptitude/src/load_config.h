// load_config.h
//
//  Copyright 2000 Daniel Burrows
//

#ifndef LOAD_CONFIG_H
#define LOAD_CONFIG_H

#include <cwidget/config/keybindings.h>

/** \brief Provides routines to load in the vscreen frontend's configuration.
 * 
 *  \file load_config.h
 */

void load_styles(std::string group, bool usetheme);
//  Loads in color definitions from the given group.

void load_bindings(std::string group, cwidget::config::keybindings *toload, bool usetheme);
//  Loads values from the given APT configuration group into the given
// keybindings.

#endif
