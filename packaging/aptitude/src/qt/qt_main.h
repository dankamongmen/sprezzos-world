/** \file qt_main.h */   // -*-c++-*-
//
//  Copyright 1999-2010 Daniel Burrows
//  Copyright 2008-2009 Obey Arthur Liu
//  Copyright (C) 2010 Piotr Galiszewski
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#ifndef APTITUDE_QT_MAIN_H
#define APTITUDE_QT_MAIN_H

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      /** \brief Run the Qt GUI frontend to aptitude.
       *
       *  \return \b true if the Qt GUI was able to start at all.
       */
      bool main(int argc, char **argv);
    }
  }
}

#endif // APTITUDE_QT_MAIN_H
