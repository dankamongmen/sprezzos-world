#ifndef _gimp_call_h_
#define _gimp_call_h_

/* Zinc (Zinc Is Not Cineon)
 * An image processing program
 * Copyright (C) 2000 - 2005 David Hodson hodsond@acm.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <string>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

//
// Encapsulates gimp calls
//
class GimpCall {

public:

  GimpCall(std::string fn);
  ~GimpCall();

  GimpCall& param(bool);
  GimpCall& param(int);
  GimpCall& param(float);
  GimpCall& param(const std::string&);
  GimpCall& imageParam(int);
  GimpCall& drawableParam(int);
  GimpCall& layerParam(int);
  GimpCall& displayParam(int);
  //  GimpCall& colourParam(float, float, float);
  GimpCall& param(const GimpParam&);

  const GimpCall& execute(bool&);
  const GimpCall& intReturn(int&) const;
  const GimpCall& intArrayReturn(int*&) const;
  const GimpCall& displayReturn(int&) const;
  const GimpCall& imageReturn(int&) const;
  const GimpCall& layerReturn(int&) const;

  static void dumpParams(GimpParam* params, int numParams);

protected:

  std::string _fnName;

  GimpParam* _returns;
  int _nReturns;
  GimpParam* _params;
  int _nParams;
  int _maxParams;
  int _returnNum;

};

#endif
