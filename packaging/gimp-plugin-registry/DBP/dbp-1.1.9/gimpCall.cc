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

#include "gimpCall.h"

#include <iostream>
using namespace std;

GimpCall::GimpCall(string fn):
  _fnName(fn),
  _returns(0),
  _nReturns(0) {

  _maxParams = 20;
  _params = new GimpParam[_maxParams];
  _nParams = 0;
}

GimpCall::~GimpCall() {
  gimp_destroy_params(_returns, _nReturns);
  delete[] _params;
}

GimpCall&
GimpCall::param(bool val) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = GIMP_PDB_INT32;
    _params[_nParams].data.d_int32 = val ? 1 : 0;
    _nParams++;
  }
  return *this;
}

GimpCall&
GimpCall::param(int val) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = GIMP_PDB_INT32;
    _params[_nParams].data.d_int32 = val;
    _nParams++;
  }
  return *this;
}

GimpCall&
GimpCall::param(float val) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = GIMP_PDB_FLOAT;
    _params[_nParams].data.d_float = val;
    _nParams++;
  }
  return *this;
}

GimpCall&
GimpCall::param(const string& val) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = GIMP_PDB_STRING;
    _params[_nParams].data.d_string = const_cast<gchar*>(val.c_str());
    _nParams++;
  }
  return *this;
}

GimpCall&
GimpCall::imageParam(int val) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = GIMP_PDB_IMAGE;
    _params[_nParams].data.d_image = val;
    _nParams++;
  }
  return *this;
}

GimpCall&
GimpCall::drawableParam(int val) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = GIMP_PDB_DRAWABLE;
    _params[_nParams].data.d_drawable = val;
    _nParams++;
  }
  return *this;
}

GimpCall&
GimpCall::layerParam(int val) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = GIMP_PDB_LAYER;
    _params[_nParams].data.d_layer = val;
    _nParams++;
  }
  return *this;
}

GimpCall&
GimpCall::displayParam(int val) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = GIMP_PDB_DISPLAY;
    _params[_nParams].data.d_display = val;
    _nParams++;
  }
  return *this;
}
#if 0
GimpCall&
GimpCall::colourParam(float r, float g, float b) {
  if (_nParams < _maxParams) {
    GimpParamColor colour;
    colour.red = int(255.0 * r);
    colour.green = int(255.0 * g);
    colour.blue = int(255.0 * b);
    _params[_nParams].type = GIMP_PDB_COLOR;
    _params[_nParams].data.d_color = colour;
    _nParams++;
  }
  return *this;
}
#endif
GimpCall&
GimpCall::param(const GimpParam& param) {
  if (_nParams < _maxParams) {
    _params[_nParams].type = param.type;
    _params[_nParams].data = param.data;
    _nParams++;
  }
  return *this;
}

const GimpCall&
GimpCall::execute(bool& success) {

  //cout << "GimpCall " << _fnName << ", params" << endl;
  //GimpCall::dumpParams(_params, _nParams);

  _returns =
    gimp_run_procedure2(const_cast<gchar*>(_fnName.c_str()),
      &_nReturns, _nParams, _params);

  //cout << "returned " << endl;
  //GimpCall::dumpParams(_returns, _nReturns);
  //cout << endl;

  success = _returns &&
    (_nReturns > 0) &&
    (_returns[0].type == GIMP_PDB_STATUS) &&
    (_returns[0].data.d_status == GIMP_PDB_SUCCESS);

  _returnNum = 1;
  return *this;
}

const GimpCall&
GimpCall::intReturn(int& value) const {
  if (_returns &&
      (_nReturns > _returnNum) &&
      (_returns[_returnNum].type == GIMP_PDB_INT32)) {
    value = _returns[_returnNum].data.d_int32;
  }
  ++(const_cast<int&>(_returnNum));
  return *this;
}

const GimpCall&
GimpCall::intArrayReturn(int*& value) const {
  if (_returns &&
      (_nReturns > _returnNum) &&
      (_returns[_returnNum].type == GIMP_PDB_INT32ARRAY)) {
    value = _returns[_returnNum].data.d_int32array;
  }
  ++(const_cast<int&>(_returnNum));
  return *this;
}

const GimpCall&
GimpCall::displayReturn(int& value) const {
  if (_returns &&
      (_nReturns > _returnNum) &&
      (_returns[_returnNum].type == GIMP_PDB_DISPLAY)) {
    value = _returns[_returnNum].data.d_display;
  }
  ++(const_cast<int&>(_returnNum));
  return *this;
}

const GimpCall&
GimpCall::imageReturn(int& value) const {
  if (_returns &&
      (_nReturns > _returnNum) &&
      (_returns[_returnNum].type == GIMP_PDB_IMAGE)) {
    value = _returns[_returnNum].data.d_image;
  }
  ++(const_cast<int&>(_returnNum));
  return *this;
}

const GimpCall&
GimpCall::layerReturn(int& value) const {
  if (_returns &&
      (_nReturns > _returnNum) &&
      (_returns[_returnNum].type == GIMP_PDB_LAYER)) {
    value = _returns[_returnNum].data.d_layer;
  }
  ++(const_cast<int&>(_returnNum));
  return *this;
}

void
GimpCall::dumpParams(GimpParam* params, int nParams) {

  for (int i = 0; i < nParams; ++i) {
    cout << i << ": ";
    switch (params[i].type) {
    case GIMP_PDB_INT32:
      cout << "int32 " << params[i].data.d_int32 << endl;
      break;
    case GIMP_PDB_INT16:
      cout << "int16 " << (int)params[i].data.d_int16 << endl;
      break;
    case GIMP_PDB_INT8:
      cout << "int8  " << (int)params[i].data.d_int8 << endl;
      break;
    case GIMP_PDB_FLOAT:
      cout << "float " << params[i].data.d_float << endl;
      break;
    case GIMP_PDB_IMAGE:
      cout << "image " << params[i].data.d_int32 << endl;
      break;
    case GIMP_PDB_DRAWABLE:
      cout << "drawb " << params[i].data.d_int32 << endl;
      break;
    case GIMP_PDB_INT32ARRAY:
      cout << "int [";
      for (int j = 0; j < params[i-1].data.d_int32; ++j) {
        cout << " " << params[i].data.d_int32array[j];
      }
      cout << "]" << endl;
      break;
    case GIMP_PDB_STRING:
      cout << "string " << params[i].data.d_string << endl;
      break;
    default:
      cout << "other " << params[i].data.d_int32 << endl;
      break;
    }
  }
}
