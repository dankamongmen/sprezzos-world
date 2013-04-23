/* DBP (Dave's Batch Processor)
 * A simple batch processor for the GIMP
 * Copyright (C) 2001 - 2007 David Hodson
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

#ifndef _DBP_OP_H_
#define _DBP_OP_H_

#include <string>
#include <list>
#include <vector>
#include "gimpCall.h"

namespace Dbp {

struct FloatParam {
  float _value;
  float _min;
  float _max;
  FloatParam(float init, float minVal, float maxVal, int decimals);
};

struct Location {
  std::string _path;
  std::string _name;
  std::string _extn;

  // construct with full path, dir and filename, or dir, name, and extn
  Location();
  Location(const std::string& path);
  Location(const std::string& dir, const std::string& file);
  Location(const std::string& dir, const std::string& name, const std::string& extn);

  bool operator==(const Location&) const;
  bool operator<(const Location&) const;
  std::string fullPath() const;
};

// An Op processes a drawable
class Op {

public:
  Op();
  virtual ~Op();
  virtual bool execute(
    int& image, int& drawableId, Location& file) = 0;

  bool _enabled;
};

// A Filter is an Op which consists of a single gimp pdb call
// The call is specified in the constructor, which initialises the params
// addParams() adds the parameters to the pdb call
struct Filter: public Dbp::Op {
  Filter(const std::string& gimpFnName);
  virtual bool execute(
    int& image, int& drawableId, Location& file);

protected:
  std::string _gimpFnName;
  virtual void addParams(GimpCall&) = 0;
};

// actually, the file list and the input op should be separate...
//
struct InputOp: public Dbp::Op {
  InputOp();
  virtual bool execute(
    int& image, int& drawableId, Location& file);

  typedef std::list<Location> FileList;
  FileList _files;
};

struct TurnOp: public Dbp::Op {
  TurnOp();
  virtual bool execute(
    int& image, int& drawableId, Location& file);

  enum { TURN_90 = 0, TURN_180 = 1, TURN_270 = 2 };
  int _turn;
};

struct BlurOp: public Dbp::Filter {
  BlurOp();

  float _radius;     /* range 1.0 to 100.0, default 1.0, minStep 0.1 */

protected:
  virtual void addParams(GimpCall&);
};

struct RecolourOp: public Dbp::Op {
  RecolourOp();
  virtual bool execute(
    int& image, int& drawableId, Location& file);

  bool _auto;

  float _brightness; /* range -1.0 to 1.0, default 0.0, minStep 0.001 */
  float _contrast;   /* range -1.0 to 1.0, default 0.0, minStep 0.001 */
  float _saturation; /* 0.0 - 2.0, 1.0, 0.001 */
  float _gamma;      /* range 0.1 to 10.0, default 1.0, minStep 0.01 */

  bool _invert;
  bool _mono;
};

struct ResizeOp: public Dbp::Op {
  ResizeOp();
  virtual bool execute(
    int& image, int& drawableId, Location& file);

  bool _relative;

  bool _keepAspect;
  float _xScale;
  float _yScale;

  int _xSize;
  int _ySize;
  enum FitOptions { FIT_EXACTLY, PAD_TO_FIT, FIT_INSIDE, FIT_OUTSIDE };
  FitOptions _fit;
};

struct CropOp: public Dbp::Op {
  CropOp();

  virtual bool execute(
    int& image, int& drawableId, Location& file);

  int _width;
  int _height;
  int _x;
  int _y;
};

struct SharpenOp: public Dbp::Filter {
  SharpenOp();

  float _radius;
  float _amount;
  float _threshhold;

protected:
  virtual void addParams(GimpCall&);
};

struct RenameOp: public Dbp::Op {
  RenameOp();
  virtual bool execute(
    int& image, int& drawableId, Location& file);

  void modify(Location&) const;
  std::string _dirPath;
  std::string _prefix;
  std::string _postfix;
//  bool _numericRename;

  // this doesn't really belong here, but anyway...
  bool _flatten;
  bool _convertToGreyscale;
  bool _convertToIndexed;
  enum DitherOptions { NO_DITHER = 0, FLOYD_STEINBERG = 1, REDUCED_BLEED = 2, FIXED_DITHER = 3 };
  int _ditherType;
  int _numberOfIndexedColours;
};

struct OpParam {
  std::string _name;
  GimpParam _gimpType;
  enum { Toggle, IntSlider, FloatSlider, Menu, String, None } _guiType;
  // don't bother with derived classes...
  float _minFloat;
  float _maxFloat;
  int _minInt;
  int _maxInt;
  int _numDecimals;
  typedef std::vector<std::pair<int, std::string> > Choice;
  Choice _choices;
  OpParam& choice(int, std::string);
  void choose(int);
  std::string _string;
};

struct OutputFormat {
  OutputFormat(int, std::string, std::string, std::string);
  int _tag;
  std::string _name;
  std::string _fileExtension;
  std::string _functionName;
  std::vector<OpParam> _params;
  void param(std::string, bool);
  void param(std::string, int, int, int);
  void param(std::string, float, float, float, int);
  void param(std::string, std::string);
  OpParam& param(std::string);
};

struct OutputOp: public Dbp::Op {
  OutputOp();
  virtual bool execute(
    int& image, int& drawableId, Location& file);

  std::string outputFileName(Location file);
  bool fileExists(const std::string& fileName);

  std::vector<OutputFormat> _format;
  int _selection;
};

struct DbpData {
  DbpData();
  ~DbpData();

  InputOp _input;
  TurnOp _turn;
  BlurOp _blur;
  RecolourOp _recolour;
  ResizeOp _resize;
  CropOp _crop;
  SharpenOp _sharpen;
  RenameOp _rename;
  OutputOp _output;

  int numExistingOutputFiles(const InputOp::FileList& files);
  // process and output several files
  void start(InputOp::FileList& files);
  // process (but don't output) a single file
  void start(Location file);

  bool done();
  void step();
  // only valid if !done()
  Location current() const;
  float progress() const;

  void setVisible(bool);
  bool visible() const;

private:

  bool _visible;

  void removeDisplay();
  InputOp::FileList _files;
  InputOp::FileList::iterator _current;
  int _fileNum;
  bool _test;
  bool _done;
  int _display;
  int _image;
  int _drawable;
  Location _location;

  enum {
    DoInput, DoTurn, DoBlur, DoRecolour, DoResize, DoCrop,
    DoSharpen, DoRename, DoOutput }
    _stage;
};

} // namespace Dbp

#endif // _DBP_OP_H_
