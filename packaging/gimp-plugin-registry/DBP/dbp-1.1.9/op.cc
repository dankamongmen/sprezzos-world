/* DBP (Dave's Batch Processor)
 * A simple batch processor for the GIMP
 * Copyright (C) 2001 - 2005 David Hodson
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

#include "op.h"

#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

using namespace Dbp;

FloatParam::FloatParam(float init, float minVal, float maxVal, int decimals):
  _value(init), _min(minVal), _max(maxVal) {
}

Location::Location() {
}

Location::Location(const std::string& path):
  _name(path) {
  // is there a directory separator in the path?
  std::string::size_type slash = _name.rfind(G_DIR_SEPARATOR);
  if (slash != std::string::npos) {
    _path = std::string(_name, 0, slash);
    _name.erase(0, slash+1);
  }
  // is there a dot in the file name?
  std::string::size_type dot = _name.rfind('.');
  if (dot != std::string::npos) {
    _extn = std::string(_name, dot+1, std::string::npos);
    _name.erase(dot, std::string::npos);
  }
}

Location::Location(const std::string& path, const std::string& fullname):
  _path(path), _name(fullname) {
  // is there a dot in the file name?
  std::string::size_type dot = _name.rfind('.');
  if (dot != std::string::npos) {
    _extn = std::string(_name, dot+1, std::string::npos);
    _name.erase(dot, std::string::npos);
  }
}

Location::Location(
  const std::string& path, const std::string& name, const std::string& extn):
  _path(path), _name(name), _extn(extn) {
}

bool
Location::operator==(const Location& b) const {
  return (_path == b._path) && (_name == b._name) && (_extn == b._extn);
}

bool
Location::operator<(const Location& b) const {
  return fullPath() < b.fullPath();
}

std::string
Location::fullPath() const {
  std::string fullPath;
  if (! _path.empty()) {
    fullPath += _path + G_DIR_SEPARATOR_S;
  }
  fullPath += _name;
  if (! _extn.empty()) {
    fullPath += "." + _extn;
  }
  return fullPath;
}

Op::Op(): _enabled(false) {
}

Op::~Op() {
}

Filter::Filter(const std::string& gimpFnName):
  _gimpFnName(gimpFnName) {
}

bool
Filter::execute(int& image, int& drawableId, Location& file) {

  if (! _enabled) {
    return true;
  }

  GimpCall call(_gimpFnName);
  call.param(GIMP_RUN_NONINTERACTIVE);
  call.imageParam(image);
  call.drawableParam(drawableId);
  addParams(call);
  bool result;
  call.execute(result);
  return result;  
}

InputOp::InputOp() {
}

bool
InputOp::execute(int& image, int& drawableId, Location& file) {

  // set return values
  image = -1;
  drawableId = -1;

  // load file
  gchar* path = new gchar[file.fullPath().length() + 1];
  strcpy((char*)path, file.fullPath().c_str());
  image = gimp_file_load(GIMP_RUN_NONINTERACTIVE, path, path);
  delete[] path;

  // doesn't really matter if this fails...
  gimp_image_undo_disable(image);

  // can this fail??
  gint numLayers = 0;
  gint* layers = gimp_image_get_layers(image, &numLayers);
  if (numLayers == 0) {
    gimp_image_delete(image);
    image = -1;
  } else if (numLayers > 1) {
    drawableId = gimp_image_merge_visible_layers(image, GIMP_CLIP_TO_IMAGE);
  } else {
    drawableId = layers[0];
  }
  g_free(layers);

  return (image != -1);
}

TurnOp::TurnOp():
  _turn(TURN_90) {
}

bool
TurnOp::execute(int& image, int& drawableId, Location& file) {

  if (! _enabled) {
    return true;
  }

  gimp_image_rotate(image, (GimpRotationType)_turn);
  return true;
}

BlurOp::BlurOp():
  Filter("plug_in_gauss_iir"),
  _radius(1.0) {
}

void
BlurOp::addParams(GimpCall& call) {
  call.param(_radius);
  call.param(TRUE);
  call.param(TRUE);
}

RecolourOp::RecolourOp():
  _auto(false), _brightness(0.0), _contrast(0.0),
  _saturation(1.0), _gamma(1.0), _invert(false), _mono(false) {
}

bool
RecolourOp::execute(int& image, int& drawableId, Location&) {

  if (! _enabled) {
    return true;
  }

  if (_auto) {
    gimp_levels_stretch(drawableId);

  } else {
    if ((_brightness != 0.0) || (_contrast != 0.0)) {
      int brightness = static_cast<int>(_brightness * 127.0);
      int contrast = static_cast<int>(_contrast * 127.0);

      gimp_brightness_contrast(drawableId, brightness, contrast);
    }
  }

  if (_saturation != 1.0) {
    GimpHueRange hueRange = GIMP_ALL_HUES;
    float hueOffset = 0.0;
    float lightness = 0.0;
    float saturation = (_saturation - 1.0) * 100.0;
    gimp_hue_saturation(drawableId, hueRange, hueOffset, lightness, saturation);
  }

  if (_invert) {
    gimp_invert(drawableId);
  }
  if (_mono) {
    gimp_image_convert_grayscale(image);
  }

  return true;
}

ResizeOp::ResizeOp():
  _relative(true),
  _keepAspect(true),
  _xScale(1.0),
  _yScale(1.0),
  _xSize(96),
  _ySize(64),
  _fit(PAD_TO_FIT) {
}

bool
ResizeOp::execute(int& image, int& drawableId, Location&) {

  if (! _enabled) {
    return true;
  }

  bool result = true;
  int newWidth = 0;
  int newHeight = 0;
  int xPos = 0;
  int yPos = 0;

  int oldWidth = gimp_image_width(image);
  int oldHeight = gimp_image_height(image);

  if (_relative) {

    /* just use xScale and yScale */
    /* xScale == yScale if keepAspect set */
    newWidth = static_cast<int>(oldWidth * _xScale);
    newHeight = static_cast<int>(oldHeight * _yScale);
    result = gimp_image_scale(image, newWidth, newHeight);

  } else {

    newWidth = _xSize;
    newHeight = _ySize;

    /* is required x scale greater than y scale? */
    bool moreX = (oldHeight * newWidth > oldWidth * newHeight);

    switch (_fit) {

    case FIT_EXACTLY:

      /* just use xSize and ySize */
      result = gimp_image_scale(image, newWidth, newHeight);
      break;

    case PAD_TO_FIT:
      if (moreX) {
        newWidth = oldWidth * newHeight / oldHeight;
      } else {
        newHeight = oldHeight * newWidth / oldWidth;
      }
      result = gimp_image_scale(image, newWidth, newHeight);
      xPos = (_xSize - newWidth) / 2;
      yPos = (_ySize - newHeight) / 2;
      result = result &&
        gimp_image_resize(image, _xSize, _ySize, xPos, yPos);
      result = result &&
        gimp_layer_resize_to_image_size(drawableId);
      break;

    case FIT_INSIDE:
      if (moreX) {
        newWidth = oldWidth * newHeight / oldHeight;
      } else {
        newHeight = oldHeight * newWidth / oldWidth;
      }
      result = gimp_image_scale(image, newWidth, newHeight);
      break;

    case FIT_OUTSIDE:
      if (moreX) {
        newHeight = oldHeight * newWidth / oldWidth;
      } else {
        newWidth = oldWidth * newHeight / oldHeight;
      }
      result = gimp_image_scale(image, newWidth, newHeight);
      break;
    }
  }
  return result;
}

CropOp::CropOp():
  _width(1), _height(1), _x(0), _y(0) {
}

bool
CropOp::execute(int& image, int& drawableId, Location& file) {

  if (! _enabled) {
    return true;
  }
  GimpCall call("gimp_image_crop");
  call.imageParam(image);
  call.param(_width);
  call.param(_height);
  call.param(_x);
  call.param(_y);
  bool result;
  call.execute(result);
  return result;  
}

SharpenOp::SharpenOp():
  Filter("plug_in_unsharp_mask"),
  _radius(1.0), _amount(1.0), _threshhold(0.0) {
}

void
SharpenOp::addParams(GimpCall& call) {
  call.param(_radius);
  call.param(_amount);
//  call.param(_threshhold);
  // Up to 2.2.9 and 2.3.5 (I think), the threshhold parameter had the wrong type.
  // (It's an int, but was given as float.)
  // Fixed in 2.2.10 and 2.3.6(?)
#if (GIMP_MAJOR_VERSION == 2) && \
    ( (GIMP_MINOR_VERSION == 1) || \
      ( (GIMP_MINOR_VERSION == 2) && (GIMP_MICRO_VERSION <= 9) ) || \
      ( (GIMP_MINOR_VERSION == 3) && (GIMP_MICRO_VERSION <= 5) ) )
  GimpParam p;
  p.data.d_int32 = static_cast<int>(_threshhold);
  p.type = GIMP_PDB_FLOAT;
  call.param(p);
#else
  call.param(static_cast<int>(_threshhold));
#endif
}

RenameOp::RenameOp():
//  _numericRename(false),
  _flatten(false),
  _convertToGreyscale(false),
  _convertToIndexed(false),
  _numberOfIndexedColours(256) {
}

bool
RenameOp::execute(int& image, int& drawable, Location& file) {
  gboolean ok = TRUE;
  if (_flatten) {
    drawable = gimp_image_flatten(image);
  }
  if (_convertToGreyscale) {
    ok = gimp_image_convert_grayscale(image);
  }
  if (_convertToIndexed) {
    gint32 paletteType = 0; // MAKE_PALETTE
    gboolean ditherAlpha = FALSE; // no ?
    gboolean removeUnusedColours = FALSE; // irrelevant with MAKE_PALETTE
    const char* paletteName = "";
    ok = ok && gimp_image_convert_indexed(image,
      (GimpConvertDitherType)_ditherType, (GimpConvertPaletteType)paletteType,
      _numberOfIndexedColours, ditherAlpha,
      removeUnusedColours, paletteName);
  }
  modify(file);
  return (ok == TRUE);
}

void
RenameOp::modify(Location& file) const {
  if (! _dirPath.empty()) {
    file._path = _dirPath;
  }
//  if (_numericRename) {
//    stringstream name;
//    name << _prefix << setw(4) << frame << _postfix;
//    file._name = name.str();
//  } else {
    file._name = _prefix + file._name + _postfix;
//  }
}

OutputFormat::OutputFormat(int tag, std::string name, std::string extn, std::string fn):
  _tag(tag),
  _name(name),
  _fileExtension(extn),
  _functionName(fn) {
}

void
OutputFormat::param(std::string name, bool init) {
  OpParam par;
  par._name = name;
  par._gimpType.type = GIMP_PDB_INT32;
  par._gimpType.data.d_int32 = init ? 1 : 0;
  par._guiType = OpParam::Toggle;
  _params.push_back(par);
}

void
OutputFormat::param(std::string name, int init, int minVal, int maxVal) {
  OpParam par;
  par._name = name;
  par._gimpType.type = GIMP_PDB_INT32;
  par._gimpType.data.d_int32 = init;
  par._guiType = OpParam::IntSlider;
  par._minInt = minVal;
  par._maxInt = maxVal;
  par._numDecimals = 0;
  _params.push_back(par);
}

void
OutputFormat::param(std::string name, float init, float minVal, float maxVal, int decs) {
  OpParam par;
  par._name = name;
  par._gimpType.type = GIMP_PDB_FLOAT;
  par._gimpType.data.d_float = init;
  par._guiType = OpParam::FloatSlider;
  par._minFloat = minVal;
  par._maxFloat = maxVal;
  par._numDecimals = decs;
  _params.push_back(par);
}

void
OutputFormat::param(std::string name, std::string init) {
  OpParam par;
  par._name = name;
  par._gimpType.type = GIMP_PDB_STRING;
  // messy... and possibly broken
  par._string = init;
  // lots of assumpions here!!!!
  par._gimpType.data.d_string = (gchar*)(par._string.c_str());
  par._guiType = OpParam::String;
  _params.push_back(par);
}

OpParam&
OutputFormat::param(std::string name) {
  OpParam par;
  par._name = name;
  par._gimpType.type = GIMP_PDB_INT32;
  par._gimpType.data.d_int32 = 0;
  par._guiType = OpParam::Menu;
  _params.push_back(par);
  return _params.back();
}

OpParam&
OpParam::choice(int value, std::string name) {
  _choices.push_back(make_pair(value, name));
  return *this;
}

void
OpParam::choose(int val) {
  _gimpType.data.d_int32 = val;
}

OutputOp::OutputOp():
  _selection(0) {

  int tag = 0;

  OutputFormat bmp(tag, "BMP", "bmp", "file_bmp_save");
  _format.push_back(bmp);
  ++tag;

  OutputFormat cineon(tag, "Cineon", "cin", "file_cineon_save");
  cineon.param("Gamma", 1.0, 0.1, 10.0, 2);
  cineon.param("BlackPoint", 95, 0, 1023);
  cineon.param("WhitePoint", 685, 0, 1023);
  _format.push_back(cineon);
  ++tag;

  OutputFormat dpx(tag, "DPX", "dpx", "file_dpx_save");
  dpx.param("Gamma", 1.0, 0.1, 10.0, 2);
  dpx.param("BlackPoint", 95, 0, 1023);
  dpx.param("WhitePoint", 685, 0, 1023);
  _format.push_back(dpx);
  ++tag;

  // need to convert to indexed...
  OutputFormat gif(tag, "GIF", "gif", "file_gif_save");
  gif.param("Interlace", false);
  gif.param("Loop", false);
  gif.param("Delay", 10, 0, 1000); // millisecs (?)
  gif.param("Disposal")
    .choice(0, "Don't Care")
    .choice(1, "Combine")
    .choice(2, "Replace")
    .choose(0); // enum DontCare, Combine, Replace
  _format.push_back(gif);
  ++tag;

  OutputFormat jpg(tag, "JPG", "jpg", "file_jpeg_save");
  jpg.param("Quality", 0.75, 0.0, 1.0, 2);
  jpg.param("Smoothing", 0.0, 0.0, 1.0, 2);
  jpg.param("Optimise", true);
  jpg.param("Progressive", false);
  jpg.param("Comment", std::string("DBP")); // string...
  jpg.param("Sampling")
    .choice(0, "2x2")
    .choice(1, "2x1")
    .choice(2, "1x1")
    .choose(0); // enum 2x2, 2x1, 1x1
  jpg.param("Baseline", true);
  jpg.param("Restarts", 0, 0, 64);
  jpg.param("Algorithm")
    .choice(0, "FastInt")
    .choice(1, "Integer")
    .choice(2, "Float")
    .choose(1); // enum FastInt, Integer, Float
  _format.push_back(jpg);
  ++tag;

  OutputFormat miff(tag, "MIFF", "miff", "file_miff_save");
  _format.push_back(miff);
  ++tag;

  OutputFormat pat(tag, "PAT", "pat", "file_pat_save");
  pat.param("Comment", std::string("DBP")); // string...
  _format.push_back(pat);
  ++tag;

  OutputFormat png(tag, "PNG", "png", "file_png_save");
  png.param("Interlacing", true);
  png.param("Deflate", 9, 0, 9);
  png.param("Save Background", true);
  png.param("Save Gamma", true);
  png.param("Save Offset", true);
  png.param("Save Time", true);
  png.param("Save Resolution", true);
  _format.push_back(png);
  ++tag;

  OutputFormat pnm(tag, "PNM", "pnm", "file_pnm_save");
  pnm.param("Raw", true);
  _format.push_back(pnm);
  ++tag;

  OutputFormat tga(tag, "TGA", "tga", "file_tga_save");
  tga.param("RLE", true);
  tga.param("Bottom-Left Origin", true);
  _format.push_back(tga);
  ++tag;

  OutputFormat tiff(tag, "TIFF", "tif", "file_tiff_save");
  tiff.param("Compression")
    .choice(0, "None")
    .choice(1, "LZW")
    .choice(2, "PackBits")
    .choice(3, "Deflate")
    .choice(4, "Jpeg")
    .choose(0); // enum None, LZW, PackBits, Deflate, Jpeg
  _format.push_back(tiff);
  ++tag;

  OutputFormat xcf(tag, "XCF", "xcf", "gimp_xcf_save");
  _format.push_back(xcf);
  ++tag;
}

bool
OutputOp::execute(int& image, int& drawableId, Location& file) {

  // don't overwrite files!
  // this is not a bug, it's a feature.

  std::string outputPath = outputFileName(file);
  if (fileExists(outputPath)) {
    return false;
  }

  OutputFormat& format = _format[_selection];
  GimpCall call(format._functionName);
  call.param(GIMP_RUN_NONINTERACTIVE);
  call.imageParam(image);
  call.drawableParam(drawableId);
  call.param(outputPath);
  call.param(outputPath);

  int numParams = format._params.size();
  for (int i = 0; i < numParams; ++i) {
    OpParam& param = format._params[i];
    call.param(param._gimpType);
  }

  bool result;
  call.execute(result);
  return result;
}

std::string
OutputOp::outputFileName(Location file) {
  file._extn = _format[_selection]._fileExtension;
  return file.fullPath();
}

bool
OutputOp::fileExists(const std::string& fileName) {
  struct stat foo;
  return (stat(fileName.c_str(), &foo) == 0);
}

DbpData::DbpData():
  _visible(false),
  _current(_files.end()),
  _fileNum(0),
  _test(false),
  _done(false),
  _display(-1),
  _image(-1),
  _drawable(-1) {
}

DbpData::~DbpData() {
  removeDisplay();
}

void
DbpData::removeDisplay() {
  if (_display != -1) {
    gimp_display_delete(_display);
    _display = -1;
  }
  if (_image != -1) {
    gimp_image_delete(_image);
    _image = -1;
  }
}

int
DbpData::numExistingOutputFiles(const InputOp::FileList& files) {
  int n = 0;
  InputOp::FileList::const_iterator iter = files.begin();
  while (iter != files.end()) {
    Location file = *iter;
    _rename.modify(file);
    if (_output.fileExists(_output.outputFileName(file))) {
      ++n;
    }
    ++iter;
  }
  return n;
}

void
DbpData::start(InputOp::FileList& files) {
  _done = false;
  _files = files;
  _current = _files.begin();
  _fileNum = 0;
  _stage = DoInput;
  _test = false;
  if (_current == _files.end()) {
    _done = true;
  } else {
    _location = *_current;
  }
}

void
DbpData::start(Location file) {
  _done = false;
  _files.clear();
  _current = _files.begin();
  _fileNum = 0;
  _stage = DoInput;
  _test = true;
  _location = file;
}

bool
DbpData::done() {
  return _done;
}

void
DbpData::step() {
  if (done()) {
    return;
  }
  bool ok = true;
  switch (_stage) {
  case DoInput:
    {
      int oldImage = _image;
      ++_fileNum;
      ok = _input.execute(_image, _drawable, _location);
      if (ok && _visible) {
        // rename, to protect original image file
        gimp_image_set_filename(_image, "DBP Internal - LOOK, DON'T TOUCH");
        // show it
        if (_display == -1) {
          _display = gimp_display_new(_image);
        } else {
          ok = gimp_displays_reconnect(oldImage, _image);
        }
      } else {
        if (_display != -1) {
          gimp_display_delete(_display);
          _display = -1;
        }
      }
      if (oldImage != -1) {
        gimp_image_delete(oldImage);
      }
    }
    _stage = DoTurn;
    break;
  case DoTurn:
    ok = _turn.execute(_image, _drawable, _location);
    _stage = DoBlur;
    break;
  case DoBlur:
    ok = _blur.execute(_image, _drawable, _location);
    _stage = DoRecolour;
    break;
  case DoRecolour:
    ok = _recolour.execute(_image, _drawable, _location);
    _stage = DoResize;
    break;
  case DoResize:
    ok = _resize.execute(_image, _drawable, _location);
    _stage = DoCrop;
    break;
  case DoCrop:
    ok = _crop.execute(_image, _drawable, _location);
    _stage = DoSharpen;
    break;
  case DoSharpen:
    ok = _sharpen.execute(_image, _drawable, _location);
    _stage = DoRename;
    break;
  case DoRename:
    ok = _rename.execute(_image, _drawable, _location);
    _stage = DoOutput;
    if (_test) {
      _done = true;
    }
    break;
  case DoOutput:
    ok = _output.execute(_image, _drawable, _location);
    _stage = DoInput;
    ++_current;
    if (_current == _files.end()) {
      _done = true;
    } else {
      _location = *_current;
    }
    break;
  }
  // don't want Gimp to think the image needs saving!
  gimp_image_clean_all(_image);
  // make sure everything's on screen
  gimp_displays_flush();

  if (!ok) {
    // stop if something goes wrong
    // need error reporting...
    _done = true;
  }
}

Location
DbpData::current() const {
  Location loc;
  if (! _done) {
    loc = _location;
  }
  return loc;
}

float
DbpData::progress() const {
  if (_files.empty()) {
    return 0.0;
  }
  return (float)_fileNum / _files.size();
}

void
DbpData::setVisible(bool value) {
  _visible = value;
  if (! _visible) {
    removeDisplay();
  }
}
