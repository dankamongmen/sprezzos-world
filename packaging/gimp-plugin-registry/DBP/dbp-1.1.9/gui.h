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

#ifndef _DBP_GUI_H_
#define _DBP_GUI_H_

#include <gtk/gtk.h>
#include <string>

#include "op.h"

namespace Dbp {


// used to lay out rows of labelled controls
//
class ControlLayout {
public:
  ControlLayout();
  GtkWidget* widget() const;
  void add(const std::string&, GtkWidget*, GtkWidget* = 0);

private:

  GtkWidget* _widget;
  static const int _maxRows;
  int _rows;
};


// typesafe, member function callbacks from gtk
//
struct GtkCBBase {
  GtkCBBase(GtkObject*, std::string);
  virtual ~GtkCBBase();
  virtual void fn() = 0;
  static void callback(GtkObject*, GtkCBBase*);
};

template <class Owner>
class GtkCB: public GtkCBBase {
public:
  typedef void (Owner::*Fn)();
  GtkCB(GtkObject*, std::string, Owner*, Fn fn);
  virtual void fn();
private:
  Owner* _owner;
  Fn _fn;
};

template <class Owner>
GtkCB<Owner>::GtkCB(GtkObject* object, std::string name, Owner* owner, Fn fn):
  GtkCBBase(object, name),
  _owner(owner),
  _fn(fn) {
}

template <class Owner>
void
GtkCB<Owner>::fn() {
  (_owner->*_fn)();
}

template <class Owner>
GtkCBBase*
gtkSignal(GtkObject* object, const char* name, void (Owner::*fn)(), Owner* owner) {
  return new GtkCB<Owner>(object, name, owner, fn);
}
template <class Owner>
GtkCBBase*
gtkSignal(GtkWidget* object, const char* name, void (Owner::*fn)(), Owner* owner) {
  return new GtkCB<Owner>(GTK_OBJECT(object), name, owner, fn);
}

// common base class for gui components
//
class Gui {

public:

  Gui();
  virtual ~Gui();
  GtkWidget* widget();

protected:

  virtual GtkWidget* build() = 0;
  static int _digitWidth;

  // toggle control for boolean value
  GtkWidget* add(ControlLayout&, const std::string& label, bool& value);
  // toggle control for int value
  GtkWidget* addBool(ControlLayout&, const std::string& label, int& value);
  // slide control for float or double value
  GtkObject* add(ControlLayout&, const std::string& label, float& value,
    float minVal, float maxVal, int numDigits);
  GtkObject* add(ControlLayout&, const std::string& label, gdouble& value,
    float minVal, float maxVal, int numDigits);
  // slide control for int value
  void add(ControlLayout&, const std::string& label, int& value,
    int minVal, int maxVal);
  // menu control for enum value
  void add(ControlLayout&, const std::string& label, int value, GtkWidget* options);
  void addItem(GtkWidget* options, std::string name, int* tag, int& value);

  static GtkWidget* stdButton(const std::string& label);
  static GtkWidget* stdToggle(const std::string& label);
  static GtkWidget* checkButtonFor(int& value);
  static GtkWidget* checkButtonFor(bool& value);
  static GtkWidget* spinnerFor(GtkObject* adj, int numDigits);
  static GtkWidget* sliderFor(GtkObject* adj, int numDigits);

  void connect(GtkCBBase*);

private:

  GtkWidget* _widget;
  std::list<GtkCBBase*> _callbacks;

  // simple callback functions
  static void toggleValueUpdate(GtkWidget* toggle, bool* ptr);
  static void toggleIntValueUpdate(GtkWidget* toggle, int* ptr);
  static void adjDoubleUpdate(GtkObject* adj, double* ptr);
  static void adjFloatUpdate(GtkObject* adj, float* ptr);
  static void adjIntUpdate(GtkObject* adj, int* ptr);
  // note: needs user data set on widget
  static void switchValueUpdate(GtkWidget* widget, int* ptr);

};

struct InputGui: public Dbp::Gui {
  InputGui(InputOp&);
  virtual ~InputGui();
  virtual GtkWidget* build();

private:
  InputOp& _op;
  GtkWidget* _selector;
  GtkWidget* _selectorList;
  GtkWidget* _inputListView;
  GtkWidget* _removeButton;
  GtkWidget* _clearButton;

  GtkListStore* _inputList;
  std::list<std::string> _filenames;
  
  void openFileSelector();
  void addFiles();
  void closeFileSelector();
  void removeFiles();
  static void removeSelectedFile(
    GtkTreeModel*, GtkTreePath*, GtkTreeIter*, gpointer);
  void removeFile(std::string);
  void clearFileList();
  void rebuildInputList();
};

struct TurnGui: public Dbp::Gui {
  TurnGui(TurnOp&);
  virtual GtkWidget* build();
private:
  static int _turnOptionTags[];
  TurnOp& _op;
  GtkWidget* _enable;
  ControlLayout _controls;
  void setEnabled();
};

struct BlurGui: public Dbp::Gui {
  BlurGui(BlurOp&);
  virtual GtkWidget* build();
private:
  BlurOp& _op;
  GtkWidget* _enable;
  ControlLayout _controls;
  void setEnabled();
};

struct RecolourGui: public Dbp::Gui {
  RecolourGui(RecolourOp&);
  virtual GtkWidget* build();
private:
  RecolourOp& _op;
  GtkWidget* _enable;
  GtkWidget* _controls;
  GtkWidget* _auto;
  GtkWidget* _manualFrame;
  ControlLayout _manualControls;
  GtkWidget* _invert;
  GtkWidget* _mono;
  void setEnabled();
  void setAuto();
};

struct ResizeGui: public Dbp::Gui {
  ResizeGui(ResizeOp&);
  virtual GtkWidget* build();
private:
  ResizeOp& _op;

  GtkWidget* _enable;
  GtkWidget* _controls;
  ControlLayout _absControls;
  ControlLayout _relControls;

  GtkWidget* _relativeToggle;
  GtkWidget* _absoluteToggle;
  GtkWidget* _fixedToggle;
  GtkWidget* _relativeFrame;
  GtkObject* _xAdjust;
  GtkObject* _yAdjust;
  GtkWidget* _absoluteFrame;
  GtkWidget* _fitOptions;
  GtkWidget* _selectFit;
  GtkWidget* _warning;

  void setEnabled();
  void selectMode();
  void selectAspect();
  void handleXAdjust();
  void handleYAdjust();
  void selectFit();
  void setWarning();

  static int _fitOptionTags[];
  void addFitOption(std::string, int*);
};

struct CropGui: public Dbp::Gui {
  CropGui(CropOp&);
  virtual GtkWidget* build();
private:
  CropOp& _op;
  GtkWidget* _enable;
  ControlLayout _controls;
  void setEnabled();
};

struct SharpenGui: public Dbp::Gui {
  SharpenGui(SharpenOp&);
  virtual GtkWidget* build();
private:
  SharpenOp& _op;
  GtkWidget* _enable;
  ControlLayout _controls;
  void setEnabled();
};

struct RenameGui: public Dbp::Gui {
  RenameGui(RenameOp&);
  virtual GtkWidget* build();
private:
  RenameOp& _op;

  Location _exampleSrc;

  GtkWidget* _dirSelector;
  GtkWidget* _dirLabel;
  GtkWidget* _prefix;
  GtkWidget* _postfix;
  GtkWidget* _example;

  ControlLayout _controls;

  static int _ditherOptionTags[];

  void recalcDirName();
  void recalcExample();

  void setPrefix();
  void setPostfix();
  void clearDirPath();
  void okDirSelector();
  void cancelDirSelector();
  void raiseDirSelector();

};

struct OutputGui: public Dbp::Gui {
  OutputGui(OutputOp&);
  virtual GtkWidget* build();

private:
  OutputOp& _op;

  GtkWidget* _formats;
  GtkWidget* _options;

  std::vector<ControlLayout> _layout;

  void addOption(std::string text, int* tag);
  void selectOption();
};

class DbpGui: public Gui {

public:

  DbpGui(DbpData&, std::string name);
  virtual ~DbpGui();

  GtkWidget* build();

  bool step();
  bool done();

  void run();

private:

  DbpData& _data;

  GtkWidget* _notebook;
  GtkWidget* _progress;
  GtkWidget* _messageText;
  GtkWidget* _test;
  GtkWidget* _start;
  GtkWidget* _cancel;
  GtkWidget* _show;

  gint _idleProcess;
  bool _done;

  std::string _name;

  void add(const std::string& label, Gui* gui);
  std::list<Gui*> _guis;

  void test();
  void start();
  void cancel();
  void show();
  void setBusy(bool);

  void setProgressTitle(const char*);
  void setProgressAmount(float);

  static gboolean idleProcess(gpointer data);
  static void gimpProgressStartCallback(const gchar* message, gboolean cancellable, gpointer user_data);
  static void gimpProgressEndCallback(gpointer user_data);
  static void gimpProgressTextCallback(const gchar* message, gpointer user_data);
  static void gimpProgressValueCallback(gdouble value, gpointer user_data);
};

} // namespace Dbp

#endif // _DBP_GUI_H_
