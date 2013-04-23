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

#include "gui.h"

#include <algorithm>
#include <iostream>

#include <libintl.h>
#define _(String) gettext(String)
#define gettext_noop(String) String
#define N_(String) gettext_noop(String)

using namespace Dbp;

const int
ControlLayout::_maxRows = 15;

static const gchar* dataKey = "user_data";

ControlLayout::ControlLayout() {
  // why do tables need to be static sized??
  _widget = gtk_table_new(3, _maxRows, FALSE);
  _rows = 0;
  gtk_widget_show(_widget);
}

void
ControlLayout::add(const std::string& name, GtkWidget* one, GtkWidget* two) {

  if (_rows == _maxRows) {
    return;
  }

  GtkAttachOptions NONE = static_cast<GtkAttachOptions>(0);

  GtkWidget* label = gtk_label_new(name.c_str());
  gtk_widget_show(label);
  gtk_table_attach(GTK_TABLE(_widget), label, 0, 1, _rows, _rows+1, NONE, NONE, 5, 2);

  int startTwoAt = 1;
  if (one) {
    gtk_table_attach(GTK_TABLE(_widget), one, 1, 2, _rows, _rows+1, NONE, NONE, 5, 2);
    startTwoAt = 2;
  }
  if (two) {
    gtk_table_attach_defaults(GTK_TABLE(_widget), two, startTwoAt, 3, _rows, _rows+1);
  }
  ++_rows;
}

GtkWidget*
ControlLayout::widget() const {
  return _widget;
}



GtkCBBase::GtkCBBase(GtkObject* object, std::string name) {
  GCallback fn = (GCallback)&GtkCBBase::callback;
  g_signal_connect(object, name.c_str(), fn, this);
}

GtkCBBase::~GtkCBBase() {
}

void
GtkCBBase::callback(GtkObject*, GtkCBBase* cb) {
  cb->fn();
}


int Gui::_digitWidth = 0;

Gui::Gui():
  _widget(0) {
}

Gui::~Gui() {
  // should really release widget, but we know it's going anyway
  while (! _callbacks.empty()) {
    delete _callbacks.front();
    _callbacks.pop_front();
  }
}

GtkWidget*
Gui::widget() {
  if (_widget == 0) {
    _widget = build();
  }
  return _widget;
}

GtkWidget*
Gui::add(ControlLayout& controls, const std::string& label, bool& value) {
  GtkWidget* toggle = checkButtonFor(value);
  controls.add(label, toggle);
  return toggle;
}

GtkWidget*
Gui::addBool(ControlLayout& controls, const std::string& label, int& value) {
  GtkWidget* toggle = checkButtonFor(value);
  controls.add(label, toggle);
  return toggle;
}

GtkObject*
Gui::add(ControlLayout& controls, const std::string& label, float& value,
    float minVal, float maxVal, int numDigits) {

  float step = 1.0 / pow(10.0, numDigits);
  GtkObject* adj =
    gtk_adjustment_new(value, minVal, maxVal, step, step*10.0, 0.0);
  g_signal_connect(adj, "value_changed",
    (GCallback)&Gui::adjFloatUpdate, &value);
  controls.add(label, spinnerFor(adj, numDigits), sliderFor(adj, numDigits));
  return adj;
}

GtkObject*
Gui::add(ControlLayout& controls, const std::string& label, gdouble& value,
    float minVal, float maxVal, int numDigits) {

  float step = 1.0 / pow(10.0, numDigits);
  GtkObject* adj =
    gtk_adjustment_new(value, minVal, maxVal, step, step*10.0, 0.0);
  g_signal_connect(adj, "value_changed",
    (GCallback)&Gui::adjDoubleUpdate, &value);
  controls.add(label, spinnerFor(adj, numDigits), sliderFor(adj, numDigits));
  return adj;
}

void
Gui::add(ControlLayout& controls, const std::string& label, int& value,
    int minVal, int maxVal) {
  GtkObject* adj =
    gtk_adjustment_new(value, minVal, maxVal, 1, 10.0, 0.0);
  g_signal_connect(adj, "value_changed",
    (GCallback)&Gui::adjIntUpdate, &value);
  controls.add(label, spinnerFor(adj, 0), sliderFor(adj, 0));
}

void
Gui::add(ControlLayout& controls, const std::string& label, int value, GtkWidget* options) {
  GtkWidget* menu = gtk_option_menu_new();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(menu), options);
  gtk_option_menu_set_history(GTK_OPTION_MENU(menu), value);
  controls.add(label, menu);
}

void
Gui::addItem(GtkWidget* options, std::string label, int* tag, int& value) {
  GtkWidget* item = gtk_menu_item_new_with_label(label.c_str());
  g_object_set_data(G_OBJECT(item), dataKey, tag);
  gtk_menu_shell_append(GTK_MENU_SHELL(options), item);
  g_signal_connect(GTK_OBJECT(item), "activate",
    (GCallback)&Gui::switchValueUpdate, &value);
}

GtkWidget*
Gui::stdButton(const std::string& label) {
  GtkWidget* button = gtk_button_new_with_label(label.c_str());
  gtk_misc_set_padding(GTK_MISC(GTK_BIN(button)->child), 4, 0);
  gtk_container_set_border_width(GTK_CONTAINER(button), 2);
  return button;
}

GtkWidget*
Gui::stdToggle(const std::string& label) {
  GtkWidget* button = gtk_toggle_button_new_with_label(label.c_str());
  gtk_misc_set_padding(GTK_MISC(GTK_BIN(button)->child), 4, 0);
  gtk_container_set_border_width(GTK_CONTAINER(button), 2);
  return button;
}


/*
 * gtk_signal functions, get widget value and assign somewhere
 *
 */

void
Gui::toggleValueUpdate(GtkWidget* toggle, bool* ptr) {
  *ptr = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(toggle)) ? true : false;
}

void
Gui::toggleIntValueUpdate(GtkWidget* toggle, int* ptr) {
  *ptr = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(toggle));
}

void
Gui::adjDoubleUpdate(GtkObject* adj, double* ptr) {
  *ptr = GTK_ADJUSTMENT(adj)->value;
}

void
Gui::adjFloatUpdate(GtkObject* adj, float* ptr) {
  *ptr = GTK_ADJUSTMENT(adj)->value;
}

void
Gui::adjIntUpdate(GtkObject* adj, int* ptr) {
  *ptr = static_cast<int>(GTK_ADJUSTMENT(adj)->value);
}

/* note: needs user data set on widget */
void
Gui::switchValueUpdate(GtkWidget* widget, int* ptr) {
  *ptr = *((int*)g_object_get_data(G_OBJECT(widget), dataKey));
}


/*
 * create a check button attached to an int variable
 *
 */

GtkWidget*
Gui::checkButtonFor(bool& value) {
  GtkWidget* result;
  result = gtk_check_button_new();
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(result), value ? TRUE : FALSE);
  g_signal_connect(GTK_OBJECT(result), "toggled", (GCallback)&Gui::toggleValueUpdate, &value);
  return result;
}

GtkWidget*
Gui::checkButtonFor(int& value) {
  GtkWidget* result;
  result = gtk_check_button_new();
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(result), value);
  g_signal_connect(GTK_OBJECT(result), "toggled", (GCallback)&Gui::toggleIntValueUpdate, &value);
  return result;
}

GtkWidget*
Gui::spinnerFor(GtkObject* adj, int numDigits) {
  GtkWidget* spinner =
    gtk_spin_button_new(GTK_ADJUSTMENT(adj), 0.1, numDigits);
  if (_digitWidth == 0) {
    PangoContext* pangoContext = gtk_widget_get_pango_context(spinner);
    PangoLayout* pangoLayout = pango_layout_new(pangoContext);
    pango_layout_set_text(pangoLayout, "0", -1);
    pango_layout_get_pixel_size(pangoLayout, &_digitWidth, 0);
  }
  gtk_widget_set_size_request(spinner, _digitWidth * 10, -1);
  return spinner;
}

GtkWidget*
Gui::sliderFor(GtkObject* adj, int numDigits) {
  GtkWidget* slider = gtk_hscale_new(GTK_ADJUSTMENT(adj));
  gtk_scale_set_digits(GTK_SCALE(slider), numDigits);
  gtk_scale_set_draw_value(GTK_SCALE(slider), FALSE);
  return slider;
}

void
Gui::connect(GtkCBBase* cb) {
  _callbacks.push_back(cb);
}

InputGui::InputGui(InputOp& input):
  _op(input),
  _selector(0) {
}

InputGui::~InputGui()
{
  _filenames.clear();
}

GtkWidget*
InputGui::build() {

  GtkWidget* hbox;
  GtkWidget* vbox;
  GtkWidget* button;
  GtkWidget* scroll;

  // create a multiple file selection widget
  //
  GtkWindow* parentWindow = 0;
  _selector = gtk_file_chooser_dialog_new(_("Add Image Files"),
    parentWindow, GTK_FILE_CHOOSER_ACTION_OPEN,
    GTK_STOCK_CLOSE, GTK_RESPONSE_CANCEL,
    GTK_STOCK_ADD, GTK_RESPONSE_ACCEPT,
    GTK_STOCK_OPEN, GTK_RESPONSE_OK,
    (char*)0); // to fix gcc compiler warning - may be an error on 64 bit processors
  gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(_selector), TRUE);

  // the input gui
  //
  vbox = gtk_vbox_new(FALSE, 0);

  scroll = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(
    GTK_SCROLLED_WINDOW(scroll), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
  gtk_box_pack_start(GTK_BOX(vbox), scroll, TRUE, TRUE, 0);
  _inputList = gtk_list_store_new(1, G_TYPE_STRING);
  _inputListView = gtk_tree_view_new_with_model(GTK_TREE_MODEL(_inputList));
  GtkCellRenderer* renderer =
    gtk_cell_renderer_text_new();
  GtkTreeViewColumn *column = gtk_tree_view_column_new_with_attributes(
    _("Images"), renderer,
    "text", 0,
    (char*)0); // to fix gcc compiler warning - may be an error on 64 bit processors
  gtk_tree_view_append_column(GTK_TREE_VIEW(_inputListView), column);
  gtk_tree_selection_set_mode(
    gtk_tree_view_get_selection(GTK_TREE_VIEW(_inputListView)),
    GTK_SELECTION_MULTIPLE);
  gtk_scrolled_window_add_with_viewport(
    GTK_SCROLLED_WINDOW(scroll), _inputListView);

  hbox = gtk_hbox_new(TRUE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  button = stdButton(_("Add Files"));
  connect(gtkSignal(button, "clicked", &InputGui::openFileSelector, this));
  gtk_box_pack_start(GTK_BOX(hbox), button, TRUE, FALSE, 0);

  _removeButton = stdButton(_("Remove Files"));
  connect(gtkSignal(_removeButton, "clicked", &InputGui::removeFiles, this));
  gtk_box_pack_start(GTK_BOX(hbox), _removeButton, TRUE, FALSE, 0);
  gtk_widget_set_sensitive(_removeButton, FALSE);

  _clearButton = stdButton(_("Clear List"));
  connect(gtkSignal(_clearButton, "clicked", &InputGui::clearFileList, this));
  gtk_box_pack_start(GTK_BOX(hbox), _clearButton, TRUE, FALSE, 0);
  gtk_widget_set_sensitive(_clearButton, FALSE);

  gtk_widget_show_all(vbox);
  return vbox;
}

void
InputGui::addFiles() {
  GSList* files = gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(_selector));
  GSList* current = files;
  while (current) {
    Location newFile((const char*)current->data);
    _op._files.push_back(newFile);
    g_free(current->data);
    current = current->next;
  }
  g_slist_free(files);
  gtk_file_chooser_unselect_all(GTK_FILE_CHOOSER(_selector));

  // tidy up the input list
  _op._files.sort();
  _op._files.unique();

  // fix the displayed input list
  rebuildInputList();
}

void
InputGui::openFileSelector() {
  gint response;
  response = gtk_dialog_run(GTK_DIALOG(_selector));
  while (response == GTK_RESPONSE_ACCEPT) {
    addFiles();
    response = gtk_dialog_run(GTK_DIALOG(_selector));
  }
  if (response == GTK_RESPONSE_OK) {
    addFiles();
  }
  gtk_widget_hide(_selector);
}

void
InputGui::closeFileSelector() {
  gtk_widget_hide(_selector);
}

struct LocationMatches {
  std::string _target;
  LocationMatches(std::string target): _target(target) {}
  bool operator()(Location loc) { return loc.fullPath() == _target; }
};

void
InputGui::removeSelectedFile(
  GtkTreeModel* model, GtkTreePath* path, GtkTreeIter* iter, gpointer data)
{
  gchar* filename;
  gtk_tree_model_get(model, iter, 0, &filename, -1);
  static_cast<InputGui*>(data)->removeFile(filename);
  g_free(filename);
}

void
InputGui::removeFile(std::string path) {
  _op._files.remove_if(LocationMatches(path));
}

void
InputGui::removeFiles() {
  gtk_tree_selection_selected_foreach(
    gtk_tree_view_get_selection(GTK_TREE_VIEW(_inputListView)),
    &removeSelectedFile, this);
  rebuildInputList();
}

void
InputGui::clearFileList() {
  _op._files.clear();
  rebuildInputList();
}

void
InputGui::rebuildInputList() {
  _filenames.clear();
  gtk_list_store_clear(_inputList);
  InputOp::FileList::iterator iter = _op._files.begin();
  while (iter != _op._files.end()) {
    _filenames.push_back((*iter).fullPath());
    GtkTreeIter newItem;
    gtk_list_store_append(_inputList, &newItem);
    gtk_list_store_set(_inputList, &newItem, 0, _filenames.back().c_str(), -1);
    ++iter;
  }
  if (_op._files.empty()) {
    gtk_widget_set_sensitive(_removeButton, FALSE);
    gtk_widget_set_sensitive(_clearButton, FALSE);
  } else {
    gtk_widget_set_sensitive(_removeButton, TRUE);
    gtk_widget_set_sensitive(_clearButton, TRUE);
  }
}

int
TurnGui::_turnOptionTags[] = {
  TurnOp::TURN_90,
  TurnOp::TURN_180,
  TurnOp::TURN_270
};

TurnGui::TurnGui(TurnOp& turn): _op(turn) {
}

GtkWidget*
TurnGui::build() {
  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);

  _enable = gtk_check_button_new_with_label(_("Enable"));
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(_enable), _op._enabled);
  connect(gtkSignal(_enable, "toggled", &TurnGui::setEnabled, this));
  gtk_box_pack_start(GTK_BOX(vbox), _enable, FALSE, FALSE, 0);

  gtk_box_pack_start(GTK_BOX(vbox), _controls.widget(), FALSE, FALSE, 0);

  /* pop up menu */
  GtkWidget* _turnOptions = gtk_menu_new();

  addItem(_turnOptions, _("Clockwise"), &_turnOptionTags[0], _op._turn);
  addItem(_turnOptions, _("Upside-down"), &_turnOptionTags[1], _op._turn);
  addItem(_turnOptions, _("Anti-clockwise"), &_turnOptionTags[2], _op._turn);

  gtk_widget_show_all(_turnOptions);
  gtk_widget_hide(_turnOptions);

  add(_controls, _("Turn"), _op._turn, _turnOptions);
  if (!_op._enabled) {
    gtk_widget_set_sensitive(_controls.widget(), FALSE);
  }

  gtk_widget_show_all(vbox);
  return vbox;
}

void
TurnGui::setEnabled() {
  gint enabled = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(_enable));
  if (enabled) {
    _op._enabled = true;
    gtk_widget_set_sensitive(_controls.widget(), TRUE);
  } else {
    _op._enabled = false;
    gtk_widget_set_sensitive(_controls.widget(), FALSE);
  }
}

BlurGui::BlurGui(BlurOp& blur): _op(blur) {
}

GtkWidget*
BlurGui::build() {
  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);

  _enable = gtk_check_button_new_with_label(_("Enable"));
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(_enable), _op._enabled);
  connect(gtkSignal(_enable, "toggled", &BlurGui::setEnabled, this));
  gtk_box_pack_start(GTK_BOX(vbox), _enable, FALSE, FALSE, 0);

  gtk_box_pack_start(GTK_BOX(vbox), _controls.widget(), FALSE, FALSE, 0);
  add(_controls, _("Radius"), _op._radius, 1.0, 100.0, 1);
  if (!_op._enabled) {
    gtk_widget_set_sensitive(_controls.widget(), FALSE);
  }

  gtk_widget_show_all(vbox);
  return vbox;
}

void
BlurGui::setEnabled() {
  gint enabled = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(_enable));
  if (enabled) {
    _op._enabled = true;
    gtk_widget_set_sensitive(_controls.widget(), TRUE);
  } else {
    _op._enabled = false;
    gtk_widget_set_sensitive(_controls.widget(), FALSE);
  }
}

RecolourGui::RecolourGui(RecolourOp& recolour): _op(recolour) {
}

GtkWidget*
RecolourGui::build() {
  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);

  _enable = gtk_check_button_new_with_label(_("Enable"));
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(_enable), _op._enabled);
  connect(gtkSignal(_enable, "toggled", &RecolourGui::setEnabled, this));
  gtk_box_pack_start(GTK_BOX(vbox), _enable, FALSE, FALSE, 0);

  _controls = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), _controls, FALSE, FALSE, 0);

  _auto = gtk_check_button_new_with_label(_("Auto Levels"));
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(_auto), _op._auto);
  connect(gtkSignal(_auto, "toggled", &RecolourGui::setAuto, this));
  gtk_box_pack_start(GTK_BOX(_controls), _auto, FALSE, FALSE, 0);

  _manualFrame = gtk_frame_new(_("Manual"));
  gtk_widget_set_sensitive(_manualFrame, _op._auto ? FALSE : TRUE);
  gtk_box_pack_start(GTK_BOX(_controls), _manualFrame, FALSE, FALSE, 0);

  gtk_container_add(GTK_CONTAINER(_manualFrame), _manualControls.widget());
  add(_manualControls, _("Bright"), _op._brightness, -1.0, 1.0, 3);
  add(_manualControls, _("Contrast"), _op._contrast, -1.0, 1.0, 3);
  add(_manualControls, _("Saturation"), _op._saturation, 0.0, 2.0, 3);
#if NYI
  add(_("Gamma"), _op._gamma, 0.1, 10.0, 2);
#endif

  _invert = checkButtonFor(_op._invert);
  gtk_button_set_label(GTK_BUTTON(_invert), _("Invert"));
  gtk_box_pack_start(GTK_BOX(_controls), _invert, FALSE, FALSE, 0);

  _mono = checkButtonFor(_op._mono);
  gtk_button_set_label(GTK_BUTTON(_mono), _("Convert to Grey"));
  gtk_box_pack_start(GTK_BOX(_controls), _mono, FALSE, FALSE, 0);

  if (!_op._enabled) {
    gtk_widget_set_sensitive(_controls, FALSE);
  }

  gtk_widget_show_all(vbox);
  return vbox;
}

void
RecolourGui::setEnabled() {
  gint enabled = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(_enable));
  if (enabled) {
    _op._enabled = true;
    gtk_widget_set_sensitive(_controls, TRUE);
  } else {
    _op._enabled = false;
    gtk_widget_set_sensitive(_controls, FALSE);
  }
}

void
RecolourGui::setAuto() {
  gint autoLevels = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(_auto));
  if (autoLevels) {
    _op._auto = true;
    gtk_widget_set_sensitive(_manualFrame, FALSE);
  } else {
    _op._auto = false;
    gtk_widget_set_sensitive(_manualFrame, TRUE);
  }
}

ResizeGui::ResizeGui(ResizeOp& resize): _op(resize) {
}

int
ResizeGui::_fitOptionTags[] = {
  (int)ResizeOp::FIT_EXACTLY,
  (int)ResizeOp::PAD_TO_FIT,
  (int)ResizeOp::FIT_INSIDE,
  (int)ResizeOp::FIT_OUTSIDE
};

GtkWidget*
ResizeGui::build() {

  /* pop up menu */
  _fitOptions = gtk_menu_new();

  addFitOption(_("Exactly"), &_fitOptionTags[0]);
  addFitOption(_("Padded"), &_fitOptionTags[1]);
  addFitOption(_("Inside"), &_fitOptionTags[2]);
  addFitOption(_("Outside"), &_fitOptionTags[3]);

  gtk_widget_show_all(_fitOptions);
  gtk_widget_hide(_fitOptions);

  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);
  gtk_widget_show(vbox);

  _enable = gtk_check_button_new_with_label(_("Enable"));
  connect(gtkSignal(_enable, "toggled", &ResizeGui::setEnabled, this));
  gtk_box_pack_start(GTK_BOX(vbox), _enable, FALSE, FALSE, 0);

  _controls = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), _controls, FALSE, FALSE, 0);

  GSList* relativeGroup = NULL;
  _relativeToggle = gtk_radio_button_new_with_label(NULL, _("Relative"));
  gtk_toggle_button_set_active(
    GTK_TOGGLE_BUTTON(_relativeToggle), _op._relative);
  connect(gtkSignal(_relativeToggle, "toggled", &ResizeGui::selectMode, this));
  gtk_box_pack_start(GTK_BOX(_controls), _relativeToggle, FALSE, FALSE, 0);

  relativeGroup = gtk_radio_button_get_group(GTK_RADIO_BUTTON(_relativeToggle));
  _absoluteToggle = gtk_radio_button_new_with_label(relativeGroup, _("Absolute"));
  gtk_toggle_button_set_active(
    GTK_TOGGLE_BUTTON(_absoluteToggle), !_op._relative);
  gtk_box_pack_start(GTK_BOX(_controls), _absoluteToggle, FALSE, FALSE, 0);

  _relativeFrame = gtk_frame_new(_("Relative"));
  gtk_widget_set_sensitive(_relativeFrame, _op._relative);
  gtk_box_pack_start(GTK_BOX(vbox), _relativeFrame, FALSE, FALSE, 0);

  gtk_container_add(GTK_CONTAINER(_relativeFrame), _relControls.widget());

  _fixedToggle = add(_relControls, _("Keep Aspect"), _op._keepAspect);
  connect(gtkSignal(_fixedToggle, "toggled", &ResizeGui::selectAspect, this));
  _xAdjust = add(_relControls, _("X Scale"), _op._xScale, 0.01, 2.0, 2);
  connect(gtkSignal(_xAdjust, "value_changed", &ResizeGui::handleXAdjust, this));
  _yAdjust = add(_relControls, _("Y Scale"), _op._yScale, 0.01, 2.0, 2);
  connect(gtkSignal(_yAdjust, "value_changed", &ResizeGui::handleYAdjust, this));

  _absoluteFrame = gtk_frame_new(_("Absolute"));
  gtk_widget_set_sensitive(_absoluteFrame, ! _op._relative);
  gtk_box_pack_start(GTK_BOX(vbox), _absoluteFrame, FALSE, FALSE, 0);

  gtk_container_add(GTK_CONTAINER(_absoluteFrame), _absControls.widget());

  add(_absControls, _("Width"), _op._xSize, 1, 6144);
  add(_absControls, _("Height"), _op._ySize, 1, 4096);

  _selectFit = gtk_option_menu_new();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(_selectFit), _fitOptions);
  gtk_option_menu_set_history(GTK_OPTION_MENU(_selectFit), (int)_op._fit);

  _warning = gtk_label_new("");
  setWarning();
  _absControls.add(_("Fit"), _selectFit, _warning);

  if (!_op._enabled) {
    gtk_widget_set_sensitive(_controls, FALSE);
    gtk_widget_set_sensitive(_relativeFrame, FALSE);
    gtk_widget_set_sensitive(_absoluteFrame, FALSE);
  }

  gtk_widget_show_all(vbox);
  return vbox;
}

void
ResizeGui::addFitOption(std::string label, int* tag) {
  GtkWidget* item = gtk_menu_item_new_with_label(label.c_str());
  g_object_set_data(G_OBJECT(item), dataKey, tag);
  gtk_menu_shell_append(GTK_MENU_SHELL(_fitOptions), item);
  connect(gtkSignal(item, "activate", &ResizeGui::selectFit, this));
}

void
ResizeGui::setEnabled() {
  gint enabled = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(_enable));
  if (enabled) {
    _op._enabled = true;
    gtk_widget_set_sensitive(_controls, TRUE);
    if (_op._relative) {
      gtk_widget_set_sensitive(_relativeFrame, TRUE);
      gtk_widget_set_sensitive(_absoluteFrame, FALSE);
    } else {
      gtk_widget_set_sensitive(_relativeFrame, FALSE);
      gtk_widget_set_sensitive(_absoluteFrame, TRUE);
    }
  } else {
    _op._enabled = false;
    gtk_widget_set_sensitive(_controls, FALSE);
    gtk_widget_set_sensitive(_relativeFrame, FALSE);
    gtk_widget_set_sensitive(_absoluteFrame, FALSE);
  }
}

void
ResizeGui::selectMode() {
  if (GTK_TOGGLE_BUTTON(_relativeToggle)->active) {
    _op._relative = true;
    gtk_widget_set_sensitive(_relativeFrame, TRUE);
    gtk_widget_set_sensitive(_absoluteFrame, FALSE);
  } else {
    _op._relative = false;
    gtk_widget_set_sensitive(_relativeFrame, FALSE);
    gtk_widget_set_sensitive(_absoluteFrame, TRUE);
  }
}

void
ResizeGui::selectAspect() {
  if (_op._keepAspect) {
    gtk_adjustment_set_value(
      GTK_ADJUSTMENT(_yAdjust), GTK_ADJUSTMENT(_xAdjust)->value);
  }
}

void
ResizeGui::handleXAdjust() {
  GtkAdjustment* x = GTK_ADJUSTMENT(_xAdjust);
  GtkAdjustment* y = GTK_ADJUSTMENT(_yAdjust);
  if (_op._keepAspect && (x->value != y->value)) {
    gtk_adjustment_set_value(y, x->value);
  }
}

void
ResizeGui::handleYAdjust() {
  GtkAdjustment* x = GTK_ADJUSTMENT(_xAdjust);
  GtkAdjustment* y = GTK_ADJUSTMENT(_yAdjust);
  if (_op._keepAspect && (x->value != y->value)) {
    gtk_adjustment_set_value(x, y->value);
  }
}

void
ResizeGui::selectFit() {
  GtkWidget* currentItem = gtk_menu_get_active(GTK_MENU(_fitOptions));
  int data =  *((int*)g_object_get_data(G_OBJECT(currentItem), dataKey));
  _op._fit = (ResizeOp::FitOptions)data;
  setWarning();
}

void
ResizeGui::setWarning() {
  switch (_op._fit) {
  case ResizeOp::FIT_EXACTLY:
    gtk_label_set_text(GTK_LABEL(_warning), _("Note: may change aspect ratio."));
    break;
  case ResizeOp::PAD_TO_FIT:
    gtk_label_set_text(GTK_LABEL(_warning), _("Pads with Gimp background colour"));
    break;
  case ResizeOp::FIT_INSIDE:
    gtk_label_set_text(GTK_LABEL(_warning), "");
    break;
  case ResizeOp::FIT_OUTSIDE:
    gtk_label_set_text(GTK_LABEL(_warning), "");
    break;
  }
}

CropGui::CropGui(CropOp& crop): _op(crop) {
}

GtkWidget*
CropGui::build() {
  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);

  _enable = gtk_check_button_new_with_label(_("Enable"));
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(_enable), _op._enabled);
  connect(gtkSignal(_enable, "toggled", &CropGui::setEnabled, this));
  gtk_box_pack_start(GTK_BOX(vbox), _enable, FALSE, FALSE, 0);

  gtk_box_pack_start(GTK_BOX(vbox), _controls.widget(), FALSE, FALSE, 0);

  add(_controls, _("Width"), _op._width, 1, 6144);
  add(_controls, _("Height"), _op._height, 1, 4096);
  add(_controls, _("X Pos"), _op._x, 0, 6143);
  add(_controls, _("Y Pos"), _op._y, 0, 4095);
  if (!_op._enabled) {
    gtk_widget_set_sensitive(_controls.widget(), FALSE);
  }

  gtk_widget_show_all(vbox);
  return vbox;
}

void
CropGui::setEnabled() {
  gint enabled = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(_enable));
  if (enabled) {
    _op._enabled = true;
    gtk_widget_set_sensitive(_controls.widget(), TRUE);
  } else {
    _op._enabled = false;
    gtk_widget_set_sensitive(_controls.widget(), FALSE);
  }
}

SharpenGui::SharpenGui(SharpenOp& sharpen): _op(sharpen) {
}

GtkWidget*
SharpenGui::build() {
  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);

  _enable = gtk_check_button_new_with_label(_("Enable"));
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(_enable), _op._enabled);
  connect(gtkSignal(_enable, "toggled", &SharpenGui::setEnabled, this));
  gtk_box_pack_start(GTK_BOX(vbox), _enable, FALSE, FALSE, 0);

  gtk_box_pack_start(GTK_BOX(vbox), _controls.widget(), FALSE, FALSE, 0);

  add(_controls, _("Radius"), _op._radius, 0.1, 120.0, 1);
  add(_controls, _("Amount"), _op._amount, 0.0, 5.0, 2);
  add(_controls, _("Threshhold"), _op._threshhold, 0.0, 255.0, 0);
  if (!_op._enabled) {
    gtk_widget_set_sensitive(_controls.widget(), FALSE);
  }

  gtk_widget_show_all(vbox);
  return vbox;
}

void
SharpenGui::setEnabled() {
  gint enabled = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(_enable));
  if (enabled) {
    _op._enabled = true;
    gtk_widget_set_sensitive(_controls.widget(), TRUE);
  } else {
    _op._enabled = false;
    gtk_widget_set_sensitive(_controls.widget(), FALSE);
  }
}

int
RenameGui::_ditherOptionTags[] = {
  (int)RenameOp::NO_DITHER,
  (int)RenameOp::FLOYD_STEINBERG,
  (int)RenameOp::REDUCED_BLEED,
  (int)RenameOp::FIXED_DITHER
};

RenameGui::RenameGui(RenameOp& rename):
  _op(rename),
  _exampleSrc("/home/me/pics", "test", "img") {
}

GtkWidget*
RenameGui::build() {

  // build a directory selector
  _dirSelector = gtk_file_chooser_dialog_new(_("Destination Directory"),
  	0, GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
	GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
	GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
	NULL);
  /* now the page */

  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);
  gtk_widget_show(vbox);
  gtk_box_pack_start(GTK_BOX(vbox), _controls.widget(), FALSE, FALSE, 0);

  _dirLabel = gtk_label_new("");
  _controls.add(_("To Directory:"), 0, _dirLabel);

  GtkWidget* hbox = gtk_hbox_new(TRUE, 0);

  GtkWidget* button;

  button = gtk_button_new_with_label(_("Select Dir"));
  connect(gtkSignal(button, "clicked", &RenameGui::raiseDirSelector, this));
  gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 0);

  button = gtk_button_new_with_label(_("Source Dir"));
  connect(gtkSignal(button, "clicked", &RenameGui::clearDirPath, this));
  gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 0);

  gtk_widget_show_all(hbox);
  _controls.add("", 0, hbox);

  _prefix = gtk_entry_new();
  connect(gtkSignal(_prefix, "changed", &RenameGui::setPrefix, this));
  _controls.add(_("Add Prefix:"), 0, _prefix);

  _postfix = gtk_entry_new();
  connect(gtkSignal(_postfix, "changed", &RenameGui::setPostfix, this));
  _controls.add(_("Add Postfix:"), 0, _postfix);

  _controls.add(_("Example:"), 0, 0);
  GtkWidget* label = gtk_label_new(_exampleSrc.fullPath().c_str());
  _controls.add(_("Original:"), 0, label);

  _example = gtk_label_new("");
  _controls.add(_("becomes:"), 0, _example);

  _controls.add(_("Before writing:"), 0, 0);
  add(_controls, _("Flatten"), _op._flatten);
  add(_controls, _("Convert Grey"), _op._convertToGreyscale);
  add(_controls, _("Convert Indexed"), _op._convertToIndexed);

  /* pop up menu */
  GtkWidget* ditherOptions = gtk_menu_new();

  addItem(ditherOptions, _("No Dither"), &_ditherOptionTags[0], _op._ditherType);
  addItem(ditherOptions, _("Floyd-Steinberg"), &_ditherOptionTags[1], _op._ditherType);
  addItem(ditherOptions, _("Reduced Bleed"), &_ditherOptionTags[2], _op._ditherType);
  addItem(ditherOptions, _("Fixed Position"), &_ditherOptionTags[3], _op._ditherType);

  gtk_widget_show_all(ditherOptions);
  gtk_widget_hide(ditherOptions);

  add(_controls, _("Dither"), _op._ditherType, ditherOptions);
  add(_controls, _("Colours"), _op._numberOfIndexedColours, 2, 256);

  recalcExample();
  recalcDirName();

  gtk_widget_show_all(vbox);
  return vbox;
}

void
RenameGui::recalcDirName() {
  if (_op._dirPath.empty()) {
    gtk_label_set_text(GTK_LABEL(_dirLabel), _("same as source"));
  } else {
    gtk_label_set_text(GTK_LABEL(_dirLabel), _op._dirPath.c_str());
  }
}

void
RenameGui::recalcExample() {
  Location exampleDst = _exampleSrc;
  _op.modify(exampleDst);
  gtk_label_set_text(GTK_LABEL(_example), exampleDst.fullPath().c_str());
}

void
RenameGui::setPrefix() {
  _op._prefix = gtk_entry_get_text(GTK_ENTRY(_prefix));
  recalcExample();
}

void
RenameGui::setPostfix() {
  _op._postfix = gtk_entry_get_text(GTK_ENTRY(_postfix));
  recalcExample();
}

void
RenameGui::clearDirPath() {
  // this line breaks on ??? some system
  //_op._dirPath.clear();
  // do this instead
  _op._dirPath = "";
  recalcDirName();
  recalcExample();
}

void
RenameGui::okDirSelector() {
  char* filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(_dirSelector));
  std::string dir = filename;
  g_free(filename);
  // file selector returns slash-terminated directory path, remove slash
  int len = dir.length();
  if ((len > 0) && (dir[len - 1] == G_DIR_SEPARATOR)) {
    dir.erase(len - 1);
  }

  _op._dirPath = dir;
  gtk_widget_hide(_dirSelector);
  recalcDirName();
  recalcExample();
}

void
RenameGui::cancelDirSelector() {
  gtk_widget_hide(_dirSelector);
}

void
RenameGui::raiseDirSelector() {
  if (gtk_dialog_run(GTK_DIALOG(_dirSelector)) == GTK_RESPONSE_ACCEPT) {
  	okDirSelector();
  } else {
    cancelDirSelector();
  }
}

OutputGui::OutputGui(OutputOp& op):
  _op(op) {
}

GtkWidget*
OutputGui::build() {

  int numFormats = _op._format.size();

  // pop up menu
  _options = gtk_menu_new();

  for (int i = 0; i < numFormats; ++i) {
    addOption(_op._format[i]._name.c_str(), &_op._format[i]._tag);
  }
  gtk_widget_show_all(_options);
  gtk_widget_hide(_options);

  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);

  GtkWidget* hbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  GtkWidget* label = gtk_label_new(_("Format:"));
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 4);

  GtkWidget* formats = gtk_option_menu_new();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(formats), _options);
  gtk_option_menu_set_history(GTK_OPTION_MENU(formats), 0);
  gtk_box_pack_start(GTK_BOX(hbox), formats, FALSE, FALSE, 0);

  //_layout.resize(numFormats);
  for (int i = 0; i < numFormats; ++i) {
    OutputFormat& format = _op._format[i];
    ControlLayout layout; // = _layout[i];
    int numParams = format._params.size();
    for (int j = 0; j < numParams; ++j) {
      OpParam& param = format._params[j];
      switch (param._guiType) {
      case OpParam::Toggle:
        // warning here, need to fix type safety
      //  add(layout, param._name, (bool&)param._gimpType.data.d_int32);
        addBool(layout, param._name, param._gimpType.data.d_int32);
        break;
      case OpParam::IntSlider:
        add(layout, param._name, param._gimpType.data.d_int32,
          param._minInt, param._maxInt);
        break;
      case OpParam::FloatSlider:
        add(layout, param._name, param._gimpType.data.d_float,
          param._minFloat, param._maxFloat, param._numDecimals);
        break;
      case OpParam::Menu:
        {
          GtkWidget* options = gtk_menu_new();
	  OpParam::Choice::iterator iter = param._choices.begin();
          while (iter != param._choices.end()) {
            int& tag = (*iter).first;
            const std::string& name = (*iter).second;
            addItem(options, name, &tag, param._gimpType.data.d_int32);
            ++iter;
	        }
          gtk_widget_show_all(options);
          gtk_widget_hide(options);
          add(layout, param._name, param._gimpType.data.d_int32, options);
        }
        break;
      case OpParam::String:
        // need to handle this
        break;
      case OpParam::None:
        break;
      }
    }
    _layout.push_back(layout);
    gtk_box_pack_start(GTK_BOX(vbox), layout.widget(), FALSE, FALSE, 0);
  }
  gtk_widget_show_all(vbox);
  //gtk_menu_set_active(GTK_MENU(_options), (int)_op._format);
  selectOption();

  return vbox;
}

void
OutputGui::addOption(std::string text, int* tag) {
  GtkWidget* item = gtk_menu_item_new_with_label(text.c_str());
  g_object_set_data(G_OBJECT(item), dataKey, tag);
  gtk_menu_shell_append(GTK_MENU_SHELL(_options), item);
  connect(gtkSignal(item, "activate", &OutputGui::selectOption, this));
}

void
OutputGui::selectOption() {
  GtkWidget* current = gtk_menu_get_active(GTK_MENU(_options));
  int selection = *((int*)g_object_get_data(G_OBJECT(current), dataKey));
  _op._selection = selection;
  int numLayouts = _layout.size();
  for (int i = 0; i < numLayouts; ++i) {
    if (i == selection) {
      gtk_widget_show(_layout[i].widget());
    } else {
      gtk_widget_hide(_layout[i].widget());
    }
  }
}


DbpGui::DbpGui(DbpData& data, std::string name):
  _data(data),
  _name(name) {
}

DbpGui::~DbpGui() {
  while (! _guis.empty()) {
    delete _guis.front();
    _guis.pop_front();
  }
}

GtkWidget*
DbpGui::build() {

  GtkWidget* dialog;

  int usesPreview = FALSE;
  gimp_ui_init(_name.c_str(), usesPreview);
  GtkDialogFlags flags = GtkDialogFlags(0);
  dialog = gimp_dialog_new(
    _("David's Batch Processor"),
    _name.c_str(),
    0, flags, 0, 0,
    GTK_STOCK_QUIT, 0,
    (char*)0); // to fix gcc compiler warning - may be an error on 64 bit processors
  g_signal_connect_swapped(
    GTK_OBJECT(dialog), "response",
    G_CALLBACK(gtk_widget_destroy), GTK_OBJECT(dialog));

  g_signal_connect(
    GTK_OBJECT(dialog), "destroy",
    G_CALLBACK(gtk_main_quit), NULL);

  _notebook = gtk_notebook_new();
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), _notebook, TRUE, TRUE, 0);
  // insert pages later...

  GtkWidget* frame = gtk_frame_new(_("Processing"));
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), frame, FALSE, FALSE, 5);

  GtkWidget* vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(frame), vbox);

  _messageText = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(vbox), _messageText, FALSE, FALSE, 3);

  _progress =
    gtk_progress_bar_new();
  gtk_box_pack_start(GTK_BOX(vbox), _progress, FALSE, FALSE, 0);

  GtkWidget* hbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  _start = Gui::stdButton(_("Start"));
  connect(gtkSignal(_start, "clicked", &DbpGui::start, this));
  gtk_box_pack_start(GTK_BOX(hbox), _start, FALSE, FALSE, 0);

  _cancel = Gui::stdButton(_("Cancel"));
  connect(gtkSignal(_cancel, "clicked", &DbpGui::cancel, this));
  gtk_box_pack_start(GTK_BOX(hbox), _cancel, FALSE, FALSE, 0);

  _test = Gui::stdButton(_("Test"));
  connect(gtkSignal(_test, "clicked", &DbpGui::test, this));
  gtk_box_pack_start(GTK_BOX(hbox), _test, FALSE, FALSE, 0);

  _data.setVisible(false);
  _show = Gui::stdToggle(_("Show Images"));
  connect(gtkSignal(_show, "toggled", &DbpGui::show, this));
  gtk_box_pack_end(GTK_BOX(hbox), _show, FALSE, FALSE, 0);

  setBusy(false);

  gtk_widget_show_all(dialog);

  // now add other widgets, so they don't get show_all()
  //
  add(_("Input"), new InputGui(_data._input));
  add(_("Turn"), new TurnGui(_data._turn));
  add(_("Blur"), new BlurGui(_data._blur));
  add(_("Colour"), new RecolourGui(_data._recolour));
  add(_("Resize"), new ResizeGui(_data._resize));
  add(_("Crop"), new CropGui(_data._crop));
  add(_("Sharpen"), new SharpenGui(_data._sharpen));
  add(_("Rename"), new RenameGui(_data._rename));
  add(_("Output"), new OutputGui(_data._output));

  return dialog;
}

void
DbpGui::add(const std::string& label, Gui* gui) {
  _guis.push_back(gui);
  GtkWidget* pageLabel = gtk_label_new(label.c_str());
  gtk_notebook_append_page(GTK_NOTEBOOK(_notebook), gui->widget(), pageLabel);
}

void
DbpGui::setBusy(bool busy) {
  if (busy) {
    gtk_widget_set_sensitive(_test, FALSE);
    gtk_widget_set_sensitive(_start, FALSE);
    gtk_widget_set_sensitive(_cancel, TRUE);
  } else {
    gtk_widget_set_sensitive(_test, TRUE);
    gtk_widget_set_sensitive(_start, TRUE);
    gtk_widget_set_sensitive(_cancel, FALSE);
  }
}

gboolean
DbpGui::idleProcess(gpointer data) {
  DbpGui* gui = (DbpGui*)data;
  return gui->step() ? TRUE : FALSE;
}

void
DbpGui::test() {

  // need a test file...
  if (_data._input._files.empty()) {
    gtk_label_set_text(GTK_LABEL(_messageText), _("No file to test with!"));
    return;
  }

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(_show), TRUE);
  setBusy(true);
  _data.start(_data._input._files.front());
  _idleProcess = g_idle_add(&idleProcess, this);
}

void
DbpGui::start() {

  int n = _data.numExistingOutputFiles(_data._input._files);
  if (n > 0) {
    gtk_label_set_text(GTK_LABEL(_messageText), _("Output files exist, please delete them!"));
    return;
  }

  setBusy(true);
  _data.start(_data._input._files);
  _idleProcess = g_idle_add(&idleProcess, this);
}

bool
DbpGui::step() {
  if (_data.done()) {
    gtk_label_set_text(GTK_LABEL(_messageText), _("-- done --"));
    setBusy(false);
    return false;
  }
  gtk_label_set_text(GTK_LABEL(_messageText), _data.current().fullPath().c_str());
  gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(_progress), _data.progress());
  _data.step();
  return true;
}

void
DbpGui::cancel() {
  if (_idleProcess != 0) {
    g_source_remove(_idleProcess);
    _idleProcess = 0;
  }
  setBusy(false);
}

void
DbpGui::show() {
  _data.setVisible(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(_show)));
}

void
DbpGui::setProgressTitle(const char* c)
{
}

void
DbpGui::setProgressAmount(float f)
{
}

void
DbpGui::gimpProgressStartCallback(const gchar* message, gboolean cancellable, gpointer user_data)
{
  DbpGui* gui = static_cast<DbpGui*>(user_data);
  gui->setProgressTitle(message);
  gui->setProgressAmount(0.0);
}

void
DbpGui::gimpProgressEndCallback(gpointer user_data)
{
  DbpGui* gui = static_cast<DbpGui*>(user_data);
  gui->setProgressAmount(0.0);
}

void
DbpGui::gimpProgressTextCallback(const gchar* message, gpointer user_data)
{
  DbpGui* gui = static_cast<DbpGui*>(user_data);
  gui->setProgressTitle(message);
}

void
DbpGui::gimpProgressValueCallback(gdouble value, gpointer user_data)
{
  DbpGui* gui = static_cast<DbpGui*>(user_data);
  gui->setProgressAmount(value);
}

void
DbpGui::run()
{
  // Progress API was changed in 2.3.??
  // Assume no one's using early 2.3 releases by now...
#if (GIMP_MAJOR_VERSION == 2) && \
    (GIMP_MINOR_VERSION <= 2)

  const gchar* progressCallbacks = gimp_progress_install(
      &gimpProgressStartCallback,
      &gimpProgressEndCallback,
      &gimpProgressTextCallback,
      &gimpProgressValueCallback,
      this);

#else

  GimpProgressVtable vtable = {
      &gimpProgressStartCallback,
      &gimpProgressEndCallback,
      &gimpProgressTextCallback,
      &gimpProgressValueCallback,
      0, /* pulse */
      0, /* get_window */
      0, /* _gimp_reserved1 */
      0, /* _gimp_reserved2 */
      0, /* _gimp_reserved3 */
      0, /* _gimp_reserved4 */
      0, /* _gimp_reserved5 */
      0, /* _gimp_reserved6 */
      0, /* _gimp_reserved7 */
      0  /* _gimp_reserved8 */
  };
  const gchar* progressCallbacks =
      gimp_progress_install_vtable(&vtable, (gpointer)this);

#endif

  gtk_main();
  gimp_progress_uninstall(progressCallbacks);
}
