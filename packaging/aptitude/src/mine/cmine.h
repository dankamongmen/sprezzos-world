// cmine.h     -*-c++-*-
//
//  Copyright 2000 Daniel Burrows
//

#ifndef CMINE_H
#define CMINE_H

#include "board.h"

#include <cwidget/widgets/minibuf_win.h>
#include <cwidget/widgets/editline.h>

/** \brief A Curses interface to Minesweeper, using the vscreen architecture
 * 
 * 
 *  A Curses interface to Minesweeper, using the vscreen architecture.  The
 *  idea is that it'll be possible to drop this into a vscreen-using program
 *  and have it Just Work.  Currently vscreen is (aside from the manually
 *  extracted code in this tree) only used in aptitude, but if I ever get
 *  around to breaking it into its own library, this'll be useful.  Also, as
 *  Caldera demonstrated, playing games while your packages are downloading has
 *  a high reading on the Cool Useless Feature Scale :-)
 *  
 *  (that's also the reason that there's room made for some stuff that's not
 *  really accessible from the version I'm turning in -- in particular,
 *  configuration of colors and keybindings.  To make use of them you really
 *  need a configuration file, and I don't want to write a parser here..aptitude
 *  just piggybacks on the apt configuration, so I don't really have a parser
 *  from that that I can borrow either..hmmm..I think the apt parser is
 *  public-domain, but I'm not sure how that interacts with our collaboration
 *  policy :) )
 * 
 * \file cmine.h
 */

namespace cwidget
{
  namespace widgets
  {
    class radiogroup;
  }

  namespace config
  {
    class keybindings;
  }
}

class cmine:public cwidget::widgets::widget
{
  //cwidget::widgets::multiplex *status_multiplex;
  mine_board *board;
  int curx, cury; // Cursor location
  int basex, basey; // Where the board starts relative to the window.
  int prevwidth, prevheight;
  // The width and height last time we adjusted basex,basey..needed at the
  // moment, I'm looking for a cleaner solution.
  int timeout_num;

  static cwidget::widgets::editline::history_list load_history, save_history;

  void set_board(mine_board *_board);
  // Deletes the old board, and replaces it with the new one.  cur[xy] and
  // base[xy] are set so that the board appears in the center of the screen,
  // with the cursor approxiomately in its center.
  void paint_square(int x, int y, const cwidget::style &st);

  void checkend();
  // Checks whether the game is over, prints cute message if so.

  class update_header_event;
  friend class update_header_event;

  void update_header();
  // Updates the displayed header information (time, etc)

  void do_load_game(std::wstring s);
  void do_save_game(std::wstring s);
  void do_new_game();
  void do_continue_new_game(bool start,
			    cwidget::widgets::widget &w,
			    cwidget::widgets::radiogroup *grp);
  void do_custom_game();
  void do_start_custom_game(cwidget::widgets::widget &w,
			    cwidget::widgets::editline &heightedit,
			    cwidget::widgets::editline &widthedit,
			    cwidget::widgets::editline &minesedit);
protected:
  void paint_header(const cwidget::style &st);

  cmine();
public:
  static cwidget::util::ref_ptr<cmine> create()
  {
    cwidget::util::ref_ptr<cmine> rval(new cmine);
    rval->decref();
    return rval;
  }

  bool handle_key(const cwidget::config::key &k);
  void paint(const cwidget::style &st);
  ~cmine();

  int width_request();
  int height_request(int w);

  bool get_cursorvisible() {return false;}
  cwidget::widgets::point get_cursorloc() {return cwidget::widgets::point(0,0);}
  bool focus_me() {return true;}

  static cwidget::config::keybindings *bindings;
  static void init_bindings();
};

#endif
