// board.h            -*-c++-*-
//
//  Copyright 2000 Daniel Burrows
//

#ifndef BOARD_H
#define BOARD_H

#include <string>
#include <iostream>
#include <cwidget/generic/util/eassert.h>
#include <time.h>

/** \brief Contains the information and routines necessary to play a game of
 *  minesweeper
 * 
 *  \file board.h
 */

class mine_board
// A 'dumb' class that just contains information necessary to manipulate the
// game board.  Quick note: this doesn't contain any bounds-checking; the
// caller should do that.  (this isn't really the best way to do it but I
// don't think it's a critical issue)
{
public:
  struct board_entry
  // Making this a class would be total overkill.  Just tracks information
  // about a square.  (note that the state /could/ be contained in a single
  // char, using the same format as for the save files; I prefer decomposing
  // it this way)
  //
  // (of course, structs and classes are really the same, and I gave it a 
  // default constructor..hmmm..)
  {
    bool contains_mine;
    // True if the square contains a mine

    bool flagged;
    // True if it's been flagged by the player
    bool uncovered;
    // True if it's been uncovered by the player

    int adjacent_mines;
    // The number of mines in adjacent squares.

    board_entry():
      contains_mine(false), flagged(false), uncovered(false), adjacent_mines(0)
    {}
  };

  enum game_state {playing, won, lost};
private:
  board_entry *board;
  // The actual board.  Access via get_entry() is a Good Idea; C++ doesn't
  // handle dynamically allocated two-dimensional arrays in an automated
  // fashion.

  int width, height;
  // The size of the board.
  int covered_squares;
  // The number of squares which are still covered.
  int mines,flags;
  // The number of mines and flags on the board.
  // It would be obvious to store the number of unflagged mines instead of
  // the number of flags, but that makes things more complicated, since
  // updating the number of flags requires knowledge of how many mines there
  // are (in particular, loading games is more difficult, not much but a
  // little) -- besides which, this is just cleaner.

  game_state state;
  int minex, miney;
  // If the player has lost, these are the coordinates of the mine that did
  // him or her in.

  time_t starttime;
  // When the game started.
  time_t endtime;
  // How long it took for the game to end (if the game is over, of course)

  void seed_square(int x, int y);
  // Adds a mine to the given square, updating counters as needed.  Used to
  // generate the board.

  board_entry &get_entry(int x, int y) {return board[x+width*y];}

  void init_board(int _width, int _height, int _mines);
  // Used by the constructors..shouldn't be called after the board is
  // initialized (which is much of why it's private)
public:
  mine_board(int _width, int _height, int _mines)
  {
    init_board(_width, _height, _mines);
  }
  // Creates a new, random board with the given width, height, and number of
  // mines
  mine_board(int _width, int _height)
  {
    init_board(_width, _height, _width*_height/5);
    // Use a density of 1 mine per 5 squares
  }

  mine_board():board(NULL), width(0), height(0), covered_squares(0), mines(0), flags(0), state(playing) {}
  // Creates an empty board (used to load a game, but handling parse errors
  // inside a constructor is a major pain) -- if this constructor is used, the
  // load() method should be called before anything else is done.

  bool load(std::istream &s);
  // Loads a game from the given stream.
  void print(std::ostream &o);
  // Prints just the board to the given stream (without the save-game header)
  void save(std::ostream &o)
  {
    o<<width<<" "<<height<<" "<<mines<<" "<<mines-flags<<" "<<get_duration()<<std::endl;
    print(o);
  }

  // Player actions:

  void uncover(int x, int y);
  // Uncovers a given square
  void sweep(int x, int y);
  // Sweeps around a given square if it is uncovered and the number of adjacent
  // flags equals the number of adjacent mines.  (say that 5 times fast!)

  bool flag(int x, int y);
  // Adds a flag at the given square.  Returns true if successful, false if
  // it wasn't possible.
  bool unflag(int x, int y);
  // Removes a flag at the given square
  bool toggle_flag(int x, int y)
  {
    if(state!=playing)
      return false;
    return get_entry(x, y).flagged?unflag(x, y):flag(x, y);
  }

  // Query functions:

  int get_width() {return width;}
  int get_height() {return height;}
  game_state get_state() {return state;}
  int get_minex() {eassert(state==lost); return minex;}
  int get_miney() {eassert(state==lost); return miney;}
  double get_duration()
  {
    if(state==playing)
      return difftime(time(0), starttime);
    else
      return difftime(endtime, starttime);
  }
  int get_nummines() {return mines;}
  int get_numflags() {return flags;}

  const board_entry &get_square(int x, int y) {return board[x+y*width];}
  // Like the internal get_entry(), but returns a const reference (to
  // discourage evil people doing stuff they shouldn't (although it's still
  // possible with casting))

  ~mine_board() {delete[] board;}
};

// Helper routines to create hardcoded difficulty levels
inline mine_board *easy_game() {return new mine_board(8, 8, 10);}
inline mine_board *intermediate_game() {return new mine_board(16, 16, 40);}
inline mine_board *hard_game() {return new mine_board(16, 30, 100);}

#endif
