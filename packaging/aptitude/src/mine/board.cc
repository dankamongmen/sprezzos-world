// board.cc
//
//  Copyright 2000 Daniel Burrows
//
//  Definitions for the Minesweeper board (from board.h)

#include "board.h"

#include <stdlib.h>

using namespace std;

int rngrand(int max)
  // Returns a random number between 0 and max-1
{
  return (int) ((float) rand()*max/RAND_MAX);
}

void mine_board::init_board(int _width, int _height, int _mines)
{
  width=_width;
  height=_height;
  covered_squares=_width*_height;
  mines=0;
  flags=0;
  state=playing;

  if(_mines>width*height)
    _mines=width*height;

  // Allocate a (blank) board
  board=new board_entry[width*height];

  for(int i=0; i<_mines; i++)
    {
      int x,y;
      do
	{
	  x=rngrand(_width);
	  y=rngrand(_height);
	} while(get_entry(x, y).contains_mine);
      seed_square(x, y);
    }

  starttime=time(0);
  eassert(mines==_mines);
}

void mine_board::seed_square(int x, int y)
  // This could be an ugly routine with 9 cases.  I've decided to do something
  // slightly less efficient but quicker to write..you could argue it's more
  // readable and/or elegant, but really I'm just lazy ;-)
  // Anyway, I don't think this will be a speed bottleneck, so a sloppy version
  // is probably fine
{
  eassert(!get_entry(x, y).contains_mine);
  for(int i=x-1; i<=x+1; i++)
    if(i>=0 && i<width)
      for(int j=y-1; j<=y+1; j++)
	if(j>=0 && j<height)
	  {
	    if(!(i==x && j==y)) // exclude the central square
	      get_entry(i, j).adjacent_mines++;
	  }
  get_entry(x, y).contains_mine=true;
  mines++;
}

bool mine_board::load(istream &s)
  // Performs some consistency checks (eg, calculates the number of flags
  // independently of the value stored in the header) -- should check the
  // number of mines around revealed squares but that's too much of a pain..
{
  int _total_mines;
  int _remaining_mines,_flags;
  int _duration;

  s>>width>>height>>_total_mines>>_remaining_mines>>_duration;
  time(&starttime);
  starttime-=_duration;

  _flags=_total_mines-_remaining_mines;

  flags=mines=0;
  covered_squares=width*height;
  state=playing;

  if(s.fail())
    return false;

  delete[] board; // Just in case.
  board=new board_entry[width*height];

  for(int y=0; y<height; y++)
    for(int x=0; x<width; x++)
      {
	char c;
	s>>c;
	if(s.fail())
	  {
	    delete[] board;
	    board=NULL;
	    return false;
	  }

	switch(c)
	  {
	  case '+':
	    break;
	  case '*':
	    seed_square(x, y);
	    break;
	  case 'f':
	    get_entry(x, y).flagged=true;
	    flags++;
	    break;
	  case 'F':
	    get_entry(x, y).flagged=true;
	    flags++;
	    seed_square(x, y);
	    // This makes sure that the counters are properly updated.
	    break;
	  default:
	    if(c>='0' && c<='9')
	      {
		get_entry(x, y).uncovered=true;
		covered_squares--;
	      }
	    else
	      {
		delete[] board;
		board=NULL;
		return false;
	      }
	  }
      }

  if(flags!=_flags ||
     mines!=_total_mines)
    {
      delete[] board;
      board=NULL;
      return false;
    }

  return true;
}

void mine_board::print(ostream &o)
{
  for(int y=0; y<height; y++)
    for(int x=0; x<width; x++)
      {
	if(get_entry(x, y).flagged)
	  {
	    if(get_entry(x, y).contains_mine)
	      o<<"F";
	    else
	      o<<"f";
	  }
	else if(!get_entry(x, y).uncovered)
	  {
	    if(get_entry(x, y).contains_mine)
	      o<<"*";
	    else
	      o<<"+";
	  }
	else
	  o<<get_entry(x, y).adjacent_mines;

	if(x<height-1)
	  o<<" ";
	else
	  o<<endl;
      }
}

void mine_board::uncover(int x, int y)
{
  if(state==playing && !get_entry(x, y).flagged && !get_entry(x, y).uncovered)
    {
      get_entry(x, y).uncovered=true;
      covered_squares--;
      if(get_entry(x, y).contains_mine)
	{
	  state=lost;
	  minex=x;
	  miney=y;
	  endtime=time(0);
	}
      else if(get_entry(x, y).adjacent_mines==0)
	sweep(x, y);
    }
  if(state==playing && covered_squares==mines)
    {
      state=won;
      endtime=time(0);
    }
}

void mine_board::sweep(int x, int y)
{
  if(state==playing && get_entry(x, y).uncovered)
    {
      // Calculate the number of flags adjacent to the square
      int nflags=0;
      // Same iteration as in seed_square
      for(int i=x-1; i<=x+1; i++)
	if(i>=0 && i<width)
	  for(int j=y-1; j<=y+1; j++)
	    if(j>=0 && j<height && !(i==x && j==y))
	      if(get_entry(i, j).flagged)
		nflags++;
      if(nflags==get_entry(x,y).adjacent_mines)
	// Ok, we should sweep around the square
	{
	  for(int i=x-1; i<=x+1; i++)
	    if(i>=0 && i<width)
	      for(int j=y-1; j<=y+1; j++)
		if(j>=0 && j<height && !(i==x && j==y))
		  uncover(i, j);
	}
    }
}

bool mine_board::flag(int x, int y)
{
  if(state==playing && !get_entry(x, y).flagged && !get_entry(x, y).uncovered)
    {
      get_entry(x, y).flagged=true;
      flags++;
      return true;
    }
  else
    return false;
}

bool mine_board::unflag(int x, int y)
{
  if(state==playing && flags>0 && get_entry(x, y).flagged && !get_entry(x, y).uncovered)
    {
      get_entry(x, y).flagged=false;
      flags--;
      return true;
    }
  else
    return false;
}
