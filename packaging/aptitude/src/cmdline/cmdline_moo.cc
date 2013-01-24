// cmdline_moo.cc
//
//   Copyright 2004 Daniel Burrows

#include "cmdline_moo.h"

#include <aptitude.h>

#include <stdio.h>

int cmdline_moo(int argc, char *argv[], int verbose)
{
  switch(verbose)
    {
    case 0:
      printf(_("There are no Easter Eggs in this program.\n"));
      break;
    case 1:
      printf(_("There really are no Easter Eggs in this program.\n"));
      break;
    case 2:
      printf(_("Didn't I already tell you that there are no Easter Eggs in this program?\n"));
      break;
    case 3:
      printf(_("Stop it!\n"));
      break;
    case 4:
      printf(_("Okay, okay, if I give you an Easter Egg, will you go away?\n"));
      break;
    case 5:
      printf(_("All right, you win.\n"));
      printf("\n");
      printf("                               /----\\\n");
      printf("                       -------/      \\\n");
      printf("                      /               \\\n");
      printf("                     /                |\n");
      printf("   -----------------/                  --------\\\n");
      printf("   ----------------------------------------------\n");
      break;

    default:
      printf(_("What is it?  It's an elephant being eaten by a snake, of course.\n"));
      break;
    }

  return 0;
}
