/*
    electricsheep - collaborative screensaver
    Copyright (C) 1999-2003 Scott Draves <source@electricsheep.org>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

static char *electricsheep_voter_c_id =
"@(#) $Id: electricsheep-voter.c,v 1.8 2005-07-11 07:22:17 spotspot Exp $";

/*
 * When an arrow key is pressed transmit a vote to the server.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/param.h>
#include "config.h"

char *client_version = "LNX_" VERSION;

/* instead of watching for X events, watch for
   griffin powermate button presses */
#define POWERMATE   0

/* instead of wathing for X events,
   just pop up logos */
#define FLYINGLOGOS 0


#if POWERMATE
#include <linux/input.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#elif !FLYINGLOGOS
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include "mpeg2dec/libvo/vroot.h"
#endif

char *nick, *url, *uid;
char *history_file;
char *dream_server;
char *leave_prefix;

char *splash_prefix = PACKAGE_DATA_DIR "/electricsheep";

void vote(int how) {
    FILE *hf;
    char sheep_name[1000];
    char vote_file_name[MAXPATHLEN];
    char pbuf[MAXPATHLEN];
    hf = fopen(history_file, "rb");
    if (NULL == hf) {
	perror(history_file);
	return;
    }
    if (NULL == fgets(sheep_name, 1000, hf)) {
	fprintf(stderr, "could not read sheep name from history file.\n");
	fclose(hf);
	return;
    }
    fclose(hf);
    if (strlen(sheep_name) > 1) {
	sheep_name[strlen(sheep_name)-1] = 0; /* chop newline */

	if (0 == fork()) {
	  sprintf(pbuf, "echo '!%s-%s.png 10 30 30' > "
		  "%soverlay_fifo", splash_prefix,
		  (how>0)?"smile":"frown", leave_prefix);
	  system(pbuf);
	  exit(0);
	}
	sprintf(pbuf, "curl 'http://%s/cgi/vote?id=%s&vote=%d&u=%s&v=%s' >& /dev/null",
		dream_server, sheep_name, how, uid, client_version);
	if (0 == fork()) {
	  system(pbuf);
	  exit(0);
	}
    } else {
	fprintf(stderr, "empty sheep name.\n");
    }
}

#if POWERMATE

#define NUM_VALID_PREFIXES 2

static const char *valid_prefix[NUM_VALID_PREFIXES] = {
  "Griffin PowerMate",
  "Griffin SoundKnob"
};

#define NUM_EVENT_DEVICES 16

int open_powermate(const char *dev, int mode)
{
  int fd = open(dev, mode);
  int i;
  char name[255];

  if(fd < 0){
    fprintf(stderr, "Unable to open \"%s\": %s\n", dev, strerror(errno));
    return -1;
  }

  if(ioctl(fd, EVIOCGNAME(sizeof(name)), name) < 0){
    fprintf(stderr, "\"%s\": EVIOCGNAME failed: %s\n", dev, strerror(errno));
    close(fd);
    return -1;
  }

  // it's the correct device if the prefix matches what we expect it to be:
  for(i=0; i<NUM_VALID_PREFIXES; i++)
    if(!strncasecmp(name, valid_prefix[i], strlen(valid_prefix[i])))
      return fd;

  close(fd);
  return -1;
}

int abs_offset = 0;

void process_event(struct input_event *ev)
{
#ifdef VERBOSE
  fprintf(stderr, "type=0x%04x, code=0x%04x, value=%d\n",
	  ev->type, ev->code, (int)ev->value);
#endif

  switch(ev->type){
  case EV_KEY:
    if (ev->value) vote(1);
    break;
  }
}

#define BUFFER_SIZE 32
void watch_powermate(int fd)
{
  struct input_event ibuffer[BUFFER_SIZE];
  int r, events, i;

  while(1){
    r = read(fd, ibuffer, sizeof(struct input_event) * BUFFER_SIZE);
    if( r > 0 ){
      events = r / sizeof(struct input_event);
      for(i=0; i<events; i++)
	process_event(&ibuffer[i]);
    }else{
      fprintf(stderr, "read() failed: %s\n", strerror(errno));
      return;
    }
  }
}
#endif

#if POWERMATE
int find_powermate(int mode)
{
  char devname[256];
  int i, r;

  for(i=0; i<NUM_EVENT_DEVICES; i++){
    sprintf(devname, "/dev/input/event%d", i);
    r = open_powermate(devname, mode);
    if(r >= 0)
      return r;
  }

  return -1;
}
#endif

void fly_logo(char *cmd) {
  char buf[MAXPATHLEN];
  sprintf(buf, "echo '%s' > %soverlay_fifo", cmd, leave_prefix);
  system(buf);
}
  

int main(int argc, char **argv) {
  int window;
#if POWERMATE
  int powermate = -1;
#elif !FLYINGLOGOS
  Display *display;
  XWindowAttributes xgwa;
#endif

  if (argc != 8) {
    printf("usage: electricsheep-voter window nick url "
	   "history_file dream_server leave_prefix uid\n");
    printf("%d\n", argc);
    return 0;
  }
  window = strtol(argv[1], NULL, 0);
  nick = argv[2];
  url = argv[3];
  history_file = argv[4];
  dream_server = argv[5];
  leave_prefix = argv[6];
  uid = argv[7];

#if POWERMATE
    

  powermate = find_powermate(O_RDONLY);


  if(powermate < 0){
    fprintf(stderr, "Unable to locate powermate\n");
    return 1;
  }

  watch_powermate(powermate);

  close(powermate);

#elif  FLYINGLOGOS
  while (1) {
    sleep(10);
    fly_logo("!/home/sheep/smile.ppm 30 60 30");
    sleep(10);
    fly_logo("!/home/sheep/frown.ppm 30 60 30");
  }
#else
  display = XOpenDisplay(NULL);
  if (NULL == display) {
    fprintf(stderr, "error: cannot open display.\n");
    return EXIT_FAILURE;
  }

  if (window == -3 || window == -2) {
    /* monitor the (virtual) root window */
    window = DefaultRootWindow (display);
  } else if (window == -1) {
    fprintf(stderr, "error: voting not allowed in separate window.\n");
    return EXIT_FAILURE;
  }

  XGetWindowAttributes(display, window, &xgwa);
  xgwa.your_event_mask = KeyPressMask;
  XSelectInput(display, window, xgwa.your_event_mask);

  while (1) {
    XEvent event;
    char s[2];
    int size;
    KeySym keysym;
    XNextEvent(display, &event);
    if (event.type != KeyPress) continue;
    size = XLookupString ((XKeyEvent *) &event, s, 1, &keysym, 0);
    switch (keysym) {
    case XK_Up:
      vote(1);
      break;
    case XK_Down:
      vote(-1);
      break;
    }
  }

#endif

  /* not reached */
  return EXIT_SUCCESS;
}
