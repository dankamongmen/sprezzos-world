/*
    electricsheep - a collaborative screensaver
    Copyright (C) 1999-2008 Spotworks LLC

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

*/


typedef struct {
    char *nick;
    char *url;
    char *password;
    int cache_size;
    char *uid;
    char *video_driver;
    double frame_rate;
    int nrepeats;
    double play_evenly;
    int zoom;
    int no_animation;
    int standalone;
    int hide_errors;
    char *proxy_name;
    char *proxy_user;
    int save_frames;
} prefs_t;

#define MAXBUF (5*PATH_MAX)
#define bufmax 300

extern prefs_t prefs;
extern char curl_cmd[MAXBUF];
extern char nick_buf[bufmax];
extern char url_buf[bufmax];
extern FILE *logout;
/*
extern char *dream_server;
extern char *registered_server;
*/
extern char *server;
extern char *client_version;
extern char *hide_stderr;

// by default xscreensaver starts us at priority 10.
// everything but the avi decoder runs at that (10) plus this:
#define nice_level 10

extern int in_message;
extern int server_error;
extern char server_error_type[];

void set_rc_file(char *rc_file, int argc, char **argv);
void read_rc(prefs_t *prefs, char *file_name);
void write_rc(prefs_t *prefs, char *file_name);
void default_rc(prefs_t *prefs);
int mysystem(char *cmd, char *msg);
int interruptable_system(char *command);
void cleanup_and_exit(int status);
void encode(char *dst, char *src);
void init_curl_cmd(int registration);
void init_list_cmd (char *buf);
void get_end_element(void *userData, const char *name);
void character_handler(void *userData, const XML_Char *s, int len);
char *encry(char *pw, char *nick);
