/*
    electricsheep - a collaborative screensaver
    Copyright (C) 1999-2009 Spotworks LLC

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <limits.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>
#include <time.h>
#include <sys/wait.h>
#include <stdint.h>
#include <expat.h>

#include "electricsheep.h"
#include "config.h"

#include "md5.h"

extern int debug;

int in_message = 0;
int server_error = 0;
char server_error_type[MAXBUF];

char *dream_server = "v2d7c.sheepserver.net";
// char *dream_server = "test.sheepserver.net";
char *redirection_server = "community.sheepserver.net";
char *server;
FILE *logout = NULL;
char curl_cmd[MAXBUF];
char nick_buf[bufmax];
char url_buf[bufmax];

char *client_version = "LNX_" VERSION;
char *hide_stderr = "2> /dev/null";

void encode(char *dst, char *src) {
    static char *hex = "0123456789ABCDEF";
    char t;
    while ((t = *src++)) {
	if (isalnum(t)) {
	    *dst++ = t;
	} else {
	    *dst++ = '%';
	    *dst++ = hex[(t >> 4) & 15];
	    *dst++ = hex[t & 15];
	}
    }
}


char *encry(char *pw, char *nick) {
  static char *salt = "sh33p";
  unsigned char digest[16];
  char md5_pw[33];
  char *pw_salted = malloc(strlen(pw) + strlen(salt) + strlen(nick) + 1);
  int i, j;
  sprintf(pw_salted, "%s%s%s", pw, salt, nick);
  md5_buffer(pw_salted, strlen(pw_salted), digest);
  for (i = 0, j = 0; i < sizeof(digest); i++) {
    char *hex_digits = "0123456789ABCDEF";
    md5_pw[j++] = hex_digits[digest[i] >> 4];
    md5_pw[j++] = hex_digits[digest[i] & 0x0F];
  }
  md5_pw[j] = 0;
  free(pw);
  free(pw_salted);
  return strdup(md5_pw);
}

void rc_start_element(void *userData, const char *name, const char **atts) {
    prefs_t *prefs = (prefs_t *) userData;
    int i = 0;
    if (!strcmp("preferences", name) || !strcmp("electricsheep_preferences", name)) {
	while (atts[i]) {
	    if (!strcmp("nick", atts[i])) {
		free(prefs->nick);
		prefs->nick = strdup(atts[i+1]);
	    } else if (!strcmp("url", atts[i])) {
		free(prefs->url);
		prefs->url = strdup(atts[i+1]);
	    } else if (!strcmp("password", atts[i])) {
	        free(prefs->password);
		/* assume nick comes before pw in the xml XXX */
		prefs->password = encry(strdup(atts[i+1]), prefs->nick);
	    } else if (!strcmp("password_md5", atts[i])) {
	        free(prefs->password);
		prefs->password = strdup(atts[i+1]);
	    } else if (!strcmp("video_driver", atts[i])) {
		free(prefs->video_driver);
		prefs->video_driver = strdup(atts[i+1]);
	    } else if (!strcmp("uid", atts[i])) {
		free(prefs->uid);
		prefs->uid = strdup(atts[i+1]);
	    } else if (!strcmp("cache", atts[i])) {
		prefs->cache_size = atoi(atts[i+1]);
	    } else if (!strcmp("zoom", atts[i])) {
		prefs->zoom = atoi(atts[i+1]);
	    } else if (!strcmp("no_animation", atts[i])) {
		prefs->no_animation = atoi(atts[i+1]);
	    } else if (!strcmp("standalone", atts[i])) {
		prefs->standalone = atoi(atts[i+1]);
	    } else if (!strcmp("proxy", atts[i])) {
		prefs->proxy_name = strdup(atts[i+1]);
	    } else if (!strcmp("proxy_user", atts[i])) {
		prefs->proxy_user = strdup(atts[i+1]);
	    } else if (!strcmp("hide_errors", atts[i])) {
		prefs->hide_errors = atoi(atts[i+1]);
	    } else if (!strcmp("save_frames", atts[i])) {
		prefs->save_frames = atoi(atts[i+1]);
	    } else if (!strcmp("nrepeats", atts[i])) {
		prefs->nrepeats = atoi(atts[i+1]);
	    } else if (!strcmp("frame_rate", atts[i])) {
		prefs->frame_rate = atof(atts[i+1]);
	    } else if (!strcmp("play_evenly", atts[i])) {
		prefs->play_evenly = atof(atts[i+1]);
	    } else if (!strcmp("version", atts[i])) {
	      // nothing to check
	    } else {
		fprintf(logout, "ignoring attribute %s\n", atts[i]);
	    }
	    i += 2;
	}
    }
}

void set_rc_file(char *rc_file, int argc, char **argv) {
    char *hom;
    if (argc > 1 && !strcmp(argv[1], "--preferences")) {
	if (argc > 2)
	    strcpy(rc_file, argv[2]);
	else {
	    fprintf(logout,
		    "%s: --preferences option requires a file name argument.\n",
		    argv[0]);
	    exit(1);
	}
	return;
    } 
    hom = getenv("HOME");
    if (!hom) {
	fprintf(logout, "%s: HOME envar not defined\n", argv[0]);
	exit(1);
    }
    snprintf(rc_file, PATH_MAX, "%s/.electricsheep/preferences.xml", hom);
}

void write_rc(prefs_t *prefs, char *fn) {
    /* make sure if the file is created that
       it has mode 600 */
    FILE *fp;
    mode_t old_mask = umask(0);
    umask(0077);
    fp = fopen(fn, "w");
    umask(old_mask);
    if (NULL == fp) {
	perror(fn);
	exit(1);
    }
    fprintf(fp, "<electricsheep_preferences\n");
    fprintf(fp, " version=\"%s\"\n", client_version);
    fprintf(fp, " nick=\"%s\"\n", prefs->nick);
    fprintf(fp, " url=\"%s\"\n", prefs->url);
    fprintf(fp, " password_md5=\"%s\"\n", prefs->password);
    fprintf(fp, " cache=\"%d\"\n", prefs->cache_size);
    fprintf(fp, " nrepeats=\"%d\"\n", prefs->nrepeats);
    fprintf(fp, " frame_rate=\"%g\"\n", prefs->frame_rate);
    fprintf(fp, " play_evenly=\"%g\"\n", prefs->play_evenly);
    fprintf(fp, " uid=\"%s\"\n", prefs->uid);
    fprintf(fp, " zoom=\"%d\"\n", prefs->zoom);
    fprintf(fp, " video_driver=\"%s\"\n", prefs->video_driver);
    fprintf(fp, " no_animation=\"%d\"\n", prefs->no_animation);
    fprintf(fp, " standalone=\"%d\"\n", prefs->standalone);
    fprintf(fp, " hide_errors=\"%d\"\n", prefs->hide_errors);
    fprintf(fp, " save_frames=\"%d\"\n", prefs->save_frames);
    if (prefs->proxy_name)
	fprintf(fp, " proxy=\"%s\"\n", prefs->proxy_name);
    if (prefs->proxy_user)
	fprintf(fp, " proxy_user=\"%s\"\n", prefs->proxy_user);
    fprintf(fp, "/>\n");
    fclose(fp);
}

/* 64 random bits encoded as ascii */
void make_uniqueid(prefs_t *prefs) {
  static char *rdevice = "/dev/urandom";
  uint32_t d[2];
  int rfd;
  struct timeval tv;
  char ub[17];

  if (debug) printf("setting unique id.\n");
  rfd = open(rdevice, 0);
  if (-1 == rfd) {
    perror(rdevice);
    exit(1);
  }
  if (8 != read(rfd, (void *) d, 8)) {
    perror(rdevice);
    exit(1);
  }
  if (-1 == gettimeofday(&tv, NULL)) {
    perror("gettimeofday");
    exit(1);
  }
  d[0] ^= tv.tv_sec;
  d[1] ^= tv.tv_usec;
  snprintf(ub, 17, "%08lX%08lX",
	   (long unsigned int) d[0],
	   (long unsigned int) d[1]);
  free(prefs->uid);
  prefs->uid = strdup(ub);
}

void default_rc(prefs_t *prefs) {
    free(prefs->nick);
    prefs->nick = strdup("");
    free(prefs->url);
    prefs->url = strdup("");
    free(prefs->password);
    prefs->password = strdup("");
    prefs->cache_size = 2000;
    free(prefs->video_driver);
    prefs->video_driver = strdup("");
    make_uniqueid(prefs);
    prefs->frame_rate = 23.0;
    prefs->nrepeats = 2;
    prefs->play_evenly = 1.0;
    prefs->zoom = 1;
    prefs->no_animation = 0;
    prefs->standalone = 0;
    prefs->hide_errors = 0;
    prefs->proxy_name = 0;
    prefs->proxy_user = 0;
    prefs->save_frames = 0;
}

/* from the system(3) manpage */
int interruptable_system(char *command) {
    int pid, status;

    if (command == 0)
	return 1;
    pid = fork();
    if (pid == -1)
	return -1;
    if (pid == 0) {
	char *argv[4];
	argv[0] = "sh";
	argv[1] = "-c";
	argv[2] = command;
	argv[3] = 0;
	execv("/bin/sh", argv);
	exit(127);
    }
    do {
	if (waitpid(pid, &status, 0) == -1) {
	    if (EINTR == errno)
		cleanup_and_exit(0);
	} else
	    return status;
    } while(1);
    // notreached
}

int mysystem(char *cmd, char *msg) {
    int n;
    if (0) fprintf(logout, "subprocess; (%s)\n", cmd);
    if (0 != (n = interruptable_system(cmd))) {
	if (SIGINT != n) {
	    if (!prefs.hide_errors)
		fprintf(logout, "subprocess error: %s, %d=%d<<8+%d\n",
			msg, n, n>>8, n&255);
	    return 1;
	}
	fprintf(logout, "control-c during %s, exiting\n", msg);
	cleanup_and_exit(1);
    }
    return 0;
}

#define XML_BUFF_SIZE 4000

void read_rc(prefs_t *prefs, char *fn) {
    FILE *fp;
    XML_Parser parser;
    parser = XML_ParserCreate(NULL);
    XML_SetUserData(parser, prefs);
    XML_SetElementHandler(parser, rc_start_element, NULL);

    fp = fopen(fn, "r");

    if (NULL == fp) {
	char *last_slash;
	default_rc(prefs);
	last_slash = strrchr(fn, '/');
	if (last_slash) {
	    char b[PATH_MAX + 100];
	    strcpy(b, "mkdir -p ");
	    strncat(b, fn, last_slash - fn);
	    mysystem(b, "mkdir for preferences");
	}
	
	write_rc(prefs, fn);
	return;
    }

    while (1) {
	int bytes_read;
	void *buff = XML_GetBuffer(parser, XML_BUFF_SIZE);
	if (buff == NULL) {
	    fprintf(logout, "unable to allocate buffer.\n");
	    exit(1);
	}

	bytes_read = fread(buff, 1, XML_BUFF_SIZE, fp);
	if (bytes_read == 0)
	    break;

	if (! XML_ParseBuffer(parser, bytes_read, bytes_read == 0)) {
	    fprintf(logout, "parse error.\n");
	    exit(1);
	}
    }

    XML_ParserFree(parser);
}

void get_end_element(void *userData, const char *name) {
    if (!strcmp("message", name)) {
	in_message = 0;
    }
}

void character_handler(void *userData, const XML_Char *s, int len) {
    if (in_message) {
	if (len != fwrite(s, sizeof(XML_Char), len, logout)) {
	    perror("writing xml message");
	}
    }
}

void query_start_element(void *userData, const char *name, const char **atts) {
    int i = 0;
    if (!strcmp("redir", name)) {
	while (atts[i]) {
	    if (!strcmp(atts[i], "host")) {
	      char *end;
	      const char *h = atts[i+1];
	      if (!strncmp("http://", h, 7)) {
		dream_server = strdup(h+7);
	      } else {
	        dream_server = strdup(h);
	      }
	      if (end = strrchr(dream_server, '/')) {
		*end = 0;
	      }
	      if (debug) fprintf(logout, "set server to %s\n", dream_server);
	    }
	    i += 2;
	}
    } else if (!strcmp("message", name)) {
	in_message = 1;
    } else if (!strcmp("error", name)) {
	server_error = 1;
	while (atts[i]) {
	    if (!strcmp(atts[i], "type")) {
		strncpy(server_error_type, atts[i+1], MAXBUF);
	    }
	    i += 2;
	}
    }
}

char *query_redirection() {
  char pbuf[MAXBUF];
  FILE *lf;
  int done;
  XML_Parser parser;

  parser = XML_ParserCreate(NULL);
  XML_SetElementHandler(parser, query_start_element, get_end_element);
  XML_SetCharacterDataHandler(parser, character_handler);
  in_message = 0;
  server_error = 0;

  /* XXX curl_cmd should not have use any auth */
  snprintf(pbuf, MAXBUF, "%s 'http://%s/query.php?q=redir&u=%s&p=%s&v=%s&i=%s'",
	   curl_cmd, redirection_server, nick_buf, prefs.password, client_version, prefs.uid);

  if (debug) fprintf(logout, "query %s\n", pbuf);

  lf = popen(pbuf, "r");

  if (NULL == lf) {
    perror("could not fork/pipe\n");
    cleanup_and_exit(1);
  }

  do {
    size_t len = fread(pbuf, 1, MAXBUF, lf);
    done = len < MAXBUF;
    if (0 == len) {
      // lost contact, no data to parse
      break;
    }
    if (!XML_Parse(parser, pbuf, len, done)) {
      fprintf(logout, "%s at line %u\n",
	      XML_ErrorString(XML_GetErrorCode(parser)),
	      (unsigned)XML_GetCurrentLineNumber(parser));
      break;
    }
    if (debug > 1) fprintf(logout, "query read loop len=%d\n", len);
  } while (!done);
  XML_ParserFree(parser);

  pclose(lf);

  if (server_error) {
    fprintf(logout,
	    "server reported error for query: %s\n",
	    server_error_type);
  }
}

void init_curl_cmd(int registration) {

  if (debug) fprintf(logout, "init_curl_cmd %d\n", registration);

    if (prefs.proxy_name) {
	snprintf(curl_cmd, MAXBUF, "nice -n %d curl --location --proxy %s",
		 nice_level, prefs.proxy_name);
	if (prefs.proxy_user) {
	    strcat(curl_cmd, " --proxy-user ");
	    strcat(curl_cmd, prefs.proxy_user);
	}
    } else
	snprintf(curl_cmd, MAXBUF, "nice -n %d curl --location", nice_level);
    strcat(curl_cmd, " --silent");
    if (!prefs.hide_errors) {
	strcat(curl_cmd, " --show-error");
    }
    if (registration && prefs.password && prefs.password[0]) {
	query_redirection();
	strcat(curl_cmd, " --basic --user \'");
	strcat(curl_cmd, prefs.nick);
	strcat(curl_cmd, ":");
	strcat(curl_cmd, prefs.password);
	strcat(curl_cmd, "\'");
	if (debug)
	    fprintf(logout, "using authentication, pw=%s nick=%s\n",
		    prefs.password, prefs.nick);
	if (strlen(prefs.password)*3 > bufmax-3) {
	    fprintf(logout, "password too long.");
	    cleanup_and_exit(1);
	}
    }
    server = dream_server;
    if (debug) fprintf(logout, "curl_cmd = %s\nserver = %s\n", curl_cmd, server);
}

void init_list_cmd (char *buf) {
    snprintf(buf, MAXBUF, "%s 'http://%s/cgi/list?v=%s&u=%s'"
	    "| gunzip -c %s", curl_cmd, server,
	    client_version, prefs.uid, hide_stderr);
}
