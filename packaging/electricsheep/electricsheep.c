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
#include <unistd.h>
#include <ctype.h>
#include <math.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <signal.h>
#include <time.h>
#include <utime.h>
#include <dirent.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/file.h>
#include <fcntl.h>
#include <expat.h>

#include "electricsheep.h"
#include "config.h"
#include "getdate.h"
/* for old versions of ubuntu xxx should use autoconf
#include "ffmpeg/avformat.h"
*/
#include "libavformat/avformat.h" 

#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif

#ifdef HAVE_SYS_STATVFS_H
#define STATFS statvfs
#else
#define STATFS statfs
#endif


prefs_t prefs;

char *leave_prefix = NULL;

int min_megabytes = 0;

/* update cache every 1000 sheep =~ 90 minutes in addition to when new
   downloads are found */
int cache_update_delay = 1000;

/* wait this long before going to work */
#if 1
/* production */
int init_delay = 300;
int init_delay_list = 30;
int list_freshness = 600;
#else
/* debug */
int init_delay = 2;
int init_delay_list = 1;
int list_freshness = 60;
#endif

#define copy_buffer_size 64000

double protected_flock_fraction = 0.2;

int debug = 0;
int stats = 0;

int nrestarts = 0;
int nplays = 0;
int nloopplays = 0;

int bracket_begin_id = -1;
int bracket_end_id = -1;

time_t simulated_time;

/* bug: perror does not go to the log */
char *logfile = 0;

time_t bracket_begin_time = (time_t)(-1);
time_t bracket_end_time = (time_t)(-1);

int nthreads = -1;

// cheeze XXX
#define max_cp_size 300000

char *vote_prog = "electricsheep-voter";

char cps_name[PATH_MAX] = "";
char jpg_name[PATH_MAX] = "";
char avi_name[PATH_MAX] = "";
char fifo_name[PATH_MAX] = "";


typedef struct {
    int history_size;
    int *path_history;
    int last_sheep;
    int nrepeated;
    int max_repeats;
    int reset_fuse;
    int reset_fuse_length;
} playback_t;

playback_t playback;

AVFormatContext *output_ctx = NULL;

char *window_id = NULL;
int on_root = 0;
int timeout = 401;
int tryagain = 696;
int get_tryagain = 696;


int parasite = 0;
int read_only = 0;

pid_t displayer_pid = 0;
pid_t downloader_pid = 0;
pid_t decoder_pid = 0;
pid_t ui_pid = 0;


FILE *avi_pipe = NULL;

int current_anim_id = -1;
int start_id = -1;

int generation = -1;

#define max_url_length 1000

typedef struct {
    int generation;
    int id;
    int deleted;
    int readonly;
    int type;
    time_t mtime;
    time_t atime;
    double rank;
    int nplays;
    int size;
    int rating;
    int first;
    int last;
    char path[PATH_MAX];
    char url[max_url_length];
} sheep_t;

int nserver_sheep, ncached_sheep;
sheep_t *server_sheep = NULL;
sheep_t *cached_sheep = NULL;

char *thread_name = NULL;

char monitoraspect[30];

void make_display_process();

FILE *play_count_file = NULL;
void flush_counts();
void print_count_standard_deviation();

void print_stats() {
    fprintf(logout, "nplays = %d, ", nplays);
    fprintf(logout, "nloopplays = %d, ", nloopplays);
    fprintf(logout, "nrestarts = %d, ", nrestarts);
    fprintf(logout, "ncached_sheep = %d\n", ncached_sheep);
    if (nplays)
	fprintf(logout, "restart_rate = %g\n", nrestarts / (double)nplays);

    print_count_standard_deviation();
}

void cleanup() {
    if (debug) fprintf(logout, "cleanup.\n");

    if (debug && !strcmp(thread_name, "display"))
	print_stats();

    if (play_count_file) flush_counts();

    if (avi_name[0]) unlink(avi_name);
    if (cps_name[0]) unlink(cps_name);
    if (jpg_name[0]) unlink(jpg_name);
    unlink(fifo_name);
}

void handle_sig_term(int sig) {
    if (debug)
	fprintf(logout, "handle_sig_term %s %d\n",
	       (thread_name ? thread_name : "(nyet)"), sig);

    cleanup();

    /* remove handler to prevent infinite loop */
    signal(SIGTERM, SIG_DFL);

    /* terminate process group, ie self & all children */
    kill(0, SIGTERM);
}

void cleanup_and_exit(int status) {
    if (debug) fprintf(logout, "cleanup_and_exit %d\n", status);
    handle_sig_term(0);
}




/* not fatal if subprocess fails */
void mysystem2(char *cmd, char *msg) {
    int n;
    if (0) fprintf(logout, "subprocess; (%s)\n", cmd);
    if (0 != (n = interruptable_system(cmd))) {
	if (SIGINT != n)
	    fprintf(logout, "subprocess failure: %s, %d=%d<<8+%d\n",
		    msg, n, n>>8, n&255);
	else
	    fprintf(logout, "control-c during %s\n", msg);
    }
}

void timestamp(char *prefix) {
  time_t t;
  prefix = prefix ? prefix : "";
  if (debug) {
      time(&t);
      fprintf(logout, "time %s %s", prefix, ctime(&t));
  }
}



void print_sheep(char *name, sheep_t *an, int nanims) {
    int i;
    fprintf(logout, "print_sheep %s=%d\n", name, nanims);
    char bb[100];
    for (i = 0; i < nanims; i++) {
	strcpy(bb, ctime(&an[i].mtime));
	bb[24] = 0;
	fprintf(logout, "[%05d] gen=%d id=%d deleted=%d readonly=%d "
	       "type=%d mtime=%d atime=%d size=%d rating=%d nplays=%d rank=%g"
	       "first=%d last=%d path=%s url=%s mtime=%s atime=%s",
	       i, an[i].generation,
	       an[i].id, an[i].deleted, an[i].readonly, an[i].type,
	       (int)an[i].mtime, (int)an[i].atime, an[i].size,
		an[i].rating, an[i].nplays, an[i].rank, an[i].first,
		an[i].last, an[i].path, an[i].url, bb, ctime(&an[i].atime));
    }
}

int filename_is_avi(char *name) {
    int n = strlen(name);
    return !(n <= 4 || 0 != strcmp(&name[n-4], ".avi"));
}

int filename_is_xxx(char *name) {
    int n = strlen(name);
    return !(n <= 4 || 0 != strcmp(&name[n-4], ".xxx"));
}

void touch_file(char *name) {
    char tbuf[MAXBUF];
    FILE *touch;
    snprintf(tbuf, MAXBUF, "%s%s", leave_prefix, name);
    touch = fopen(tbuf, "w");
    if (NULL == touch) {
	perror(tbuf);
	exit(1);
    }
    fclose(touch);
}

int zap_download_marker() {
    char dbuf[MAXBUF];
    FILE *mfp;
    snprintf(dbuf, MAXBUF, "%s%s", leave_prefix, "downloaded");
    mfp = fopen(dbuf, "r");
    if (NULL == mfp) {
	return 0;
    }
    fclose(mfp);
    if (debug) fprintf(logout, "zapping %s\n", dbuf);
    unlink(dbuf);
    return 1;
}

int bad_sheep(int idx) {
    return
	cached_sheep[idx].deleted
	|| (0 == cached_sheep[idx].size);
}

void update_cached_sheep_path(char *path) {
    DIR *d;
    struct dirent *e;
    struct stat sbuf;
    char fbuf[MAXBUF];

    if (debug) fprintf(logout, "updating cache path=%s\n", path);

    d = opendir(path);
    if (!d) {
	perror(path);
	cleanup_and_exit(1);
    }
    while ((e = readdir(d))) {
	sheep_t *an = &cached_sheep[ncached_sheep];
	int i;
	if (e->d_name[0] == '.') continue;
	/* what happens if there's an xxx file and an avi file with the
	   same name/numbers? */
	if (filename_is_xxx(e->d_name)) {
	    if (4 != sscanf(e->d_name, "%d=%d=%d=%d.xxx",
			    &an->generation, &an->id,
			    &an->first, &an->last)) {
		continue;
	    }
	    an->deleted = 1;
	    for (i = 0; i < nserver_sheep; i++) {
		if (server_sheep[i].id == an->id) {
		    break;
		}
	    }
	    if (i == nserver_sheep && nserver_sheep) {
		snprintf(fbuf, MAXBUF, "%s%s", path, e->d_name);
		if (debug)
		    fprintf(logout, "removing marker %s nserver_sheep=%d\n",
			   fbuf, nserver_sheep);
		unlink(fbuf);
		rmdir(path); /* cheesy but legal not to test for empty */
		continue;
	    }
	    ncached_sheep++;
	    cached_sheep = realloc(cached_sheep, (1+ncached_sheep)*sizeof(sheep_t));
	    continue;
	}

	snprintf(fbuf, PATH_MAX, "%s%s", path, e->d_name);
	if (-1 == stat(fbuf, &sbuf)) continue;

	if (S_ISDIR(sbuf.st_mode)) {
	    strncat(fbuf, "/", PATH_MAX);
	    update_cached_sheep_path(fbuf);
	    continue;
	}
 
	if (!filename_is_avi(e->d_name)) continue;

	if (4 != sscanf(e->d_name, "%d=%d=%d=%d.avi",
			&an->generation, &an->id, &an->first, &an->last)) {
	    continue;
	}

	/* i have a feeling these should also be done in bad_sheep */

	if ((bracket_begin_id != -1 && an->id < bracket_begin_id) ||
	    (bracket_end_id != -1 && an->id > bracket_end_id))
	  continue;

	if ((bracket_begin_time != (time_t)(-1) &&
	     sbuf.st_mtime < bracket_begin_time) ||
	    (bracket_end_time != (time_t)(-1) &&
	     sbuf.st_mtime > bracket_end_time))
	  continue;

	an->rating = 0;
	for (i = 0; i < nserver_sheep; i++) {
	    if (server_sheep[i].id == an->id) {
		an->rating = server_sheep[i].rating;
		break;
	    }
	}

	an->deleted = 0;
	an->readonly = !((sbuf.st_mode & S_IWUSR) &&
			 (sbuf.st_uid == getuid()));
	an->size = sbuf.st_size;
	an->mtime = sbuf.st_mtime;
	an->atime = sbuf.st_atime;
	an->nplays = 0;
	an->url[0] = 0;
	an->type = 0;
	strncpy(an->path, path, PATH_MAX);
	ncached_sheep++;
	cached_sheep = realloc(cached_sheep, (1+ncached_sheep)*sizeof(sheep_t));
    }
    closedir(d);
}

#define max_sheep 100000
#define max_play_count ((1<<16)-1)
#define log_page_size 10
#define log_count_size (log_page_size-1)
#define play_count_size (1<<log_count_size)
#define n_dirty_bits (1+(max_sheep>>log_count_size))
unsigned short play_counts[max_sheep];
char play_dirty[n_dirty_bits];
#define play_write_rate 10
int changed_count;

void print_play_counts() {
    if (debug) {
	int i;
	fprintf(logout, "play counts:");
	for (i = 0; i < max_sheep; i++) {
	    if (play_counts[i])
		fprintf(logout, " %d:%d", i, play_counts[i]);
	}
	fprintf(logout, "\n");
    }
}

void play_count_init() {
    int nread;
    char fn[PATH_MAX];
    
    if (debug) fprintf(logout, "play_counts init, thread=%s\n", thread_name);
    snprintf(fn, PATH_MAX, "%splay_counts", leave_prefix);
    memset(play_counts, 0, max_sheep * sizeof(unsigned short));
    memset(play_dirty, 0, n_dirty_bits);
    play_count_file = fopen(fn, "r+b");
    if (NULL == play_count_file) {
	if (debug) fprintf(logout, "creating play_counts, thread=%s\n", thread_name);
	play_count_file = fopen(fn, "w+b");
	if (NULL == play_count_file) {
	    perror(fn);
	    if (debug) fprintf(logout, "running without play_counts\n");
	}
	if (max_sheep != fwrite(play_counts, sizeof(unsigned short),
				max_sheep, play_count_file)) {
	    perror(fn);
	    cleanup_and_exit(1);
	}
	fflush(play_count_file);
	return;
    } 
    nread = fread(play_counts, sizeof(unsigned short), max_sheep, play_count_file);
    if (nread < max_sheep) {
	perror(fn);
	cleanup_and_exit(1);
    }
    changed_count = 0;
    print_play_counts();
}

void flush_counts() {
    int i;
    if (debug) fprintf(logout, "writing play counts\n");
    for (i = 0; i < n_dirty_bits; i++) {
	if (play_dirty[i]) {
	    if (debug) fprintf(logout, "writing dirty page %d\n", i);
	    if (0 != fseek(play_count_file,
			   i * (1 << log_page_size), SEEK_SET)) {
		fprintf(logout, "error seeking play counts, dismissing.\n");
		perror("fseek");
		fclose(play_count_file);
		play_count_file = NULL;
		return;
	    }
	    if (play_count_size !=
		fwrite(play_counts + i * play_count_size,
		       sizeof(unsigned short),
		       play_count_size, play_count_file)) {
		fprintf(logout, "error writing play counts, dismissing.\n");
		perror("fwrite");
		fclose(play_count_file);
		play_count_file = NULL;
		return;
	    }
	    fflush(play_count_file);
	    play_dirty[i] = 0;
	}
    }
}

void play_count(int idx) {
    if (NULL == play_count_file) return;
    if (play_counts[idx] < max_play_count) {
	int z = ++play_counts[idx];
	if (debug) fprintf(logout, "bumped %d to %d, page=%d\n", idx, z,
			   idx>>log_count_size);
    } else {
	if (debug) fprintf(logout, "ntbumped %d\n", idx);
    }
    play_dirty[idx>>log_count_size] = 1;
    changed_count++;
    if (changed_count < 4 || (0 == changed_count%play_write_rate))
	flush_counts();
}

void print_count_standard_deviation() {
    int i;
    double n = 0.0, s = 0.0, ss = 0.0, t, sd;
    for (i = 0; i < ncached_sheep; i++) {
	if (bad_sheep(i)) continue;
	n += 1.0;
	t = (double)play_counts[cached_sheep[i].id];
	s += t;
	ss += t*t;
    }
    if (0.0 == n) {
	fprintf(logout, "count standard deviation undefined\n");
	return;
    }
    t = s / n;
    sd = sqrt(ss/n - t*t);
    fprintf(logout, "count standard deviation = %g, n = %g\n", sd, n);
    fprintf(logout, "count average = %g\n", t);
}

int compare_ints(const void *p1, const void *p2) {
    int a, b;
    a = * (int *) p1;
    b = * (int *) p2;
    if (a < b) return -1;
    if (b < a) return 1;
    return 0;
}

void compute_play_ranks(double rank) {
    int i, median, n = 0;
    int ncheck = 0;
    int *tms;
    if (debug) {
	fprintf(logout, "thread %s compute ranks\n", thread_name);
    }
    tms = (int *) malloc(ncached_sheep * sizeof(int));
    for (i = 0; i < ncached_sheep; i++) {
	if (!bad_sheep(i)) {
	    tms[n] = play_counts[cached_sheep[i].id];
	    if (debug > 1)
		fprintf(logout, "n=%d i=%d:%d play_count=%d\n",
			n, i, cached_sheep[i].id, tms[n]);
	    n++;
	}
    }
    if (n < 1) {
      if (debug) {
	fprintf(logout, "median rank undefined\n");
      }
      return;
    }
    qsort(tms, n, sizeof(int), compare_ints);
    if (0 && debug) {
	for (i = 0; i < n; i++)
	    fprintf(logout, "%d %d\n", i, tms[i]);
    }
    median = tms[(int)(n * rank)];
    if (debug)
	fprintf(logout, "median rank %g n = %d plays = %d\n", rank, n, median);
    for (i = 0; i < ncached_sheep; i++) {
	if (play_counts[cached_sheep[i].id] > median) {
	    cached_sheep[i].rank = 1.0;
	    ncheck++;
	} else {
	    cached_sheep[i].rank = 0.0;
	}
    }
    if (debug)
	fprintf(logout, "ncheck = %d/%d\n", ncheck, ncached_sheep);
    free(tms);
}

void update_cached_sheep() {
    ncached_sheep = 0;
    if (NULL == cached_sheep)
	cached_sheep = malloc(sizeof(sheep_t));
    update_cached_sheep_path(leave_prefix);
    if (debug > 1) {
	print_sheep("ncached_sheep", cached_sheep, ncached_sheep);
    }
    compute_play_ranks(0.8);
}

static int irandom(int n) {
    return random()%n;
}

void cached_file_name(char *buf, sheep_t *an) {
    snprintf(buf, MAXBUF, "%s%05d=%05d=%05d=%05d.avi",
	    an->path, an->generation, an->id, an->first, an->last);
}

void deleted_file_name(char *buf, sheep_t *an) {
    snprintf(buf, MAXBUF, "%s%05d=%05d=%05d=%05d.xxx",
	    an->path, an->generation, an->id, an->first, an->last);
}

int check_for_eddy(playback_t *pb) {
    int i, j, c = 0, n = pb->history_size;
    if (n > 50) n = 50;
    for (i = 0; i < n; i++) {
	int diff = 1;
	if (-1 == pb->path_history[i]) break;
	if (debug > 1)
	    fprintf(logout, "edd checking %d %d %d\n", c, i, pb->path_history[i]);
	for (j = 0; j < i; j++) {
	    if (pb->path_history[j] == pb->path_history[i])
		diff = 0;
	}
	c += diff;
	if (c <= i/3) {
	    if (debug) fprintf(logout, "eddy %d/%d\n", c, i);
	    return 1;
	}
    }
    return 0;
}

void copy_out_file(char *fname) {
    AVOutputFormat *ofmt;
    AVFormatContext *ictx;
    int input_stream_index;
    int j;
    struct stat sbuf;

    if (debug) fprintf(logout, "playing %s\n", fname);

    if (-1 == stat(fname, &sbuf)) {
      perror(fname);
      return;
    }
    if (0 == sbuf.st_size) {
      if (debug) fprintf(logout, "zero %s\n", fname);
      return;
    }

    if (0 > av_open_input_file(&ictx, fname, NULL, 0, NULL)) {
	perror(fname);
	exit(1);
    }

    if (0 > av_find_stream_info(ictx)) {
	fprintf(logout, "%s: could not find codec parameters\n", fname);
	exit(1);
    }

    input_stream_index = -1;
    for (j = 0; j < ictx->nb_streams; j++) {
	AVCodecContext *enc = ictx->streams[j]->codec;
	if (CODEC_TYPE_VIDEO == enc->codec_type) {
	    input_stream_index = j;
	    break;
	}
    }
    if (-1 == input_stream_index) {
	fprintf(logout, "%s: no video found\n", fname);
	exit(1);
    }

    if (NULL == output_ctx) {
	AVCodecContext *codec, *icodec;
	AVStream *st;
	char pipe[20];
	if (1)
	    sprintf(pipe, "pipe:%d", fileno(avi_pipe));
	else {
	    strcpy(pipe, "pipe:");
	    if (-1 == dup2(fileno(avi_pipe), STDOUT_FILENO)) {
		perror("dup2p");
		cleanup_and_exit(1);
	    }
	}

	output_ctx = avformat_alloc_context();

	ofmt = guess_format(NULL, fname, NULL);
	if (!ofmt) {
	    fprintf(logout, "could not determine format from %s.\n", fname);
	    exit(1);
	}
	output_ctx->oformat = ofmt;

	st = av_new_stream(output_ctx, output_ctx->nb_streams);
	st->stream_copy = 1;
	av_set_parameters(output_ctx, NULL);

	icodec = ictx->streams[input_stream_index]->codec;
	codec = output_ctx->streams[0]->codec;

	codec->codec_id = icodec->codec_id;
	codec->codec_type = icodec->codec_type;
	if(!codec->codec_tag)
	    codec->codec_tag = icodec->codec_tag;
	codec->bit_rate = icodec->bit_rate;
	codec->extradata= icodec->extradata;
	codec->extradata_size= icodec->extradata_size;

	codec->time_base = icodec->time_base;
	codec->width = icodec->width;
	codec->height = icodec->height;
	codec->has_b_frames = icodec->has_b_frames;

	if (url_fopen(&output_ctx->pb, pipe, URL_WRONLY) < 0) {
	    fprintf(logout, "Could not open '%s'\n", pipe);
	    exit(1);
	}

	av_write_header(output_ctx);
    }

    while (1) {
	AVPacket ipkt;
	AVPacket opkt;
	if (0 > av_read_frame(ictx, &ipkt)) break;
	av_init_packet(&opkt);
	if (av_parser_change(ictx->streams[input_stream_index]->parser, output_ctx->streams[0]->codec,
			     &opkt.data, &opkt.size, ipkt.data, ipkt.size,
			     ipkt.flags & PKT_FLAG_KEY))
	    opkt.destruct= av_destruct_packet;
		
	if (-1 == av_interleaved_write_frame(output_ctx, &opkt)) {
	    perror("av_interleaved_write_frame");
	    exit(1);
	}
	av_free_packet(&opkt);
	av_free_packet(&ipkt);
    }
    av_close_input_file(ictx);
}

time_t search_time;
int max_ply;
int *search_history;

double search_next_sheep(int idx, int ply, int *ret_best_idx) {
    int i, j, sym = cached_sheep[idx].last;
    int edged = sym != cached_sheep[idx].first;
    int gen = cached_sheep[idx].generation;
    double try, best, lbest, self;
    int best_idx = -1;
    int lbest_idx = -1;
    int d = search_time - cached_sheep[idx].atime;

    self = (double) d;

    if (debug)
	fprintf(logout, "self ply=%d %d=%d\n", ply, idx, cached_sheep[idx].id);

    if (max_ply == ply) {
	fprintf(logout, "search_history =");
	for (j = 0; j < ply; j++)
	    fprintf(logout, " %d:%d", search_history[j], cached_sheep[search_history[j]].id);
	fprintf(logout, "\n");
	    
	if (debug)
	    fprintf(logout, "return %g\n", self);
	return self;
    }

    search_history[ply] = idx;

    for (i = 0; i < ncached_sheep; i++) {
	if (!cached_sheep[i].deleted &&
	    (cached_sheep[i].generation == gen) &&
	    cached_sheep[i].first == sym) {
	    if (debug > 1) fprintf(logout, "succ x=%d id=%d\n", i, cached_sheep[i].id);
	    for (j = 0; j < ply; j++)
		if (search_history[j] == idx) {
		    if (debug > 1) fprintf(logout, "found in history %d\n", j);
		    break;
		}
	    if (j < ply) {
		if (debug > 1) fprintf(logout, "skipping %d\n", j);
		continue;
	    }
	    try = search_next_sheep(i, ply + 1, NULL);
	    if (-1 == best_idx || try > best) {
		best_idx = i;
		best = try;
	    }
	    if (cached_sheep[i].first == cached_sheep[i].last) {
		if (-1 == lbest_idx || try > lbest) {
		    lbest_idx = i;
		    lbest = try;
		}
	    }
	}
    }
    if (edged && lbest_idx >= 0) {
	if (debug)
	    fprintf(logout, "replacing best=%g with lbest=%g lbest_idx=%d\n", best, lbest, lbest_idx);
	best = lbest;
	best_idx = lbest_idx;
    } else if (-1 == best_idx) {
	if (debug)
	    fprintf(logout, "no successor found %g\n", self);
	return -1.0;
    }

    if (self > best) {
	if (debug)
	    fprintf(logout, "replacing best=%g with self=%g\n", best, self);
	best = self;
    }
	
    if (debug)
	fprintf(logout, "search best %d=%d %g self=%g ret=%g\n",
		best_idx, cached_sheep[best_idx].id, best, self, best);
    if (ret_best_idx) *ret_best_idx = best_idx;
    return best;
}

int start_search_next_sheep(int idx) {
    int best;
    double score;
    search_time = time(0);
    max_ply = 5;
    search_history = malloc(sizeof(int) * max_ply);
    score = search_next_sheep(idx, 0, &best);
    free(search_history);
    if (debug)
	fprintf(logout, "finish search best %d=%d %g\n",
		best, cached_sheep[best].id, score);
    return best;
}

int play_evenly(int idx) {
    double v = irandom(100)/100.0;
    int r = v >= cached_sheep[idx].rank * prefs.play_evenly;
    if (0)
	fprintf(logout, "play_evenly %d %d=%d r=%g v=%g pe=%g\n",
		r, idx, cached_sheep[idx].id, cached_sheep[idx].rank,
		v, prefs.play_evenly);
    return r;
}

int next_sheep(playback_t *pb, int idx) {
    int i, succ = -1;
    int lsucc = -1;
    int sym = cached_sheep[idx].last;
    int edged = sym != cached_sheep[idx].first;
    int next_idx;

    if (0 == pb->reset_fuse--
	|| pb->nrepeated >= pb->max_repeats
	|| check_for_eddy(pb)) {
      if (debug) fprintf(logout, "reset nrepeated=%d reset_fuse=%d\n",
			pb->nrepeated, pb->reset_fuse);
      pb->reset_fuse = pb->reset_fuse_length;
      if (pb->history_size > 1) pb->path_history[1] = -1;
      return -1;
    }


    if (0) {
	return start_search_next_sheep(idx);
    } 
    for (i = 0; i < ncached_sheep; i++) {
	if (!cached_sheep[i].deleted &&
	    (cached_sheep[i].generation == cached_sheep[idx].generation) &&
	    cached_sheep[i].first == sym) {

	    if (!play_evenly(i)) {
		if (debug)
		    fprintf(logout, "suppressed %d=%d\n", i, cached_sheep[i].id);
		continue;
	    }
		
	    if (debug > 1) fprintf(logout, "succ x=%d id=%d\n", i, cached_sheep[i].id);

	    if (-1 == succ ||
		cached_sheep[i].atime < cached_sheep[succ].atime)
		succ = i;

	    if (cached_sheep[i].first == cached_sheep[i].last) {

		if (-1 == lsucc ||
		    cached_sheep[i].atime < cached_sheep[lsucc].atime)
		    lsucc = i;
	    }
	}
    }

    if (edged && lsucc >= 0) {
	next_idx = lsucc;
	if (debug) fprintf(logout, "lsuccto %d\n", cached_sheep[next_idx].id);
    } else if (succ >= 0) {
	next_idx = succ;
	if (debug) fprintf(logout, "esuccto %d\n", cached_sheep[next_idx].id);
    } else {
	if (debug) fprintf(logout, "no succ\n");
	next_idx = -1;
    }

    return next_idx;
}

int start_sheep() {
    int idx = -1;
    if (read_only) {
	/* random sheep.  read_only generally means we have started more
	   than one client, like for multiple screens, and we don't want to
	   play the same sheep sequence on each screen. */
	do {
	    idx = irandom(ncached_sheep);
	    if (debug) fprintf(logout, "idx=%d ncached_sheep=%d random\n", idx, ncached_sheep);
	} while (bad_sheep(idx));
    } else {
	int i;
	/* least recently accessed sheep */
	time_t best_atime = -1;
	for (i = 0; i < ncached_sheep; i++) {
	    if (!bad_sheep(i) &&
		(-1 == best_atime
		 || cached_sheep[i].atime < best_atime)) {
		best_atime = cached_sheep[i].atime;
		idx = i;
	    }
	}
    }
    if (debug)
	fprintf(logout, "idx=%d ncached_sheep=%d best_atime\n", idx, ncached_sheep);
    return idx;
}

void play_update_history(playback_t *pb, int id) {
    int h;
    for (h = pb->history_size - 1; h > 0; h--) {
	pb->path_history[h] = pb->path_history[h-1];
    }
    pb->path_history[0] = id;

    if (debug > 1) {
	fprintf(logout, "history =");
	for (h = 0; h < pb->history_size; h++) {
	    if (-1 == pb->path_history[h])
		break;
	    else
		fprintf(logout, " %d", pb->path_history[h]);
	}
	fprintf(logout, "\n");
    }

    if (pb->last_sheep == id)
      pb->nrepeated++;
    else
      pb->nrepeated = 0;
    pb->last_sheep = id;
}




/* traverse the graph of sheep, and concatenates the avi files into stdin
   of the child.  another process decodes the avi and displays it on
   the screen. */
void do_display() {
    int i;
    int idx, nidx;
    int niters;
    char fbuf[MAXBUF];


    if (start_id >= 0) {
	current_anim_id = start_id;
	start_id = -1;
    }

    idx = -1;
    if (-1 != current_anim_id) {
	for (i = 0; i < ncached_sheep; i++) {
	    if (!cached_sheep[i].deleted &&
		cached_sheep[i].id == current_anim_id) {
		idx = i;
		break;
	    }
	}
    }

    if (idx != -1 && bad_sheep(idx)) idx = -1;

    /* XXX this could spin if there there are only bad sheep? */
    while (-1 == idx) {
	int n = 0;
	for (i = 0; i < ncached_sheep; i++) {
	    if (!bad_sheep(i)) n++;
	}
	if (0 == n) {
	    /* XXX danger of burn-in from static video in case server is never reached */
	    /* conflicts with -cache 8192 option to mplayer */
	    // copy_out_file(PACKAGE_DATA_DIR "/electricsheep-wait.avi");
	    sleep(2);
	    update_cached_sheep();
	    fprintf(stderr, "downloading sheep, please wait...\n");
	    return;
	}
	idx = start_sheep();

	if (-1 == idx) {
	    if (debug) fprintf(logout, "failed to find new anim!\n");
	} else {
	    current_anim_id = cached_sheep[idx].id;
	    if (debug) fprintf(logout, "found new anim id=%d.\n", current_anim_id);
	}
    }

    if (cached_sheep[idx].first == cached_sheep[idx].last) {
	niters = prefs.nrepeats;
    } else {
	niters = 1;
    }

    if (1) {
	// play an anim
	int i;
	time_t now;

	if (debug) fprintf(logout, "play anim x=%d id=%d iters=%d path=%s\n",
			  idx, current_anim_id, niters,
			   cached_sheep[idx].path);
	if (stats) print_stats();

	nplays++;
	if (cached_sheep[idx].first == cached_sheep[idx].last) {
	    nloopplays++;
	}

	now = cached_sheep[idx].atime = time(0);

	play_count(current_anim_id);

	play_update_history(&playback, current_anim_id);

	cached_file_name(fbuf, &cached_sheep[idx]);

	/* since relatime is default, explicitly set atime */
	if (1) {
	  struct utimbuf tbuf;
	  struct stat sbuf;
	  if (-1 == stat(fbuf, &sbuf)) {
	    fprintf(logout, "utime prestat error: ");
	    perror(fbuf);
	  }
	  tbuf.actime = now;
	  tbuf.modtime = sbuf.st_mtime;
	  if (-1 == utime(fbuf, &tbuf)) {
	    if (debug) {
	      fprintf(logout, "utime error: ");
	      perror(fbuf);
	    }
	  }
	  if (debug) fprintf(logout, "set atime of %d to %u\n",
			     idx, (unsigned)now);
	}

	for (i = 0; i < niters; i++)
	    copy_out_file(fbuf);
    }
    
    nidx = next_sheep(&playback, idx);
    if (-1 == nidx) {
	nrestarts++;
	current_anim_id = -1;
    } else {
	current_anim_id = cached_sheep[nidx].id;
    }

}

int cache_overflow(double bytes) {
    struct STATFS buf;
    int result;

    if (-1 == STATFS(leave_prefix, &buf)) {
	perror(leave_prefix);
	cleanup_and_exit(1);
    }

    /* use of min_megabytes means if another process fills the disk,
       the sheep will sacrifice themselves to try to keep open space.
       good?  maybe this criterion should only apply if downloading
       is unlimited */
    result = (prefs.cache_size &&
	    (bytes > (1024.0 * 1024 * prefs.cache_size))) ||
	(min_megabytes &&
	 ((buf.f_bavail * (double) buf.f_bsize) <
	  (min_megabytes * 1024.0 * 1024)));
    if (debug > 1)
      fprintf(logout, "cache_overflow(%g)=%d\n", bytes, result);
    return result;
}

double cache_used() {
    double total = 0;
    int i;
    for (i = 0; i < ncached_sheep; i++) {
	if (cached_sheep[i].deleted ||
	    cached_sheep[i].readonly) continue;
	if (debug > 1)
	    fprintf(logout, "summing total=%g id=%d rating=%d\n",
		   total, cached_sheep[i].id, cached_sheep[i].rating);
    
	total += cached_sheep[i].size;
    }
    return total;
}

int compare_times(const void *p1, const void *p2) {
    time_t a, b;
    a = * (time_t *) p1;
    b = * (time_t *) p2;
    if (a < b) return -1;
    if (b < a) return 1;
    return 0;
}

time_t median_mtime(double rank) {
    int i, n = 0;
    time_t *tms;
    time_t median;
    if (0 == ncached_sheep) {
	if (debug) fprintf(logout, "median of no sheep error\n");
	return -1;
    }
    tms = (time_t *) malloc(ncached_sheep * sizeof(time_t));
    for (i = 0; i < ncached_sheep; i++) {
	if (!bad_sheep(i))
	    tms[n++] = cached_sheep[i].mtime;
    }
    qsort(tms, n, sizeof(time_t), compare_times);
    if (debug > 1) {
	fprintf(logout, "median sort %d\n", n);
	for (i = 0; i < n; i++)
	    fprintf(logout, "%d %u %s", i, (unsigned)tms[i], ctime(&tms[i]));
    }
    median = tms[(int)(n * rank)];
    free(tms);
    if (debug) fprintf(logout, "median = %u = %s",
		       (unsigned)median, ctime(&median));
    return median;
}

/* choose a sheep to kill */
int reap_sheep() {
    int best = -1;
    if (0 && irandom(2)) {
	/* of the sheep with the lowest rating, pick the oldest */
	time_t oldest_time = 0;
	int worst_rating = 0;
	int i;

	if (debug) fprintf(logout, "deleting lowest/oldest\n");

	for (i = 0; i < ncached_sheep; i++) {
	    if (cached_sheep[i].deleted || cached_sheep[i].readonly) continue;

	    if (!oldest_time ||
		(cached_sheep[i].rating < worst_rating) ||
		((cached_sheep[i].rating == worst_rating) &&
		 (cached_sheep[i].mtime < oldest_time))) {
		best = i;
		oldest_time = cached_sheep[i].mtime;
		worst_rating = cached_sheep[i].rating;
	    }
	}
    } else if (1) {
	/* use the play counts to delete most played sheep */
	int i;
	time_t med;
	if (debug) fprintf(logout, "deleting highest play count\n");
	
	med = median_mtime(1.0 - protected_flock_fraction);
	for (i = 0; i < ncached_sheep; i++) {
	    if (cached_sheep[i].deleted || cached_sheep[i].readonly) continue;
	    if (cached_sheep[i].mtime > med) continue;
	    if (best < 0 ||
		(play_counts[cached_sheep[i].id] >
		 play_counts[cached_sheep[best].id])) {
		best = i;
	    }
	}
    } else {
	/* of the sheep with the lowest rating AND creation times
	   older than media, pick the most commonly played
	   as estimated by simulation */
	int max_plays = 0;
	int worst_rating = 0;
	int h, i, idx;
	time_t med;

	playback_t search = playback;

	search.path_history = malloc(sizeof(int) * search.history_size);
	for (h = 0; h < search.history_size; h++)
	    search.path_history[h] = -1;

	if (debug) fprintf(logout, "begin histogram\n");
	for (i = 0; i < ncached_sheep; i++) {
	    cached_sheep[i].nplays = 0;
	}
	/* newest 20 percent are protected */
	med = median_mtime(1.0 - protected_flock_fraction);

	idx = -1;
	for (i = 0; i < 10000; i++) {
	    if (idx >= 0) {
		int nm = cached_sheep[idx].nplays++;
		int id = cached_sheep[idx].id;
		cached_sheep[idx].atime = simulated_time++;
		if (debug) fprintf(logout, "simulating %d nplays %d\n", id, nm);

		/* check readonly too */
		if (cached_sheep[idx].mtime <= med) {
		    if (!max_plays ||
			(cached_sheep[idx].rating < worst_rating) ||
			((cached_sheep[idx].rating == worst_rating) &&
			 (nm > max_plays))) {
			best = idx;
			max_plays = nm;
			worst_rating = cached_sheep[idx].rating;
			if (debug)
			    fprintf(logout, "best=%d plays=%d rating=%d\n",
				   cached_sheep[best].id, max_plays,
				   worst_rating);
		    }
		}

		play_update_history(&search, id);
		idx = next_sheep(&search, idx);
	    } else {
		idx = start_sheep();
	    }
	}

	if (debug > 1)
	    print_sheep("hist", cached_sheep, ncached_sheep);

	/* what if best hasn't been set? XXX */
	if (debug) fprintf(logout, "end histogram best=%d plays=%d\n",
			  cached_sheep[best].id, max_plays);

	free(search.path_history);
    }
    return best;
}

void delete_sheep(int idx);

void delete_terminals_recursively(int idx) {
  int i, id = cached_sheep[idx].id;
  int first = cached_sheep[idx].first;
  int last = cached_sheep[idx].last;
  int nfirst = 0, nlast = 0;
  if (debug) fprintf(logout, "delete terminals id=%d first=%d last=%d\n", id, first, last);
  for (i = 0; i < ncached_sheep; i++) {
    if (cached_sheep[i].deleted) continue;
    if (cached_sheep[i].first == first) {
      nfirst++;
      if (debug) fprintf(logout, "found first %d\n", cached_sheep[i].id);
    }
    if (cached_sheep[i].last == last) {
      nlast++;
      if (debug) fprintf(logout, "found last %d\n", cached_sheep[i].id);
    }
    if (nfirst && nlast) {
      if (debug) fprintf(logout, "delete terminals none needed\n");
      return;
    }
  }
  if (!nfirst) {
    for (i = 0; i < ncached_sheep; i++) {
      if (cached_sheep[i].deleted) continue;
      if (cached_sheep[i].last == first) {
	if (debug) fprintf(logout, "got0 %d\n", cached_sheep[i].id);
	delete_sheep(i);
      }
    }
  }
  if (!nlast) {
    for (i = 0; i < ncached_sheep; i++) {
      if (cached_sheep[i].deleted) continue;
      if (cached_sheep[i].first == last) {
	if (debug) fprintf(logout, "got1 %d\n", cached_sheep[i].id);
	delete_sheep(i);
      }
    }
  }
  if (debug) fprintf(logout, "delete terminals done\n");
}

void delete_sheep(int idx) {
  int fd;
  char buf[MAXBUF];

  cached_file_name(buf, &cached_sheep[idx]);
  if (debug) fprintf(logout, "deleting %s\n", buf);
  cached_sheep[idx].deleted = 1;

  delete_terminals_recursively(idx);

  if (-1 == unlink(buf)) {
    perror(buf);
    return;
  }
  deleted_file_name(buf, &cached_sheep[idx]);
  if (-1 == (fd = creat(buf, S_IRUSR|S_IWUSR))) {
    perror(buf);
    exit(1);
  }
  close(fd);
}

/* make enough room in cache to download SIZE more bytes */
void delete_cached(int size) {
    double total;
    if (debug) fprintf(logout, "begin delete cached %d\n", size);

    /* XXX loop? */
    while (ncached_sheep) {
	total = size + cache_used();
	if (cache_overflow(total)) {
	    if (debug) fprintf(logout, "cache overflow %g\n", total);
	    int best = reap_sheep();
	    if (best >= 0 && cache_overflow(total)) {
	      delete_sheep(best);
	    }
	} else
	    return;
    }
    if (debug) fprintf(logout, "nothing cached\n");
}

void download_anim(int idx) {
    char pbuf[MAXBUF];
    char tfb[MAXBUF];
    char cfb[MAXBUF];
    struct stat sbuf;

    if (debug) fprintf(logout, "download %d id=%d\n", idx, server_sheep[idx].id);
    
    cached_file_name(cfb, &server_sheep[idx]);
    snprintf(tfb, MAXBUF, "%s.tmp", cfb);
    strcpy(avi_name, tfb);
    
    snprintf(pbuf, MAXBUF, "%s --output %s %s",
	     curl_cmd, tfb, server_sheep[idx].url);

    if (debug) fprintf(logout, "about to %s\n", pbuf);

    mysystem(pbuf, "anim download");

    if (-1 == stat(tfb, &sbuf)) {
	if (!prefs.hide_errors)
	    fprintf(logout, "download failed of sheep %d\n",
		    server_sheep[idx].id);
	unlink(tfb);
	avi_name[0] = 0;
	sleep(tryagain);
	return;
    }
    if (sbuf.st_size != server_sheep[idx].size) {

      if (debug)
	fprintf(logout, "deleted incomplete sheep id=%d got=%ld want=%ld\n",
	       server_sheep[idx].id, (long)sbuf.st_size,
	       (long)server_sheep[idx].size);

	unlink(tfb);
	avi_name[0] = 0;
	sleep(tryagain);
	return;
    }

    if (-1 == rename(tfb, cfb)) {
	perror("move download temp to cache");
	fprintf(logout, "move %s to %s\n", tfb, cfb);
	cleanup_and_exit(1);
    }
    avi_name[0] = 0;
    if (debug) fprintf(logout, "download complete %d id=%d\n",
		      idx, server_sheep[idx].id);
    touch_file("downloaded");
}

void
get_control_points(char *buf, int buf_size) {
    int n;
    char pbuf[MAXBUF];
    FILE *cp;
  
    snprintf(pbuf, MAXBUF, "%s 'http://%s/cgi/get?"
	     "n=%s&w=%s&v=%s&u=%s&r=%.3g&c=%.3g' | gunzip -c %s",
	     curl_cmd, server, nick_buf,
	     url_buf, client_version, prefs.uid, 1.0, 1.0,
	     hide_stderr);

    if (debug) fprintf(logout, "get_control_points %s\n", pbuf);

    cp = popen(pbuf, "r");

    if (NULL == cp) {
	perror("could not fork/pipe\n");
	cleanup_and_exit(1);
    }

    while ((n = fread(buf, 1, buf_size, cp))) {
	buf += n;
	buf_size -= n;
	if (1 >= buf_size) {
	    fprintf(logout, "cp buffer overflow - the server is spamming me!\n");
	    cleanup_and_exit(1);
	}
    }
    *buf = 0; /* null terminate */

    if (ferror(cp)) {
	perror("get pipe error\n");
	cleanup_and_exit(1);
    }

    if (-1 == pclose(cp)) {
	perror("pclose of get failed\n");
	cleanup_and_exit(1);
    }
}

void
put_image(char *fname, int job, int anim_id, int gen) {
    char pbuf[MAXBUF];
    struct stat sbuf;

    if (-1 == stat(fname, &sbuf)) {
	perror(fname);
	cleanup_and_exit(1);
    }

    snprintf(pbuf, MAXBUF, "%s --upload-file %s "
	    "'http://%s/cgi/put?j=%d&id=%d&s=%ld&g=%d&v=%s&u=%s'",
	    curl_cmd, fname, server, job, anim_id,
	    (long)sbuf.st_size, gen, client_version, prefs.uid);
    if (debug) fprintf(logout, "about to put %s\n", pbuf);
    mysystem(pbuf, "put image");
}

int get_gen;
int get_id;
int get_job;

int got_sheep = 0;


void
get_start_element(void *userData, const char *name, const char **atts) {
    int i = 0;
    if (!strcmp("get", name)) {
	while (atts[i]) {
	    const char *a = atts[i+1];
	    if (!strcmp(atts[i], "gen")) {
		get_gen = atoi(a);
	    } else if (!strcmp(atts[i], "id")) {
		get_id = atoi(a);
	    } else if (!strcmp(atts[i], "job")) {
		get_job = atoi(a);
	    } else if (!strcmp(atts[i], "retry")) {
		get_tryagain = atoi(atts[i+1]);
		if (get_tryagain <= 0) {
		    fprintf(logout, "get_tryagain must be positive.\n");
		    cleanup_and_exit(1);
		}
	    }
	    i += 2;
	}
    } else if (!strcmp("args", name)) {
	while (atts[i]) {
	    const char *a = atts[i+1];
	    if (strlen(atts[i]) < 32 &&
		strlen(a) < 256) {
		if (setenv(atts[i], a, 1)) {
		    perror("setenv");
		    cleanup_and_exit(1);
		}
	    } else {
		fprintf(logout, "oversized envar %s=%s.\n", atts[i], a);
		cleanup_and_exit(1);
	    }
	    i += 2;
	}
    } else if (!strcmp("message", name)) {
	in_message = 1;
    } else if (!strcmp("flame", name)) {
	got_sheep = 1;
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

void do_render() {
    char sheep_text[max_cp_size];
    char *cp_text;
    int cps_fd, jpg_fd;
    XML_Parser parser;

    timestamp("do_render");

    get_control_points(sheep_text, max_cp_size);

    if (0 == sheep_text[0]) {
	if (!prefs.hide_errors)
	    fprintf(logout,
		    "lost contact with %s, cannot render frames.\n",
		    server);
	sleep(tryagain);
	return;
    }

    get_gen = get_id = get_job = -1;

    parser = XML_ParserCreate(NULL);
    XML_SetElementHandler(parser, get_start_element, get_end_element);
    XML_SetCharacterDataHandler(parser, character_handler);
    in_message = 0;
    server_error = 0;
    got_sheep = 0;
    if (!XML_Parse(parser, sheep_text, strlen(sheep_text), 1)) {
	fprintf(logout, "%s at line %u\n",
		XML_ErrorString(XML_GetErrorCode(parser)),
		(unsigned)XML_GetCurrentLineNumber(parser));
	XML_ParserFree(parser);
	fputs(sheep_text, logout);
	sleep(get_tryagain);
	return;
    }
    XML_ParserFree(parser);
    if (server_error) {
	fprintf(logout, "server reported error for get: %s\n",
		server_error_type);
	sleep(get_tryagain);
	return;
    }
    if (!got_sheep) {
	if (debug) fprintf(logout, "nothing to render, will try again later.\n");
	sleep(get_tryagain);
	return;
    }

    cp_text = sheep_text;

    strcpy(cps_name, "/tmp/electricsheep.cps.XXXXXX");
    cps_fd = mkstemp(cps_name);

    if (strlen(cp_text) != write(cps_fd, cp_text, strlen(cp_text))) {
	perror(cps_name);
	cleanup_and_exit(1);
    }

    strcpy(jpg_name, "/tmp/electricsheep.XXXXXX");
    jpg_fd = mkstemp(jpg_name);

    {
	char b[MAXBUF];
	snprintf(b, MAXBUF, "nice -n %d env verbose=0 nthreads=1 in=%s out=%s flam3-animate",
		nice_level, cps_name, jpg_name);
	if (debug) fprintf(logout, "about to render %s\n", b);
	if (mysystem(b, "render")) {
	    if (debug) fprintf(logout, "render failed, trying again later\n");
	    unlink(jpg_name);
	    jpg_name[0] = 0;
	    close(jpg_fd);
	    unlink(cps_name);
	    cps_name[0] = 0;
	    close(cps_fd);
	    sleep(tryagain);
	    return;
	}
    }

    put_image(jpg_name, get_job, get_id, get_gen);

    close(cps_fd);
    close(jpg_fd);

    /* hm should we close after unlink? */

    if (prefs.save_frames) {
       /* Copy frame to the leave_prefix area with the proper extension       */
       char b[MAXBUF];
       char *imtype;
       
       imtype = getenv("format");
       snprintf(b, MAXBUF, "cp %s %s/%d.%05d.%05d.%s",
		jpg_name, leave_prefix, get_gen, get_id, get_job, imtype?imtype:"png");
       
       if (debug) fprintf(logout, "copying image to save dir\n");
       if (mysystem(b, "copyimage")) {
          if (debug) fprintf(logout, "copy image failed.  skipping.\n");
       }
    }

    if (unlink(jpg_name)) {
	perror(jpg_name);
	cleanup_and_exit(1);
    }
    jpg_name[0] = 0;
    if (unlink(cps_name)) {
	perror(cps_name);
	cleanup_and_exit(1);
    }
    cps_name[0] = 0;
}

void
list_start_element(void *userData, const char *name, const char **atts) {
    int i = 0;
    if (!strcmp("list", name)) {
	while (atts[i]) {
	    if (!strcmp(atts[i], "gen")) {
		generation = atoi(atts[i+1]);
		if (generation <= 0) {
		    fprintf(logout, "generation must be positive.\n");
		    cleanup_and_exit(1);
		}
	    } else if (!strcmp(atts[i], "retry")) {
		tryagain = atoi(atts[i+1]);
		if (tryagain <= 0) {
		    fprintf(logout, "tryagain must be positive.\n");
		    cleanup_and_exit(1);
		}
	    }
	    i += 2;
	}
    } else if (!strcmp("sheep", name)) {
	sheep_t *an = &server_sheep[nserver_sheep];
	const char *state = NULL;
	if (-1 == generation) {
	    fprintf(logout, "malformed list.  "
		    "received sheep without generation set.\n");
	    cleanup_and_exit(1);
	}
	memset(an, 0, sizeof(sheep_t));
	an->generation = generation;
	an->rank = 0.0;
	strncpy(an->path, leave_prefix, PATH_MAX);
	while (atts[i]) {
	    const char *a = atts[i+1];
	    if (!strcmp(atts[i], "id")) {
		an->id = atoi(a);
	    } else if (!strcmp(atts[i], "type")) {
		an->type = atoi(a);
	    } else if (!strcmp(atts[i], "time")) {
		an->mtime = (time_t) atoi(a);
	    } else if (!strcmp(atts[i], "size")) {
		an->size = atoi(a);
	    } else if (!strcmp(atts[i], "rating")) {
		an->rating = atoi(a);
	    } else if (!strcmp(atts[i], "first")) {
		an->first = atoi(a);
	    } else if (!strcmp(atts[i], "last")) {
		an->last = atoi(a);
	    } else if (!strcmp(atts[i], "state")) {
		state = a;
	    } else if (!strcmp(atts[i], "url")) {
		strncpy(an->url, a, max_url_length);
		an->url[max_url_length-1] = 0;
	    }
	    i += 2;
	}
	if (!strcmp(state, "done") && (0 == an->type)) {
	    nserver_sheep++;
	    server_sheep = realloc(server_sheep, (1+nserver_sheep) * sizeof(sheep_t));
	} else if (!strcmp(state, "expunge")) {
	    char buf[MAXBUF];
	    if (debug) fprintf(logout, "expunging id=%d.\n", an->id);
	    cached_file_name(buf, an);
	    // ok to fail
	    unlink(buf);
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

time_t server_sheep_timestamp = -1;

void update_server_sheep() {
    char pbuf[MAXBUF];
    FILE *lf;
    int done;
    XML_Parser parser;

    if (-1 == server_sheep_timestamp)
      server_sheep_timestamp = time(0);
    else {
      time_t now = time(0);
      if ((now - server_sheep_timestamp) < list_freshness) {
	if (debug) fprintf(logout, "skipping cgi/list\n");
	return;
      }
      server_sheep_timestamp = now;
    }
	
    parser = XML_ParserCreate(NULL);
    XML_SetElementHandler(parser, list_start_element, get_end_element);
    XML_SetCharacterDataHandler(parser, character_handler);
    in_message = 0;
    server_error = 0;

    init_list_cmd(pbuf);

    if (debug) fprintf(logout, "list %s\n", pbuf);

    lf = popen(pbuf, "r");

    if (NULL == lf) {
	perror("could not fork/pipe\n");
	cleanup_and_exit(1);
    }

    nserver_sheep = 0;
    if (NULL == server_sheep)
	server_sheep = malloc(sizeof(sheep_t));

    generation = -1;

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
	if (debug > 1) fprintf(logout, "list read loop len=%d\n", len);
    } while (!done);
    XML_ParserFree(parser);

    pclose(lf);

    if (server_error) {
	fprintf(logout,
		"server reported error for list: %s\n",
		server_error_type);
	sleep(tryagain);
	return;
    }

    if (-1 == generation) {
	if (!prefs.hide_errors)
	    fprintf(logout,
		    "lost contact with %s, cannot retrieve sheep.\n",
		    server);
	sleep(tryagain);
	return;
    }
    if (debug) fprintf(logout, "update_server_sheep done\n");
}




/* download anims, delete old anims, update cache */
void
do_http_download() {
    int i, j;
    int best_rating;
    time_t best_mtime = 0;
    int best_anim = -1;

    timestamp("do_http_download");

    update_server_sheep();

    if (debug > 1) {
	print_sheep("nserver_sheep", server_sheep, nserver_sheep);
    } else if (debug) {
      fprintf(logout, "nserver_sheep=%d\n", nserver_sheep);
    }
    update_cached_sheep();
    if (debug) {
      fprintf(logout, "ncached_sheep=%d\n", ncached_sheep);
    }

    for (i = 0; i < nserver_sheep; i++) {
	for (j = 0; j < ncached_sheep; j++) {
	    if (server_sheep[i].id == cached_sheep[j].id)
		break;
	}
	if ((j == ncached_sheep) &&
	    !cache_overflow((double)server_sheep[i].size)) {
	    /* anim on the server fits in cache but is not in cache */
	    if (best_mtime == 0 ||
		(server_sheep[i].rating > best_rating) ||
		(server_sheep[i].rating == best_rating &&
		 server_sheep[i].mtime < best_mtime)) {
		best_rating = server_sheep[i].rating;
		best_mtime = server_sheep[i].mtime;
		best_anim = i;
	    }
	}
    }
    if (-1 != best_anim) {
        if (debug)
	  fprintf(logout, "best_anim=%d best_anim_id=%d "
		 "best_rating=%d best_mtime=%d\n",
		 best_anim, server_sheep[best_anim].id,
		 best_rating, (int)best_mtime);
	delete_cached(server_sheep[best_anim].size);
	download_anim(best_anim);
    } else {
	if (debug) fprintf(logout, "nothing to download, sleeping for %d.\n", tryagain);
	sleep(tryagain);
    }
}

void make_download_process() {

    if (-1 == (downloader_pid = fork()))
	perror("downloader fork");
    else if (0 == downloader_pid) {
	/* child */
#ifdef HAVE_SETPROCTITLE
	setproctitle("download");
#endif
	thread_name = "download";
	if (ncached_sheep > 2) {
	    sleep(init_delay_list);
	}
	init_curl_cmd(1);
	while (1) {
	    do_http_download();
	}
    }
    /* parent returns */
}

void make_render_process() {
    pid_t p;

    sleep(init_delay);

    while (nthreads-- > 1) { 
	if (-1 == (p = fork()))
	    perror("render fork");
	else if (0 == p) {
	    break;
	}
	sleep(10);
    }

    if (debug) fprintf(logout, "entering render loop %d\n", nthreads);
#ifdef HAVE_SETPROCTITLE
    setproctitle("render #%d", nthreads);
#endif
    thread_name = "render";
    while (1) {
	do_render();
    }
}

/* fork off two processes, one to traverse the graph of sheep, and the
   other to decode and display them.  the two are connected by
   pipes.  the decoder is an external program that we just exec. */

void
make_display_process()  {
    int h;
    int cnt;
    pid_t generator_pid;
    for (h = 0; h < playback.history_size; h++)
	playback.path_history[h] = -1;
    
    if (-1 == (displayer_pid = fork()))
	perror("displayer fork");
    else if (0 == displayer_pid) {
	/* child */
	int pipe_fds[2];
	char fps[25];
	if (-1 == pipe(pipe_fds)) {
	    perror("pipe1");
	    cleanup_and_exit(1);
	}
	if (-1 == (decoder_pid = fork())) {
	    perror("decoder fork");
	} else if (0 == decoder_pid) {
	    char *argv[25];
	    int c = 0;
	    /* child */
	    snprintf(fps, 24, "%g", prefs.frame_rate);
	    if (1) {
	      int devnull = open("/dev/null", O_WRONLY);
	      if (-1 == devnull) {
		perror("/dev/null");
		cleanup_and_exit(1);
	      }
	      if (-1 == dup2(devnull, STDOUT_FILENO)) {
		perror("dup2a");
		cleanup_and_exit(1);
	      }
	      if (-1 == dup2(devnull, STDERR_FILENO)) {
		perror("dup2b");
		cleanup_and_exit(1);
	      }
		argv[c++] = "mplayer";
		argv[c++] = "-nolirc";
		argv[c++] = "-cache";
		argv[c++] = "8192";
		argv[c++] = "-really-quiet";
		if (window_id) {
		  argv[c++] = "-monitoraspect";
		  argv[c++] = monitoraspect;
		}
		argv[c++] = "-fps";
		argv[c++] = fps;
		if (prefs.video_driver && prefs.video_driver[0]) {
		    argv[c++] = "-vo";
		    argv[c++] = prefs.video_driver;
		}
		if (NULL != window_id) {
		    argv[c++] = "-wid";
		    argv[c++] = window_id;
		    argv[c++] = "-nostop-xscreensaver";
		} else if (on_root) {
		    argv[c++] = "-rootwin";
		    argv[c++] = "-nostop-xscreensaver";
		}
		argv[c++] = "-";
		argv[c++] = NULL;
		
	    }
	    if (0) {
		int i;
		fprintf(logout, "decoder execvp ");
		for (i = 0; i < c-1; i++) {
		    fprintf(logout, "%s ", argv[i]);
		}
		fprintf(logout, "\n");
	    }
	    if (-1 == dup2(pipe_fds[0], STDIN_FILENO)) {
		perror("decoder child dup2 1");
		cleanup_and_exit(1);
	    }
	    /* for some reason this makes it start more reliably */
	    usleep(300000);
	    execvp("mplayer", argv);
	    perror("mplayer");
	    cleanup_and_exit(1);
	} else {
#ifdef HAVE_SETPROCTITLE
	    setproctitle("display");
#endif
	    thread_name = "display";
	    avi_pipe = fdopen(pipe_fds[1], "w");
	    if (NULL == avi_pipe) {
		perror("fdopen 1");
		cleanup_and_exit(1);
	    }

	    if ((generator_pid=fork())==0) {
	      
	      update_cached_sheep();
	      cnt = 0;
	      av_register_all();
	      while (1) {
		timestamp("display loop");
		do_display();
		if (!prefs.standalone && (zap_download_marker() ||
				    !(++cnt%cache_update_delay))) {
		  update_cached_sheep();
		} else if (debug) {
		  fprintf(logout, "skipping update cache %d/%d\n",
			 cnt, cache_update_delay);
		}
	      }
	      /* child never gets here */
	    } else {
	      /* wait end of decoder pid (kill window, e.g. screensaver stop on KDE) */
	      thread_name = "waiter";
	      if (-1 == waitpid(decoder_pid, 0, 0)) {
		perror("waitpid decoder_pid");
		exit(1);
	      }
	      /* XXX SIGTERM too harsh. should exit without error. */
	      cleanup_and_exit(1);
	    }
	    
	}
    }
    /* parent returns */
}

/* set the number of threads by reading /proc/cpuinfo */
void
auto_nthreads() {
#ifndef _SC_NPROCESSORS_ONLN
    char line[MAXBUF];
    FILE *f = fopen("/proc/cpuinfo", "r");
    if (NULL == f) goto def;
    nthreads = 0;
    while (fgets(line, MAXBUF, f)) {
	if (!strncmp("processor\t:", line, 11))
	    nthreads++;
    }
    fclose(f);
    if (nthreads < 1) goto def;
    return;
 def:
    fprintf(logout,
	    "could not read /proc/cpuinfo, using one render thread.\n");
    nthreads = 1;
#else
    nthreads = sysconf(_SC_NPROCESSORS_ONLN);
    if (nthreads < 1) nthreads = 1;
#endif
}

void parse_bracket(char *arg, int *bracket_id, time_t *bracket_time) {
  int i, id, n;
  if (0 == arg || 0 == arg[0]) return;
  n = strlen(arg);
  for (i = 0; i < n; i++) {
    if (!isdigit(arg[i])) {
      if (-1 == (*bracket_time = get_date(arg, NULL))) {
	fprintf(logout, "warning: badly formatted date ignored: %s\n", arg);
      }
      return;
    }
  }
  if (1 == sscanf(arg, "%d", &id)) {
    *bracket_id = id;
    return;
  }
  fprintf(logout, "bad conversion of %s\n", arg);
}

#define iarg(oname, vname) \
if (!strcmp(oname, o)) { \
      if (*argc > 2) \
	vname = atoi((*argv)[2]); \
      else goto fail; \
      (*argc)-=2; \
      (*argv)+=2; \
    }

#define darg(oname, vname) \
if (!strcmp(oname, o)) { \
      if (*argc > 2) \
	vname = atof((*argv)[2]); \
      else goto fail; \
      (*argc)-=2; \
      (*argv)+=2; \
    }

#define sarg(oname, vname) \
if (!strcmp(oname, o)) { \
      if (*argc > 2) \
	vname = (*argv)[2]; \
      else goto fail; \
      (*argc)-=2; \
      (*argv)+=2; \
    }

char *help_string =
"electric sheep v%s - the collective dream of sleeping\n"
"                      computers from all over the internet.\n"
"                      see http://electricsheep.org\n"
"\n"
"usage: electricsheep [options]\n"
"electricsheep-preferences to set default options\n"
"\n"
"--preferences file (the location for the preferences,\n"
"               the default is ~/.electricsheep/preferences.xml)\n"
"--nick name (credit frames to this name on the server, default is none)\n"
"--url url (in server credit, link name to this url, default is none)\n"
"--password string (authenticate yourself to the server)\n"
"--nthreads N (number of rendering threads, default is 1)\n"
"--frame-rate N (frames/second)\n"
"--timeout N (seconds, default is 401)\n"
"--tryagain N (seconds between retries to server, default is 696)\n"
"--no-animation 0/1 (invisibility if 1, default 0)\n"
"--standalone 0/1 (disables render & download, default 0)\n"
"--save-dir path (directory in which to save anims)\n"
"--save-frames 0/1 (keeps images rendered in the saved anim directory)\n"
"--max-megabytes N (maximum disk space used to save anims in megabytes,\n"
"               default is 2000, 0 means no limit)\n"
"--min-megabytes N (minimum disk space to leave free in megabytes,\n"
"               default is 100, 0 means no limit)\n"
"--nice n (priority adjustment for render process, default 10)\n"
"--nrepeats n (number of times to repeat loopable animations, default 2)\n"
"--proxy url (connect to server through proxy (see curl(1)))\n"
"--proxy-user user:password (proxy account (see curl(1)))\n"
"--start-sheep id (play this sheep first)\n"
"--root 0/1 (display on root window, default 0)\n"
"-window-id id (display in an existing window, note single dash!)\n"
"--debug 0/1 (prints additional info)\n"
"--bracket-begin id/date (play no sheep before this one or this time)\n"
"--bracket-end id/date (play no sheep after this one or this time)\n"
"--data-dir dir (directory to find splash images and other data files)\n"
"--logfile file (file to write the log instead of stdout)\n"
"--video-driver name (passed on to mplayer -vo, try \"x11\" and \"gl\")\n"
;


char rc_file_name[PATH_MAX];



void
flags_init(int *argc, char ***argv) {
    char *arg0 = (*argv)[0];
    char *bracket_begin = 0;
    char *bracket_end = 0;
    while (*argc > 1 &&
	   (*argv)[1][0] == '-') {
	char *o = (*argv)[1];
	if (!strcmp("--help", o)) {
	    printf(help_string, VERSION);
	    exit(0);
	}
	else iarg("--frame-rate", prefs.frame_rate)
        else iarg("--timeout", timeout)
	else iarg("--tryagain", tryagain)
	else sarg("--nick", prefs.nick)
	else sarg("--url", prefs.url)
	else sarg("--save-dir", leave_prefix)
	else iarg("--max-megabytes", prefs.cache_size)
	else iarg("--min-megabytes", min_megabytes)
	else iarg("--no-animation", prefs.no_animation)
	else iarg("--parasite", parasite)
	else iarg("--standalone", prefs.standalone)
	else iarg("--nrepeats", prefs.nrepeats)
	else iarg("--max-repeats", playback.max_repeats)
	else iarg("--nthreads", nthreads)
	else iarg("--debug", debug)
	else sarg("--proxy", prefs.proxy_name)
	else iarg("--start-sheep", start_id)
	else sarg("--proxy-user", prefs.proxy_user)
	else sarg("-window-id", window_id)
	else iarg("--root", on_root)
	else iarg("--read-only",read_only)
	else iarg("--hide-errors",prefs.hide_errors)
	else sarg("--bracket-begin",bracket_begin)
	else sarg("--bracket-end",bracket_end)
	else sarg("--logfile",logfile)
	else sarg("--video-driver",prefs.video_driver)
	else sarg("--password", prefs.password)
	else iarg("--save-frames", prefs.save_frames)
	else if (!strcmp("--preferences", o)) {
	    /* skip it */
	    (*argc)-=2; 
	    (*argv)+=2; 
	} else {
	    fprintf(logout, "bad option: %s, try --help\n", o);
	    exit(1);
	}
    }
    (*argv)[0] = arg0;

    parse_bracket(bracket_begin, &bracket_begin_id, &bracket_begin_time);
    parse_bracket(bracket_end, &bracket_end_id, &bracket_end_time);

    if (window_id && strlen(window_id) > 20) {
	fprintf(logout, "window-id too long: %s.\n", window_id);
	exit(1);
    }

    if (leave_prefix && strlen(leave_prefix) > PATH_MAX) {
	fprintf(logout, "save-dir too long: %s.\n", leave_prefix);
	exit(1);
    }
	
    if (prefs.proxy_user && strlen(prefs.proxy_user) > 100) {
	fprintf(logout, "proxy-user too long: %s.\n", prefs.proxy_user);
	exit(1);
    }

    if (prefs.proxy_name && strlen(prefs.proxy_name) > 100) {
	fprintf(logout, "proxy-name too long: %s.\n", prefs.proxy_name);
	exit(1);
    }

    if (on_root && window_id) {
	on_root = 0;
    }

    if (window_id && prefs.zoom) {
      prefs.zoom = 0;
    }

    if (playback.history_size <= 0) {
      fprintf(logout, "history must be positive, not %d.\n",
	      playback.history_size);
      exit(1);
    }
    playback.path_history = malloc(sizeof(int) * playback.history_size);

    if (-1 == nthreads) {
	nthreads = 1;
    } else if (0 > nthreads) {
        auto_nthreads();
    }

    if (debug) {
	hide_stderr = "";
    }

    if (!leave_prefix) {
	char b[MAXBUF];
	char *hom = getenv("HOME");
	if (!hom) {
	    fprintf(logout, "HOME envar not defined\n");
	    cleanup_and_exit(1);
	}
	if (strlen(hom) > PATH_MAX) {
	    fprintf(logout, "HOME envar too long: %s.\n", hom);
	    cleanup_and_exit(1);
	}
	snprintf(b, MAXBUF, "%s/.electricsheep/", hom);
	leave_prefix = strdup(b);
    } else if (leave_prefix[strlen(leave_prefix)-1] != '/') {
	char b[MAXBUF];
	snprintf(b, MAXBUF, "%s/", leave_prefix);
	leave_prefix = strdup(b);
    }
    if (setenv("sheep_leave_prefix", leave_prefix, 1)) {
	fprintf(logout,
		"warning: unable to set envar sheep_leave_prefix to %s.\n",
		leave_prefix);
    }

    if (strlen(prefs.nick)*3 > bufmax-3) {
	fprintf(logout, "nick_name too long.");
	cleanup_and_exit(1);
    }

    if (strlen(prefs.url)*3 > bufmax-3) {
	fprintf(logout, "url_name too long.");
	cleanup_and_exit(1);
    }
    encode(url_buf, prefs.url);

    return;
 fail:
    fprintf(logout, "no argument to %s\n", (*argv)[1]);
    exit(1);
}

void
do_lock() {
    char fn[MAXBUF];
    int fd;

    struct flock fl;
  
    snprintf(fn, MAXBUF, "%slock", leave_prefix);
    if (-1 == (fd = creat(fn, S_IRWXU))) {
	perror(fn);
	cleanup_and_exit(1);
    }

    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;

    if (-1 == fcntl(fd, F_SETLK, &fl)) {
	if ((EAGAIN == errno) || (EACCES == errno)) {
	    fprintf(logout, "detected another electricsheep process.\n"
		    "using read only access, ie disabling "
		    "downloading and rendering of sheep.\n");
	    parasite = 1;
	    read_only = 1;
	} else {
	    perror(fn);
	    cleanup_and_exit(1);
	}
    }
    /* leave the file open & locked until our process terminates */
}

void set_monitoraspect(char *wid) {
  int i, w=0, h;
  FILE *p;
  char cb[MAXBUF];
  int delay = 100 * 1000;

  for (i = 0; i < 3; i++) {
      snprintf(cb, MAXBUF, "xwininfo -id %s | egrep '(Width|Height)'", wid);

      p = popen(cb, "r");
      if (NULL == p) {
	  fprintf(logout, "xwininfo pipe failed\n");
	  break;
      }
  
      if (2 != fscanf(p, " Width: %d Height: %d", &w, &h)) {
	  fprintf(logout, "scanning window dimensions failed, try again in %dus\n", delay);
	  usleep(delay);
	  delay *= 2;
	  continue;
      }
      if (w <= 1 || h <= 1) {
	  if (debug) fprintf(logout, "waiting for window %dus\n", delay);
	  usleep(delay);
	  delay *= 2;
	  continue;
      }
  }
  if (w > 1) {
      sprintf(monitoraspect, "%d:%d", w, h);
      if (debug) fprintf(logout, "monitoraspect=%s\n", monitoraspect);
  } else {
      strcpy(monitoraspect, "4:3");
      if (debug) fprintf(logout, "could not read monitoraspect, assuming 4:3");
  }
}

int main(int argc, char **argv) {
    // would like to write this into argv[0] so it shows
    // up in ps output, but not clear to me how to allocate
    // the space for that.  assigning argv[0] doesn't work.
    thread_name = "main";
    logout = stderr;

    /* create our own group so all workers/children may
       be killed together without hassle */
#ifdef SETPGRP_VOID
    if (-1 == setpgrp()) perror("setpgrp");
#else
    if (-1 == setpgrp(getpid(), getpid())) perror("setpgrp");
#endif
    signal(SIGTERM, handle_sig_term);
    signal(SIGINT, handle_sig_term);

    if (1) {
	playback.history_size = 50;
	playback.last_sheep = -1;
	playback.nrepeated = 0;
	playback.max_repeats = 1;
	playback.reset_fuse = 0;
	playback.reset_fuse_length = -1; /* disabled */
    }

    set_rc_file(rc_file_name, argc, argv);
    default_rc(&prefs);
    read_rc(&prefs, rc_file_name);
    flags_init(&argc, &argv);

    if (logfile && logfile[0]) {
	logout = fopen(logfile, "a");
	if (NULL == logout) {
	    logout = stderr;
	    perror(logfile);
	} else
	    setlinebuf(logout);
    }

    if (debug) {
	fprintf(logout, "=====================================\n"
	       "electric sheep v%s\n", VERSION);
	timestamp("start");
    }

    if (NULL == window_id)
	window_id = getenv("XSCREENSAVER_WINDOW");

    if (window_id) set_monitoraspect(window_id);


    encode(nick_buf, prefs.nick);
    do_lock();
    playback.reset_fuse = playback.reset_fuse_length;
    init_curl_cmd(0);
    srandom(time(0) + getpid());

    play_count_init();

    if (!prefs.no_animation) {
        make_display_process();
    }


    simulated_time = time(0);

    if (!read_only) {
	
	update_cached_sheep();
	delete_cached(0);
	if (!prefs.standalone) {
	    make_download_process();
	}
    }

    if (!parasite && !prefs.standalone)
	make_render_process(); // does not return

    if (displayer_pid) {
	if (-1 == waitpid(displayer_pid, 0, 0)) {
	    perror("waitpid displayer_pid");
	    exit(1);
	}
    }
    if (downloader_pid) {
	if (-1 == waitpid(displayer_pid, 0, 0)) {
	    perror("waitpid downloader_pid");
	    exit(1);
	}
    }
    if (ui_pid) {
	if (-1 == waitpid(ui_pid, 0, 0)) {
	    perror("waitpid ui_pid");
	    exit(1);
	}
    }
    exit(0);
}
