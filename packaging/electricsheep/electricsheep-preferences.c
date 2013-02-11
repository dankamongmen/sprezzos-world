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

static char *electricsheep_preferences_c_id =
"@(#) $Id: electricsheep-preferences.c,v 1.4 2008-05-16 19:44:08 spotspot Exp $";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <expat.h>

#include "electricsheep.h"
#include "config.h"

char rc_file_name[PATH_MAX];

char *pw_marker = " 31E671r91:96";

prefs_t prefs;

int debug = 0;

GtkLabel *title_label;
GtkLabel *test_label;
GtkEntry *nick_entry;
GtkEntry *url_entry;
GtkEntry *password_entry;
GtkEntry *video_driver_entry;
GtkSpinButton *frame_rate_spin;
GtkSpinButton *nrepeats_spin;
GtkSpinButton *cache_spin;
GtkCheckButton *no_animation_check;
GtkCheckButton *standalone_check;
GtkCheckButton *hide_errors_check;
GtkCheckButton *save_frames_check;
GtkButton *test_button;

void clear_prefs(prefs_t *prefs) {
    free(prefs->nick);
    prefs->nick = NULL;
    free(prefs->url);
    prefs->url = NULL;
    free(prefs->password);
    prefs->password = NULL;
    free(prefs->video_driver);
    prefs->video_driver = NULL;
    prefs->cache_size = -1;
    free(prefs->uid);
    prefs->uid = NULL;
    prefs->frame_rate = -1.0;
    prefs->play_evenly = 1.0;
    prefs->nrepeats = -1;
    prefs->zoom = 1;
    prefs->no_animation = -1;
    prefs->standalone = -1;
    prefs->hide_errors = -1;
    prefs->save_frames = -1;
}

void init_ui() {
    gtk_label_set_text(title_label, "Electric Sheep v" VERSION);
    if (prefs.nick)
	gtk_entry_set_text(nick_entry, prefs.nick);
    if (prefs.url)
	gtk_entry_set_text(url_entry, prefs.url);
    if (prefs.password)
	gtk_entry_set_text(password_entry, prefs.password[0] ? pw_marker : "");
    if (prefs.video_driver)
	gtk_entry_set_text(video_driver_entry, prefs.video_driver);
    if (prefs.frame_rate > 0.0)
	gtk_spin_button_set_value(frame_rate_spin, prefs.frame_rate);
    if (prefs.nrepeats >= 0)
	gtk_spin_button_set_value(nrepeats_spin, prefs.nrepeats);
    if (prefs.cache_size >= 0)
	gtk_spin_button_set_value(cache_spin, prefs.cache_size);
    if (prefs.no_animation >= 0)
	gtk_toggle_button_set_active((GtkToggleButton *) no_animation_check, prefs.no_animation);
    if (prefs.standalone >= 0)
	gtk_toggle_button_set_active((GtkToggleButton *) standalone_check, prefs.standalone);
    if (prefs.hide_errors >= 0)
	gtk_toggle_button_set_active((GtkToggleButton *) hide_errors_check, prefs.hide_errors);
    if (prefs.save_frames >= 0)
	gtk_toggle_button_set_active((GtkToggleButton *) save_frames_check, prefs.save_frames);
}

void read_ui() {
    prefs.nick = (char*)gtk_entry_get_text(nick_entry);
    prefs.url =  (char*)gtk_entry_get_text(url_entry);
    if (0 == prefs.nick[0]) {
      prefs.password = strdup("");
    } else {
      char *pw = (char*)gtk_entry_get_text(password_entry);
      pw = strdup(pw);
      if (strcmp(pw, pw_marker))
	prefs.password = encry(pw, prefs.nick);
    }
    prefs.video_driver = (char*)gtk_entry_get_text(video_driver_entry);
    prefs.frame_rate = gtk_spin_button_get_value(frame_rate_spin);
    prefs.nrepeats = (int) gtk_spin_button_get_value(nrepeats_spin);
    prefs.cache_size = (int) gtk_spin_button_get_value(cache_spin);
    prefs.no_animation = (int) gtk_toggle_button_get_active((GtkToggleButton *) no_animation_check);
    prefs.standalone = (int) gtk_toggle_button_get_active((GtkToggleButton *) standalone_check);
    prefs.hide_errors = (int) gtk_toggle_button_get_active((GtkToggleButton *) hide_errors_check);
    prefs.save_frames = (int) gtk_toggle_button_get_active((GtkToggleButton *) save_frames_check);
}



void on_cancelButton_clicked(GtkWidget *widget, gpointer user_data) {
    exit(0);
}

void on_saveButton_clicked(GtkWidget *widget, gpointer user_data) {
    read_ui();
    write_rc(&prefs, rc_file_name);
    exit(0);
}

void on_helpButton_clicked(GtkWidget *widget, gpointer user_data) {
    if (system("gnome-open http://electricsheep.org/client/LNX_" VERSION ".html &"))
	perror("gnome-open");
}

static void tick() {
    while (gtk_events_pending ())
	gtk_main_iteration ();
}

void on_testButton_clicked(GtkWidget *widget, gpointer user_data) {
    char buf[MAXBUF];
    FILE *lf;
    int talking = 0;

    gtk_label_set_text(test_label, "testing...");
    tick();
    read_ui();
    init_curl_cmd(1);
    encode(url_buf, prefs.url);
    encode(nick_buf, prefs.nick);

    snprintf(buf, MAXBUF, "%s 'http://%s/'", curl_cmd, server);
    lf = popen(buf, "r");
    if (NULL == lf) {
	perror("could not fork/pipe0\n");
	cleanup_and_exit(1);
    }    
    while (fgets(buf, MAXBUF, lf)) {
	if (!strcmp(buf, "</html>\n")) {
	    gtk_label_set_text(test_label, "talking...");
	    fclose(lf);
	    tick();
	    talking = 1;
	    break;
	}
    }
    if (!talking) {
	gtk_label_set_text(test_label, "failure");
	fclose(lf);
	return;
    }

    read_ui();
    init_curl_cmd(1);
    init_list_cmd(buf);
    if (debug) fprintf(logout, "test list %s\n", buf);
    lf = popen(buf, "r");
    if (NULL == lf) {
	perror("could not fork/pipe\n");
	cleanup_and_exit(1);
    }
    gtk_label_set_text(test_label, "talking....");
    tick();
    while (fgets(buf, MAXBUF, lf)) {
	if (!strcmp(buf, "</list>\n")) {
	    if (prefs.password && prefs.password[0])
		gtk_label_set_text(test_label, "good, registered");
	    else
		gtk_label_set_text(test_label, "good, anonymous");
	    fclose(lf);
	    return;
	}
    }
    if (prefs.password && prefs.password[0])
	gtk_label_set_text(test_label, "login failure");
    else
	gtk_label_set_text(test_label, "failure to connect 2");
    fclose(lf);
}

void get_widgets(GladeXML *xml) {
    title_label = (GtkLabel *) glade_xml_get_widget(xml, "titleLabel");
    if (NULL == title_label) {
	fprintf(logout, "titleLabel not found\n");
	exit(1);
    }
    test_label = (GtkLabel *) glade_xml_get_widget(xml, "testLabel");
    if (NULL == test_label) {
	fprintf(logout, "testLabel not found\n");
	exit(1);
    }
    test_button = (GtkButton *) glade_xml_get_widget(xml, "testButton");
    if (NULL == test_button) {
	fprintf(logout, "testButton not found\n");
	exit(1);
    }
    nick_entry = (GtkEntry *) glade_xml_get_widget(xml, "nickEntry");
    if (NULL == nick_entry) {
	fprintf(logout, "nickEntry not found\n");
	exit(1);
    }
    url_entry = (GtkEntry *) glade_xml_get_widget(xml, "urlEntry");
    if (NULL == url_entry) {
	fprintf(logout, "urlEntry not found\n");
	exit(1);
    }
    password_entry = (GtkEntry *) glade_xml_get_widget(xml, "passEntry");
    if (NULL == password_entry) {
	fprintf(logout, "passEntry not found\n");
	exit(1);
    }
    video_driver_entry = (GtkEntry *) glade_xml_get_widget(xml, "vdEntry");
    if (NULL == video_driver_entry) {
	fprintf(logout, "vdEntry not found\n");
	exit(1);
    }
    frame_rate_spin = (GtkSpinButton *) glade_xml_get_widget(xml, "frameSpin");
    if (NULL == frame_rate_spin) {
	fprintf(logout, "frameSpin not found\n");
	exit(1);
    }
    nrepeats_spin = (GtkSpinButton *) glade_xml_get_widget(xml, "repeatSpin");
    if (NULL == nrepeats_spin) {
	fprintf(logout, "repeatSpin not found\n");
	exit(1);
    }
    cache_spin = (GtkSpinButton *) glade_xml_get_widget(xml, "cacheSpin");
    if (NULL == cache_spin) {
	fprintf(logout, "cacheSpin not found\n");
	exit(1);
    }
    no_animation_check = (GtkCheckButton *) glade_xml_get_widget(xml, "noanimCheck");
    if (NULL == no_animation_check) {
	fprintf(logout, "noanimCheck not found\n");
	exit(1);
    }
    standalone_check = (GtkCheckButton *) glade_xml_get_widget(xml, "standaloneCheck");
    if (NULL == standalone_check) {
	fprintf(logout, "standaloneCheck not found\n");
	exit(1);
    }
    hide_errors_check = (GtkCheckButton *) glade_xml_get_widget(xml, "hideCheck");
    if (NULL == hide_errors_check) {
	fprintf(logout, "hideCheck not found\n");
	exit(1);
    }
    save_frames_check = (GtkCheckButton *) glade_xml_get_widget(xml, "saveCheck");
    if (NULL == save_frames_check) {
	fprintf(logout, "saveCheck not found\n");
	exit(1);
    }
}

void cleanup_and_exit(int status) {
    exit(status);
}

int main(int argc, char *argv[]) {
    GladeXML *xml;
    logout = stderr;
    gtk_init(&argc, &argv);
    clear_prefs(&prefs);
    set_rc_file(rc_file_name, argc, argv);
    read_rc(&prefs, rc_file_name);
    xml = glade_xml_new(PACKAGE_DATA_DIR "/electricsheep-preferences.glade", NULL, NULL);
    get_widgets(xml);
    init_ui();
    glade_xml_signal_autoconnect(xml);
    gtk_main();
    return 0;
}
