# check if gtk is avialable

from apt_listchanges import frontend

import pygtk
pygtk.require('2.0')
import gtk
import gobject
import gtk.glade

from ALChacks import *

# set the i18n dirs
gtk.glade.bindtextdomain("apt-listchanges", "/usr/share/locale")
gtk.glade.textdomain("apt-listchanges")

class gtk2(frontend):
    def flush_interface(self):
        while gtk.events_pending():
            gtk.main_iteration()

    def cb_close(self, widget):
        if self.button_close.get_property("sensitive") == False:
            # window manager was used to close before the parsing was complete
            sys.exit()
        gtk.main_quit()

    def __init__(self, packages, config):
        frontend.__init__(self,packages, config)
        try:
            file("apt-listchanges/apt-listchanges.glade").close()
            self.glade = gtk.glade.XML("apt-listchanges/apt-listchanges.glade")
        except:
            self.glade = gtk.glade.XML("/usr/share/apt-listchanges/apt-listchanges.glade")
        self.window_main = self.glade.get_widget("window_main")
        self.window_main.connect("destroy", self.cb_close)
        self.glade.signal_connect("on_button_close_clicked", self.cb_close)
        self.progressbar_main = self.glade.get_widget("progressbar_main")
        self.button_close = self.glade.get_widget("button_close")
        self.flush_interface()

    def display_output(self,text):
        self.button_close.set_sensitive(True)
        buf = self.glade.get_widget("textview_main").get_buffer()
        buf.set_text(self._render(text))
        gtk.main()

    def update_progress(self):
        if not hasattr(self,'progress'):
            # First call
            self.progress = 0.0
            self.progressbar_main.show()
        self.progress += 1.0
        self.progressbar_main.set_fraction(self.progress / self.packages)
        self.progressbar_main.set_text(("%i%%" % (self.progress*100 / self.packages)))
        self.flush_interface()

    def progress_done(self):
        self.progressbar_main.hide()
        self.flush_interface()

    def confirm(self):
        m = gtk.MessageDialog(self.window_main,
                              gtk.DIALOG_MODAL,
                              gtk.MESSAGE_QUESTION,
                              gtk.BUTTONS_YES_NO)
        m.set_default_response(gtk.RESPONSE_YES)
        m.set_markup("<big><b>%s</b></big>\n\n%s" % (_("Continue Installation?"), _("You can abort the installation if you select 'no'.")))
        if m.run() == gtk.RESPONSE_NO:
            return False
        return True
