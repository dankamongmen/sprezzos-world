#!/usr/bin/env python
# -*- coding: utf-8 -*-
# This is the main file of btn4ws.
#
# Copyright (c) 1999-2009 Jan Dittberner <jan@dittberner.info>
#
# This file is part of btn4ws.
#
# btn4ws is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# btn4ws is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with btn4ws; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
# version: $Id: btn4ws.py 21 2009-03-15 07:56:39Z jan $
#

"""
Gimp script to generate button images for websites. This script is a
port of the older gimp-perl version to python.

(c) 2007, 2008, 2009 Jan Dittberner <jan@dittberner.info>
"""
import os, urllib, logging, sys, pickle
import gimp, gimpplugin, gimpui, gimpcolor
import pygtk
pygtk.require('2.0')
import gtk
from gimpenums import *
from gimpshelf import shelf
pdb = gimp.pdb

btn4ws_version = "0.8.0.1"

logging.basicConfig(level=logging.WARN,
                    format='%(asctime)s %(levelname)s %(message)s',
                    stream=sys.stderr)

class text_to_name_mapper:
    """
    Text string to name mapper class. This class provides mappings for several target
    environments.
    """
    def __init__(self, strings):
        self.mapping = {}
        for string in strings: self.mapping[string] = {}
        self.idnum = 1
        logging.debug("self.mapping=" + str(self.mapping))
    
    def asitemid(self, text):
        """
        Get a img itemid for the given text.
        """
        if 'itemid' not in self.mapping[text]:
            self.mapping[text]['itemid'] = "id%03d" % (self.idnum)
            self.idnum += 1
        #logging.debug("self.mapping=" + str(self.mapping))
        return self.mapping[text]['itemid']

    def asjavascriptid(self, text):
        """
        Get a javascript itemid for the given text.
        """
        if 'jsid' not in self.mapping[text]:
            self.mapping[text]['jsid'] = "img%03d" % (self.idnum)
            self.idnum += 1
        #logging.debug("self.mapping=" + str(self.mapping))
        return self.mapping[text]['jsid']

    def aslinktarget(self, text):
        """
        Get a link target for the given text.
        """
        if 'link' not in self.mapping[text]:
            self.mapping[text]['link'] = urllib.quote(text)
        #logging.debug("self.mapping=" + str(self.mapping))
        return "%s.html" % (self.mapping[text]['link'])

    def asfilename(self, text, extension = 'png', prefix= '', dirname = None):
        """
        Get a filename for the given text with optional extension, prefix and dirname.
        """
        if 'file' not in self.mapping[text]:
            self.mapping[text]['file'] = text.encode('ascii', 'ignore')
        fname = "%s%s.%s" % (prefix, self.mapping[text]['file'], extension)
        #logging.debug("self.mapping=" + str(self.mapping))
        if dirname:
            return os.path.join(dirname, fname)
        return fname

class text_to_name_mapper:
    """
    Text string to name mapper class. This class provides mappings for
    several target environments.
    """
    def __init__(self, strings):
        self.mapping = {}
        for string in strings: self.mapping[string] = {}
        self.idnum = 1
        logging.debug("self.mapping=" + str(self.mapping))
    
    def asitemid(self, text):
        """
        Get a img itemid for the given text.
        """
        if 'itemid' not in self.mapping[text]:
            self.mapping[text]['itemid'] = "id%03d" % (self.idnum)
            self.idnum += 1
        logging.debug("self.mapping=" + str(self.mapping))
        return self.mapping[text]['itemid']

    def asjavascriptid(self, text):
        """
        Get a javascript itemid for the given text.
        """
        if 'jsid' not in self.mapping[text]:
            self.mapping[text]['jsid'] = "img%03d" % (self.idnum)
            self.idnum += 1
        logging.debug("self.mapping=" + str(self.mapping))
        return self.mapping[text]['jsid']

    def aslinktarget(self, text):
        """
        Get a link target for the given text.
        """
        if 'link' not in self.mapping[text]:
            self.mapping[text]['link'] = urllib.quote(text)
        logging.debug("self.mapping=" + str(self.mapping))
        return "%s.html" % (self.mapping[text]['link'])

    def asfilename(self, text, extension = 'png', prefix= '', dirname = None):
        """
        Get a filename for the given text with optional extension,
        prefix and dirname.
        """
        if 'file' not in self.mapping[text]:
            self.mapping[text]['file'] = text.encode('ascii', 'ignore')
        fname = "%s%s.%s" % (prefix, self.mapping[text]['file'], extension)
        logging.debug("self.mapping=" + str(self.mapping))
        if dirname:
            return os.path.join(dirname, fname)
        return fname

class IntEntry(gtk.Entry):
    """Input field for integer numbers."""

    def __init__(self, max = 0):
        gtk.Entry.__init__(self, max)
        self.set_property("truncate-multiline", True)
        self.connect("insert-text", self._cb_int_field_insert)

    def _cb_int_field_insert(self, w, new_text, new_text_length, position):
        """Allow integer input only."""
        if not new_text.isdigit():
            w.stop_emission("insert-text")

class Btn4wsDialog(gtk.Assistant):
    """This class is the input dialog field for btn4ws"""
    def _cb_delete_event(self, widget, event, data = None):
        logging.debug("delete_event")
        return False

    def __init__(self, args):
        self.data = args
        self.pages = {}
        gtk.Assistant.__init__(self)

        self._addIntroPage()
        self._addPathSelectionPage()
        self._addBasicSettingsPage()
        self._addLayoutPage()
        self._addEffectsPage()
        self._addLastPage()
        self.show()
        self.connect("delete_event", self._cb_delete_event)

        for pagename in self.pages.iterkeys():
            self.checkcompletion(pagename)

    def _addIntroPage(self):
        label = gtk.Label("""Buttons for website allows you to produce a series of buttons for use on a website. On the next pages you may choose several options to change the content and the look of the buttons.""")
        label.set_line_wrap(True)
        label.show()
        self.append_page(label)
        self.set_page_title(label, "Introduction")
        self.set_page_complete(label, True)

    def _addPathSelectionPage(self):
        page = gtk.VBox(False, 5)
        self.pages["pathselection"] = page
        page.set_border_width(5)
        page.show()
        self.append_page(page)
        self.set_page_title(page,
                            "Select the input file and output directory")
        self.set_page_type(page, gtk.ASSISTANT_PAGE_CONTENT)
        
        label = gtk.Label("Please choose the file containing your button labels and the directory where the generated files should be put.")
        label.set_line_wrap(True)
        label.show()
        page.pack_start(label, True, True, 0)

        table = gtk.Table(rows=2, columns=2, homogeneous=False)
        table.show()
        label = gtk.Label("Button label file")
        label.show()
        table.attach(label, 0, 1, 0, 1)
        button = gtk.FileChooserButton("Choose file")
        button.set_action(gtk.FILE_CHOOSER_ACTION_OPEN)
        if self.data["filename"]:
            button.set_filename(self.data["filename"])
        button.connect("selection-changed", self._cb_file_selected)
        button.show()
        table.attach(button, 1, 2, 0, 1)

        label = gtk.Label("Output directory")
        label.show()    
        table.attach(label, 0, 1, 1, 2)
        button = gtk.FileChooserButton("Choose directory")
        button.set_action(gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER)
        if self.data["outdir"]:
            button.set_filename(self.data["outdir"])
        else:
            self.data["outdir"] = button.get_filename()
        button.connect("selection-changed", self._cb_dir_selected)
        button.show()
        table.attach(button, 1, 2, 1, 2)
        
        page.pack_end(table)

    def _addBasicSettingsPage(self):
        page = gtk.VBox(False, 5)
        self.pages["basicsettings"] = page
        page.set_border_width(5)
        page.show()
        self.append_page(page)
        self.set_page_title(page,
                           "Select the basic button settings")
        self.set_page_type(page, gtk.ASSISTANT_PAGE_CONTENT)

        label = gtk.Label("Please choose the basic layout settings of your buttons.")
        label.set_line_wrap(True)
        label.show()
        page.pack_start(label, True, True, 0)

        table = gtk.Table(rows=4, columns=2, homogeneous=False)
        table.show()

        # font
        label = gtk.Label("Button text font")
        label.show()
        table.attach(label, 0, 1, 0, 1)
        fontsel = gtk.FontButton()
        fontsel.set_show_size(True)
        if self.data["font"]:
            fontsel.set_font_name(self.data["font"])
        else:
            self.data["font"] = fontsel.get_font_name()
        fontsel.show()
        fontsel.connect("font-set", self._cb_set_font)
        table.attach(fontsel, 1, 2, 0, 1)

        # strcolor
        label = gtk.Label("Button text color")
        label.show()
        table.attach(label, 0, 1, 1, 2)
        colorsel = gimpui.ColorSelector()
        if self.data["strcolor"]:
            colorsel.set_color(self.data["strcolor"])
        else:
            self.data["strcolor"] = colorsel.get_color()
        colorsel.show()
        colorsel.connect("color-changed", self._cb_set_color, "strcolor",
                         "basicsettings")
        table.attach(colorsel, 1, 2, 1, 2)

        # background toggle
        bgtoggle = gtk.CheckButton("Use a pattern for button")
        bgtoggle.set_active(self.data["usepattern"])
        bgtoggle.show()
        table.attach(bgtoggle, 1, 2, 2, 3)

        # background color / pattern
        if self.data["usepattern"]:
            label = gtk.Label("Button pattern")
        else:
            label = gtk.Label("Button color")
        label.show()
        patternsel = gimpui.PatternSelectButton()
        if self.data["pattern"]:
            patternsel.set_pattern(self.data["pattern"])
        patternsel.connect("pattern-set", self._cb_set_pattern)
        colorsel = gimpui.ColorSelector()
        if self.data["buttoncolor"]:
            colorsel.set_color(self.data["buttoncolor"])
        else:
            self.data["buttoncolor"] = colorsel.get_color()
        colorsel.connect("color-changed", self._cb_set_color, "buttoncolor",
                         "basicsettings")
        bgtoggle.connect("toggled", self._cb_bgtoggle_toggle, label,
                         "Button pattern", patternsel,
                         "Button color", colorsel)
        if self.data["usepattern"]:
            patternsel.show()
        else:
            colorsel.show()

        table.attach(label, 0, 1, 3, 4)
        table.attach(patternsel, 1, 2, 3, 4)
        table.attach(colorsel, 1, 2, 3, 4)

        page.pack_end(table)

    def _addLayoutPage(self):
        page = gtk.VBox(False, 5)
        self.pages["layout"] = page
        page.set_border_width(5)
        page.show()
        self.append_page(page)
        self.set_page_title(page,
                           "Select the layout settings")
        self.set_page_type(page, gtk.ASSISTANT_PAGE_CONTENT)

        label = gtk.Label("Select the layout options for your buttons")
        label.set_line_wrap(True)
        label.show()
        page.pack_start(label, True, True, 0)

        table = gtk.Table(rows=5, columns=2, homogeneous=False)
        table.show()

        #roundradius
        label = gtk.Label("Round radius")
        label.show()
        entry = IntEntry(max = 2)
        if self.data["roundradius"]:
            entry.set_text(str(self.data["roundradius"]))
        entry.connect("changed", self._cb_set_intvalue, "roundradius",
                      "layout")
        entry.show()
        table.attach(label, 0, 1, 0, 1)
        table.attach(entry, 1, 2, 0, 1)

        #bevelwidth
        label = gtk.Label("Bevel width")
        label.show()
        entry = IntEntry(max = 2)
        entry.connect("changed", self._cb_set_intvalue, "bevelwidth",
                      "layout")
        entry.show()
        table.attach(label, 0, 1, 1, 2)
        table.attach(entry, 1, 2, 1, 2)

        #padding
        label = gtk.Label("Padding")
        label.show()
        entry = IntEntry(max = 2)
        entry.connect("changed", self._cb_set_intvalue, "padding",
                      "layout")
        entry.show()
        table.attach(label, 0, 1, 2, 3)
        table.attach(entry, 1, 2, 2, 3)

        #transparency
        transp = gtk.CheckButton("Transparent button background")
        transp.set_active(self.data["transparency"])
        transp.show()
        transp.connect("toggled", self._cb_toggle_simple, "transparency",
                       "layout")
        table.attach(transp, 1, 2, 3, 4)

        #bgcolor
        label = gtk.Label("Background color")
        label.show()
        colorsel = gimpui.ColorSelector()
        if self.data["bgcolor"]:
            colorsel.set_color(self.data["bgcolor"])
        else:
            self.data["bgcolor"] = colorsel.get_color()
        colorsel.connect("color-changed", self._cb_set_color, "bgcolor",
                         "layout")
        colorsel.show()
        table.attach(label, 0, 1, 4, 5)
        table.attach(colorsel, 1, 2, 4, 5)

        page.pack_end(table)

    def _addEffectsPage(self):
        page = gtk.VBox(False, 5)
        self.pages["effects"] = page
        page.set_border_width(5)
        page.show()
        self.append_page(page)
        self.set_page_title(page,
                           "Select the effect settings")
        self.set_page_type(page, gtk.ASSISTANT_PAGE_CONTENT)

        table = gtk.Table(rows=7, columns=2, homogeneous=False)
        table.show()

        #nova
        novatoggle = gtk.CheckButton("Enable nova effect")
        novatoggle.set_active(self.data["nova"])
        novatoggle.show()
        table.attach(novatoggle, 1, 2, 0, 1)

        #novacolor
        label = gtk.Label("Nova color")
        label.show()
        novacolor = gimpui.ColorSelector()
        if self.data["novacolor"]:
            novacolor.set_color(self.data["novacolor"])
        else:
            self.data["novacolor"] = novacolor.get_color()
        novacolor.connect("color-changed", self._cb_set_color, "novacolor",
                          "effects")
        novacolor.set_sensitive(self.data["nova"])
        novacolor.show()
        table.attach(label, 0, 1, 1, 2)
        table.attach(novacolor, 1, 2, 1, 2)
        
        #novaradius
        label = gtk.Label("Nova radius")
        label.show()
        novaradius = IntEntry(max = 2)
        if self.data["novaradius"] is not None:
            novaradius.set_text(str(self.data["novaradius"]))
        novaradius.connect("changed", self._cb_set_intvalue, "novaradius",
                           "effects")
        novaradius.set_sensitive(self.data["nova"])
        novaradius.show()
        table.attach(label, 0, 1, 2, 3)
        table.attach(novaradius, 1, 2, 2, 3)

        #novasparkles
        label = gtk.Label("Nova sparkles")
        label.show()
        novasparkles = IntEntry(max = 2)
        if self.data["novasparkles"] is not None:
            novasparkles.set_text(str(self.data["novasparkles"]))
        novasparkles.connect("changed", self._cb_set_intvalue, "novasparkles",
                             "effects")
        novasparkles.set_sensitive(self.data["nova"])
        novasparkles.show()
        table.attach(label, 0, 1, 3, 4)
        table.attach(novasparkles, 1, 2, 3, 4)
        novatoggle.connect("toggled", self._cb_nova_toggle, novacolor,
                           novaradius, novasparkles)
        
        #glow
        glowtoggle = gtk.CheckButton("Enable glow effect")
        glowtoggle.set_active(self.data["glow"])
        glowtoggle.show()
        table.attach(glowtoggle, 1, 2, 4, 5)

        #glowcolor        
        label = gtk.Label("Glow color")
        label.show()
        glowcolor = gimpui.ColorSelector()
        if self.data["glowcolor"]:
            glowcolor.set_color(self.data["glowcolor"])
        else:
            self.data["glowcolor"] = glowcolor.get_color()
        glowcolor.connect("color-changed", self._cb_set_color, "glowcolor",
                          "effects")
        glowcolor.set_sensitive(self.data["glow"])
        glowcolor.show()
        table.attach(label, 0, 1, 5, 6)
        table.attach(glowcolor, 1, 2, 5, 6)

        #glowsize
        label = gtk.Label("Glow size")
        label.show()
        glowsize = IntEntry(max = 2)
        glowsize.connect("changed", self._cb_set_intvalue, "glowsize",
                         "effects")
        glowsize.set_sensitive(self.data["glow"])
        glowsize.show()
        table.attach(label, 0, 1, 6, 7)
        table.attach(glowsize, 1, 2, 6, 7)
        glowtoggle.connect("toggled", self._cb_glow_toggle, glowcolor,
                           glowsize)

        page.pack_end(table)

    def _addLastPage(self):
        page = gtk.VBox(False, 5)
        self.pages["output"] = page
        page.set_border_width(5)
        page.show()
        self.append_page(page)
        self.set_page_title(page,
                           "Output formats")
        self.set_page_type(page, gtk.ASSISTANT_PAGE_CONFIRM)

        label = gtk.Label("Choose the output data.")
        label.show()
        page.pack_start(label)

        #makejscript
        toggle = gtk.CheckButton("Make HTML, CSS and JavaScript")
        toggle.set_active(self.data["makejscript"])
        toggle.connect("toggled", self._cb_toggle_simple, "makejscript",
                       "output")
        toggle.show()
        page.pack_start(toggle)

        #makeinactive
        toggle = gtk.CheckButton("Make inactive buttons")
        toggle.set_active(self.data["makeinactive"])
        toggle.connect("toggled", self._cb_toggle_simple, "makeinactive",
                       "output")
        toggle.show()
        page.pack_start(toggle)

        #makeactive
        toggle = gtk.CheckButton("Make active buttons")
        toggle.set_active(self.data["makeactive"])
        toggle.connect("toggled", self._cb_toggle_simple, "makeactive",
                       "output")
        toggle.show()
        page.pack_start(toggle)

        #makepressed
        toggle = gtk.CheckButton("Make pressed buttons")
        toggle.set_active(self.data["makepressed"])
        toggle.connect("toggled", self._cb_toggle_simple, "makepressed",
                       "output")
        toggle.show()
        page.pack_start(toggle)

        #writexcf
        toggle = gtk.CheckButton("Write the XCF file")
        toggle.set_active(self.data["writexcf"])
        toggle.connect("toggled", self._cb_toggle_simple, "writexcf",
                       "output")
        toggle.show()
        page.pack_start(toggle)

    def checkcompletion(self, pagename):
        criteriamatched = False
        if pagename == "pathselection":
            criteriamatched = self.data["filename"] is not None and \
                self.data["outdir"] is not None
        elif pagename == "basicsettings":
            criteriamatched = self.data["font"] is not None and \
                self.data["strcolor"] is not None and \
                ((self.data["usepattern"] and self.data["pattern"]) or \
                     (not self.data["usepattern"] and \
                          self.data["buttoncolor"]))
        elif pagename == "layout":
            criteriamatched = self.data["roundradius"] is not None and \
                self.data["bevelwidth"] is not None and \
                self.data["padding"] is not None and \
                (self.data["transparency"] or self.data["bgcolor"] is not None)
        elif pagename == "effects":
            criteriamatched = (self.data["nova"] == False or ( \
                    self.data["novacolor"] is not None and \
                        self.data["novaradius"] is not None and \
                        self.data["novasparkles"] is not None)) and \
                        (self.data["glow"] == False or ( \
                    self.data["glowcolor"] is not None and \
                        self.data["glowsize"] is not None))
        elif pagename == "output":
            criteriamatched = self.data["makejscript"] is not None and \
                self.data["makeinactive"] is not None and \
                self.data["makeactive"] is not None and \
                self.data["makepressed"] is not None and \
                self.data["writexcf"] is not None
        if criteriamatched:
            self.set_page_complete(self.pages[pagename], True)
        else:
            self.set_page_complete(self.pages[pagename], False)

    def _cb_set_intvalue(self, w, fieldname, pagename):
        try:
            self.data[fieldname] = int (w.get_text())
        except ValueError:
            pass
        self.checkcompletion(pagename)

    def _cb_toggle_simple(self, w, datafield, pagename):
        self.data[datafield] = w.get_active()
        self.checkcompletion(pagename)

    def _cb_file_selected(self, w):
        self.data["filename"] = w.get_filename()
        self.checkcompletion("pathselection")

    def _cb_dir_selected(self, w):
        self.data["outdir"] = w.get_filename()
        self.checkcompletion("pathselection")

    def _cb_set_font(self, w):
        self.data["font"] = w.get_font_name()
        self.checkcompletion("basicsettings")

    def _cb_set_color(self, w, fieldname, pagename):
        self.data[fieldname] = w.get_color()
        self.checkcompletion(pagename)

    def _cb_set_pattern(self, w, patternname, width, height, bpp, mask_data,
                        finished):
        if finished:
            self.data["pattern"] = patternname
        self.checkcompletion("basicsettings")

    def _cb_bgtoggle_toggle(self, w, label, patterntext, patternsel,
                            colortext, colorsel):
        if w.get_active():
            self.data["usepattern"] = True
            colorsel.hide()
            patternsel.show()
            label.set_text(patterntext)
        else:
            self.data["usepattern"] = False
            patternsel.hide()
            colorsel.show()
            label.set_text(colortext)
        self.checkcompletion("basicsettings")

    def _cb_nova_toggle(self, w, colorfield, radiusfield, sparksfield):
        if w.get_active():
            self.data["nova"] = True
            colorfield.set_sensitive(True)
            radiusfield.set_sensitive(True)
            sparksfield.set_sensitive(True)
        else:
            self.data["nova"] = False
            colorfield.set_sensitive(False)
            radiusfield.set_sensitive(False)
            sparksfield.set_sensitive(False)
        self.checkcompletion("effects")

    def _cb_glow_toggle(self, w, colorfield, sizefield):
        if w.get_active():
            self.data["glow"] = True
            colorfield.set_sensitive(True)
            sizefield.set_sensitive(True)
        else:
            self.data["glow"] = False
            colorfield.set_sensitive(False)
            sizefield.set_sensitive(False)
        self.checkcompletion("effects")

class btn4wsplugin(gimpplugin.plugin):
    """This is the btn4ws gimp plugin."""
    def gimp2html_color(self, color):
        """
        Converts a color tuple to a hex encoded color for CSS.
        """
        return "#%02x%02x%02x" % (color[0], color[1], color[2])

    def parsefont(self, font):
        """
        Parses a font into its fontname and size parts.
        """
        parts = font.split(" ")
        return (" ".join(parts[:-1]), parts[-1])

    def toprocess(self, item):
        """
        Decides whether the plugin is able to process the item or not.
        """
        item = item.strip()
        return len(item) > 0 and not item.startswith('#')

    def getmaxextents(self, strings, fontsize, fontname):
        """
        Gets the maximum width and height of texts in strings array
        with the given font.
        """
        getextents = pdb['gimp_text_get_extents_fontname']
        maxx = 0
        maxy = 0
        for extents in [getextents(string, fontsize, 1, fontname)
                        for string in strings]:
            maxx = max(maxx, extents[0])
            maxy = max(maxy, extents[1])
        return (maxx, maxy)

    def writejs(self, dirname, strings, width, height, t2nm):
        buf = [
        "//",
        "// JavaScript generated by btn4ws version %s" % (btn4ws_version),
        "//",
        "",
        "// function to show image for given image_object",
        "function hilite(ObjID, imgObjName) {",
        "  ObjID.src = eval(imgObjName + '.src');",
        "  return true;",
        "}",
        ""
        ]
        for item in strings:
            for prefix in ('a_', 'i_', 'p_'):
                buf.append(
                    "%(prefix)s%(jsid)s = new Image(%(width)d, %(height)d); "
                    "%(prefix)s%(jsid)s.src = '%(fname)s';"
                    % {
                    'prefix' : prefix,
                    'jsid'   : t2nm.asjavascriptid(item),
                    'width'  : width,
                    'height' : height,
                    'fname'  : urllib.quote(t2nm.asfilename(item, 'png',
                                                            prefix))})
        jsfile = open(os.path.join(dirname, 'imgobjs.js'), 'w')
        jsfile.write("\n".join(buf))
        jsfile.close()

    def writecss(self, dirname, bgcolor):
        buf = [
        "html, body { background-color:%s; }" % (self.gimp2html_color(bgcolor)),
        "a img { border-width: 0; }"
        ]
        cssfile = open(os.path.join(dirname, 'format.css'), 'w')
        cssfile.write("\n".join(buf))
        cssfile.close()

    def writehtml(self, dirname, strings, width, height, t2nm):
        buf = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"',
        ' "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">',
        '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">',
        '<head>',
        ' <title>A JavaScript MouseOver Example</title>',
        ' <script src="imgobjs.js" type="text/javascript"></script>',
        ' <link rel="stylesheet" type="text/css" href="format.css"/>',
        '</head>',
        '<body>',
        ' <div>'
        ]
        for item in strings:
            buf.append(
            '<a href="%(target)s"'
            ' onmouseover="return hilite(%(imgid)s, \'a_%(jsid)s\');"'
            ' onmouseout="return hilite(%(imgid)s, \'i_%(jsid)s\');"'
            ' onmousedown="return hilite(%(imgid)s, \'p_%(jsid)s\');"'
            ' onmouseup="return hilite(%(imgid)s, \'a_%(jsid)s\');">'
            '<img src="%(fname)s" class="nav" id="%(imgid)s" width="%(width)d" height="%(height)d"'
            ' alt="%(text)s" /></a><br />' % {
                'target' : t2nm.aslinktarget(item),
                'imgid'  : t2nm.asitemid(item),
                'jsid'   : t2nm.asjavascriptid(item),
                'fname'  : urllib.quote(t2nm.asfilename(item, 'png', 'i_')),
                'width'  : width,
                'height' : height,
                'text'   : item})
        buf.extend([
        ' </div>',
        ' <p><a href="http://validator.w3.org/check/referer">'
        '<img src="http://www.w3.org/Icons/valid-xhtml11" alt="Valid XHTML 1.1!" height="31"'
        'width="88" /></a></p>',
        '</body>',
        '</html>',
        ''
        ])            
        htmlfile = open(os.path.join(dirname, 'example.html'), 'w')
        htmlfile.write("\n".join(buf))
        htmlfile.close()

    def saveaspng(self, fname, image, transparency):
        imgcopy = pdb['gimp_image_duplicate'](image)
        if transparency:
            imgcopy.merge_visible_layers(CLIP_TO_BOTTOM_LAYER)
        else:
            imgcopy.flatten()
        pdb['file_png_save'](imgcopy, imgcopy.active_layer, fname, fname,
                             False, 9, False, False, False, False, True)
        gimp.delete(imgcopy)

    def __init__(self):
        self.data = {}
        self.inputdata = {}
        self.datafile = os.path.join(gimp.directory, "btn4wsrc")

    def checkdata(self, data):
        logging.debug("checkdata " + str(data))
        valid = True
        if data["filename"] is None:
            logging.error("filename is None")
            valid = False
        else:
            try:
                if not os.path.isfile(data["filename"]):
                    logging.error("%s is not a file.", data["filename"])
                    valid = False
            except OSError, e:
                logging.error(e)
                valid = False
        if data["outdir"] is None:
            logging.error("outdir is None")
        else:
            try:
                if not os.path.isdir(data["outdir"]):
                    logging.error("%s is not a directory.", data["outdir"])
                    valid = False
            except OSError, e:
                logging.error(e)
                valid = False
        # simple None checks
        for key in ("font", "strcolor", "roundradius", "bevelwidth",
                    "padding", "makejscript", "makeinactive", "makeactive",
                    "makepressed", "writexcf"):
            if data[key] is None:
                logging.error("%s is None" % (key))
                valid = False
        if data["usepattern"]:
            if data["pattern"] is None:
                logging.error("usepattern is True and pattern is None")
                valid = False
        elif data["buttoncolor"] is None:
            logging.error("usepattern is False and buttoncolor is None")
            valid = False
        if not data["transparency"] and data["bgcolor"] is None:
            logging.error("transparency is not enabled and bgcolor is None")
            valid = False
        if data["nova"]:
            if data["novacolor"] is None:
                logging.error("nova is enabled and novacolor is None")
                valid = False
            if data["novaradius"] is None:
                logging.error("nova is enabled and novaradius is None")
                valid = False
            if data["novasparkles"] is None:
                logging.error("nova is enabled and novasparkles is None")
                valid = False
        if data["glow"]:
            if data["glowcolor"] is None:
                logging.error("glow is enabled and glowcolor is None")
                valid = False
            if data["glowsize"] is None:
                logging.error("glow is enabled and glowsize is None")
        return valid

    def makebuttons(self, filename = None, outdir = None, font = None,
               strcolor = None, transparency = False, bgcolor = None,
               glow = False, glowcolor = None, usepattern = False,
               pattern = None, buttoncolor = None, roundradius = None,
               padding = None, glowsize = None, bevelwidth = None,
               nova = False, novasparkles = None, novaradius = None,
               novacolor = None, writexcf = False, makeinactive = True,
               makeactive = True, makepressed = True, makejscript = True):
        # import used gimp pdb functions
        createtext = pdb['gimp_text_fontname']
        selectionlayeralpha = pdb['gimp_selection_layer_alpha']
        selectionfeather = pdb['gimp_selection_feather']
        bucketfill = pdb['gimp_edit_bucket_fill']
        selectionall = pdb['gimp_selection_all']
        editclear = pdb['gimp_edit_clear']
        rectselect = pdb['gimp_rect_select']
        ellipseselect = pdb['gimp_ellipse_select']
        selectionshrink = pdb['gimp_selection_shrink']
        selectionnone = pdb['gimp_selection_none']
        fill = pdb['gimp_edit_fill']
        bumpmap = pdb['plug_in_bump_map']
        novaplugin = pdb['plug_in_nova']
        xcfsave = pdb['gimp_xcf_save']
        
        gimp.progress_init()
        stringfile = open(filename)
        strings = [line.strip()
                   for line in stringfile.readlines()
                   if self.toprocess(line)]
        stringfile.close()
        t2nm = text_to_name_mapper(strings)
        (fontname, fontsize) = self.parsefont(font)
        (maxx, maxy) = self.getmaxextents(strings, fontsize, fontname)
        logging.debug("fontname: %s, fontsize: %d, maxx: %d, maxy: %d",
                      fontname, int(fontsize), maxx, maxy)
        width = maxx + (padding*4)
        height = maxy + (padding*4)
        logging.debug("width: %d, height: %d", width, height)
        
        if roundradius > height/2:
            roundradius = height/2 - 1
        if roundradius > width/2:
            roundradius = width/2 - 1
        logging.debug("roundradius: %d", roundradius)

        for text in strings:
            image = gimp.Image(width, height, RGB)
            image.disable_undo()
            gimp.set_foreground(strcolor)
            textlayer = createtext(image, None, padding*2, padding*2, text,
                                   0, 1, fontsize, 1, fontname)
            # center the text
            textlayer.set_offsets((image.width - textlayer.width)/2 - 1,
                                  (image.height - textlayer.height)/2 - 1)
            textlayer.lock_alpha = True
            textlayer.name = text
            if glow:
                texteffect = textlayer.copy(True)
                image.add_layer(texteffect, len(image.layers))
                offs = texteffect.offsets
                texteffect.resize(image.width, image.height, offs[0], offs[1])
                texteffect.lock_alpha = False
                image.active_layer = texteffect
                selectionlayeralpha(texteffect)
                selectionfeather(image, glowsize)
                gimp.set_foreground(glowcolor)
                bucketfill(texteffect, FG_BUCKET_FILL, NORMAL_MODE, 100, 0,
                           True, 0, 0)
            btnlayer0 = gimp.Layer(image, "Background", width, height,
                                   RGBA_IMAGE, 100, NORMAL_MODE)
            image.add_layer(btnlayer0, len(image.layers))
            selectionall(image)
            editclear(btnlayer0)
            offs = btnlayer0.offsets
            rectselect(image, offs[0] + roundradius, offs[1],
                       btnlayer0.width - roundradius*2, btnlayer0.height,
                       CHANNEL_OP_REPLACE, 0, 0)
            rectselect(image, offs[0], offs[1] + roundradius,
                       btnlayer0.width, btnlayer0.height - roundradius*2,
                       CHANNEL_OP_ADD, 0, 0)
            ellipseselect(image, offs[0], offs[1],
                          roundradius*2, roundradius*2,
                          CHANNEL_OP_ADD, False, 0, 0)
            ellipseselect(image, offs[0] + btnlayer0.width - roundradius*2,
                          offs[1],
                          roundradius*2, roundradius*2,
                          CHANNEL_OP_ADD, False, 0, 0)
            ellipseselect(image, offs[0],
                          offs[1] + btnlayer0.height - roundradius*2,
                          roundradius*2, roundradius*2,
                          CHANNEL_OP_ADD, False, 0, 0)
            ellipseselect(image, offs[0] + btnlayer0.width - roundradius*2,
                          offs[1] + btnlayer0.height - roundradius*2,
                          roundradius*2, roundradius*2,
                          CHANNEL_OP_ADD, False, 0, 0)
            selectionshrink(image, 1)
            selectionfeather(image, 2)
            if usepattern:
                pdb['gimp_context_set_pattern'](pattern)
                bucketfill(btnlayer0, PATTERN_BUCKET_FILL, NORMAL_MODE, 100, 0,
                           True, 0, 0)
            else:
                gimp.set_background(buttoncolor)
                bucketfill(btnlayer0, BG_BUCKET_FILL, NORMAL_MODE, 100, 0,
                           True, 0, 0)
            selectionnone(image)
            selectionlayeralpha(btnlayer0)
            selectionfeather(image, 2)
            bumplayer = gimp.Layer(image, "Bumpmap", width, height, RGBA_IMAGE,
                                   100, NORMAL_MODE)
            gimp.set_background(0, 0, 0)
            image.add_layer(bumplayer, 0)
            fill(bumplayer, BACKGROUND_FILL)
            for index in range(1, bevelwidth -1):
                greyness = index*255/bevelwidth;
                gimp.set_background(greyness, greyness, greyness)
                bucketfill(bumplayer, BG_BUCKET_FILL, NORMAL_MODE, 100, 0,
                           False, 0, 0)
                selectionshrink(image, 1)
            gimp.set_background(255, 255, 255)
            bucketfill(bumplayer, BG_BUCKET_FILL, NORMAL_MODE, 100, 0, False,
                       0, 0)
            selectionnone(image)
            btnlayer1 = btnlayer0.copy(True)
            btnlayer2 = btnlayer0.copy(True)
            image.add_layer(btnlayer1, len(image.layers))
            image.add_layer(btnlayer2, len(image.layers))
            bumpmap(image, btnlayer1, bumplayer, 125, 45, 3, 0, 0, 0, 0, 0,
                    0, 1)
            bumpmap(image, btnlayer2, bumplayer, 125, 45, 3, 0, 0, 0, 0, 0,
                    1, 1)
            image.remove_layer(bumplayer)
            #gimp.delete(bumplayer)
            if nova:
                novalayer = gimp.Layer(image, "Nova", width, height,
                                       RGBA_IMAGE, 75, NORMAL_MODE)
                image.add_layer(novalayer, 0)
                selectionall(image)
                image.active_layer = novalayer
                editclear(novalayer)
                selectionnone(image)
                novaplugin(image, novalayer, width/4, height/4,
                           novacolor, novaradius, novasparkles, 0)
            blackboard = gimp.Layer(image, "Blackboard", width, height,
                                    RGBA_IMAGE, 100, NORMAL_MODE)
            image.add_layer(blackboard, len(image.layers))
            selectionall(image)
            if transparency:
                blackboard.lock_alpha = True
                editclear(blackboard)
            else:
                gimp.set_background(bgcolor)
                bucketfill(blackboard, BG_BUCKET_FILL, NORMAL_MODE, 100, 0,
                           False, 0, 0)
            selectionnone(image)
            if writexcf:
                fname = t2nm.asfilename(text, 'xcf', dirname = outdir)
                xcfsave(0, image, textlayer, fname, fname)
            if makepressed:
                btnlayer0.visible = False
                btnlayer1.visible = False
                btnlayer2.visible = True
                if nova: novalayer.visible = True
                self.saveaspng(t2nm.asfilename(text, 'png', 'p_', outdir),
                               image, transparency)
            if makeactive:
                btnlayer0.visible = False
                btnlayer1.visible = True
                btnlayer2.visible = False
                if nova: novalayer.visible = True
                self.saveaspng(t2nm.asfilename(text, 'png', 'a_', outdir),
                               image, transparency)
            if makeinactive:
                btnlayer0.visible = True
                btnlayer1.visible = False
                btnlayer2.visible = False
                if nova: novalayer.visible = False
                self.saveaspng(t2nm.asfilename(text, 'png', 'i_', outdir),
                               image, transparency)
            image.enable_undo()
            #gimp.Display(image)
            gimp.progress_update((strings.index(text)+1)/len(strings))
            gimp.delete(image)
        if makejscript:
            self.writejs(outdir, strings, width, height, t2nm)
            self.writecss(outdir, bgcolor)
            self.writehtml(outdir, strings, width, height, t2nm)
        #gimp.displays_flush()

    def _cb_destroy(self, widget, data = None):
        logging.debug("destroy")
        gtk.main_quit()

    def _loaddata(self, data):
        try:
            if os.path.exists(self.datafile):
                return pickle.load(open(self.datafile))
        except OSError, e:
            pass
        return data

    def _storedata(self, data):
        try:
            pickle.dump(data, open(self.datafile, 'w'))
        except OSError, e:
            pass

    def _cb_apply(self, widget):
        self.data = widget.data
        logging.debug(str(self.data))
        if self.checkdata(self.data):            
            self.makebuttons(**self.data)
            shelf["btn4ws"] = self.data
            self._storedata(self.data)
        else:
            logging.error("checking data failed")

    def btn4ws(self, runmode, filename = None, outdir = None, font = None,
               strcolor = None, transparency = False, bgcolor = None,
               glow = False, glowcolor = None, usepattern = False,
               pattern = None, buttoncolor = None, roundradius = None,
               padding = None, glowsize = None, bevelwidth = None,
               nova = False, novasparkles = None, novaradius = None,
               novacolor = None, writexcf = False, makeinactive = True,
               makeactive = True, makepressed = True, makejscript = True):
        """
        This function controls the creation of the buttons and is
        registered as gimp plugin.
        """
        self.inputdata = {
            "filename" : filename, "outdir" : outdir, "font" : font,
            "strcolor" : strcolor, "transparency" : transparency,
            "bgcolor" : bgcolor, "glow" : glow, "glowcolor" : glowcolor,
            "usepattern" : usepattern, "pattern" : pattern,
            "buttoncolor" : buttoncolor, "roundradius" : roundradius,
            "padding" : padding, "glowsize" : glowsize,
            "bevelwidth" : bevelwidth, "nova" : nova,
            "novasparkles" : novasparkles, "novaradius" : novaradius,
            "novacolor" : novacolor, "writexcf" : writexcf,
            "makeinactive" : makeinactive, "makeactive" : makeactive,
            "makepressed" : makepressed, "makejscript" : makejscript
            }
        if runmode in (RUN_INTERACTIVE, RUN_WITH_LAST_VALS):
            if shelf.has_key("btn4ws"):
                self.inputdata = shelf["btn4ws"]
            else:
                self.inputdata = self._loaddata(self.inputdata)
            dialog = Btn4wsDialog(self.inputdata)
            dialog.connect("close", self._cb_destroy)
            dialog.connect("cancel", self._cb_destroy)
            dialog.connect("destroy", self._cb_destroy)
            dialog.connect("apply", self._cb_apply)
            gtk.main()
        elif runmode == RUN_NONINTERACTIVE:
            logging.debug("runmode noninteractive")
            if self.checkdata(self.inputdata):
                self.makebuttons(**self.inputdata)
            else:
                logging.error("checking data failed")
        else:
            logging.error("unknown runmode %d" % runmode)
            return

    def start(self):
        gimp.main(self.init, self.quit, self.query, self._run)

    def init(self):
        logging.debug("init")

    def quit(self):
        logging.debug("quit")

    def query(self):
        logging.debug("query")
        gimp.install_procedure(
            "btn4ws",
            "Buttons for website", """Creates buttons for a website. Which have the same size, layout, effects on it. It's possible to create JavaScript code, CSS and XHTML examples for MouseOver effects also.""",
            "Jan Dittberner",
            "Jan Dittberner <jan@dittberner.info>",
            "%s, %s" % (btn4ws_version,
                        "$Date: 2009-03-15 08:56:39 +0100 (Sun, 15 Mar 2009) $"),
            "<Toolbox>/Xtns/Render/Buttons for website ...",
            "", PLUGIN,
            [(PDB_INT32, "run_mode", "Run mode"),
             (PDB_STRING, "string_filename", "File containing the strings"),
             (PDB_STRING, "output_directory", "Directory for the output files"),
             (PDB_STRING, "font", "Font for the strings"), # "Sans 18"),
             (PDB_COLOR, "string_color", "Color of the strings"), #, (255, 255, 0)),
             (PDB_INT8, "transparent_background", "Keep the background transparent (This doesn't work in MS Internet Explorer <= 6.0)"), #, 0),
             (PDB_COLOR, "background_color", "Color of the background"), #, (7, 135, 255)),
             (PDB_INT8, "apply_glow", "Enable glow effect"), #, 1),
             (PDB_COLOR, "glow_color", "Color of the Glow effect"), #, (255, 180, 0)),
             (PDB_INT8, "use_pattern", "Use a pattern for the button"), #, 1),
             (PDB_STRING, "button_pattern", "Fill pattern of the button"), #, "Rain"),
             (PDB_COLOR, "button_color", "Button color (if you don't use a pattern)"), #, (0, 0, 255)),
             (PDB_INT32, "round_radius", "Radius of the round corners"), #, 20),
             (PDB_INT32, "padding", "Space around the text"), #, 3),
             (PDB_INT32, "glow_size", "Size of the Glow effect"), #, 10),
             (PDB_INT32, "bevel_width", "Width of the bevel"), #, 5),
             (PDB_INT8, "apply_nova", "Nova or not Nova?"), #, 1),
             (PDB_INT32, "nova_sparkles", "Sparkles of the Nova"), #, 5),
             (PDB_INT32, "nova_radius", "Radius of the Nova"), #, 2),
             (PDB_COLOR, "nova_color", "Color of the Nova effect"), #, (255, 238, 0)),
             (PDB_INT8, "write_xcf", "Write a GIMP xcf file"), #, 0),
             (PDB_INT8, "create_inactive", "Create Inactive Button"), #, 1),
             (PDB_INT8, "create_active", "Create Active Button"), #, 1),
             (PDB_INT8, "create_pressed", "Create Pressed Button"), #, 1),
             (PDB_INT8, "create_jscript", "Create JavaScript, HTML and CSS"), #, 1)
             ],
            [])

if __name__ == '__main__':
    btn4wsplugin().start()
