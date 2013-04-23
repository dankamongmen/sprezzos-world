The Refocus Gimp plug-in can be used to sharpen images.  Frequently,
when processing images, e.g. scanning photo's or slides, the images
become slightly blurred.  This plug-in attempts to "refocus" the
image.  In many cases this plug-in produces much better results than
plug-ins such as sharpen or unsharp mask.

This plug-in has a preview that helps you select the best parameters.

This version of the plug-in needs the Gtk+-2.0 libraries.

You can find instructions for installing and using Refocus
in the files doc.html or doc.pdf in the doc subdirectory.

This is the first public release of this plug-in.  I hope you like it.
If you have any problems or requests please mail me.


Ernst Lippe <ernstl@users.sourceforge.net>


Additional hacking stuff for Gimp >= 2.2: (added by Peter Heckert)
=================================================
I have hacked the source to compile with gimp-2.2
I made these modifications:

Replaced all strings and substrings "GimpPreview" against "RefocusPreview"
Replaced all occurences of "drawable->id" against "drawable->drawable_id"
Included gimpcompat.h where appropriate
Modified src/Makefile.am and src/Makefile.in to link against libgimp-2.0.so rather than libgimp.so

=================================================
Build and install as follows:
cd into the refocus directory
$ ./configure
$ make

The compiled binary named "refocus" should be found in the "src" directory.
Copy it into your plugins folder and you should be ready to go.
("make install-bin" might or might not work for you, copying the binary is a sure bet)
(e.g. "gimptool --install-bin <path to binary> should also work)

I wanted to have this nice plugin for gimp >= 2.2 therefore I made this.
I am not maintainer nor creator for the refocus plugin; but I am willing to record and possibly fix
installation problems so far my skills and time allow for this.

peter /dot/ heckert /at/ arcor /dot/ de


