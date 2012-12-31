Copyright (C) 2004-2012 Andrew Mihal.

This file is part of Enblend.


* Programs

This packages contains the programs Enblend and Enfuse.  The source
code and further information about Enblend are available at:
        http://enblend.sf.net


** Enblend

Enblend is a tool for compositing images using a Burt&Adelson
multiresolution spline.  This technique tries to make the seams
between the input images invisible.  The basic idea is that image
features should be blended across a transition zone proportional in
size to the spatial frequency of the features.  For example, objects
like trees and windowpanes have rapid changes in color.  By blending
these features in a narrow zone, you will not be able to see the seam
because the eye already expects to see color changes at the edge of
these features.  Clouds and sky are the opposite.  These features have
to be blended across a wide transition zone because any sudden change
in color will be immediately noticeable.

Enblend expects each input file to have an alpha channel.  The alpha
channel should indicate the region of the file that has valid image
data.  Enblend compares the alpha regions in the input files to find
the areas where images overlap.  Alpha channels can be used to
indicate to Enblend that certain portions of an input image should not
contribute to the final image.

Enblend does not align images for you.  Use a tool like Hugin or
PanoTools to do this.  The files produced by these programs are
exactly what Enblend is designed to work with.


** Enfuse

Enfuse is a tool for automatic exposure blending, contrast blending
and much more.  It can be used to fuse an exposure bracketed step
automatically into a nicely looking image.  See
        doc/enfuse.info
and on Windows also
        contrib/enfuse_droplet/enfuse_droplet_readme.txt
for more information.


* Installation

GNU Make is required to build the project, because some of the
Makefiles contain pattern rules.


** Tarball

        ./configure YOUR-OPTIONS-IF-ANY-GO-HERE
        make
        make install


** Mercurial Repository

*** AutoConf/AutoMake

In the root directory of the project issue:

        make --makefile=Makefile.scm
        ./configure YOUR-OPTIONS-IF-ANY-GO-HERE
        make
        make install

The package fully supports VPATH builds.  Thus the following command
sequence builds in a separate directory.

        cd ROOT-DIRECTORY
        make --makefile=Makefile.scm
        mkdir BUILD-DIR
        cd BUILD-DIR
        ROOT-DIRECTORY/configure YOUR-OPTIONS-IF-ANY-GO-HERE
        make
        make install


*** CMake

The canonical way to build Enblend and Enfuse is with Autotools, this
is, autoconf(1) and automake(1).  An alternative CMake build has been
added since version 4.0.

The CMake build strives to replicate the Autotools build.  It may or
may not work for you.  It is currently maintained but not supported,
meaning that it could break anywhere anytime in the future.

        cmake .
        make
        make install

Analogously to Autotools, CMake allows for VPATH builds:

        mkdir BUILD-DIR
        cd BUILD-DIR
        cmake ROOT-DIRECTORY
        make
        make install


* Specific Configuration Options

Among the usual configuration options of the GNU autoconf system, the
configure(1) script offers the following options to tailor Enblend and
Enfuse.  Remember that configure(1) creates a file called "config.h"
that can serve for fine-tuning the configuration.

We write the default values of all configuration options in capital
letters.


** --enable-gpu-support=CHECK/yes/no
   -DENABLE_GPU=OFF/on    (CMake)

Enable code in Enblend to use the GPU for optimizing the seam line
between two images if all necessary OpenGL libraries are found.
Enfuse does not benefit of this code.  OpenMP (see below) and GPU work
together.

Note that enabling GPU support does not mean Enblend will actually use
the GPU.  Besides passing option "--gpu" to Enblend the hardware as
well as the X11 drivers have to fulfill certain requirements.

See also option "--with-apple-opengl-framework".


** --enable-openmp=yes/NO
   -DENABLE_OPENMP=ON/off    (CMake)

Parallelize parts of Enblend and Enfuse with OpenMP.  See
        http://www.openmp.org/

Note that OpenMP support and the ImageCache must not be activated
both.  The default is to configure with ImageCache and without OpenMP.

If OpenMP support has been enabled, the utilization of special
features of the actual, underlying OpenMP implementation can be
controlled as usual with the environment variables OMP_NUM_THREADS,
OMP_NESTED and OMP_DYNAMIC.  See the OpenMP specification for details
on the usage of these variables.


** --enable-split-doc=YES/no
   -DNOSPLIT=OFF/on    (CMake)

Split the documentation files in INFO or HTML format into small
pieces.  The default is "yes", split documentation.


** --enable-debug=yes/NO

Compile without optimizations and enable all debug-checking code.  The
default is "no", build an optimized version without debugging symbols.


** --enable-image-cache=YES/no
   -DENABLE_IMAGECACHE=OFF/on    (CMake)

Activate the ImageCache feature, which allows for processing of large
images.  The ImageCache handles swapping parts of the images to disk.
The default is "yes", enable the ImageCache.

The ImageCache is recommended on systems where memory is scarce, but
the images to blend or fuse are large.

The ImageCache feature and OpenMP support must not be activated
together.


** --with-apple-opengl-framework=yes/NO

Sometimes AutoConf fails to detect Apple's OpenGL framework.  Use this
option on MAC OS X to force its usage without a test.

See also option "--enable-gpu-support" and the section MacOSX below.


** --with-boost-filesystem=CHECK/yes/no/<LIBRARY>

Use Boost "filesystem" library to get platform-independent path
generation.  The default is to check for usability.  "No" forces the
use of the internal path generation library.  Supply a LIBRARY name if
you want to override the default candidates "boost_filesystem-mt" for
multi-threaded compilation and "boost_filesystem" for single-thread
builds.


** --with-dmalloc=yes/NO

Compile with the debug-malloc library.  The library is available at
http://www.dmalloc.com/.


** --with-openexr=CHECK/yes/no

Build with support for reading and writing OpenEXR images.  See
http://www.openexr.com/ for the required libraries.


** --with-raster-dir=<DIR>

Set the name of the raster-graphics directory, which always is a
subdirectory of the XHTML documentation's root directory.  Default:
"raster".


* CMake Specifics

** Configuration Options

These options only apply to CMake.


*** -DCPACK_BINARY_<SYSTEM>:BOOL=OFF/on

Create a package for the specified <SYSTEM>, where <SYSTEM> is "DEB",
"RPM", or "NSIS".


*** -DCPACK_BINARY_<SYSTEM>:BOOL=ON/off

Create other packages for the specified <SYSTEM>, where <SYSTEM> is
"TBZ2", "TGZ", "STGZ", or "TZ".


*** -DPACK_SOURCE_<SYSTEM>:BOOL=OFF/on

Create a source package for the specified <SYSTEM>, where <SYSTEM> is
"TBZ2", "TGZ", "TZ", or "ZIP".


** Configuration Example

Creating a RedHat package on OpenSuSE

        cmake . \
            -DDOC=ON -DENABLE_GPU=ON \
            -DENABLE_IMAGECACHE=OFF -DENABLE_OPENMP=ON \
            -DCPACK_BINARY_RPM:BOOL=ON
        make package

This will create a package enblend-4.0.595-Linux.rpm, which you may
install with

        sudo rpm -U enblend-4.0.595-Linux.rpm


** Important Configured Make(1) Targets

help                     List all available targets.

edit_cache               If  cmake-gui(1) is installed, start the GUI
                         to edit the "CMakeCache.txt" file.

enblend                  Create an Enblend executable.

enfuse                   Create an Enfuse executable.

man                      Create the manual pages for Enblend and
                         Enfuse.

doc3                     Create the documentation.  This includes
                         html, info, and pdf documents, if the
                         cmake-option DOC was set.

install                  Install everything in the proper places.

package                  Create package(s) specified with the
                         CPACK_BINARY_<SYSTEM>:BOOL parameter of
                         CMake.  It is preferred to create a package
                         and use the package manager to install it
                         rather than using the "install" target.

rebuild_cache            In a changed environment (e.g. newly
                         installed packages) this is the way to
                         discard cached values, so that CMake again
                         starts searching for everything.

package_source           Build a source package like autotools
                         "make dist".


* Extra Make(1) Variables

** Compilation

You can override Makefile variables the usual way.  In addition the
build process supplies several variables, all starting with "EXTRA",
that add their value to the "usual suspects".  These are

        CPPFLAGS  --  EXTRACPPFLAGS
        CFLAGS    --  EXTRACFLAGS
        CXXFLAGS  --  EXTRACXXFLAGS
        LDFLAGS   --  EXTRALDFLAGS

All these "EXTRA" are intentionally unaffected by the
Automake/Autoconf generation of the Makefiles proper.  That way
developers can override configured settings in any make(1) run or
quickly build the project with new combinations of flags.

For example, to quickly add an additional define, use
        make EXTRACPPFLAGS=-DDEBUG_8BIT_ONLY
To compile for coverage analysis, say
        make EXTRACXXFLAGS="-O0 --coverage" EXTRALDFLAGS="--coverage"
analogously for profiling analysis
        make EXTRACXXFLAGS=-pg EXTRALDFLAGS=-pg


** Documentation Generation

We have introduced the variable
        EXTRATEXI2DVIFLAGS
to give the maintainers a tighter control over the generation of the
hard-copy version of the documentation.  Note that the variable does
not have an equivalent non-EXTRA sibling in the Automake system.  It
is particularly useful to supply texi2dvi(1) with "--command"
parameters that explicitly set the paper size, like, for example,
        EXTRATEXI2DVIFLAGS="--command=@afourpaper"

    Common paper-size commands are:
        @smallbook
        @afivepaper
        @afourpaper, @afourwide, @afourlatex

Moreover, you can specify the height and (optionally) width of the
main text area on a page with the
    @pagesizes HEIGHT [, WIDTH]
command.  Remember to supply both HEIGHT and WIDTH with appropriate
units ("mm", "in", ...).

Another useful Texinfo-command at this level is
    @finalout
which suppresses the black rectangles marking overfull hboxes.

Use the standard variable MAKEINFOFLAGS to control makeinfo(1), for
example, to set the maximum length of lines in Info and plain text
output with
    make MAKEINFOFLAGS="--fill-column=95" info
The variable MAKEINFOHTMLFLAGS controls the formatting of XHTML; if
set, it adds to MAKEINFOFLAGS.


* Documentation

The distribution includes some basic documentation in
        doc/enblend.info
        doc/enfuse.info
and the manual pages in
        src/enblend.1
        src/enfuse.1

After the configuration you can build documentation in PostScript,
PDF, XHTML, and DocBook-XML format.
        make ps
        make pdf
        make xhtml
        make xml

The make process automatically ensures the validity of the XHTML and
DocBook-XML documentation.  Additionally, it can be checked with
        make validate-xhtml
        make validate-xml

The DocBook-XML format can be processed with, e.g. dblatex(1), like
        dblatex --type=ps enblend.xml
        dblatex --type=ps enfuse.xml
selecting a format with "--type=FORMAT", where FORMAT is "tex", "dvi",
"ps", or "pdf" (default).  Another possibility is to use xmlto(1):
        xmlto xhtml-nochunks enblend.xml    # or just "html"
        xmlto ps enblend.xml
        xmlto pdf enblend.xml

The number of available output formats vary.  (See, e.g.
/usr/share/xmlto/format/docbook)

Note that some additional packages are required to build these
formats:
        convert        - ImageMagick's swiss army knife of graphics
                         format conversion found at
                         http://www.imagemagick.org/.
        makeinfo       - GNU documentation generator.
        gnuplot        - Render plots (.gp) in text, PNG, EPS, and PDF
                         formats.  Check out http://www.gnuplot.info/.
        freefonts      - Fonts for Gnuplot's PNG format.
        ghostscript    - Convert EPS to PDF.
        perl           - Run Perl programs.
        tetex          - Typeset the Texinfo documents in PS or PDF.
        texi2html      - Convert Texinfo to HTML.  Find more information
                         at http://www.nongnu.org/texi2html/.
        transfig       - Render XFig drawings in PNG, EPS, and PDF.
                         Check out http://www.xfig.org/ for Transfig.
        tidy           - Convert HTML to (almost) XHTML.  Tidy is
                         available at http://tidy.sourceforge.net/.
        xmllint        - Validate XHTML.  Get Xmllint e.g. from
                         http://www.xmlsoft.org/.


* Operating System Specific Instructions and Hints

** GNU/Linux

*** High-Performance Binaries

To configure and compile high-performance versions of Enblend and
Enfuse configure single CPU systems with
        --disable-image-cache
SMP boxes with
        --disable-image-cache --enable-openmp
and pass
         EXTRACXXFLAGS="-march=native -O2"
to make(1).  The resulting binaries are pretty fast, although other
configuration options or compiler flags might improve their
performance even more.


*** Xmi and Xi

To avoid direct linkage to the two X11 libraries Xmi and Xi add
"--without-x" to the parameters of configure(1).


** MacOSX

*** Compiling on MacOSX

On MacOSX you can build Enblend/Enfuse with Fink and with MacPorts.
This README only describes the MacPorts way.


**** Prerequisites

- XCode: Install the XCode version for your MacOSX version.  Download it
  from
          http://developer.apple.com/tools/download/

- MacPorts: Install MacPorts for your MacOSX version.  Download it
  from
          http://www.macports.org/


**** Provide necessary dependencies

From the command line:

    $ sudo port install make lcms boost jpeg tiff libpng OpenEXR mercurial

Note that Enblend/Enfuse can be build via AutoConf/AutoMake and via
CMake.  The latter is experimental.  If you want to build via CMake,
add "cmake" to the previous command line after "mercurial" like this:

    $ sudo port install make lcms boost jpeg tiff libpng OpenEXR mercurial cmake


**** Compile

As MacPorts resides in /opt/local, which is not a standard
library/binary/include path for most source packages, you need to
specify that during the configure step.

Via AutoConf/AutoMake:

    cd enblend
    make --makefile=Makefile.scm
    mkdir build
    cd build
    CPPFLAGS=-I/opt/local/include LDFLAGS=-L/opt/local/lib ../configure --with-apple-opengl-framework
    make
    sudo make install


Via CMake:

    cd enblend
    make --makefile=Makefile.scm
    mkdir build
    cd build
    CPPFLAGS=-I/opt/local/include LDFLAGS=-L/opt/local/lib cmake ..
    make
    sudo make install

This will install Enblend/Enfuse in /usr/local.


**** Other compilation options

Please also check the AutoConf/AutoMake and CMake variables for more
build options.


** Win

*** General

There are two different archives: one with 32-bit executables, the
other one with 64-bit executables.


**** 32-bit versions

The 32-bit archive contains two different versions of Enblend and
Enfuse:


***** enblend.exe/enfuse.exe

These executables are the standard release.  They are using one
processor core and the ImageCache (to allow for processing of very big
images).


***** enblend_openmp.exe/enfuse_openmp.exe

These executables can utilize several cores of modern multi-core
processors and are therefore significantly faster on modern
processors.  But they may fail on very big images because they are
working without the ImageCache.  In this case, please switch to the
standard version.


**** 64-bit version

The 64-bit archive contains multi-threaded versions of Enblend and
Enfuse, both compiled without the ImageCache.

All variants can utilize a modern graphic card to accelerate the
optimizing of the seam line between two images.


*** Compiling on Windows

**** Prerequisites

You will need to following tools for compiling:

- MS Visual C++ 2010, Express Edition suffices or
  MS Visual C++ 2012, Express for Desktop suffices
- CMake, at least version 2.6, though version 2.8 is preferred
- Perl, e.g. ActiveState Perl

CMake expects all sources and libraries in one folder.  So, create a
folder, e.g., "d:\src", extract Enblend/Enfuse into this folder, and
also put all libraries into this folder.

You need the following libraries for reading and writing different
image formats.  We state the version and the folder name of the
libraries used as of December 2009 in square brackets.

- libtiff  [libtiff-4.0.3]
- zlib, required by libtiff  [1.2.7 in zlib]
- libjpeg (optional)  [jpeg-8d]
- libpng (optional)  [libpng-1.5.12]
- OpenEXR and IlmBase (optional), compiled libraries are
  expected in folder "Deploy"  [OpenEXR-1.7.1 and IlmBase-1.0.3]
- vigra, required, should be compiled against same
  libtifff, libjpeg, libpng and OpenEXR as used for Enblend/Enfuse
  [1.8.0 in vigra]

Enblend/Enfuse also depend on the following libraries:

- Boost  [1.51 in boost_1_51_0]
    - Only header files are used by default.
    - Optionally, Enblend/Enfuse can use the Filesystem Library.
      However, this library needs to compiled against STLport.
- Little-CMS2  [2.4 in lcm2-2.4]

For compiling with GPU support, you need:

- GLUT for Win32  [3.7.6 in glut]
- OpenGL Extension Wrangler Library GLEW  [1.9.0 in glew]


**** Compile

1. Start cmake-gui or cmake-setup.
2. Enter path to Enblend/Enfuse source in "Where is the source code".
3. Enter path where to build the executable,
   e.g. "d:\src\build-enblend".  In following, it will be denoted as
   <BUILDDIR>.
4. Select "Configure" when asked for a generator select the
   appropriate generator.
5. Activate the appropriate options.


***** ENABLE_GPU=on/OFF

Enable code in Enblend to use the GPU for optimizing the seam line
between two images if all necessary OpenGL libraries are found.
Enfuse does not benefit of this code.  OpenMP (see below) and GPU work
together.

Note that enabling GPU support does not mean Enblend will actually use
the GPU.  Besides passing option "--gpu" to Enblend the hardware as
well as the graphic card drivers have to fulfill certain requirements.


***** ENABLE_IMAGECACHE=ON/off

Activate the ImageCache feature, which allows for processing of large
images.  The ImageCache handles swapping parts of the images to disk.
The default is "on", i.e., enable the ImageCache.

The ImageCache is recommended on systems where memory is scarce, but
the images to blend or fuse are large.

The ImageCache feature and OpenMP support must not be activated
together.


***** ENABLE_OPENMP=on/OFF

Parallelize parts of Enblend and Enfuse with OpenMP.  See
        http://www.openmp.org/

Note that OpenMP support and the ImageCache must not be activated
both!  The default is to configure with ImageCache and without OpenMP.

If OpenMP support has been enabled, the utilization of special
features of the actual, underlying OpenMP implementation can be
controlled as usual with the environment variables OMP_NUM_THREADS,
OMP_NESTED and OMP_DYNAMIC.  See the OpenMP specification for details
on the usage of these variables.


***** ENABLE_SSE2=on/OFF

Creates executable which make use of the advanced features (SSE2) of
modern processors.


***** DOC=OFF

Set the variable DOC to off.  Building the documentation requires more
tools and not all of them are available for Windows in the current
version.


**** Compile (cont.)

 6. Select "Configure".  Maybe you need start the configuration step
    several times until all dependencies are resolved.
 7. Select "Generate".
 8. Close CMake.
 9. Open solution file <BUILDDIR>\enblend.sln.
10. Select "Release" in Solution Configuration pull-down menu, and
    then choose Build > Build Solution.  This step takes some time.
11. Select project "INSTALL" in Solution Explorer, and then choose
    Build > Project Only > Build Only INSTALL.
12. Close Visual C++ 2008 Express Edition
13. Find the generated executables in <BUILDDIR>\INSTALLDIR\FILES.


* License

Enblend is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Enblend is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Enblend; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA



Local Variables:
mode: outline
End:
