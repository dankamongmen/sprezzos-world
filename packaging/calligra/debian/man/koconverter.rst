===========
Koconverter 
===========

---------------------------
Calligra Document Converter
---------------------------

:Author: This manual page was written by Adrien Grellier <adrien.grellier@laposte.net> for the Debian project (but may be used by others).
:Date: |date|
:Manual section: 1
:Manual group: office


Synopsis
========

  koconverter [Qt-options] [KDE-options] in out

Description
===========

Calligra is an integrated office suite for KDE. It offers a word processor,
a spreadsheet, a presentation program, graphics tools, and more.


Options
=======

**in**   Input file
**out**  Output file

Generic options:

--help                    Show help about options
--help-qt                 Show Qt specific options
--help-kde                Show KDE specific options
--help-all                Show all options
--author                  Show author information
-v, --version             Show version information
--license                 Show license information

Options:

--backup                  Make a backup of the destination file
--batch                   Batch mode: do not show dialogs
--interactive             Interactive mode: show dialogs (default)
--mimetype <mime>         Mimetype of the output file


SEE ALSO
=========

More detailed user documentation is available from **help:/calligra** (either enter this URL into Konqueror, or run **khelpcenter** *help:/calligra*).


.. |date| date:: %y %B %Y
