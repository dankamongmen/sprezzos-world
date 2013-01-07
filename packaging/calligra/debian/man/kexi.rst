====
Kexi
====

-------------------------------
Database creation for everyone
-------------------------------

:Author: This manual page was written by Adrien Grellier <adrien.grellier@laposte.net> for the Debian project (but may be used by others).
:Date: |date|
:Manual section: 1
:Manual group: office


Synopsis
========

kexi [Qt-options] [KDE-options] [options] [project-name]

Description
===========

Kexi is an integrated data management application. It can be used for
creating database schemas, inserting data, performing queries, and
processing data. Forms can be created to provide a custom interface to
your data. All database objects - tables, queries and forms - are stored
in the database, making it easy to share data and design.

Options
=======

Generic options:

--help                    Show help about options
--help-qt                 Show Qt specific options
--help-kde                Show KDE specific options
--help-all                Show all options
--author                  Show author information
-v, --version             Show version information
--license                 Show license information

Options related to entire projects:

--createdb                Create a new, blank project using specified
                          database driver and database name
                          and exit immediately.
                          You will be asked for confirmation
                          if overwriting is needed.
--create-opendb           Like --createdb, but also open newly
                          created database.
                          
--dropdb                  Drop (remove) a project using specified
                          database driver and database name.
                          You will be asked for confirmation.
--drv, --dbdriver <name>  Database driver to be used
                          for connecting to a database project
                          (SQLite by default).
                          Ignored if a shortcut filename
                          is provided.
-t, --type <name>         Specify the type of file provided as an argument.
                          This option is only useful if the filename does
                          not have a valid extension set and its type
                          cannot be determined unambiguously by examining
                          its contents.
                          This option is ignored if no file is specified as                                                                                                                              
                          an argument.                                                                                                                                                                   
                          Available file types are:
                          - "project" for a project file (the default)                                                                                                                                   
                          - "shortcut" for a shortcut file pointing to a project.
                          - "connection" for database connection data.                                                                                                                                   
                                                                                                                                                                                                         
--conn, --connection <shortcut_filename>                                                                                                                                                                 
                          Specify a database connection shortcut .kexic                                                                                                                                  
                          file containing connection data.                                                                                                                                               
                          Can be used with --createdb or --create-opendb                                                                                                                                 
                          for convenience instead of using options like                                                                                                                                  
                          --user, --host or --port.                                                                                                                                                      
                          Note: Options like --user, --host have                                                                                                                                         
                          precedence over settings defined in the shortcut                                                                                                                               
                          file.
--readonly                Specify that any database connections will
                          be performed without write support. This option
                          is ignored when "createdb" option is present,
                          otherwise the database could not be created.
--user-mode               Start project in User Mode, regardless 
                          of the project settings.
--design-mode             Start project in Design Mode, regardless 
                          of the project settings.
--show-navigator          Show the Project Navigator side pane even
                          if Kexi runs in User Mode.
--hide-menu               Hide the main menu (the tabbed toolbar)
                          completely. A number of commands from the main
                          menu is always visible. This option is useful
                          in User Mode.
--skip-startup-dialog     Skip displaying startup dialog window.
                          If there is no project name specified to open,
                          an empty application window will appear.

Options related to opening objects within a project:

--open [<object_type>:]<object_name> 
                          Open object of type 'object_type'
                          and name 'object_name' from specified project
                          on application start.
                          'object_type' is optional, if omitted - table
                          type is assumed.
                          Other object types can be query, report, form,
                          script (may be more or less, depending on your
                          plugins installed).
                          Use "" chars to specify names containing spaces.
                          Examples: --open MyTable,
                          --open query:"My very big query"
--design [<object_type>:]<object_name> 
                          Like --open, but the object will
                          be opened in Design Mode, if one is available.
--edittext [<object_type>:]<object_name> 
                          Like --open, but the object will
                          be opened in Text Mode, if one is available.
--exec, --execute [<object_type>:]<object_name> 
                          Start execution of object of type 'object_type'
                          and name 'object_name' on application start.
                          'object_type' is optional, if omitted - macro
                          type is assumed.
                          Other object types can be script (may be more
                          or less, depending on your plugins installed).
                          Use "" chars to specify names containing spaces.

--new <object_type>       Start new object design of type 'object_type'.


Options related to database servers:

-u, --user <name>         User name to be used
                          for connecting to a database project.
                          Ignored if a shortcut filename
                          is provided.
-h, --host <name>         Server (host) name to be used
                          for connecting to a database project.
                          Ignored if a shortcut filename
                          is provided.
--port <number>           Server's port number to be used
                          for connecting to a database project.
                          Ignored if a shortcut filename
                          is provided.

--local-socket <filename> Server's local socket filename
                          to be used for connecting to a database
                          project. Ignored if a shortcut filename
                          is provided.

--skip-conn-dialog        Skip displaying connection dialog window
                          and connect directly. Available when
                          opening .kexic or .kexis shortcut files.

Arguments:

**project-name**   Kexi database project filename, Kexi shortcut filename or name of a Kexi database project on a server to open.


SEE ALSO
=========

More detailed user documentation is available from **help:/kexi** (either enter this URL into Konqueror, or run **khelpcenter** *help:/kexi*).


.. |date| date:: %y %B %Y
