:mod:`apt_pkg` --- The low-level bindings for apt-pkg
=====================================================
.. module:: apt_pkg

The apt_pkg extensions provides a more low-level way to work with apt. It can
do everything apt can, and is written in C++. It has been in python-apt since
the beginning.

Module Initialization
---------------------

Initialization is needed for most functions, but not for all of them. Some can
be called without having run init*(), but will not return the expected value.

.. function:: init_config

    Initialize the configuration of apt. This is needed for most operations.

.. function:: init_system

    Initialize the system.

.. function:: init

    A short cut to calling :func:`init_config` and :func:`init_system`. You
    can use this if you do not use the command line parsing facilities provided
    by :func:`parse_commandline`, otherwise call :func:`init_config`, parse
    the commandline afterwards and finally call :func:`init_system`.


Working with the cache
----------------------
.. class:: Cache([progress: apt.progress.base.OpProgress])

    A Cache object represents the cache used by APT which contains information
    about packages. The object itself provides no means to modify the cache or
    the installed packages, see the classes :class:`DepCache` and
    :class:`PackageManager` for such functionality.

    The constructor takes an optional argument which must be a subclass of
    :class:`apt.progress.base.OpProgress`. This object will then be used to
    display information during the cache opening process (or possible creation
    of the cache). It may also be ``None``, in which case no progress will
    be emitted. If not given, progress will be printed to standard output.

    .. note::

        The cache supports colon-separated name:architecture pairs. For
        normal architectures, they are equal to a (name, architecture)
        tuple. For the "any" architecture behavior is different, as
        "name:any" is equivalent to ("name:any", "any"). This is done so
        that "name:any" matches all packages with that name which have
        Multi-Arch: allowed set.

    .. describe:: cache[pkgname]

        Return the :class:`Package()` object for the package name given by
        *pkgname*. If *pkgname* includes a colon, the part after the colon
        is used as the architecture.

    .. describe:: cache[name, architecture]

        Return the :class:`Package()` object for the package with the given
        name and architecture.

        .. versionadded: 0.8.0

    .. describe:: pkgname in cache

        Check whether a package with the name given by *pkgname* exists in
        the cache for the native architecture. If *pkgname* includes a
        colon, the part after the colon is used as the architecture.

    .. describe:: (name, architecture) in cache

        Check whether a package with the given name and architecture exists
        in the cache.

        .. versionadded: 0.8.0

    .. method:: update(progress, sources [, pulse_interval]) -> bool

        Update the index files used by the cache. A call to this method
        does not affect the current Cache object, instead a new one
        should be created in order to use the changed index files.

        The parameter *progress* takes an
        :class:`apt.progress.base.AcquireProgress` object which will display
        the progress of fetching the index files. The parameter *sources* takes
        a :class:`SourceList` object which lists the sources. The parameter
        *progress* takes an integer describing the interval (in microseconds)
        in which the pulse() method of the *progress* object will be called.

    .. attribute:: depends_count

        The total number of dependencies stored in the cache.

    .. attribute:: file_list

        A list of all :class:`PackageFile` objects stored in the cache.

    .. attribute:: group_count

        The number of groups in the cache.

        .. versionadded: 0.8.0

    .. attribute:: groups

        A sequence of :class:`Group` objects, implemented as a
        :class:`GroupList` object.

        .. versionadded: 0.8.0

        .. class:: GroupList

            A simple sequence-like object which only provides a length and
            an implementation of ``__getitem__`` for accessing groups at
            a certain index. Apart from being iterable, it can be used in
            the following ways:

            .. versionadded: 0.8.0

            .. describe:: list[index]

                Get the :class:`Group` object for the group at the position
                given by *index* in the GroupList *list*.

            .. describe:: len(list)

                Return the length of the GroupList object *list*.


    .. attribute:: is_multi_arch

        An attribute determining whether the cache supports multi-arch.

        .. versionadded: 0.8.0

    .. attribute:: package_count

        The total number of packages available in the cache. This value is
        equal to the length of the list provided by the :attr:`packages`
        attribute.

    .. attribute:: package_file_count

        The total number of Packages files available (the Packages files
        listing the packages). This is the same as the length of the list in
        the attribute :attr:`file_list`.

    .. attribute:: packages

        A sequence of :class:`Package` objects, implemented as a
        :class:`PackageList` object.

        .. class:: PackageList

            A simple sequence-like object which only provides a length and
            an implementation of ``__getitem__`` for accessing packages at
            a certain index. Apart from being iterable, it can be used in
            the following ways:

            .. describe:: list[index]

                Get the :class:`Package` object for the package at the position
                given by *index* in the PackageList *list*.

            .. describe:: len(list)

                Return the length of the PackageList object *list*.

    .. attribute:: provides_count

        The number of provided packages.

    .. attribute:: ver_file_count

        The total number of ``(Version, PackageFile)`` relations stored in
        the cache.

    .. attribute:: version_count

        The total number of package versions available in the cache.

Managing the cache with :class:`DepCache`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. class:: DepCache(cache: apt_pkg.Cache)

    A DepCache object provides access to more information about the
    objects made available by the :class:`Cache` object as well as
    means to mark packages for removal and installation, among other
    actions.

    The constructor takes a single argument which specifies the
    :class:`Cache` object the new object shall be related to. While
    it is theoretically possible to create multiple DepCache objects
    for the same cache, they will not be independent from each other
    since they all access the same underlying C++ object.

    Objects of this type provide several methods. Most of those methods
    are safe to use and should never raise any exception (all those
    methods for requesting state information or marking changes). If a
    method is expected to raise an exception, it will be stated in the
    description.

    .. method:: commit(acquire_progress, install_progress)

        Commit all marked changes, while reporting the progress of
        fetching packages via the :class:`apt.progress.base.AcquireProgress`
        object given by *acquire_progress* and reporting the installation
        of the package using the :class:`apt.progress.base.InstallProgress`
        object given by *install_progress*.

        If this fails, an exception of the type :exc:`SystemError` will
        be raised.

    .. method:: fix_broken() -> bool

        Try to fix all broken packages in the cache and return ``True`` in
        case of success. If an error occurred, a :exc:`SystemError`
        exception is raised.

    .. method:: get_candidate_ver(pkg: Package) -> Version

        Return the candidate version for the package given by the parameter
        *pkg* as a :class:`Version` object. The default candidate for a
        package is the version with the highest pin, although a different
        one may be set using :meth:`set_candidate_ver`. If no candidate
        can be found, return ``None`` instead.

    .. method:: init(progress: apt.progress.base.OpProgress)

        Initialize the DepCache. This is done automatically when the
        cache is opened, but sometimes it may be useful to reinitialize
        the DepCache. Like the constructor of :class:`Cache`, this
        function takes a single :class:`apt.progress.base.OpProgress`
        object to display progress information.

    .. method:: read_pinfile(file: str)

        A proxy function which calls the method :meth:`Policy.read_pinfile` of
        the :class:`Policy` object used by this object. This method raises
        a :exc:`SystemError` exception if the file could not be parsed.

    .. method:: set_candidate_ver(pkg: Package, version: Version) -> bool

        Set the candidate version of the package given by the :class:`Package`
        object *pkg* to the version given by the :class:`Version` object
        *version* and return ``True``. If odd things happen, this function
        may raise a :exc:`SystemError` exception, but this should not
        happen in normal usage. See :meth:`get_candidate_ver` for a way
        to retrieve the candidate version of a package.

    .. method:: upgrade([dist_upgrade=False]) -> bool

        Mark the packages for upgrade under the same conditions
        :program:`apt-get` does. If *dist_upgrade* is ``True``, also
        allow packages to be upgraded if they require installation/removal
        of other packages; just like apt-get dist-upgrade.

        Despite returning a boolean value, this raises :exc:`SystemError` and
        does not return ``False`` if an error occurred.

    The following methods can mark a single package for installation,
    removal, etc:

    .. method:: mark_auto(pkg: Package)

        Mark the :class:`Package` *pkg* as automatically installed.

    .. method:: mark_keep(pkg: Package)

        Mark the :class:`Package` *pkg* for keep.

    .. method:: mark_delete(pkg: Package[, purge])

        Mark the :class:`Package` *pkg* for delete. If *purge* is True,
        the configuration files will be removed as well.

    .. method:: mark_install(pkg: Package[, auto_inst=True[, from_user=True]])

        Mark the :class:`Package` *pkg* for install, and, if *auto_inst*
        is ``True``, its dependencies as well. If *from_user* is ``True``,
        the package will **not** be marked as automatically installed.

    .. method:: set_reinstall(pkg: Package)

        Set if the :class:`Package` *pkg* should be reinstalled.

    The following methods can be used to check the state of a package:

    .. method:: is_auto_installed(pkg: Package) -> bool

        Return ``True`` if the package is automatically installed, that
        is, as a dependency of another package.

    .. method:: is_garbage(pkg: Package) -> bool

        Return ``True`` if the package is garbage, that is, if it was
        automatically installed and no longer referenced by other packages.

    .. method:: is_inst_broken(pkg: Package) -> bool

        Return ``True`` if the package is broken on the current install. This
        takes changes which have not been marked not into account.

    .. method:: is_now_broken(pkg: Package) -> bool

        Return ``True`` if the package is now broken, that is, if the package
        is broken if the marked changes are applied.
        
    .. method:: is_upgradable(pkg: Package) -> bool

        Return ``True`` if the package is upgradable, the package can then
        be marked for upgrade by calling the method :meth:`mark_install`.

    .. method:: marked_delete(pkg: Package) -> bool

        Return ``True`` if the package is marked for delete.

    .. method:: marked_downgrade(pkg: Package) -> bool

        Return ``True`` if the package should be downgraded.

    .. method:: marked_install(pkg: Package) -> bool

        Return ``True`` if the package is marked for install.

    .. method:: marked_keep(pkg: Package) -> bool

        Return ``True`` if the package is marked for keep.

    .. method:: marked_reinstall(pkg: Package) -> bool

        Return ``True`` if the package should be reinstalled.

    .. method:: marked_upgrade(pkg: Package) -> bool

        Return ``True`` if the package is marked for upgrade.

    DepCache objects also provide several attributes containing information
    on the marked changes:

    .. attribute:: keep_count

        Integer, number of packages marked as keep

    .. attribute:: inst_count

        Integer, number of packages marked for installation.

    .. attribute:: del_count

        Number of packages which should be removed.

    .. attribute:: broken_count

        Number of packages which are broken.

    .. attribute:: usr_size

        The size required for the changes on the filesystem. If you install
        packages, this is positive, if you remove them its negative.

    .. attribute:: deb_size

        The size of the packages which are needed for the changes to be
        applied.

    .. attribute:: policy

        The underlying :class:`Policy` object used by the :class:`DepCache` to
        select candidate versions.

Installing with :class:`PackageManager`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. class:: PackageManager(depcache)

    Abstraction of a package manager. This object takes care of retrieving
    packages, ordering the installation, and calling the package manager to
    do the actual installation.
    
    .. method:: get_archives(fetcher, list, records) -> bool

        Add all packages marked for installation (or upgrade, anything
        which needs a download) to the :class:`Acquire` object referenced
        by *fetcher*.

        The parameter *list* specifies a :class:`SourceList` object which
        is used to retrieve the information about the archive URI for the
        packages which will be fetched.

        The parameter *records* takes a :class:`PackageRecords` object which
        will be used to look up the file name of the package.

    .. method:: do_install(status_fd: int) -> int

        Install the packages and return one of the class constants
        :attr:`RESULT_COMPLETED`, :attr:`RESULT_FAILED`,
        :attr:`RESULT_INCOMPLETE`. The argument *status_fd* can be used
        to specify a file descriptor that APT will write status information
        on (see README.progress-reporting in the apt source code for
        information on what will be written there).

    .. method:: fix_missing() -> bool

        Fix the installation if a package could not be downloaded.

    .. attribute:: RESULT_COMPLETED

        A constant for checking whether the result of the call to
        :meth:`do_install` is 'failed'.

    .. attribute:: RESULT_FAILED

        A constant for checking whether the result of the call to
        :meth:`do_install` is 'failed'.

    .. attribute:: RESULT_INCOMPLETE

        A constant for checking whether the result of the call to
        :meth:`do_install` is 'incomplete'.
        
    All instances of this class also support the following methods:
    
    .. note::
        
        This methods are provided mainly for subclassing purposes
        and should not be used in most programs. This class is a
        subclass of an internal :class:`_PackageManager` which does
        not provide that methods. As the public C++ API creates such
        an object without those methods, you should not rely on those
        methods to be available unless you used the constructor of
        :class:`PackageManager` to create the object.
    
    .. method:: configure(pkg: Package) -> bool 

        Notify the package manager that the :class:`Package` given
        by *pkg* is to be configured. Must return a ``True`` value
        or ``None`` to continue, or a value which is ``False`` if
        evaluated as boolean to abort.
        
        .. versionadded:: 0.8.0

    .. method:: install(pkg: Package, filename: str) -> bool 

        Notify the package manager that the :class:`Package` given
        by *pkg* is to be installed from the .deb located at
        *filename*. Must return a ``True`` value or ``None`` to
        continue, or a value which is ``False`` if evaluated as
        boolean to abort.
        
        
        .. versionadded:: 0.8.0

    .. method:: remove(pkg: Package, purge: bool) -> bool 

        Notify the package manager that the :class:`Package` given
        by *pkg* is to be removed. If *purge* is ``True``, the package
        shall be purged. Must return a ``True`` value or ``None`` to
        continue, or a value which is ``False`` if evaluated as boolean
        to abort.
        
        
        .. versionadded:: 0.8.0
 
    .. method:: go(status_fd: int) -> bool  
        
        Start dpkg, writing status information to the file descriptor
        given by *status_fd*. Must return a ``True`` value or ``None`` to
        continue, or a value which is ``False`` if evaluated as boolean
        to abort.
        
        .. versionadded:: 0.8.0

    .. method:: reset()

        Reset the package manager for a new round.
        
        .. versionadded:: 0.8.0

        
Installation ordering with :class:`OrderList`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. class:: OrderList(depcache: DepCache)

	Represent a :ctype:`pkgOrderList`, used for installation
	ordering. This class provides several methods and attributes,
	is complicated and should not be used by normal programs.
	
	.. versionadded:: 0.8.0
	
	This class is a sequence and supports the following operations:
	
	.. describe:: list[index]
	
		Get the package at the given index in the list. Negative
		index is supported.
		
	.. describe:: len(list)
	
		The length of the list.
		
	It also supports the append() method from :class:`list`:
	
	.. method:: append(pkg: Package)
	
		Append a new package to the end of the list. Please note that
		you may not append a package twice, as only as much packages
		as in the cache can be added.
		
	The class also defines several specific attributes and methods,
	to be described hereinafter.
		
	.. method:: score(pkg: Package)
	
		Return the score of the package. Packages are basically
		ordered by descending score.
		
	This class allows flags to be set on packages. Those flags are:
	
	.. attribute:: FLAG_ADDED
	.. attribute:: FLAG_ADD_PENDING
	.. attribute:: FLAG_IMMEDIATE
	.. attribute:: FLAG_LOOP
	.. attribute:: FLAG_UNPACKED
	.. attribute:: FLAG_CONFIGURED
	.. attribute:: FLAG_REMOVED
	.. attribute:: FLAG_STATES_MASK
	
		Same as ``FLAG_UNPACKED | FLAG_CONFIGURED | FLAG_REMOVED``
		
	.. attribute:: FLAG_IN_LIST
	.. attribute:: FLAG_AFTER
	
	The methods to work with those flags are:
		
	.. method:: flag(pkg: Package, flag: int[, unset_flags: int])

		Flag a package. Sets the flags given in *flag* and unsets
		any flags given in *unset_flags*.
		
	.. method:: is_flag(pkg: Package, flag: int)
	
		Check whether the flags in *flag* are set for the package.
		
	.. method:: wipe_flags(flags: int)
	
		Remove the flags in *flags* from all packages.
	
	.. method:: is_missing(pkg: Package)

		Check if the package is missing (not really usable right now)

	.. method:: is_now(pkg: Package)

		Check if the package is flagged for any state but removal.
		
	The following methods for ordering are provided:
	
	.. method:: order_critical()
	
		Order the packages for critical unpacking; that is, only
		respect pre-dependencies.
	
	.. method:: order_unpack()
	
		Order the packages for unpacking, repecting Pre-Depends and
		Conflicts.
	
	.. method:: order_configure()
	
		Order the packages for configuration, respecting Depends.

Improve performance with :class:`ActionGroup`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. class:: ActionGroup(depcache)

    Create a new :class:`ActionGroup()` object for the :class:`DepCache` object
    given by the parameter *depcache*.

    :class:`ActionGroup()` objects make operations on the cache faster by
    delaying certain cleanup operations until the action group is released.

    An action group is also a context manager and therefore supports the
    :keyword:`with` statement. But because it becomes active as soon as it
    is created, you should not create an ActionGroup() object before entering
    the with statement. Thus, you should always use the following form::

        with apt_pkg.ActionGroup(depcache):
            ...

    For code which has to run on Python versions prior to 2.5, you can also
    use the traditional way::

        actiongroup = apt_pkg.ActionGroup(depcache)
        ...
        actiongroup.release()

    In addition to the methods required to implement the context
    manager interface, :class:`ActionGroup` objects provide the
    following method:

    .. method:: release()

        Release the ActionGroup. This will reactive the collection of package
        garbage.

Resolving Dependencies with :class:`ProblemResolver`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. class:: ProblemResolver(depcache: DepCache)

    ProblemResolver objects take care of resolving problems with
    dependencies. They mark packages for installation/removal and
    try to satisfy all dependencies. The constructor takes a single
    argument of the type :class:`apt_pkg.DepCache` to determine the
    cache that shall be manipulated in order to resolve the problems.

    .. method:: clear(pkg: Package)

        Revert the action of calling :meth:`protect` or :meth:`remove` on
        a package, resetting it to the default state.

    .. method:: install_protect()

        Mark all protected packages for installation.

    .. method:: protect(pkg: Package)

        Mark the package given by *pkg* as protected; that is, its state
        will not be changed.

    .. method:: remove(pkg: Package)

        Mark the package given by *pkg* for removal in the resolver.

    .. method:: resolve([fix_broken: bool = True]) -> bool

        Try to intelligently resolve problems by installing and removing
        packages. If *fix_broken* is ``True``, apt will try to repair broken
        dependencies of installed packages.

    .. method:: resolve_by_keep() -> bool

        Try to resolve the problems without installing or removing packages.

:class:`Group` of packages with the same name
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. class:: Group(cache: Cache, name: str)

    .. versionadded:: 0.8.0

    A collection of packages in which all packages have the same name. Groups
    are used in multi-arch environments, where two or more packages have the
    same name, but different architectures.

    Group objects provide the following parts for sequential access:

    .. describe:: group[index]

        Get the package at the given **index** in the group.

        .. note::
            Groups are internally implemented using a linked list. The object
            keeps a pointer to the current object and the first object, so
            access to the first element, or accesses in order have a
            complexity of O(1). Random-access complexity is ranges from
            O(1) to O(n).

    Group objects also provide special methods to find single packages:

    .. method:: find_package(architecture: str) -> Package

        Find a package with the groups name and the architecture given
        in the argument *architecture*. If no such package exists, return
        ``None``.

    .. method:: find_preferred_package(prefer_nonvirtual: bool = True) -> Package

        Find the preferred package. This is the package of the native
        architecture (specified in ``APT::Architecture``) if available,
        or the package from the first foreign architecture. If no package
        could be found, return ``None``

        If **prefer_nonvirtual** is ``True``, the preferred package
        will be a non-virtual package, if one exists.
        

:class:`Package` information
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. class:: Package

   Represent a package. A package is uniquely identified by its name and
   architecture and each package can have zero or more versions which can be
   accessed via the :attr:`version_list` property. Packages can be
   installed and removed by a :class:`DepCache` object.

    Attributes:

    .. attribute: architecture

        The architecture of the package. This is relevant on multi-arch
        systems only. Please note that if a package is Architecture: all,
        this value is not "all", but the architecture of the package file
        it comes from.

        .. versionadded:: 0.7.100.3

    .. attribute:: current_ver

        The version currently installed as a :class:`Version` object, or None
        if the package is not installed.

   .. method:: get_fullname([pretty: bool = False]) -> str

        Get the full name of the package, including the architecture. If
        *pretty* is ``True``, the architecture is omitted for native packages,
        that is, an amd64 "apt" package on an amd64 system would give "apt".

        .. versionadded:: 0.7.100.3

   .. attribute:: has_provides

        A boolean value determining whether the list available via the
        attribute :attr:`provides_list` has at least one element. This
        value may be used in combination with :attr:`has_versions` to
        check whether a package is virtual; that is, it has no versions
        and is provided at least once::

            pkg.has_provides and not pkg.has_versions

   .. attribute:: has_versions

        A boolean value determining whether the list available via the
        attribute :attr:`version_list` has at least one element. This
        value may be used in combination with :attr:`has_provides` to
        check whether a package is virtual; that is, it has no versions
        and is provided at least once::

            pkg.has_provides and not pkg.has_versions

    .. attribute:: id

        The ID of the package. This can be used to store information about
        the package. The ID is an int value.

    .. attribute:: name

        This is the name of the package.

    .. attribute:: provides_list

        A list of all package versions providing this package. Each element
        of the list is a triplet, where the first element is the name of the
        provided package, the second element the provided version (empty
        string), and the third element the version providing this package
        as a :class:`Version` object.

    .. attribute:: rev_depends_list

        An iterator of :class:`Dependency` objects for dependencies on this
        package. The returned iterator is implemented by the class
        :class:`DependencyList`:

        .. class:: DependencyList

            A simple list-like type for representing multiple dependency
            objects in an efficient manner; without having to generate
            all Dependency objects in advance.

            .. describe:: list[index]

                Return the item at the position *index* in the list.

            .. method:: __len__()

                The length of the list. This method should not be used
                irectly, instead Python's built-in function :func:`len`
                should be used.

    .. attribute:: section

        The section of the package, as specified in the record. The list of
        possible sections is defined in the Policy. This is a string.

    .. attribute:: version_list

        A list of :class:`Version` objects for all versions of this package
        available in the cache.

    **States**:

    .. attribute:: selected_state

        The state we want it to be, ie. if you mark a package for installation,
        this is :attr:`apt_pkg.SELSTATE_INSTALL`.

        See :ref:`SelStates` for a list of available states.

    .. attribute:: inst_state

        The state the currently installed version is in. This is normally
        :attr:`apt_pkg.INSTSTATE_OK`, unless the installation failed.

        See :ref:`InstStates` for a list of available states.

    .. attribute:: current_state

        The current state of the package (not installed, unpacked, installed,
        etc). See :ref:`CurStates` for a list of available states.

    **Flags**:

    .. attribute:: auto

        This flag is here for compatibility purposes and does not appear to
        be used anymore in APT. To find out whether a package is marked as
        automatically installed, use :meth:`DepCache.is_auto_installed`
        instead.

    .. attribute:: essential

        Whether the package has the 'Essential' flag set; that is,
        whether it has a field 'Essential: yes' in its record.

    .. attribute:: important

        Whether the package has the (obsolete) 'Important' flag set; that is,
        whether it has a field 'Important: yes' in its record.

Example:
~~~~~~~~~
.. literalinclude:: ../examples/cache-packages.py



:class:`Version`
^^^^^^^^^^^^^^^^^
.. class:: Version

    The version object contains all information related to a specific package
    version.

    .. attribute:: arch

        The architecture of the package, eg. amd64 or all.

    .. attribute:: depends_list

        This is basically the same as :attr:`depends_list_str`,
        but instead of the ('pkgname', 'version', 'relation') tuples,
        it returns :class:`Dependency` objects, which can assist you with
        useful functions.

    .. attribute:: depends_list_str

        A dictionary of dependencies. The key specifies the type of the
        dependency ('Depends', 'Recommends', etc.).

        The value is a list, containing items which refer to the or-groups of
        dependencies. Each of these or-groups is itself a list, containing
        tuples like ('pkgname', 'version', 'relation') for each or-choice.

        An example return value for a package with a 'Depends: python (>= 2.4)'
        would be::

            {'Depends': [
                            [
                             ('python', '2.4', '>=')
                            ]
                        ]
            }

        The same for a dependency on A (>= 1) | B (>= 2)::

            {'Depends': [
                            [
                                ('A', '1', '>='),
                                ('B', '2', '>='),
                            ]
                        ]
            }

        The comparison operators are not the Debian ones, but the standard
        comparison operators as used in languages such as C and Python. This
        means that '>' means "larger than" and '<' means "less than".

    .. attribute:: downloadable

        Whether this package can be downloaded from a remote site.

    .. attribute:: file_list

        A list of (:class:`PackageFile`, int: index) tuples for all Package
        files containing this version of the package.

    .. attribute:: hash

        An integer hash value used for the internal storage.

    .. attribute:: id

        A numeric identifier which uniquely identifies this version in all
        versions in the cache.

    .. attribute:: installed_size

        The size of the package (in kilobytes), when unpacked on the disk.
        
    .. attribute:: multi_arch
    
		The multi-arch state of the package. Can be one of the following
		attributes.
		
			.. attribute:: MULTI_ARCH_NONE
			
				No multi-arch
				
			.. attribute:: MULTI_ARCH_ALL
			
				An ``Architecture: all`` package
				
			
			.. attribute:: MULTI_ARCH_FOREIGN
			
				Can satisfy dependencies of foreign-architecture
				packages
				
			.. attribute:: MULTI_ARCH_ALL_FOREIGN
			
				:attr:`MULTI_ARCH_FOREIGN` for ``Architecture: all``
				packages.
				
			.. attribute:: MULTI_ARCH_SAME
			
				Multiple versions from different architectures may be
				installed in parallel, but may only satisfy dependencies
				of packages from the same architecture
				
			.. attribute:: MULTI_ARCH_ALLOWED
			
				Installation in parallel and satisfying ``pkg:any``
				style dependencies is allowed.
				
			.. attribute:: MULTI_ARCH_ALL_ALLOWED
			
				:attr:`MULTI_ARCH_ALLOWED` for ``Architecture: all``
				packages.
				
				
			

    .. attribute:: parent_pkg

        The :class:`Package` object this version belongs to.

    .. attribute:: priority

        The integer representation of the priority. This can be used to speed
        up comparisons a lot, compared to :attr:`priority_str`.

        The values are defined in the :mod:`apt_pkg` extension, see
        :ref:`Priorities` for more information.

    .. attribute:: priority_str

        Return the priority of the package version, as a string, eg.
        "optional".

    .. attribute:: provides_list

        This returns a list of all packages provided by this version. Like
        :attr:`Package.provides_list`, it returns a list of tuples
        of the form ('virtualpkgname', '', :class:`Version`), where as the
        last item is the same as the object itself.

    .. attribute:: section

        The usual sections (eg. admin, net, etc.). Prefixed with the component
        name for packages not in main (eg. non-free/admin).

    .. attribute:: size

        The size of the .deb file, in bytes.

    .. attribute:: translated_description

        Return a :class:`Description` object for the translated description
        of this package version.

    .. attribute:: ver_str

        The version, as a string.
        


:class:`Dependency`
^^^^^^^^^^^^^^^^^^^^
.. class:: Dependency

    Represent a dependency from one package to another one.

    .. method:: all_targets

        A list of :class:`Version` objects which satisfy the dependency,
        and do not conflict with already installed ones.

        From my experience, if you use this method to select the target
        version, it is the best to select the last item unless any of the
        other candidates is already installed. This leads to results being
        very close to the normal package installation.

    .. method:: smart_target_pkg

        Return a :class:`Version` object of a package which satisfies the
        dependency and does not conflict with installed packages
        (the 'natural target').

    .. attribute:: comp_type

        The type of comparison (>=, ==, >>, <=), as string.

    .. attribute:: dep_type

        The type of the dependency, as string, eg. "Depends".

    .. attribute:: dep_type_enum

        The type of the dependency, as an integer which can be compared to
        one of the TYPE_* constants below.

    .. attribute:: dep_type_untranslated

        The type of the depndency, as an untranslated string.

    .. attribute:: id

        The ID of the package, as integer.

    .. attribute:: parent_pkg

        The :class:`Package` object of the package which declares the
        dependency. This is the same as using ParentVer.ParentPkg.

    .. attribute:: parent_ver

        The :class:`Version` object of the parent version, ie. the package
        which declares the dependency.

    .. attribute:: target_pkg

        The :class:`Package` object of the target package.

    .. attribute:: target_ver

        The target version of the dependency, as string. Empty string if the
        dependency is not versioned.

    The following constants describe all values the attribute *dep_type_enum*
    can take:

    .. attribute:: TYPE_CONFLICTS

        Constant for checking against dep_type_enum

    .. attribute:: TYPE_DEPENDS

        Constant for checking against dep_type_enum

    .. attribute:: TYPE_DPKG_BREAKS

        Constant for checking against dep_type_enum

    .. attribute:: TYPE_ENHANCES

        Constant for checking against dep_type_enum

    .. attribute:: TYPE_OBSOLETES

        Constant for checking against dep_type_enum

    .. attribute:: TYPE_PREDEPENDS

        Constant for checking against dep_type_enum
        
    .. attribute:: TYPE_RECOMMENDS

        Constant for checking against dep_type_enum
        
    .. attribute:: TYPE_REPLACES

        Constant for checking against dep_type_enum
        
    .. attribute:: TYPE_SUGGESTS

        Constant for checking against dep_type_enum

Example: Find all missing dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With the help of Dependency.AllTargets(), you can easily find all packages with
broken dependencies:

.. literalinclude:: ../examples/missing-deps.py


:class:`Description`
^^^^^^^^^^^^^^^^^^^^^
.. class:: Description

    Represent the description of the package.

    .. attribute:: language_code

        The language code of the description; or, if the description
        is untranslated, an empty string.

    .. attribute:: md5

        The MD5 checksum of the description.

    .. attribute:: file_list

        A list of tuples ``(packagefile: PackageFile, index: int)``.

Package Pinning with :class:`Policy`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. class:: Policy(cache: apt_pkg.Cache)

    Representation of the policy of the :class:`Cache` object given by
    *cache*. This provides a superset of policy-related functionality
    compared to the *DepCache* class. The DepCache can be used for most
    purposes, but there may be some cases where a special policy class
    is needed.
    
    .. method:: create_pin(type: str, pkg: str, data: str, priority: int)

        Create a pin for the policy. The parameter *type* refers to one of the
        strings 'Version', 'Release', or 'Origin'. The argument *pkg* is the
        name of the package. The parameter *data* refers to the value (such
        as 'unstable' for type='Release') and the other possible options.
        The parameter 'priority' gives the priority of the pin.
        
    .. method:: get_candidate_ver(package: apt_pkg.Package) -> apt_pkg.Version

        Get the best package for the job; that is, the package with the
        highest pin priority.

    .. method:: get_match(package: apt_pkg.Package) -> apt_pkg.Version

        Get a version for the package.

    .. method:: get_priority(package: apt_pkg.Package) -> int

        Get the pin priority of the package given by *package*.

    .. method:: read_pindir(dirname: str) -> bool

        Read the pin files in the given dir (e.g. '/etc/apt/preferences.d')
        and add them to the policy.

    .. method:: read_pinfile(filename: str) -> bool

        Read the pin file given by *filename* (e.g. '/etc/apt/preferences')
        and add it to the policy.


Index Files
-------------

.. class:: MetaIndex

   Represent a Release file as stored in the cache.

    .. attribute:: uri

      The URI the meta index file is located at, as a string.
      
    .. attribute:: dist

      The distribution stored in the meta index, as a string.
    
    .. attribute:: is_trusted

      A boolean value determining whether the meta index can be trusted. This
      is ``True`` for signed Release files.
      
    .. attribute:: index_files

      A list of all :class:`IndexFile` objects associated with this meta
      index.


.. class:: IndexFile

    Represent an index file, that is, package indexes, translation indexes,
    and source indexes.

    .. method:: archive_uri(path: str) -> str

        Return the URI to the given path in the archive.

    .. attribute:: label

        The label of the index file.

    .. attribute:: describe

        A string describing this object.

    .. attribute:: exists

        A boolean value determining whether the index file exists.

    .. attribute:: has_packages

        A boolean value determining whether the index file has packages.

    .. attribute:: size

        The size of the file, measured in bytes.

    .. attribute:: is_trusted

        A boolean value determining whether the file can be trusted; that is,
        because it is from a source with a GPG signed Release file.



.. class:: PackageFile

    Provide access to an index file stored in the cache, such as
    :file:`/var/lib/dpkg/status`.

    .. attribute:: architecture

        The architecture of the package file. This attribute normally
        contains an empty string and is thus not very useful.

    .. attribute:: archive

        The archive of the package file as set in the Release file via
        the "Suite" field. If there is no Release file, this is an empty
        string.

    .. attribute:: component

        The component of the package file, if it is provided by a repository
        using the dists/ hierarchy. For other packages files, this property
        is an empty string.

    .. attribute:: filename

        The path to the file on the local filesystem.

    .. attribute:: id

        The ID of the package. This is an integer which can be used to store
        further information about the file [eg. as dictionary key].

    .. attribute:: index_type

        A string describing the type of index. Known values are
        "Debian Package Index", "Debian Translation Index", and
        "Debian dpkg status file".

    .. attribute:: label

        The label of the package file as set in the release file
        via the 'Label' field. If there is no Release file, this
        attribute is an empty string.

    .. attribute:: not_automatic

        Whether packages from this list will be updated automatically. The
        default for example is False.

    .. attribute:: not_source

        Whether the file has no source from which it can be updated. In such a
        case, the value is ``True``; else ``False``. For example, it is
        ``False`` for :file:`/var/lib/dpkg/status`.

        Example::

            for pkgfile in cache.file_list:
                if pkgfile.not_source:
                    print 'The file %s has no source.' % pkgfile.filename

    .. attribute:: origin

        The Origin, as set in the Release file

    .. attribute:: site

        The hostname of the site.

    .. attribute:: size

        The size of the file.

    .. attribute:: version

        The version, as set in the release file (eg. "4.0" for "Etch")


The following example shows how to use PackageFile:

.. literalinclude:: ../examples/cache-pkgfile.py


Records (Release files, Packages, Sources)
------------------------------------------

.. class:: IndexRecords()

    Represent a Release file and provide means to read information from
    the file. This class provides several methods:

    .. method:: get_dist() -> str

        Return the distribution set in the Release file.

    .. method:: load(filename: str)

        Load the file located at the path given by *filename*.

    .. method:: lookup(key: str) -> (HashString, int)

        Look up the filename given by *key* and return a tuple (hash, size),
        where the first element *hash* is a :class:`HashString` object
        and the second element *size* is an int object.


.. class:: PackageRecords(cache: apt_pkg.Cache)

    Provide further information about the packages in the :class:`Cache` object
    *cache*. This efficiently parses the package files to provide information
    not available in the cache, such as maintainer, hash sums, description,
    and the file name of the package. It also provides the complete record
    of the package.

    .. method:: lookup(verfile_iter: (PackageFile, int)) -> bool

        Change the actual package to the package given by the verfile_iter.

        The parameter *verfile_iter* refers to a tuple consisting
        of (:class:`PackageFile()`, int: index), as returned by various
        ``file_list`` attributes such as :attr:`Version.file_list`.

        Example (shortened)::

            cand = depcache.GetCandidateVer(cache['python-apt'])
            records.Lookup(cand.FileList[0])
            # Now you can access the record
            print records.SourcePkg # == python-apt

    .. attribute:: filename

        Return the field 'Filename' of the record. This is the path to the
        package, relative to the base path of the archive.

    .. attribute:: md5_hash

        Return the MD5 hashsum of the package This refers to the field
        'MD5Sum' in the raw record.

    .. attribute:: sha1_hash

        Return the SHA1 hashsum of the package. This refers to the field 'SHA1'
        in the raw record.

    .. attribute:: sha256_hash

        Return the SHA256 hashsum of the package. This refers to the field
        'SHA256' in the raw record.

        .. versionadded:: 0.7.9

    .. attribute:: source_pkg

        The name of the source package, if different from the name of the
        binary package. This information is retrieved from the 'Source' field.

    .. attribute:: source_ver

        The version of the source package, if it differs from the version
        of the binary package. Just like 'source_pkg', this information
        is retrieved from the 'Source' field.

    .. attribute:: maintainer

        Return the maintainer of the package.

    .. attribute:: short_desc

        Return the short description. This is the summary on the first line of
        the 'Description' field.

    .. attribute:: long_desc

        Return the long description. These are lines 2-END from the
        'Description' field.

    .. attribute:: name

        Return the name of the package. This is the 'Package' field.

    .. attribute:: homepage

        Return the Homepage. This is the 'Homepage' field.

    .. attribute:: record

        Return the whole record as a string. If you want to access fields of
        the record not available as an attribute, you can use
        :class:`apt_pkg.TagSection` to parse the record and access the field
        name.

        Example::

            section = apt_pkg.TagSection(records.record)
            print section['SHA256'] # Use records.sha256_hash instead


.. class:: SourceRecords

    Provide an easy way to look up the records of source packages and
    provide easy attributes for some widely used fields of the record.
    
    .. note::

        If the Lookup failed, because no package could be found, no error is
        raised. Instead, the attributes listed below are simply not existing
        anymore (same applies when no Lookup has been made, or when it has
        been restarted).

    .. method:: lookup(pkgname: str) -> bool

        Look up the source package with the given name. Each call moves
        the position of the records parser forward. If there are no
        more records, return None. If the lookup failed this way,
        access to any of the attributes will result in an
        :exc:`AttributeError`.

        Imagine a package P with two versions X, Y. The first ``lookup(P)``
        would set the record to version X and the second ``lookup(P)`` to
        version Y. A third call would return ``None`` and access to any
        of the below attributes will result in an :exc:`AttributeError`

    .. method:: restart()

        Restart the lookup process. This moves the parser to the first
        package and lookups can now be made just like on a new object.

        Imagine a package P with two versions X, Y. The first ``Lookup(P)``
        would set the record to version X and the second ``Lookup(P)`` to
        version Y. If you now call ``restart()``, the internal position
        will be cleared. Now you can call ``lookup(P)`` again to move to X.

    .. attribute:: binaries

        Return a list of strings describing the package names of the binaries
        created by the source package. This matches the 'Binary' field in the
        raw record.

    .. attribute:: build_depends

        Return a dictionary representing the build-time dependencies of the
        package. The format is the same as for :attr:`Version.depends_list_str`
        and possible keys being ``"Build-Depends"``, ``"Build-Depends-Indep"``,
        ``"Build-Conflicts"`` or ``"Build-Conflicts-Indep"``.

    .. attribute:: files

        The list of files. This returns a list of tuples with the contents
        ``(str: md5, int: size, str: path, str:type)``, where
        'type' can be 'diff' (includes .debian.tar.gz), 'dsc', 'tar'.

    .. attribute:: index

        A list of :class:`IndexFile` objects associated with this
        source package record.

    .. attribute:: maintainer

        A string describing the name of the maintainer.

    .. attribute:: package

        The name of the source package.

    .. attribute:: record

        The whole record, as a string. You can use :func:`apt_pkg.ParseSection`
        if you need to parse it. You need to parse the record  to access
        fields not available via the attributes such as 'Standards-Version'

    .. attribute:: section

        A string describing the section.

    .. attribute:: version

        A string describing the version of the source package.

The Acquire interface
----------------------
The Acquire Interface is responsible for all sorts of downloading in apt. All
packages, index files, etc. downloading is done using the Acquire functionality.

The :mod:`apt_pkg` module provides a subset of this functionality which allows
you to implement file downloading in your applications. Together with the
:class:`PackageManager` class you can also fetch all the packages marked for
installation.

.. class:: Acquire([progress: apt.progress.base.AcquireProgress])

    Coordinate the retrieval of files via network or local file system
    (using ``copy://path/to/file`` style URIs). Items can be added to
    an Acquire object using various means such as creating instances
    of :class:`AcquireFile` or the methods :meth:`SourceList.get_indexes`
    and :meth:`PackageManager.get_archives`.

    Acquire objects maintain a list of items which will be fetched or have
    been fetched already during the lifetime of this object. To add new items
    to this list, you can create new :class:`AcquireFile` objects which allow
    you to add single files.

    The constructor takes an optional parameter *progress* which takes an
    :class:`apt.progress.base.AcquireProgress` object. This object may then
    report progress information (see :mod:`apt.progress.text` for reporting
    progress to a I/O stream and :mod:`apt.progress.gtk2` for GTK+ progress
    reporting).
    
    Acquire items have two methods to start and stop the fetching:

    .. method:: run() -> int

        Fetch all the items which have been added by :class:`AcquireFile` and
        return one of the constants :attr:`RESULT_CANCELLED`,
        :attr:`RESULT_CONTINUE`, :attr:`RESULT_FAILED` to describe the
        result of the run.

    .. method:: shutdown()

        Shut the fetcher down. This removes all items from the queue and
        makes all :class:`AcquireItem`, :class:`AcquireWorker`,
        :class:`AcquireItemDesc` objects useless. Accessing an object of one
        of those types can cause a segfault then.

        Removing an item does not mean that the already fetched data will
        be removed from the destination. Instead, APT might use the partial
        result and continue from thereon.

    Furthermore, they provide three attributes which provide information
    on how much data is already available and how much data still needs
    to be fetched:

    .. attribute:: fetch_needed

        The amount of data that has to be fetched in order to fetch all
        queued items.

    .. attribute:: partial_present

        The amount of data which is already available.

    .. attribute:: total_needed

        The total amount of bytes needed (including those of files which are
        already present).

    They also provide two attributes representing the items being processed
    and the workers fetching them:

    .. attribute:: items

        A list of :class:`AcquireItem` objects which are attached to the
        to this Acquire object. This includes all items ever attached to
        this object (except if they were removed using, for example,
        :meth:`shutdown()` or by deleting an :class:`AcquireFile` object.)

    .. attribute:: workers

        A list of :class:`AcquireWorker` objects which are currently active
        on this instance.

    The Acquire class comes with three constants which represents the results
    of the :meth:`run` method:

    .. attribute:: RESULT_CANCELLED

        The fetching has been aborted, e.g. due to a progress class returning
        ``False`` in its :meth:`pulse()` method.

    .. attribute:: RESULT_CONTINUE

        All items have been fetched successfully and the process has not been
        canceled.

    .. attribute:: RESULT_FAILED

        An item failed to fetch due to some reasons.


.. class:: AcquireItem

    An AcquireItem object represents a single item of an :class:`Acquire`
    object. It is an abstract class to represent various types of items
    which are implemented as subclasses. The only exported subclass is
    :class:`AcquireFile` which can be used to fetch files.

    .. attribute:: complete

        A boolean value which is True only if the item has been
        fetched successfully.

    .. attribute:: desc_uri

        An URI describing where the item is located at.

    .. attribute:: destfile

        The path to the local location where the fetched data will be
        stored at.

    .. attribute:: error_text

        The error message. For example, when a file does not exist on a HTTP
        server, this will contain a 404 error message.

    .. attribute:: filesize

        The size of the file, in bytes. If the size of the to be fetched file
        is unknown, this attribute is set to ``0``.

    .. attribute:: id

        The ID of the item. This attribute is normally set to ``0``, users may
        set a custom value here, for instance in an overridden
        :meth:`apt.progress.base.AcquireProgress.fetch` method (the progress
        class could keep a counter, increase it by one for every :meth:`fetch`
        call and assign the current value to this attribute).

    .. attribute:: is_trusted

        A boolean value determining whether the file is trusted. Only ``True``
        if the item represents a package coming from a repository which is
        signed by one of the keys in APT's keyring.

    .. attribute:: local

        A boolean value determining whether this file is locally available
        (``True``) or whether it has to be fetched from a remote source
        (``False``).

    .. attribute:: mode

        A localized string indicating the current mode e.g. ``"Fetching"``,
        it may be used as part of printing progress information.

    **Status**:

    The following attribute represents the status of the item. This class
    provides several constants for comparing against this value which are
    listed here as well.

    .. attribute:: status

        Integer, representing the status of the item. This attribute can be
        compared against the following constants to gain useful information
        on the item's status.

    .. attribute:: STAT_AUTH_ERROR

        An authentication error occurred while trying to fetch the item.

    .. attribute:: STAT_DONE

        The item is completely fetched and there have been no problems
        while fetching the item.

    .. attribute:: STAT_ERROR

        An error occurred while trying to fetch the item. This error is
        normally not related to authentication problems, as thus are
        dealt with using :attr:`STAT_AUTH_ERROR`.

    .. attribute:: STAT_FETCHING

        The item is being fetched currently.
        
    .. attribute:: STAT_IDLE

        The item is yet to be fetched.

    .. attribute:: STAT_TRANSIENT_NETWORK_ERROR

        There was a network error.


.. class:: AcquireFile(owner, uri[, md5, size, descr, short_descr, destdir, destfile])

    Create a new :class:`AcquireFile()` object and register it with *acquire*,
    so it will be fetched. You must always keep around a reference to the
    object, otherwise it will be removed from the Acquire queue again.

    The parameter *owner* refers to an :class:`Acquire()` object as returned
    by :func:`GetAcquire`. The file will be added to the Acquire queue
    automatically.

    The parameter *uri* refers to the location of the file, any protocol
    of apt is supported.

    The parameter *md5* refers to the md5sum of the file. This can be used
    for checking the file.

    The parameter *size* can be used to specify the size of the package,
    which can then be used to calculate the progress and validate the download.

    The parameter *descr* is a description of the download. It may be
    used to describe the item in the progress class. *short_descr* is the
    short form of it.

    The parameters *descr* and *short_descr* can be used to specify
    descriptions for the item. The string passed to *descr* should
    describe the file and its origin (e.g. "http://localhost sid/main
    python-apt 0.7.94.2") and the string passed to *short_descr* should
    be one word such as the name of a package.

    Normally, the file will be stored in the current directory using the
    file name given in the URI. This directory can be changed by passing
    the name of a directory to the *destdir* parameter. It is also possible
    to set a path to a file using the *destfile* parameter, but both can
    not be specified together.

    In terms of attributes, this class is a subclass of :class:`AcquireItem`
    and thus inherits all its attributes.

.. class:: AcquireWorker

    An :class:`AcquireWorker` object represents a sub-process responsible for
    fetching files from remote locations. There is no possibility to create
    instances of this class from within Python, but a list of objects of
    currently active workers is provided by :attr:`Acquire.workers`.

    Objects of this type provide several attributes which give information
    about the worker's current activity.

    .. attribute:: current_item

        The item which is currently being fetched. This returns an
        :class:`AcquireItemDesc` object.

    .. attribute:: current_size

        How many bytes of the file have been downloaded. Zero if the current
        progress of the file cannot be determined.

    .. attribute:: resumepoint

        The amount of data which was already available when the download was
        started.

    .. attribute:: status

        The most recent (localized) status string received from the
        sub-process.

    .. attribute:: total_size

        The total number of bytes to be downloaded for the item. Zero if the
        total size is unknown.

.. class:: AcquireItemDesc

    An :class:`AcquireItemDesc` object stores information about the item which
    can be used to describe the item. Objects of this class are used in the
    progress classes, see the :class:`apt.progress.base.AcquireProgress`
    documentation for information how.

    .. attribute:: description

        The long description given to the item.

    .. attribute:: owner

        The :class:`AcquireItem` object owning this object.

    .. attribute:: shortdesc

        A short description which has been given to this item.

    .. attribute:: uri

        The URI from which this item would be downloaded.


Hashes
------
The apt_pkg module also provides several hash functions. If you develop
applications with python-apt it is often easier to use these functions instead
of the ones provides in Python's :mod:`hashlib` module.

The module provides the two classes :class:`Hashes` and :class:`HashString` for
generic hash support:

.. class:: Hashes(object)

    Calculate all supported hashes of the object. *object* may either be a
    string, in which cases the hashes of the string are calculated; or a
    :class:`file()` object or file descriptor, in which case the hashes of
    its contents is calculated. The calculated hashes are then available via
    attributes:

    .. attribute:: md5

        The MD5 hash of the data, as string.

    .. attribute:: sha1

        The SHA1 hash of the data, as string.

    .. attribute:: sha256

        The SHA256 hash of the data, as string.

.. class:: HashString(type: str[, hash: str])

    HashString objects store the type of a hash and the corresponding hash.
    They are used by e.g :meth:`IndexRecords.lookup`. The first parameter,
    *type* refers to one of "MD5Sum", "SHA1" and "SHA256". The second parameter
    *hash* is the corresponding hash.

    You can also use a combined form by passing a string with type and hash
    separated by a colon as the only argument. For example::

        HashString("MD5Sum:d41d8cd98f00b204e9800998ecf8427e")
    

    .. describe:: str(hashstring)

        Convert the HashString to a string by joining the hash type and the
        hash using ':', e.g. ``"MD5Sum:d41d8cd98f00b204e9800998ecf8427e"``.

    .. attribute:: hashtype

        The type of the hash, as a string. This may be "MD5Sum", "SHA1"
        or "SHA256".

    .. method:: verify_file(filename: str) -> bool

        Verify that the file given by the parameter *filename* matches the
        hash stored in this object.

The :mod:`apt_pkg` module also provides the functions :func:`md5sum`,
:func:`sha1sum` and :func:`sha256sum` for creating a single hash from a
:class:`bytes` or :class:`file` object:

.. function:: md5sum(object)

    Return the md5sum of the object. *object* may either be a string, in
    which case the md5sum of the string is returned, or a :class:`file()`
    object (or a file descriptor), in which case the md5sum of its contents is
    returned.

    .. versionchanged:: 0.7.100
        Added support for using file descriptors.

.. function:: sha1sum(object)

    Return the sha1sum of the object. *object* may either be a string, in
    which case the sha1sum of the string is returned, or a :class:`file()`
    object (or a file descriptor), in which case the sha1sum of its contents
    is returned.

    .. versionchanged:: 0.7.100
        Added support for using file descriptors.

.. function:: sha256sum(object)

    Return the sha256sum of the object. *object* may either be a string, in
    which case the sha256sum of the string is returned, or a :class:`file()`
    object  (or a file descriptor), in which case the sha256sum of its contents
    is returned.

    .. versionchanged:: 0.7.100
        Added support for using file descriptors.

Debian control files
--------------------
Debian control files are files containing multiple stanzas of :RFC:`822`-style
header sections. They are widely used in the Debian community, and can represent
many kinds of information. One example for such a file is the
:file:`/var/lib/dpkg/status` file which contains a list of the currently
installed packages.

The :mod:`apt_pkg` module provides two classes to read those files and parts
thereof and provides a function :func:`RewriteSection` which takes a
:class:`TagSection()` object and sorting information and outputs a sorted
section as a string.

.. class:: TagFile(file, bytes: bool = False)

    An object which represents a typical debian control file. Can be used for
    Packages, Sources, control, Release, etc. Such an object provides two
    kinds of API which should not be used together:

    The first API implements the iterator protocol and should be used whenever
    possible because it has less side effects than the other one. It may be
    used e.g. with a for loop::

        tagf = apt_pkg.TagFile(open('/var/lib/dpkg/status'))
        for section in tagfile:
            print section['Package']

    .. versionchanged:: 0.7.100
        Added support for using gzip files, via :class:`gzip.GzipFile` or any
        file containing a compressed gzip stream.

    .. versionadded:: 0.8.5

        Added support for using bytes instead of str in Python 3
        
    .. method:: next()

        A TagFile is its own iterator. This method is part of the iterator
        protocol and returns a :class:`TagSection` object for the next
        section in the file. If there is no further section, this method
        raises the :exc:`StopIteration` exception.

        From Python 3 on, this method is not available anymore, and the
        global function ``next()`` replaces it.

    The second API uses a shared :class:`TagSection` object which is exposed
    through the :attr:`section` attribute. This object is modified by calls
    to :meth:`step` and :meth:`jump`. This API provides more control and may
    use less memory, but is not recommended because it works by modifying
    one object. It can be used like this::

        tagf = apt_pkg.TagFile(open('/var/lib/dpkg/status'))
        tagf.step()
        print tagf.section['Package']

    .. method:: step() -> bool

        Step forward to the next section. This simply returns ``True`` if OK,
        and ``False`` if there is no section.

    .. method:: offset() -> int

        Return the current offset (in bytes) from the beginning of the file.

    .. method:: jump(offset) -> bool

        Jump back/forward to *offset*. Use ``jump(0)`` to jump to the
        beginning of the file again. Returns ``True`` if a section could
        be parsed or ``False`` if not.

    .. attribute:: section

        This is the current :class:`TagSection()` instance.

.. class:: TagSection(text)

    Represent a single section of a debian control file.

    .. describe:: section[key]

        Return the value of the field at *key*. If *key* is not available,
        raise :exc:`KeyError`.

    .. describe:: key in section

        Return ``True`` if *section* has a key *key*, else ``False``.

      .. versionadded:: 0.7.100

    .. method:: bytes() -> int

        The number of bytes in the section.

    .. method:: find(key: str, default: str = '') -> str

        Return the value of the field at the key *key* if available,
        else return *default*.

    .. method:: find_flag(key: str) -> bool

        Find a yes/no value for the key *key*. An example for such a
        field is 'Essential'.

    .. method:: find_raw(key: str, default: str = '') -> str

        Similar to :meth:`find`, but instead of returning just the value,
        it returns the complete field consisting of 'key: value'.

    .. method:: get(key: str, default: str = '')

        Return the value of the field at the key *key* if available, else
        return *default*. 

    .. method:: keys()

        Return a list of keys in the section.

.. function:: rewrite_section(section: TagSection, order: list, rewrite_list: list) -> str

    Rewrite the section given by *section* using *rewrite_list*, and order the
    fields according to *order*.

    The parameter *order* is a :class:`list` object containing the names of the
    fields in the order they should appear in the rewritten section.
    :data:`apt_pkg.REWRITE_PACKAGE_ORDER` and
    :data:`apt_pkg.REWRITE_SOURCE_ORDER` are two predefined lists for rewriting
    package and source sections, respectively.

    The parameter *rewrite_list* is a list of tuples of the form
    ``(tag, newvalue[, renamed_to])``, where *tag* describes the field which
    should be changed, *newvalue* the value which should be inserted or
    ``None`` to delete the field, and the optional *renamed_to* can be used
    to rename the field.

.. data:: REWRITE_PACKAGE_ORDER

    The order in which the information for binary packages should be rewritten,
    i.e. the order in which the fields should appear.

.. data:: REWRITE_SOURCE_ORDER

    The order in which the information for source packages should be rewritten,
    i.e. the order in which the fields should appear.

Dependencies
------------
.. function:: check_dep(pkgver: str, op: str, depver: str) -> bool

    Check that the given requirement is fulfilled; that is, that the version
    string given by *pkg_ver* matches the version string *dep_ver* under
    the condition specified by the operator 'dep_op' (<,<=,=,>=,>).
        
    Return True if *pkg_ver* matches *dep_ver* under the condition 'dep_op';
    for example::

        >>> apt_pkg.check_dep("1.0", ">=", "1")
        True

The following two functions provide the ability to parse dependencies. They
use the same format as :attr:`Version.depends_list_str`.

.. function:: parse_depends(depends, strip_multiarch=True)

    Parse the string *depends* which contains dependency information as
    specified in Debian Policy, Section 7.1.

    Returns a list. The members of this list are lists themselves and contain
    one or more tuples in the format ``(package,version,operation)`` for every
    'or'-option given, e.g.::

        >>> apt_pkg.parse_depends("PkgA (>= VerA) | PkgB (>= VerB)")
        [[('PkgA', 'VerA', '>='), ('PkgB', 'VerB', '>=')]]

    Note that multiarch dependency information is stripped off by default.
    You can force the full dependency info (including the multiarch info)
    by passing "False" as a additional parameter to this function.

    .. note::

        The behavior of this function is different than the behavior of the
        old function :func:`ParseDepends()`, because the third field
        ``operation`` uses `>` instead of `>>` and `<` instead of `<<` which
        is specified in control files.


.. function:: parse_src_depends(depends)

    Parse the string *depends* which contains dependency information as
    specified in Debian Policy, Section 7.1.

    Returns a list. The members of this list are lists themselves and contain
    one or more tuples in the format ``(package,version,operation)`` for every
    'or'-option given, e.g.::

        >>> apt_pkg.parse_depends("PkgA (>= VerA) | PkgB (>= VerB)")
        [[('PkgA', 'VerA', '>='), ('PkgB', 'VerB', '>=')]]


    Furthemore, this function also supports to limit the architectures, as
    used in e.g. Build-Depends::

        >>> apt_pkg.parse_src_depends("a (>= 01) [i386 amd64]")
        [[('a', '01', '>=')]]

    .. note::

        The behavior of this function is different than the behavior of the
        old function :func:`ParseDepends()`, because the third field
        ``operation`` uses `>` instead of `>>` and `<` instead of `<<` which
        is specified in control files.


Configuration and Command-line parsing
--------------------------------------

.. class:: Configuration()

    Provide access to and manipulation of APT's configuration which is
    used by many classes and functions in this module to define their
    behavior. There are options to install recommends, change the root
    directory and much more. For an (incomplete) list of available options,
    see the :manpage:`apt.conf(5)` manual page.

    The most important Configuration object is the one available by the
    module's :attr:`apt_pkg.config` attribute. It stores the global
    configuration which affects the behavior of most functions and is
    initialized by a call to the function :func:`init_config`. While
    possible, it is generally not needed to create other instances of
    this class.

    For accessing and manipulating the configuration space, objects
    of this type provide an interface which resembles Python mapping
    types like :class:`dict`.

    .. describe:: key in conf

        Return ``True`` if *conf* has a key *key*, else ``False``.

    .. describe:: conf[key]

        Return the value of the option given key *key*. If it does not
        exist, raise :exc:`KeyError`.

    .. describe:: conf[key] = value

        Set the option at *key* to *value*.

    .. describe del conf[key]

        Delete the option with the name *key* in the configuration object
        *conf*.

    .. method:: get(key[, default='']) -> str

        Find the value for the given key and return it. If the given key does
        not exist, return *default* instead. 

    In addition, they provide methods to resemble the interface provided
    by the C++ class and some more mapping methods which have been enhanced
    to support some more advanced configuration features:

    .. method:: clear(key: str)

        Remove the option at *key* and all of its children.

    .. method:: dump() -> str

        Return a string containing the values in the configuration object,
        in the standard :manpage:`apt.conf(5)` format.

        .. versionadded:: 0.7.100

    .. method:: exists(key)

        Check whether an option named *key* exists in the configuration.

    .. method:: find(key[, default='']) -> str

        Return the value stored at the option named *key*, or the value
        given by the string *default* if the option in question is not
        set.

    .. method:: find_b(key[, default=False]) -> bool

        Return the boolean value stored at *key*, or the value given by
        the :class:`bool` object *default* if the requested option is
        not set.

    .. method:: find_file(key[, default='']) -> str
                find_dir(key[, default='/']) -> str

        Locate the given key using :meth:`find` and return the path to the
        file/directory. This uses a special algorithms which moves upwards
        in the configuration space and prepends the values of the options
        to the result. These methods are generally used for the options
        stored in the 'Dir' section of the configuration.

        As an example of how this works, take a look at the following options
        and their values:

        .. table::

            ============== ===========================
            Option         Value
            ============== ===========================
            Dir            /
            Dir::Etc       etc/apt/
            Dir::Etc::main apt.conf
            ============== ===========================

        A call to :meth:`find_file` would now return ``/etc/apt/apt.conf``
        because it prepends the values of "Dir::Etc" and "Dir" to the value
        of "Dir::Etc::main"::

            >>> apt_pkg.config.find_file("Dir::Etc::main")
            '/etc/apt/apt.conf'

        If the special configuration variable "RootDir" is set, this value
        would be prepended to every return value, even if the path is already
        absolute. If not, the function ends as soon as an absolute path is
        created (once an option with a value starting with "/" is read).

        The method :meth:`find_dir` does exactly the same thing as
        :meth:`find_file`, but adds a trailing forward slash before
        returning the value.

    .. method:: find_i(key[, default=0]) -> int

        Return the integer value stored at *key*, or the value given by
        the integer *default* if the requested option is not set.

    .. method:: keys([key])

        Return a recursive list of all configuration options or, if *key*
        is given, a list of all its children. This method is comparable
        to the **keys** method of a mapping object, but additionally
        provides the parameter *key*.
        
    .. method:: list([key])

        Return a non-recursive list of all configuration options. If *key*
        is not given, this returns a list of options like "Apt", "Dir", and
        similar. If *key* is given, a list of the names of its child options
        will be returned instead.

    .. method:: my_tag()

        Return the tag name of the current tree. Normally (for
        :data:`apt_pkg.config`) this is an empty string, but for
        sub-trees it is the key of the sub-tree.

    .. method:: set(key: str, value: str)

        Set the option named *key* to the value given by the argument
        *value*. It is possible to store objects of the types :class:`int`
        and :class:`bool` by calling :func:`str` on them to convert them
        to a string object. They can then be retrieved again by using the
        methods :meth:`find_i` or :meth:`find_b`.

    .. method:: subtree(key)

        Return a new apt_pkg.Configuration object which starts at the
        given option. Example::

            apttree = config.subtree('APT')
            apttree['Install-Suggests'] = config['APT::Install-Suggests']

        The configuration space is shared with the main object which means
        that all modifications in one object appear in the other one as
        well.

    .. method:: value_list([key])

        This is the opposite of the :meth:`list` method in that it returns the
        values instead of the option names.

.. data:: config

    This variable contains the global configuration which is used by
    all classes and functions in this module. After importing the
    module, this object should be initialized by calling the module's
    :func:`init_config` function.

.. function:: read_config_file(configuration: Configuration, filename: str)

    Read the configuration file *filename* and set the appropriate
    options in the configuration object *configuration*.

.. function:: read_config_dir(configuration, dirname)

    Read all configuration files in the dir given by 'dirname' in the
    correct order.


.. function:: read_config_file_isc(configuration, filename)

    Read the configuration file *filename* and set the appropriate
    options in the configuration object *configuration*. This function
    requires a slightly different format than APT configuration files,
    if you are unsure, do not use it.

.. function:: parse_commandline(configuration, options, argv)

    Parse the command line in *argv* into the configuration space. The
    list *options* contains a list of 3-tuples or 4-tuples in the form::
        
        (short_option: str, long_option: str, variable: str[, type: str])
        
    The element *short_option* is one character, the *long_option* element
    is the name of the long option, the element *variable* the name of the
    configuration option the result will be stored in and *type* is one of
    'HasArg', 'IntLevel', 'Boolean', 'InvBoolean', 'ConfigFile',
    'ArbItem'. The default type is 'Boolean'.

    .. table:: Overview of all possible types

        ===========     =====================================================
        Type            What happens if the option is given
        ===========     =====================================================
        HasArg          The argument given to the option is stored in
                        the target.
        IntLevel        The integer value in the target is increased by one
        Boolean         The target variable is set to True.
        InvBoolean      The target variable is set to False.
        ConfigFile      The file given as an argument to this option is read
                        in and all configuration options are added to the
                        configuration object (APT's '-c' option).
        ArbItem         The option takes an argument *key*=*value*, and the
                        configuration option at *key* is set to the value
                        *value* (APT's '-o' option).
        ===========     =====================================================


Locking
--------
When working on the global cache, it is important to lock the cache so other
programs do not modify it. This module provides two context managers for
locking the package system or file-based locking.

.. class:: SystemLock

    Context manager for locking the package system. The lock is established
    as soon as the method __enter__() is called. It is released when
    __exit__() is called. If the lock can not be acquired or can not be
    released an exception is raised.

    This should be used via the 'with' statement. For example::

        with apt_pkg.SystemLock():
            ... # Do your stuff here.
        ... # Now it's unlocked again

    Once the block is left, the lock is released automatically. The object
    can be used multiple times::

        lock = apt_pkg.SystemLock()
        with lock:
            ...
        with lock:
            ...

.. class:: FileLock(filename: str)

    Context manager for locking using a file. The lock is established
    as soon as the method __enter__() is called. It is released when
    __exit__() is called. If the lock can not be acquired or can not be
    released, an exception is raised.

    This should be used via the 'with' statement. For example::

        with apt_pkg.FileLock(filename):
            ...

    Once the block is left, the lock is released automatically. The object
    can be used multiple times::

        lock = apt_pkg.FileLock(filename)
        with lock:
            ...
        with lock:
            ...

For Python versions prior to 2.5, similar functionality is provided by the
following three functions:

.. function:: get_lock(filename: str, errors=False) -> int

    Create an empty file at the path specified by the parameter *filename* and
    lock it. If this fails and *errors* is **True**, the function raises an
    error. If *errors* is **False**, the function returns -1.

    The lock can be acquired multiple times within the same process, and can be
    released by calling :func:`os.close` on the return value which is the file
    descriptor of the created file.

.. function:: pkgsystem_lock()

    Lock the global pkgsystem. The lock should be released by calling
    :func:`pkgsystem_unlock` again. If this function is called n-times, the
    :func:`pkgsystem_unlock` function must be called n-times as well to release
    all acquired locks.

.. function:: pkgsystem_unlock()

    Unlock the global pkgsystem. This reverts the effect of
    :func:`pkgsystem_lock`.


Other classes
--------------
.. class:: Cdrom()

    A Cdrom object identifies Debian installation media and adds them to
    :file:`/etc/apt/sources.list`. The C++ version of this class is used by
    the apt-cdrom tool and using this class, you can re-implement apt-cdrom
    in Python, see :doc:`../tutorials/apt-cdrom`.

    The class :class:`apt.cdrom.Cdrom` is a subclass of this class and
    provides some additional functionality for higher level use and some
    shortcuts for setting some related configuration options.

    This class provides two functions which take an instance of
    :class:`apt.progress.base.CdromProgress` as their argument.

    .. method:: add(progress: apt.progress.base.CdromProgress) -> bool

        Search for a Debian installation media and add it to the list of
        sources stored in :file:`/etc/apt/sources.list`. On success, the
        boolean value ``True`` is returned. If the process failed or was
        canceled by the progress class, :exc:`SystemError` is raised or
        ``False`` is returned.

    .. method:: ident(progress: apt.progress.base.CdromProgress) -> str

        Identify the installation media and return a string which describes
        its identity. If no media could be identified, :exc:`SystemError` is
        raised or ``None`` is returned.

.. class:: SourceList

    Represent the list of sources stored in files such as
    :file:`/etc/apt/sources.list`.

    .. method:: find_index(pkgfile: PackageFile) -> IndexFile

        Return the :class:`IndexFile` object for the :class:`PackageFile`
        object given by the argument *pkgfile*. If no index could be found,
        return ``None``.

    .. method:: get_indexes(acquire: Acquire[, all: bool = False]) -> bool

        Add all indexes to the :class:`Acquire` object given by the argument
        *acquire*. If *all* is ``True``, all indexes will be added, otherwise
        only the meta indexes (Release files) will be added and others are
        fetched as needed.

    .. method:: read_main_list() -> bool

        Read the files configured in Dir::Etc::SourceList and
        Dir::Etc::sourceparts; that is (on normal system),
        :file:`/etc/apt/sources.list` and the files in
        :file:`/etc/apt/sources.list.d`.

    .. attribute:: list

        A list of :class:`MetaIndex` objects.

String functions
----------------
.. function:: base64_encode(value: bytes) -> str

    Encode the given bytes string (which may not contain a null byte)
    using base64, for example, on Python 3 and newer::

        >>> apt_pkg.base64_encode(b"A")
        'QQ=='

    on Python versions prior to 3, the 'b' before the string has to be
    omitted.

.. function:: check_domain_list(host, list)

    See if the host name given by *host* is one of the domains given in the
    comma-separated list *list* or a subdomain of one of them.

        >>> apt_pkg.check_domain_list("alioth.debian.org","debian.net,debian.org")
        True

.. function:: dequote_string(string: str)

    Dequote the string specified by the parameter *string*, e.g.::

        >>> apt_pkg.dequote_string("%61%70%74%20is%20cool")
        'apt is cool'

.. function:: quote_string(string, repl)

    Escape the string *string*, replacing any character not allowed in a URL
    or specified by *repl* with its ASCII value preceded by a percent sign
    (so for example ' ' becomes '%20').

        >>> apt_pkg.quote_string("apt is cool","apt")
        '%61%70%74%20is%20cool'

.. function:: size_to_str(size: int)

    Return a string describing the size in a human-readable manner using
    SI prefix and base-10 units, e.g. '1k' for 1000, '1M' for 1000000, etc.

    Example::

        >>> apt_pkg.size_to_str(10000)
        '10.0k'

.. function:: string_to_bool(input)

    Parse the string *input* and return one of **-1**, **0**, **1**.

    .. table:: Return values

        ===== =============================================
        Value      Meaning
        ===== =============================================
         -1   The string *input* is not recognized.
          0   The string *input* evaluates to **False**.
         +1   The string *input* evaluates to **True**.
        ===== =============================================

    Example::

        >>> apt_pkg.string_to_bool("yes")
        1
        >>> apt_pkg.string_to_bool("no")
        0
        >>> apt_pkg.string_to_bool("not-recognized")
        -1


.. function:: str_to_time(rfc_time)

    Convert the :rfc:`1123` conforming string *rfc_time* to the unix time, and
    return the integer. This is the opposite of :func:`TimeRFC1123`.

    Example::

        >> apt_pkg.str_to_time('Thu, 01 Jan 1970 00:00:00 GMT')
        0

.. function:: time_rfc1123(seconds: int) -> str

    Format the unix time specified by the integer *seconds*, according to the
    requirements of :rfc:`1123`.

    Example::

        >>> apt_pkg.time_rfc1123(0)
        'Thu, 01 Jan 1970 00:00:00 GMT'


.. function:: time_to_str(seconds: int) -> str

    Format a given duration in a human-readable manner. The parameter *seconds*
    refers to a number of seconds, given as an integer. The return value is a
    string with a unit like 's' for seconds.

    Example::

        >>> apt_pkg.time_to_str(3601)
        '1h0min1s'

.. function:: upstream_version(version: str) -> str

    Return the upstream version for the Debian package version given by
    *version*.

.. function:: uri_to_filename(uri: str) -> str

    Take a string *uri* as parameter and return a filename which can be used to
    store the file, based on the URI.

    Example::

        >>> apt_pkg.uri_to_filename('http://debian.org/index.html')
        'debian.org_index.html'


.. function:: version_compare(a: str, b: str) -> int

    Compare two versions, *a* and *b*, and return an integer value which has
    the same meaning as the built-in :func:`cmp` function's return value has,
    see the following table for details.

    .. table:: Return values

        ===== =============================================
        Value      Meaning
        ===== =============================================
        > 0   The version *a* is greater than version *b*.
        = 0   Both versions are equal.
        < 0   The version *a* is less than version *b*.
        ===== =============================================


Module Constants
----------------
.. _CurStates:

Package States
^^^^^^^^^^^^^^^
.. data:: CURSTATE_CONFIG_FILES

    Only the configuration files of the package exist on the system.

.. data:: CURSTATE_HALF_CONFIGURED

    The package is unpacked and configuration has been started, but not
    yet completed.

.. data:: CURSTATE_HALF_INSTALLED

    The installation of the package has been started, but not completed.

.. data:: CURSTATE_INSTALLED

    The package is unpacked, configured and OK.

.. data:: CURSTATE_NOT_INSTALLED

    The package is not installed.

.. data:: CURSTATE_UNPACKED

    The package is unpacked, but not configured.

.. _InstStates:

Installed states
^^^^^^^^^^^^^^^^
.. data:: INSTSTATE_HOLD

    The package is put on hold.

.. data:: INSTSTATE_HOLD_REINSTREQ

    The package is put on hold, but broken and has to be reinstalled.
    
.. data:: INSTSTATE_OK

    The package is OK.

.. data:: INSTSTATE_REINSTREQ

    The package is broken and has to be reinstalled.

.. _Priorities:

Priorities
^^^^^^^^^^^
.. data:: PRI_EXTRA

    The integer representation of the priority 'extra'.

.. data:: PRI_IMPORTANT

    The integer representation of the priority 'important'.
    
.. data:: PRI_OPTIONAL

    The integer representation of the priority 'optional'.

.. data:: PRI_REQUIRED

    The integer representation of the priority 'required'.

.. data:: PRI_STANDARD

    The integer representation of the priority 'standard'.


.. _SelStates:

Package selection states
^^^^^^^^^^^^^^^^^^^^^^^^
.. data:: SELSTATE_DEINSTALL

    The package is selected for deinstallation.

.. data:: SELSTATE_HOLD

    The package is marked to be on hold and will not be modified.

.. data:: SELSTATE_INSTALL

    The package is selected for installation.
    
.. data:: SELSTATE_PURGE

    The package is selected to be purged.

.. data:: SELSTATE_UNKNOWN

    The package is in an unknown state.


Build information
^^^^^^^^^^^^^^^^^
.. data:: DATE

    The date on which this extension has been compiled.

.. data:: LIB_VERSION

    The version of the apt_pkg library. This is **not** the version of apt,
    nor the version of python-apt.

.. data:: TIME

    The time this extension has been built.

.. data:: VERSION

    The version of apt (not of python-apt).
