// Description								/*{{{*/
// $Id: acquire.cc,v 1.1 2003/06/03 03:03:23 mvo Exp $
/* ######################################################################

   PkgManager - Wrapper for the pkgPackageManager code

   ##################################################################### */

#include "generic.h"
#include "apt_pkgmodule.h"
#include "pkgrecords.h"

#include <apt-pkg/packagemanager.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/error.h>
#include <apt-pkg/acquire.h>
#include <apt-pkg/init.h>
#include <apt-pkg/configuration.h>
#include <apt-pkg/dpkgpm.h>

#include <iostream>

static PyObject *PkgManagerGetArchives(PyObject *Self,PyObject *Args)
{
   pkgPackageManager *pm = GetCpp<pkgPackageManager*>(Self);
   PyObject *fetcher, *list, *recs;

   if (PyArg_ParseTuple(Args, "O!O!O!",
			&PyAcquire_Type,&fetcher,
			&PySourceList_Type, &list,
			&PyPackageRecords_Type, &recs) == 0)
      return 0;

   pkgAcquire *s_fetcher = GetCpp<pkgAcquire*>(fetcher);
   pkgSourceList *s_list = GetCpp<pkgSourceList*>(list);
   PkgRecordsStruct &s_records = GetCpp<PkgRecordsStruct>(recs);

   bool res = pm->GetArchives(s_fetcher, s_list,
			      &s_records.Records);

   return HandleErrors(PyBool_FromLong(res));
}

static PyObject *PkgManagerDoInstall(PyObject *Self,PyObject *Args)
{
   //PkgManagerStruct &Struct = GetCpp<PkgManagerStruct>(Self);
   pkgPackageManager *pm = GetCpp<pkgPackageManager*>(Self);
   int status_fd = -1;

   if (PyArg_ParseTuple(Args, "|i", &status_fd) == 0)
      return 0;

   pkgPackageManager::OrderResult res = pm->DoInstall(status_fd);

   return HandleErrors(MkPyNumber(res));
}

static PyObject *PkgManagerFixMissing(PyObject *Self,PyObject *Args)
{
   //PkgManagerStruct &Struct = GetCpp<PkgManagerStruct>(Self);
   pkgPackageManager *pm = GetCpp<pkgPackageManager*>(Self);

   if (PyArg_ParseTuple(Args, "") == 0)
      return 0;

   bool res = pm->FixMissing();

   return HandleErrors(PyBool_FromLong(res));
}

static PyMethodDef PkgManagerMethods[] =
{
   {"get_archives",PkgManagerGetArchives,METH_VARARGS,
    "get_archives(fetcher: Acquire, list: SourceList, recs: PackageRecords) -> bool\n\n"
    "Download the packages marked for installation via the Acquire object\n"
    "'fetcher', using the information found in 'list' and 'recs'."},
   {"do_install",PkgManagerDoInstall,METH_VARARGS,
    "do_install(status_fd: int) -> int\n\n"
    "Install the packages and return one of the class constants\n"
    "RESULT_COMPLETED, RESULT_FAILED, RESULT_INCOMPLETE. The argument\n"
    "status_fd can be used to specify a file descriptor that APT will\n"
    "write status information on (see README.progress-reporting in the\n"
    "apt source code for information on what will be written there)."},
   {"fix_missing",PkgManagerFixMissing,METH_VARARGS,
    "fix_missing() -> bool\n\n"
    "Fix the installation if a package could not be downloaded."},
   {}
};

static const char *packagemanager_doc =
    "_PackageManager objects allow the fetching of packages marked for\n"
    "installation and the installation of those packages.\n"
    "This is an abstract base class that cannot be subclassed\n"
    "in Python. The only subclass is apt_pkg.PackageManager. This\n"
    "class is an implementation-detail and not part of the API.";
PyTypeObject PyPackageManager_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg._PackageManager",           // tp_name
   sizeof(CppPyObject<pkgPackageManager*>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDeallocPtr<pkgPackageManager*>,   // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   0,                                   // tp_repr
   0,                                   // tp_as_number
   0,                                   // tp_as_sequence
   0,                                   // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT,                 // tp_flag,
   packagemanager_doc,                  // tp_doc
   0,                                   // tp_traverse
   0,                                   // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   PkgManagerMethods,                   // tp_methods
   0,                                   // tp_members
   0,                                   // tp_getset
   0,                                   // tp_base
   0,                                   // tp_dict
   0,                                   // tp_descr_get
   0,                                   // tp_descr_set
   0,                                   // tp_dictoffset
   0,                                   // tp_init
   0,                                   // tp_alloc
   0,                                   // tp_new
};


struct CppPyRef {
	PyObject *o;
	CppPyRef(const CppPyRef &o) { Py_XINCREF(o); this->o = o; }
	CppPyRef(PyObject *o) : o(o) {}
	~CppPyRef() { Py_XDECREF(o); }
	operator PyObject *() const { return o; }
	PyObject *operator->() const { return o; }
};

class PyPkgManager : public pkgDPkgPM {
	bool res(CppPyRef result) {
		if (result == NULL) {
			std::cerr << "Error in function: " << std::endl;
			PyErr_Print();
			PyErr_Clear();
			return false;
		}
		return (result != NULL &&
		        (result == Py_None || PyObject_IsTrue(result) == 1));
	}
	
	
	PyObject *GetPyPkg(const PkgIterator &Pkg) {		
		PyObject *depcache = NULL;
		PyObject *cache = NULL;
		
		depcache = GetOwner<PyPkgManager*>(pyinst);
		if (depcache != NULL && PyDepCache_Check(depcache))
			cache = GetOwner<pkgDepCache*>(depcache);

		return PyPackage_FromCpp(Pkg, true, cache);
	}

	/* Call through to Python */
	virtual bool Install(PkgIterator Pkg,string File) {
		return res(PyObject_CallMethod(pyinst, "install", "(NN)", 
		                               GetPyPkg(Pkg),
		                               CppPyString(File)));
	}
	virtual bool Configure(PkgIterator Pkg) {
		return res(PyObject_CallMethod(pyinst, "configure", "(N)", 
		                               GetPyPkg(Pkg)));
	}
	virtual bool Remove(PkgIterator Pkg,bool Purge = false) {
		return res(PyObject_CallMethod(pyinst, "remove", "(NN)", 
		                               GetPyPkg(Pkg),
		                               PyBool_FromLong(Purge)));
	}
	virtual bool Go(int StatusFd=-1) {
		return res(PyObject_CallMethod(pyinst, "go", "(i)",
		                               StatusFd));
	}
	virtual void Reset() {
		CppPyRef(PyObject_CallMethod(pyinst, "reset", NULL));
	}
	
public:
	/* Those call the protected functions from the parent class */
	bool callInstall(PkgIterator Pkg,string File) { return pkgDPkgPM::Install(Pkg, File); }
	bool callRemove(PkgIterator Pkg, bool Purge) { return pkgDPkgPM::Remove(Pkg, Purge); }
	bool callGo(int StatusFd=-1) { return pkgDPkgPM::Go(StatusFd); }
	void callReset() { return pkgDPkgPM::Reset(); }
	bool callConfigure(PkgIterator Pkg) { return pkgDPkgPM::Configure(Pkg); }
	pkgOrderList *getOrderList() { return pkgPackageManager::List; }

	PyPkgManager(pkgDepCache *Cache) : pkgDPkgPM(Cache) {};
	PyObject *pyinst;
};

static PyObject *PkgManagerNew(PyTypeObject *type,PyObject *Args,PyObject *kwds)
{
   PyObject *Owner;
   char *kwlist[] = {"depcache",0};
   if (PyArg_ParseTupleAndKeywords(Args,kwds,"O!",kwlist,&PyDepCache_Type,
                                   &Owner) == 0)
      return 0;

   PyPkgManager *pm = new PyPkgManager(GetCpp<pkgDepCache*>(Owner));

   CppPyObject<PyPkgManager*> *PkgManagerObj =
	   CppPyObject_NEW<PyPkgManager*>(NULL, type,pm);
	   
   pm->pyinst = PkgManagerObj;

   return PkgManagerObj;
}

#ifdef COMPAT_0_7
PyObject *GetPkgManager(PyObject *Self,PyObject *Args)
{
    PyErr_WarnEx(PyExc_DeprecationWarning, "apt_pkg.GetPackageManager() is "
                 "deprecated. Please see apt_pkg.PackageManager() for the "
                 "replacement.", 1);
    return PkgManagerNew(&PyPackageManager2_Type,Args,0);
}
#endif

static PyObject *PkgManagerInstall(PyObject *Self,PyObject *Args)
{
   PyPkgManager *pm = GetCpp<PyPkgManager*>(Self);
   PyObject *pkg;
   const char *file;

   if (PyArg_ParseTuple(Args, "O!s", &PyPackage_Type,&pkg, &file) == 0)
      return 0;

   return HandleErrors(PyBool_FromLong(pm->callInstall(PyPackage_ToCpp(pkg), file)));
}


static PyObject *PkgManagerConfigure(PyObject *Self,PyObject *Args)
{
   PyPkgManager *pm = GetCpp<PyPkgManager*>(Self);
   PyObject *pkg;

   if (PyArg_ParseTuple(Args, "O!", &PyPackage_Type,&pkg) == 0)
      return 0;

   return HandleErrors(PyBool_FromLong(pm->callConfigure(PyPackage_ToCpp(pkg))));
}

static PyObject *PkgManagerRemove(PyObject *Self,PyObject *Args)
{
   PyPkgManager *pm = GetCpp<PyPkgManager*>(Self);
   PyObject *pkg;
   char purge;

   if (PyArg_ParseTuple(Args, "O!b", &PyPackage_Type,&pkg, &purge) == 0)
      return 0;

   return HandleErrors(PyBool_FromLong(pm->callRemove(PyPackage_ToCpp(pkg), purge)));
}

static PyObject *PkgManagerGo(PyObject *Self,PyObject *Args)
{
   PyPkgManager *pm = GetCpp<PyPkgManager*>(Self);
   int fd;

   if (PyArg_ParseTuple(Args, "i", &fd) == 0)
      return 0;

   return HandleErrors(PyBool_FromLong(pm->callGo(fd)));
}

static PyObject *PkgManagerReset(PyObject *Self,PyObject *Args)
{
   PyPkgManager *pm = GetCpp<PyPkgManager*>(Self);

   pm->callReset();
   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}

static PyMethodDef PkgManager2Methods[] =
{
   {"install",PkgManagerInstall,METH_VARARGS,   
    "install(pkg: Package, filename: str) -> bool \n\n"
    "Add a install action. Can be overridden in subclasses.\n\n"
    "New in version 0.8.0."},
   {"configure",PkgManagerConfigure,METH_VARARGS,
    "configure(pkg: Package) -> bool \n\n"
    "Add a configure action. Can be overridden in subclasses.\n\n"
    "New in version 0.8.0."},
   {"remove",PkgManagerRemove,METH_VARARGS,
    "remove(pkg: Package, purge: bool) -> bool \n\n"
    "Add a removal action. Can be overridden in subclasses.\n\n"
    "New in version 0.8.0."},
   {"go",PkgManagerGo,METH_VARARGS,
    "go(status_fd: int) -> bool \n\n"
    "Start dpkg. Can be overridden in subclasses.\n\n"
    "New in version 0.8.0."},
   {"reset",PkgManagerReset,METH_VARARGS,
    "reset()\n\n"
    "Reset the package manager for a new round.\n"
    "Can be overridden in subclasses.\n\n"
    "New in version 0.8.0."},
   {}
};

static const char *packagemanager2_doc =
    "PackageManager(depcache: apt_pkg.DepCache)\n\n"
    "PackageManager objects allow the fetching of packages marked for\n"
    "installation and the installation of those packages. The parameter\n"
    "'depcache' specifies an apt_pkg.DepCache object where information\n"
    "about the package selections is retrieved from.\n\n"
    "Methods in this class can be overridden in sub classes\n"
    "to implement behavior different from APT's dpkg implementation.";
PyTypeObject PyPackageManager2_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.PackageManager",           // tp_name
   sizeof(CppPyObject<PyPkgManager*>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDeallocPtr<PyPkgManager*>,        // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   0,                                   // tp_repr
   0,                                   // tp_as_number
   0,                                   // tp_as_sequence
   0,                                   // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   0,                                   // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   (Py_TPFLAGS_DEFAULT |                // tp_flags
    Py_TPFLAGS_BASETYPE),
   packagemanager2_doc,                 // tp_doc
   0,                                   // tp_traverse
   0,                                   // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   PkgManager2Methods,                  // tp_methods
   0,                                   // tp_members
   0,                                   // tp_getset
   &PyPackageManager_Type,              // tp_base
   0,                                   // tp_dict
   0,                                   // tp_descr_get
   0,                                   // tp_descr_set
   0,                                   // tp_dictoffset
   0,                                   // tp_init
   0,                                   // tp_alloc
   PkgManagerNew,                       // tp_new
};


									/*}}}*/
