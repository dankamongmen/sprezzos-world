// -*- mode: cpp; mode: fold -*-
// Description								/*{{{*/
// $Id: cache.cc,v 1.5 2003/06/03 03:03:23 mdz Exp $
/* ######################################################################

   Cache - Wrapper for the cache related functions

   ##################################################################### */
									/*}}}*/
// Include Files							/*{{{*/
#include "generic.h"
#include "apt_pkgmodule.h"

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/cachefile.h>
#include <apt-pkg/sptr.h>
#include <apt-pkg/configuration.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/error.h>
#include <apt-pkg/packagemanager.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/algorithms.h>

#include <Python.h>
#include "progress.h"

class pkgSourceList;

// must be in sync with pkgCache::DepType in libapt
// it sucks to have it here duplicated, but we get it
// translated from libapt and that is certainly not what
// we want in a programing interface
const char *UntranslatedDepTypes[] =
{
   "", "Depends","PreDepends","Suggests",
   "Recommends","Conflicts","Replaces",
   "Obsoletes", "Breaks", "Enhances"
};

									/*}}}*/

template<typename T> struct IterListStruct
{
   T Iter;
   unsigned long LastIndex;

   IterListStruct(T const &I) : Iter(I), LastIndex(0) {}
   IterListStruct() {};

   bool move(unsigned long Index) {
       if (Index < 0 || (unsigned)Index >= Count())
       {
          PyErr_SetNone(PyExc_IndexError);
          return false;
       }

       if ((unsigned)Index < LastIndex)
       {
          LastIndex = 0;
          Iter = Begin();
       }

       while ((unsigned)Index > LastIndex)
       {
          LastIndex++;
          Iter++;
          if (Iter.end() == true)
          {
         PyErr_SetNone(PyExc_IndexError);
         return false;
          }
       }
       return true;
    }

    virtual unsigned Count() = 0;
    virtual T Begin() = 0;

};

struct PkgListStruct : public IterListStruct<pkgCache::PkgIterator> {
    unsigned Count() { return Iter.Cache()->HeaderP->PackageCount; }
    pkgCache::PkgIterator Begin() { return Iter.Cache()->PkgBegin(); }

    PkgListStruct(pkgCache::PkgIterator const &I) { Iter = I; }
};

struct GrpListStruct : public IterListStruct<pkgCache::GrpIterator> {
    unsigned Count() { return Iter.Cache()->HeaderP->GroupCount; }
    pkgCache::GrpIterator Begin() { return Iter.Cache()->GrpBegin(); }
    GrpListStruct(pkgCache::GrpIterator const &I) { Iter = I; }
};

struct RDepListStruct
{
   pkgCache::DepIterator Iter;
   pkgCache::DepIterator Start;
   unsigned long LastIndex;
   unsigned long Len;

   RDepListStruct(pkgCache::DepIterator const &I) : Iter(I), Start(I),
                         LastIndex(0)
   {
      Len = 0;
      pkgCache::DepIterator D = I;
      for (; D.end() == false; D++)
	 Len++;
   }
   RDepListStruct() {abort();};  // G++ Bug..
};

static PyObject *CreateProvides(PyObject *Owner,pkgCache::PrvIterator I)
{
   PyObject *List = PyList_New(0);
   for (; I.end() == false; I++)
   {
      PyObject *Obj;
      PyObject *Ver;
      Ver = CppPyObject_NEW<pkgCache::VerIterator>(Owner,&PyVersion_Type,
							I.OwnerVer());
      Obj = Py_BuildValue("ssN",I.ParentPkg().Name(),I.ProvideVersion(),
			  Ver);
      PyList_Append(List,Obj);
      Py_DECREF(Obj);
   }
   return List;
}

// Cache Class								/*{{{*/
// ---------------------------------------------------------------------

static const char *cache_update_doc =
    "update(progress, sources: SourceList, pulse_interval: int) -> bool\n\n"
    "Update the index files used by the cache. A call to this method\n"
    "does not affect the current Cache object; instead, a new one\n"
    "should be created in order to use the changed index files.\n\n"
    "The parameter 'progress' can be used to specify an\n"
    "apt.progress.base.AcquireProgress() object , which will report\n"
    "progress information while the index files are being fetched.\n"
    "The parameter 'sources', if provided, is an apt_pkg.SourcesList\n"
    "object listing the remote repositories to be used.\n"
    "The 'pulse_interval' parameter indicates how long (in microseconds)\n"
    "to wait between calls to the pulse() method of the 'progress' object.\n"
    "The default is 500000 microseconds.";
static PyObject *PkgCacheUpdate(PyObject *Self,PyObject *Args)
{
   PyObject *pyFetchProgressInst = 0;
   PyObject *pySourcesList = 0;
   int pulseInterval = 0;
   if (PyArg_ParseTuple(Args, "OO!|i", &pyFetchProgressInst,
            &PySourceList_Type, &pySourcesList, &pulseInterval) == 0)
      return 0;

   PyFetchProgress progress;
   progress.setCallbackInst(pyFetchProgressInst);
   pkgSourceList *source = GetCpp<pkgSourceList*>(pySourcesList);
   bool res = ListUpdate(progress, *source, pulseInterval);

   PyObject *PyRes = PyBool_FromLong(res);
   return HandleErrors(PyRes);
}

#ifdef COMPAT_0_7
static PyObject *PkgCacheClose(PyObject *Self,PyObject *Args)
{
   PyErr_WarnEx(PyExc_DeprecationWarning, "Cache.Close() is deprecated, "
                "because it causes segfaults. Delete the Cache instead.", 1);
   PyObject *CacheFilePy = GetOwner<pkgCache*>(Self);
   pkgCacheFile *Cache = GetCpp<pkgCacheFile*>(CacheFilePy);
   Cache->Close();

   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}

static PyObject *PkgCacheOpen(PyObject *Self,PyObject *Args)
{
   PyErr_WarnEx(PyExc_DeprecationWarning, "Cache.Open() is deprecated, "
                "because it causes memory leaks. Create a new Cache instead.",
                1);
   PyObject *CacheFilePy = GetOwner<pkgCache*>(Self);
   pkgCacheFile *Cache = GetCpp<pkgCacheFile*>(CacheFilePy);

   PyObject *pyCallbackInst = 0;
   if (PyArg_ParseTuple(Args, "|O", &pyCallbackInst) == 0)
      return 0;

   if(pyCallbackInst != 0) {
      PyOpProgress progress;
      progress.setCallbackInst(pyCallbackInst);
      if (Cache->Open(progress,false) == false)
	 return HandleErrors();
   }  else {
      OpTextProgress Prog;
      if (Cache->Open(Prog,false) == false)
	 return HandleErrors();
   }

   //std::cout << "new cache is " << (pkgCache*)(*Cache) << std::endl;
   
   // ensure that the states are correct (LP: #659438)
   pkgApplyStatus(*Cache);

   // update the cache pointer after the cache was rebuild
   ((CppPyObject<pkgCache*> *)Self)->Object = (pkgCache*)(*Cache);

   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}
#endif

static PyMethodDef PkgCacheMethods[] =
{
   {"update",PkgCacheUpdate,METH_VARARGS,cache_update_doc},
#ifdef COMPAT_0_7
   {"Open", PkgCacheOpen, METH_VARARGS,
    "Open the cache; deprecated and unsafe"},
   {"Close", PkgCacheClose, METH_VARARGS,"Close the cache"},
#endif
   {}
};

static PyObject *PkgCacheGetGroupCount(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return MkPyNumber(Cache->HeaderP->GroupCount);
}

static PyObject *PkgCacheGetGroups(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return CppPyObject_NEW<GrpListStruct>(Self,&PyGroupList_Type,Cache->GrpBegin());
}

static PyObject *PkgCacheGetPolicy(PyObject *Self, void*) {
   pkgCacheFile *CacheFile = GetCpp<pkgCacheFile *>(Self);
   std::cerr << "policy: " << CacheFile->Policy << std::endl;
   return CppPyObject_NEW<pkgPolicy*>(Self,&PyPolicy_Type,CacheFile->Policy);
}

static PyObject *PkgCacheGetPackages(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return CppPyObject_NEW<PkgListStruct>(Self,&PyPackageList_Type,Cache->PkgBegin());
}

static PyObject *PkgCacheGetPackageCount(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return MkPyNumber((int)Cache->HeaderP->PackageCount);
}

static PyObject *PkgCacheGetVersionCount(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return MkPyNumber(Cache->HeaderP->VersionCount);
}
static PyObject *PkgCacheGetDependsCount(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return MkPyNumber(Cache->HeaderP->DependsCount);
}

static PyObject *PkgCacheGetPackageFileCount(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return MkPyNumber(Cache->HeaderP->PackageFileCount);
}

static PyObject *PkgCacheGetVerFileCount(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return MkPyNumber(Cache->HeaderP->VerFileCount);
}

static PyObject *PkgCacheGetProvidesCount(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   return MkPyNumber(Cache->HeaderP->ProvidesCount);
}

static PyObject *PkgCacheGetFileList(PyObject *Self, void*) {
   pkgCache *Cache = GetCpp<pkgCache *>(Self);
   PyObject *List = PyList_New(0);
   for (pkgCache::PkgFileIterator I = Cache->FileBegin(); I.end() == false; I++)
   {
      PyObject *Obj;
      Obj = CppPyObject_NEW<pkgCache::PkgFileIterator>(Self,&PyPackageFile_Type,I);
      PyList_Append(List,Obj);
      Py_DECREF(Obj);
   }
   return List;
}

static PyObject *PkgCacheGetIsMultiArch(PyObject *Self, void*) {
    pkgCache *Cache = GetCpp<pkgCache *>(Self);
    return PyBool_FromLong(Cache->MultiArchCache());
} 

static PyGetSetDef PkgCacheGetSet[] = {
   {"depends_count",PkgCacheGetDependsCount,0,
    "The number of apt_pkg.Dependency objects stored in the cache."},
   {"file_list",PkgCacheGetFileList,0,
    "A list of apt_pkg.PackageFile objects stored in the cache."},
   {"group_count",PkgCacheGetGroupCount,0,
    "The number of apt_pkg.Group objects stored in the cache."},
   {"groups",  PkgCacheGetGroups, 0, "A list of Group objects in the cache"},
   {"policy",  PkgCacheGetPolicy, 0, "The PkgPolicy for the cache"},
   {"is_multi_arch", PkgCacheGetIsMultiArch, 0,
    "Whether the cache supports multi-arch."},
   {"package_count",PkgCacheGetPackageCount,0,
    "The number of apt_pkg.Package objects stored in the cache."},
   {"package_file_count",PkgCacheGetPackageFileCount,0,
    "The number of apt_pkg.PackageFile objects stored in the cache."},
   {"packages",PkgCacheGetPackages,0,
    "A list of apt_pkg.Package objects stored in the cache."},
   {"provides_count",PkgCacheGetProvidesCount,0,
    "Number of Provides relations described in the cache."},
   {"ver_file_count",PkgCacheGetVerFileCount,0,
    "The number of (Version, PackageFile) relations."},
   {"version_count",PkgCacheGetVersionCount,0,
    "The number of apt_pkg.Version objects stored in the cache."},
   {}
};

// Helper to call FindPkg(name) or FindPkg(name, architecture)
static pkgCache::PkgIterator CacheFindPkg(PyObject *self, PyObject *arg)
{
    const char *name;
    const char *architecture;
    pkgCache *cache = GetCpp<pkgCache *>(self);

    name = PyObject_AsString(arg);

    if (name != NULL)
        return cache->FindPkg(name);

    PyErr_Clear();

    if (PyArg_ParseTuple(arg, "ss", &name, &architecture) == 0) {
        PyErr_Clear();
        PyErr_Format(PyExc_TypeError, "Expected a string or a pair of strings");
        return pkgCache::PkgIterator();
    }

    return cache->FindPkg(name, architecture);
}

// Map access, operator []
static PyObject *CacheMapOp(PyObject *Self,PyObject *Arg)
{
   pkgCache::PkgIterator Pkg = CacheFindPkg(Self, Arg);
   if (Pkg.end() == true)
   {
      if (!PyErr_Occurred())
        PyErr_SetObject(PyExc_KeyError,Arg);
      return 0;
   }

   return CppPyObject_NEW<pkgCache::PkgIterator>(Self,&PyPackage_Type,Pkg);
}

// Check whether the cache contains a package with a given name.
static int CacheContains(PyObject *Self,PyObject *Arg)
{
   bool res = (CacheFindPkg(Self, Arg).end() == false);
   PyErr_Clear();
   return res;
}

static PyObject *PkgCacheNew(PyTypeObject *type,PyObject *Args,PyObject *kwds)
{
   PyObject *pyCallbackInst = 0;
   char *kwlist[] = {"progress", 0};

   if (PyArg_ParseTupleAndKeywords(Args, kwds, "|O", kwlist,
                                   &pyCallbackInst) == 0)
      return 0;

    if (_system == 0) {
        PyErr_SetString(PyExc_ValueError,"_system not initialized");
        return 0;
    }

   pkgCacheFile *Cache = new pkgCacheFile();

   if (pyCallbackInst == Py_None) {
      OpProgress Prog;
      if (Cache->Open(Prog,false) == false)
	     return HandleErrors();
   } else if(pyCallbackInst != 0) {
      // sanity check for the progress object, see #497049
      if (PyObject_HasAttrString(pyCallbackInst, "done") != true) {
        PyErr_SetString(PyExc_ValueError,
                        "OpProgress object must implement done()");
        return 0;
      }
      if (PyObject_HasAttrString(pyCallbackInst, "update") != true) {
        PyErr_SetString(PyExc_ValueError,
                        "OpProgress object must implement update()");
        return 0;
      }
      PyOpProgress progress;
      progress.setCallbackInst(pyCallbackInst);
      if (Cache->Open(progress,false) == false)
         return HandleErrors();
   }
   else {
      OpTextProgress Prog;
      if (Cache->Open(Prog,false) == false)
	     return HandleErrors();
   }

   // ensure that the states are correct (LP: #659438)
   pkgApplyStatus(*Cache);

   CppPyObject<pkgCacheFile*> *CacheFileObj =
	   CppPyObject_NEW<pkgCacheFile*>(0,&PyCacheFile_Type, Cache);

   CppPyObject<pkgCache *> *CacheObj =
	   CppPyObject_NEW<pkgCache *>(CacheFileObj,type,
					    (pkgCache *)(*Cache));

   // Do not delete the pointer to the pkgCache, it is managed by pkgCacheFile.
   CacheObj->NoDelete = true;
   Py_DECREF(CacheFileObj);
   return CacheObj;
}

static Py_ssize_t CacheMapLen(PyObject *Self)
{
    return GetCpp<pkgCache*>(Self)->HeaderP->PackageCount;
}

static char *doc_PkgCache = "Cache([progress]) -> Cache() object.\n\n"
    "The APT cache file contains a hash table mapping names of binary\n"
    "packages to their metadata. A Cache object is the in-core\n"
    "representation of the same. It provides access to APT’s idea of the\n"
    "list of available packages.\n"
    "The optional parameter *progress* can be used to specify an \n"
    "apt.progress.base.OpProgress() object (or similar) which reports\n"
    "progress information while the cache is being opened.  If this\n"
    "parameter is not supplied, the progress will be reported in simple,\n"
    "human-readable text to standard output. If it is None, no output\n"
    "will be made.\n\n"
    "The cache can be used like a mapping from package names to Package\n"
    "objects (although only getting items is supported). Instead of a name,\n"
    "a tuple of a name and an architecture may be used.";
static PySequenceMethods CacheSeq = {0,0,0,0,0,0,0,CacheContains,0,0};
static PyMappingMethods CacheMap = {CacheMapLen,CacheMapOp,0};
PyTypeObject PyCache_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.Cache",                     // tp_name
   sizeof(CppPyObject<pkgCache *>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDeallocPtr<pkgCache *>,      // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   0,                                   // tp_repr
   0,                                   // tp_as_number
   &CacheSeq,                           // tp_as_sequence
   &CacheMap,		                // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   (Py_TPFLAGS_DEFAULT |                // tp_flags
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_GC),
   doc_PkgCache,                        // tp_doc
   CppTraverse<pkgCache *>,        // tp_traverse
   CppClear<pkgCache *>,           // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   PkgCacheMethods,                     // tp_methods
   0,                                   // tp_members
   PkgCacheGetSet,                      // tp_getset
   0,                                   // tp_base
   0,                                   // tp_dict
   0,                                   // tp_descr_get
   0,                                   // tp_descr_set
   0,                                   // tp_dictoffset
   0,                                   // tp_init
   0,                                   // tp_alloc
   PkgCacheNew,                         // tp_new
};
									/*}}}*/
// PkgCacheFile Class							/*{{{*/
// ---------------------------------------------------------------------
PyTypeObject PyCacheFile_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "pkgCacheFile",                      // tp_name
   sizeof(CppPyObject<pkgCacheFile*>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDeallocPtr<pkgCacheFile*>,       // tp_dealloc
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
   Py_TPFLAGS_DEFAULT,                  // tp_flags
};

// Package List Class							/*{{{*/
// ---------------------------------------------------------------------
static Py_ssize_t PkgListLen(PyObject *Self)
{
   return GetCpp<PkgListStruct>(Self).Iter.Cache()->HeaderP->PackageCount;
}

static PyObject *PkgListItem(PyObject *iSelf,Py_ssize_t Index)
{
   PkgListStruct &Self = GetCpp<PkgListStruct>(iSelf);

   if (!Self.move(Index))
      return 0;
   return CppPyObject_NEW<pkgCache::PkgIterator>(GetOwner<PkgListStruct>(iSelf),&PyPackage_Type,
						      Self.Iter);
}

static PySequenceMethods PkgListSeq =
{
   PkgListLen,
   0,                // concat
   0,                // repeat
   PkgListItem,
   0,                // slice
   0,                // assign item
   0                 // assign slice
};

static const char *packagelist_doc =
    "A PackageList is an internally used structure to represent\n"
    "the 'packages' attribute of apt_pkg.Cache objects in a more\n"
    "efficient manner by creating Package objects only when they\n"
    "are accessed.";

PyTypeObject PyPackageList_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.PackageList",               // tp_name
   sizeof(CppPyObject<PkgListStruct>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDealloc<PkgListStruct>,      // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   0,                                   // tp_repr
   0,                                   // tp_as_number
   &PkgListSeq,                         // tp_as_sequence
   0,			                // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   0,                                   // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC, // tp_flags
   packagelist_doc,                     // tp_doc
   CppTraverse<PkgListStruct>,     // tp_traverse
   CppClear<PkgListStruct>,        // tp_clear
};

/* The same for groups */
static Py_ssize_t GrpListLen(PyObject *Self)
{
   return GetCpp<GrpListStruct>(Self).Iter.Cache()->HeaderP->GroupCount;
}

static PyObject *GrpListItem(PyObject *iSelf,Py_ssize_t Index)
{
   GrpListStruct &Self = GetCpp<GrpListStruct>(iSelf);

   if (!Self.move(Index))
      return 0;
   return CppPyObject_NEW<pkgCache::GrpIterator>(GetOwner<GrpListStruct>(iSelf),&PyGroup_Type,
						      Self.Iter);
}

static PySequenceMethods GrpListSeq =
{
   GrpListLen,
   0,                // concat
   0,                // repeat
   GrpListItem,
   0,                // slice
   0,                // assign item
   0                 // assign slice
};

static const char *grouplist_doc =
    "A GroupList is an internally used structure to represent\n"
    "the 'groups' attribute of apt_pkg.Cache objects in a more\n"
    "efficient manner by creating Group objects only when they\n"
    "are accessed.";

PyTypeObject PyGroupList_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.GroupList",               // tp_name
   sizeof(CppPyObject<GrpListStruct>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDealloc<GrpListStruct>,      // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   0,                                   // tp_repr
   0,                                   // tp_as_number
   &GrpListSeq,                         // tp_as_sequence
   0,			                // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   0,                                   // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC, // tp_flags
   grouplist_doc,                     // tp_doc
   CppTraverse<GrpListStruct>,     // tp_traverse
   CppClear<GrpListStruct>,        // tp_clear
};


#define Owner (GetOwner<pkgCache::PkgIterator>(Self))
#define MkGet(PyFunc,Ret) static PyObject *PyFunc(PyObject *Self,void*) \
{ \
    pkgCache::PkgIterator &Pkg = GetCpp<pkgCache::PkgIterator>(Self); \
    return Ret; \
}

MkGet(PackageGetName,PyString_FromString(Pkg.Name()))
MkGet(PackageGetArch,PyString_FromString(Pkg.Arch()))
MkGet(PackageGetSection,Safe_FromString(Pkg.Section()))
MkGet(PackageGetRevDependsList,CppPyObject_NEW<RDepListStruct>(Owner,
                               &PyDependencyList_Type, Pkg.RevDependsList()))
MkGet(PackageGetProvidesList,CreateProvides(Owner,Pkg.ProvidesList()))
MkGet(PackageGetSelectedState,MkPyNumber(Pkg->SelectedState))
MkGet(PackageGetInstState,MkPyNumber(Pkg->InstState))
MkGet(PackageGetCurrentState,MkPyNumber(Pkg->CurrentState))
MkGet(PackageGetID,MkPyNumber(Pkg->ID))
#
MkGet(PackageGetAuto,PyBool_FromLong((Pkg->Flags & pkgCache::Flag::Auto) != 0))
MkGet(PackageGetEssential,PyBool_FromLong((Pkg->Flags & pkgCache::Flag::Essential) != 0))
MkGet(PackageGetImportant,PyBool_FromLong((Pkg->Flags & pkgCache::Flag::Important) != 0))
#undef MkGet
#undef Owner

static const char PackageGetFullName_doc[] =
    "get_fullname([pretty: bool = False]) -> str\n\n"
    "Get the full name of the package, including the architecture. If\n"
    "'pretty' is True, the architecture is omitted for native packages,\n"
    "that is, and amd64 apt package on an amd64 system would give 'apt'.";
static PyObject *PackageGetFullName(PyObject *Self,PyObject *Args,PyObject *kwds)
{
   pkgCache::PkgIterator &Pkg = GetCpp<pkgCache::PkgIterator>(Self);
   char pretty = 0;
   char *kwlist[] = {"pretty", 0};

   if (PyArg_ParseTupleAndKeywords(Args, kwds, "|b", kwlist,
                                   &pretty) == 0)
      return 0;

   
   return CppPyString(Pkg.FullName(pretty));
}

static PyObject *PackageGetVersionList(PyObject *Self,void*)
{
   pkgCache::PkgIterator &Pkg = GetCpp<pkgCache::PkgIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::PkgIterator>(Self);

   PyObject *List = PyList_New(0);
   for (pkgCache::VerIterator I = Pkg.VersionList(); I.end() == false; I++)
   {
      PyObject *Obj;
      Obj = CppPyObject_NEW<pkgCache::VerIterator>(Owner,&PyVersion_Type,I);
      PyList_Append(List,Obj);
      Py_DECREF(Obj);
   }
   return List;
}

static PyObject *PackageGetHasVersions(PyObject *Self,void*)
{
   pkgCache::PkgIterator &Pkg = GetCpp<pkgCache::PkgIterator>(Self);
   return PyBool_FromLong(Pkg.VersionList().end() == false);
}

static PyObject *PackageGetHasProvides(PyObject *Self,void*)
{
   pkgCache::PkgIterator &Pkg = GetCpp<pkgCache::PkgIterator>(Self);
   return PyBool_FromLong(Pkg.ProvidesList().end() == false);
}

static PyObject *PackageGetCurrentVer(PyObject *Self,void*)
{
   pkgCache::PkgIterator &Pkg = GetCpp<pkgCache::PkgIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::PkgIterator>(Self);
   if (Pkg->CurrentVer == 0)
   {
      Py_INCREF(Py_None);
      return Py_None;
   }
   return CppPyObject_NEW<pkgCache::VerIterator>(Owner,&PyVersion_Type,
							 Pkg.CurrentVer());
}


static PyMethodDef PackageMethods[] =
{
   {"get_fullname",(PyCFunction)PackageGetFullName,METH_VARARGS|METH_KEYWORDS,
    PackageGetFullName_doc},
   {}
};

static PyGetSetDef PackageGetSet[] = {
    {"name",PackageGetName,0,
     "The name of the package."},
    {"architecture",PackageGetArch,0, "The architecture of the package."},
    {"section",PackageGetSection,0,
     "The section of the package."},
    {"rev_depends_list",PackageGetRevDependsList,0,
     "An apt_pkg.DependencyList object of all reverse dependencies."},
    {"provides_list",PackageGetProvidesList,0,
     "A list of all packages providing this package. The list contains\n"
     "tuples in the format (providesname, providesver, version)\n"
     "where 'version' is an apt_pkg.Version object."},
    {"selected_state",PackageGetSelectedState,0,
     "The state of the selection, which can be compared against the constants\n"
     "SELSTATE_DEINSTALL, SELSTATE_HOLD, SELSTATE_INSTALL, SELSTATE_PURGE,\n"
     "SELSTATE_UNKNOWN of the apt_pkg module."},
    {"inst_state",PackageGetInstState,0,
     "The state of the install, which be compared against the constants\n"
     "INSTSTATE_HOLD, INSTSTATE_HOLD_REINSTREQ, INSTSTATE_OK,\n"
     "INSTSTATE_REINSTREQ of the apt_pkg module."},
    {"current_state",PackageGetCurrentState,0,
     "The current state, which can be compared against the constants\n"
     "CURSTATE_CONFIG_FILES, CURSTATE_HALF_CONFIGURED,\n"
     "CURSTATE_HALF_INSTALLED, CURSTATE_INSTALLED, CURSTATE_NOT_INSTALLED,\n"
     "CURSTATE_UNPACKED of the apt_pkg module."},
    {"id",PackageGetID,0,
     "The numeric ID of the package"},
    {"auto",PackageGetAuto,0,
     "Ignore it, it does nothing. You want to use\n"
     "DepCache.is_auto_installed instead."},
    {"essential",PackageGetEssential,0,
     "Boolean value determining whether the package is essential."},
    {"important",PackageGetImportant,0,
     "Boolean value determining whether the package has the 'important'\n"
     "flag set ('Important: yes' in the Packages file). No longer used."},
    {"version_list",PackageGetVersionList,0,
     "A list of all apt_pkg.Version objects for this package."},
    {"current_ver",PackageGetCurrentVer,0,
     "The version of the package currently installed or None."},
    {"has_versions",PackageGetHasVersions,0,
     "Whether the package has at least one version in the cache."},
    {"has_provides",PackageGetHasProvides,0,
     "Whether the package is provided by at least one other package."},
    {}
};

static PyObject *PackageRepr(PyObject *Self)
{
   pkgCache::PkgIterator &Pkg = GetCpp<pkgCache::PkgIterator>(Self);

   return PyString_FromFormat("<%s object: name:'%s' section: "
                              "'%s' id:%u>", Self->ob_type->tp_name,
                              Pkg.Name(), (Pkg.Section() ? Pkg.Section() : ""),
                              Pkg->ID);
}

static const char *package_doc =
    "Represent a package. A package is uniquely identified by its name\n"
    "and each package can have zero or more versions which can be\n"
    "accessed via the version_list property. Packages can be installed\n"
    "and removed by apt_pkg.DepCache.";

PyTypeObject PyPackage_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.Package",                 // tp_name
   sizeof(CppPyObject<pkgCache::PkgIterator>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDealloc<pkgCache::PkgIterator>,  // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   PackageRepr,                         // tp_repr
   0,                                   // tp_as_number
   0,                                   // tp_as_sequence
   0,			                // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC, // tp_flags
   package_doc,                         // tp_doc
   CppTraverse<pkgCache::PkgIterator>, // tp_traverse
   CppClear<pkgCache::PkgIterator>,// tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   PackageMethods,                      // tp_methods
   0,                                   // tp_members
   PackageGetSet,                       // tp_getset
};

#define Description_MkGet(PyFunc,Ret) static PyObject \
   *PyFunc(PyObject *Self,void*) { \
       pkgCache::DescIterator &Desc = GetCpp<pkgCache::DescIterator>(Self); \
       return Ret; }

Description_MkGet(DescriptionGetLanguageCode,
                  PyString_FromString(Desc.LanguageCode()))
Description_MkGet(DescriptionGetMd5,Safe_FromString(Desc.md5()))
#undef Description_MkGet

static PyObject *DescriptionGetFileList(PyObject *Self,void*)
{
   pkgCache::DescIterator &Desc = GetCpp<pkgCache::DescIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::DescIterator>(Self);

   /* The second value in the tuple is the index of the VF item. If the
      user wants to request a lookup then that number will be used.
      Maybe later it can become an object. */
   PyObject *List = PyList_New(0);
   for (pkgCache::DescFileIterator I = Desc.FileList(); I.end() == false; I++)
   {
      PyObject *DescFile;
      PyObject *Obj;
      DescFile = CppPyObject_NEW<pkgCache::PkgFileIterator>(Owner,&PyPackageFile_Type,I.File());
      Obj = Py_BuildValue("NN",DescFile,MkPyNumber(I.Index()));
      PyList_Append(List,Obj);
      Py_DECREF(Obj);
   }
   return List;
}

static PyGetSetDef DescriptionGetSet[] = {
    {"language_code",DescriptionGetLanguageCode,0,
     "The language code of the description. Empty string for untranslated\n"
     "descriptions."},
    {"md5",DescriptionGetMd5,0,
     "The MD5 hash of the description."},
    {"file_list",DescriptionGetFileList,0,
     "A list of all apt_pkg.PackageFile objects related to this description."},
    {}
};

static PyObject *DescriptionRepr(PyObject *Self)
{
   pkgCache::DescIterator &Desc = GetCpp<pkgCache::DescIterator>(Self);
   return PyString_FromFormat("<%s object: language_code:'%s' md5:'%s' ",
                              Self->ob_type->tp_name, Desc.LanguageCode(),
                              Desc.md5());
}

static const char *description_doc =
    "Represent a package description and some attributes. Needed for\n"
    "things like translated descriptions.";

PyTypeObject PyDescription_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.Description",               // tp_name
   sizeof(CppPyObject<pkgCache::DescIterator>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDealloc<pkgCache::DescIterator>,          // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   DescriptionRepr,                         // tp_repr
   0,                                   // tp_as_number
   0,		                        // tp_as_sequence
   0,			                // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC, // tp_flags
   description_doc,                     // tp_doc
   CppTraverse<pkgCache::DescIterator>, // tp_traverse
   CppClear<pkgCache::DescIterator>,// tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   0,                                   // tp_methods
   0,                                   // tp_members
   DescriptionGetSet,                   // tp_getset
};
									/*}}}*/
// Version Class							/*{{{*/
// ---------------------------------------------------------------------

/* This is the simple depends result, the elements are split like
   ParseDepends does */
static PyObject *MakeDepends(PyObject *Owner,pkgCache::VerIterator &Ver,
			     bool AsObj)
{
   PyObject *Dict = PyDict_New();
   PyObject *LastDep = 0;
   unsigned LastDepType = 0;
   for (pkgCache::DepIterator D = Ver.DependsList(); D.end() == false;)
   {
      pkgCache::DepIterator Start;
      pkgCache::DepIterator End;
      D.GlobOr(Start,End);

      // Switch/create a new dict entry
      if (LastDepType != Start->Type || LastDep != 0)
      {
	 PyObject *Dep = PyString_FromString(UntranslatedDepTypes[Start->Type]);
	 LastDepType = Start->Type;
	 LastDep = PyDict_GetItem(Dict,Dep);
	 if (LastDep == 0)
	 {
	    LastDep = PyList_New(0);
	    PyDict_SetItem(Dict,Dep,LastDep);
	    Py_DECREF(LastDep);
	 }
	 Py_DECREF(Dep);
      }

      PyObject *OrGroup = PyList_New(0);
      while (1)
      {
	 PyObject *Obj;
	 if (AsObj == true)
	    Obj = CppPyObject_NEW<pkgCache::DepIterator>(Owner,&PyDependency_Type,
							 Start);
	 else
	 {
	    if (Start->Version == 0)
	       Obj = Py_BuildValue("sss",
				   Start.TargetPkg().Name(),
				   "",
				   Start.CompType());
	    else
	       Obj = Py_BuildValue("sss",
				   Start.TargetPkg().Name(),
				   Start.TargetVer(),
				   Start.CompType());
	 }
	 PyList_Append(OrGroup,Obj);
	 Py_DECREF(Obj);

	 if (Start == End)
	    break;
	 Start++;
      }

      PyList_Append(LastDep,OrGroup);
      Py_DECREF(OrGroup);
   }

   return Dict;
}

static inline pkgCache::VerIterator Version_GetVer(PyObject *Self) {
   return GetCpp<pkgCache::VerIterator>(Self);
}

// Version attributes.
static PyObject *VersionGetVerStr(PyObject *Self, void*) {
   return PyString_FromString(Version_GetVer(Self).VerStr());
}
static PyObject *VersionGetSection(PyObject *Self, void*) {
   return Safe_FromString(Version_GetVer(Self).Section());
}
static PyObject *VersionGetArch(PyObject *Self, void*) {
   return Safe_FromString(Version_GetVer(Self).Arch());
}
static PyObject *VersionGetFileList(PyObject *Self, void*) {
   pkgCache::VerIterator &Ver = GetCpp<pkgCache::VerIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::VerIterator>(Self);
   PyObject *List = PyList_New(0);
   for (pkgCache::VerFileIterator I = Ver.FileList(); I.end() == false; I++)
   {
      PyObject *PkgFile;
      PyObject *Obj;
      PkgFile = CppPyObject_NEW<pkgCache::PkgFileIterator>(Owner,&PyPackageFile_Type,I.File());
      Obj = Py_BuildValue("NN",PkgFile,MkPyNumber(I.Index()));
      PyList_Append(List,Obj);
      Py_DECREF(Obj);
   }
   return List;
}

static PyObject *VersionGetDependsListStr(PyObject *Self, void*) {
   pkgCache::VerIterator &Ver = GetCpp<pkgCache::VerIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::VerIterator>(Self);
   return MakeDepends(Owner,Ver,false);
}
static PyObject *VersionGetDependsList(PyObject *Self, void*) {
   pkgCache::VerIterator &Ver = GetCpp<pkgCache::VerIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::VerIterator>(Self);
   return MakeDepends(Owner,Ver,true);
}
static PyObject *VersionGetParentPkg(PyObject *Self, void*) {
   PyObject *Owner = GetOwner<pkgCache::VerIterator>(Self);
   return CppPyObject_NEW<pkgCache::PkgIterator>(Owner,&PyPackage_Type,
                                                      Version_GetVer(Self).ParentPkg());
}
static PyObject *VersionGetProvidesList(PyObject *Self, void*) {
   PyObject *Owner = GetOwner<pkgCache::VerIterator>(Self);
   return CreateProvides(Owner,Version_GetVer(Self).ProvidesList());
}
static PyObject *VersionGetSize(PyObject *Self, void*) {
   return MkPyNumber(Version_GetVer(Self)->Size);
}
static PyObject *VersionGetInstalledSize(PyObject *Self, void*) {
   return MkPyNumber(Version_GetVer(Self)->InstalledSize);
}
static PyObject *VersionGetHash(PyObject *Self, void*) {
   return MkPyNumber(Version_GetVer(Self)->Hash);
}
static PyObject *VersionGetID(PyObject *Self, void*) {
   return MkPyNumber(Version_GetVer(Self)->ID);
}
static PyObject *VersionGetPriority(PyObject *Self, void*) {
   return MkPyNumber(Version_GetVer(Self)->Priority);
}
static PyObject *VersionGetPriorityStr(PyObject *Self, void*) {
   return Safe_FromString(Version_GetVer(Self).PriorityType());
}
static PyObject *VersionGetDownloadable(PyObject *Self, void*) {
   return PyBool_FromLong(Version_GetVer(Self).Downloadable());
}
static PyObject *VersionGetTranslatedDescription(PyObject *Self, void*) {
   pkgCache::VerIterator &Ver = GetCpp<pkgCache::VerIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::VerIterator>(Self);
   return CppPyObject_NEW<pkgCache::DescIterator>(Owner,
                    &PyDescription_Type,
                    Ver.TranslatedDescription());
}

static PyObject *VersionGetMultiArch(PyObject *Self, void*)
{
	return MkPyNumber(Version_GetVer(Self)->MultiArch);
}

#if 0 // FIXME: enable once pkgSourceList is stored somewhere
static PyObject *VersionGetIsTrusted(PyObject *Self, void*) {
   else if (strcmp("IsTrusted", Name) == 0)
   {
      pkgSourceList Sources;
      Sources.ReadMainList();
      for(pkgCache::VerFileIterator i = Ver.FileList(); !i.end(); i++)
      {
	 pkgIndexFile *index;
	 if(Sources.FindIndex(i.File(), index) && !index->IsTrusted())
	    Py_RETURN_FALSE;
      }
      Py_RETURN_TRUE;
   }
}
#endif

#define NOTNULL(x) (x ? x : "")

static PyObject *VersionRepr(PyObject *Self)
{
   pkgCache::VerIterator &Ver = GetCpp<pkgCache::VerIterator>(Self);
   return PyString_FromFormat("<%s object: Pkg:'%s' Ver:'%s' Section:'%s' "
                              " Arch:'%s' Size:%lu ISize:%lu Hash:%u ID:%u "
                              "Priority:%u>", Self->ob_type->tp_name,
                              Ver.ParentPkg().Name(), Ver.VerStr(),
                              NOTNULL(Ver.Section()), NOTNULL(Ver.Arch()),
                              (unsigned long)Ver->Size,
                              (unsigned long)Ver->InstalledSize,
	                          Ver->Hash, Ver->ID, Ver->Priority);
}
#undef NOTNULL

static PyObject *version_richcompare(PyObject *obj1, PyObject *obj2, int op)
{
    if (!PyVersion_Check(obj2))
        return Py_INCREF(Py_NotImplemented), Py_NotImplemented;

    const pkgCache::VerIterator &a = GetCpp<pkgCache::VerIterator>(obj1);
    const pkgCache::VerIterator &b = GetCpp<pkgCache::VerIterator>(obj2);
    const int comparison = _system->VS->CmpVersion(a.VerStr(), b.VerStr());
    switch (op) {
        case Py_LT: return PyBool_FromLong(comparison < 0);
        case Py_LE: return PyBool_FromLong(comparison <= 0);
        case Py_EQ: return PyBool_FromLong(comparison == 0);
        case Py_NE: return PyBool_FromLong(comparison != 0);
        case Py_GE: return PyBool_FromLong(comparison >= 0);
        case Py_GT: return PyBool_FromLong(comparison > 0);
        default: return NULL; // should not happen.
    }
}

static PyGetSetDef VersionGetSet[] = {
   {"arch",VersionGetArch,0,
    "The architecture of this specific version of the package."},
   {"depends_list",VersionGetDependsList,0,
    "A dictionary mapping dependency types to lists (A) of lists (B) of\n"
    "apt_pkg.Dependency objects. The lists (B) represent or dependencies\n"
    "like 'a || b'."},
   {"depends_list_str",VersionGetDependsListStr,0,
    "Same as depends_list, except that the apt_pkg.Dependency objects\n"
    "are 3-tuples of the form (name, version, operator); where operator\n"
    "is one of '<', '<=', '=', '>=', '>'."},
   {"downloadable",VersionGetDownloadable,0,
    "Whether the version can be downloaded."},
   {"file_list",VersionGetFileList,0,
    "A list of tuples (packagefile: apt_pkg.PackageFile, index: int) for the\n"
    "PackageFile objects related to this package. The index can be used\n"
    "to retrieve the record of this package version."},
   {"hash",VersionGetHash,0,
    "The numeric hash of the version used in the internal storage."},
   {"id",VersionGetID,0,
    "The numeric ID of the package."},
   {"installed_size",VersionGetInstalledSize,0,
    "The installed size of this package version."},
   {"multi_arch",VersionGetMultiArch,0,
    "Multi-arch state of this package, as an integer. See\n"
    "the various MULTI_ARCH_* members."},
   {"parent_pkg",VersionGetParentPkg,0,
    "The parent package of this version."},
   {"priority",VersionGetPriority,0,
    "The priority of the package as an integer, which can be compared to\n"
    "the constants PRI_EXTRA, PRI_IMPORTANT, PRI_OPTIONAL, PRI_REQUIRED,\n"
    "PRI_STANDARD of the apt_pkg module."},
   {"priority_str",VersionGetPriorityStr,0,
    "The priority of the package, as a string."},
   {"provides_list",VersionGetProvidesList,0,
    "A list of all packages provided by this version. The list contains\n"
    "tuples in the format (providesname, providesver, version)\n"
    "where 'version' is an apt_pkg.Version object."},
   {"section",VersionGetSection,0,
    "The section of this package version."},
   {"size",VersionGetSize,0,
    "The size of the package file."},
   {"translated_description",VersionGetTranslatedDescription,0,
    "An apt_pkg.Description object for the translated description if\n"
    "available or the untranslated fallback."},
   {"ver_str",VersionGetVerStr,0,
    "The version string."},
   {}
};

PyTypeObject PyVersion_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.Version",                   // tp_name
   sizeof(CppPyObject<pkgCache::VerIterator>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDealloc<pkgCache::VerIterator>,          // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   VersionRepr,                         // tp_repr
   0,                                   // tp_as_number
   0,		                        // tp_as_sequence
   0,			                // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,                  // tp_flags
   "Version Object",                    // tp_doc
   CppTraverse<pkgCache::VerIterator>, // tp_traverse
   CppClear<pkgCache::VerIterator>,// tp_clear
   version_richcompare,                 // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   0,                                   // tp_methods
   0,                                   // tp_members
   VersionGetSet,                       // tp_getset
};

									/*}}}*/

// PackageFile Class							/*{{{*/
// ---------------------------------------------------------------------
static PyObject *PackageFile_GetFileName(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.FileName());
}

static PyObject *PackageFile_GetArchive(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.Archive());
}

static PyObject *PackageFile_GetComponent(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.Component());
}

static PyObject *PackageFile_GetVersion(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.Version());
}

static PyObject *PackageFile_GetOrigin(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.Origin());
}

static PyObject *PackageFile_GetLabel(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.Label());
}

static PyObject *PackageFile_GetArchitecture(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.Architecture());
}

static PyObject *PackageFile_GetSite(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.Site());
}

static PyObject *PackageFile_GetIndexType(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return Safe_FromString(File.IndexType());
}
static PyObject *PackageFile_GetSize(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return MkPyNumber(File->Size);
}

static PyObject *PackageFile_GetNotSource(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return PyBool_FromLong((File->Flags & pkgCache::Flag::NotSource) != 0);
}
static PyObject *PackageFile_GetNotAutomatic(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return PyBool_FromLong((File->Flags & pkgCache::Flag::NotAutomatic) != 0);
}

static PyObject *PackageFile_GetID(PyObject *Self,void*)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);
    return MkPyNumber(File->ID);
}

#define S(s) (s == NULL ? "" : s)
static PyObject *PackageFileRepr(PyObject *Self)
{
    pkgCache::PkgFileIterator &File = GetCpp<pkgCache::PkgFileIterator>(Self);

    return PyString_FromFormat("<%s object: filename:'%s'"
                               "  a=%s,c=%s,v=%s,o=%s,l=%s arch='%s' site='%s'"
                               " IndexType='%s' Size=%lu ID:%u>",
                               Self->ob_type->tp_name, File.FileName(),
                               S(File.Archive()),
                               S(File.Component()),S(File.Version()),
                               S(File.Origin()),S(File.Label()),
                               S(File.Architecture()),S(File.Site()),
                               S(File.IndexType()),File->Size,File->ID);
}
#undef S

static PyGetSetDef PackageFileGetSet[] = {
  {"architecture",PackageFile_GetArchitecture,0,
   "The architecture of the package file. Unused, empty string nowadays."},
  {"archive",PackageFile_GetArchive,0,
   "The archive of the package file (i.e. 'Suite' in the Release file)."},
  {"component",PackageFile_GetComponent,0,
   "The component of this package file (e.g. 'main')."},
  {"filename",PackageFile_GetFileName,0,
   "The path to the file."},
  {"id",PackageFile_GetID,0,
   "The numeric ID of this PackageFile object."},
  {"index_type",PackageFile_GetIndexType,0,
   "A string describing the type of index. Known values are\n"
   "'Debian Package Index', 'Debian Translation Index', and\n"
   "'Debian dpkg status file'."},
  {"label",PackageFile_GetLabel,0,
   "The label set in the release file (e.g. 'Debian')."},
  {"not_automatic",PackageFile_GetNotAutomatic,0,
   "Whether the NotAutomatic flag is set in the Release file."},
  {"not_source",PackageFile_GetNotSource,0,
   "Whether this package file lacks an active (sources.list) source;"
   "packages listed in such a file cannot be downloaded."},
  {"origin",PackageFile_GetOrigin,0,
   "The origin set in the release file."},
  {"site",PackageFile_GetSite,0,
   "The hostname of the location this file comes from."},
  {"size",PackageFile_GetSize,0,
   "The size of the file."},
  {"version",PackageFile_GetVersion,0,
   "The version set in the release file (e.g. '5.0.X' for lenny, where X\n"
   "is a point release)."},
  {}
};

static const char *packagefile_doc =
    "A package file is an index file stored in the cache with some\n"
    "additional pieces of information.";

PyTypeObject PyPackageFile_Type = {
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.PackageFile",                                // tp_name
   sizeof(CppPyObject<pkgCache::PkgFileIterator>),  // tp_basicsize
   0,                                                    // tp_itemsize
   CppDealloc<pkgCache::PkgFileIterator>,           // tp_dealloc
   0,                                                    // tp_print
   0,                                                    // tp_getattr
   0,                                                    // tp_setattr
   0,                                                    // tp_compare
   PackageFileRepr,                                      // tp_repr
   0,                                                    // tp_as_number
   0,                                                    // tp_as_sequence
   0,                                                    // tp_as_mapping
   0,                                                    // tp_hash
   0,                                                    // tp_call
   0,                                                    // tp_str
   _PyAptObject_getattro,                                // tp_getattro
   0,                                                    // tp_setattro
   0,                                                    // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,              // tp_flags
   packagefile_doc,                                      // tp_doc
   CppTraverse<pkgCache::PkgFileIterator>,          // tp_traverse
   CppClear<pkgCache::PkgFileIterator>,             // tp_clear
   0,                                                    // tp_richcompare
   0,                                                    // tp_weaklistoffset
   0,                                                    // tp_iter
   0,                                                    // tp_iternext
   0,                                                    // tp_methods
   0,                                                    // tp_members
   PackageFileGetSet,                                    // tp_getset
};

// depends class
static PyObject *DependencyRepr(PyObject *Self)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);

   return PyString_FromFormat("<%s object: pkg:'%s' ver:'%s' comp:'%s'>",
	                          Self->ob_type->tp_name, Dep.TargetPkg().Name(),
	                          (Dep.TargetVer() == 0 ? "" : Dep.TargetVer()),
	                          Dep.CompType());
}

static PyObject *DepSmartTargetPkg(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::DepIterator>(Self);

   pkgCache::PkgIterator P;
   if (Dep.SmartTargetPkg(P) == false)
   {
      Py_INCREF(Py_None);
      return Py_None;
   }

   return CppPyObject_NEW<pkgCache::PkgIterator>(Owner,&PyPackage_Type,P);
}

static PyObject *DepAllTargets(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::DepIterator>(Self);

   SPtr<pkgCache::Version *> Vers = Dep.AllTargets();
   PyObject *List = PyList_New(0);
   for (pkgCache::Version **I = Vers; *I != 0; I++)
   {
      PyObject *Obj;
      Obj = CppPyObject_NEW<pkgCache::VerIterator>(Owner,&PyVersion_Type,
							pkgCache::VerIterator(*Dep.Cache(),*I));
      PyList_Append(List,Obj);
      Py_DECREF(Obj);
   }
   return List;
}

static PyMethodDef DependencyMethods[] =
{
   {"smart_target_pkg",DepSmartTargetPkg,METH_VARARGS,
    "smart_target_pkg() -> apt_pkg.Package\n\n"
    "Return the first package which provides a package with the name\n"
    "of the target package."},
   {"all_targets",DepAllTargets,METH_VARARGS,
    "all_targets() -> list\n\n"
    "A list of all apt_pkg.Version objects satisfying the dependency."},
   {}
};

// Dependency Class							/*{{{*/
// ---------------------------------------------------------------------

static PyObject *DependencyGetTargetVer(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   if (Dep->Version == 0)
      return PyString_FromString("");
   return PyString_FromString(Dep.TargetVer());
}

static PyObject *DependencyGetTargetPkg(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::DepIterator>(Self);
   return CppPyObject_NEW<pkgCache::PkgIterator>(Owner,&PyPackage_Type,
                                                      Dep.TargetPkg());
}

static PyObject *DependencyGetParentVer(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::DepIterator>(Self);
   return CppPyObject_NEW<pkgCache::VerIterator>(Owner,&PyVersion_Type,
                                                      Dep.ParentVer());
}

static PyObject *DependencyGetParentPkg(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   PyObject *Owner = GetOwner<pkgCache::DepIterator>(Self);
   return CppPyObject_NEW<pkgCache::PkgIterator>(Owner,&PyPackage_Type,
                                                      Dep.ParentPkg());
}

static PyObject *DependencyGetCompType(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   return PyString_FromString(Dep.CompType());
}

static PyObject *DependencyGetDepType(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   return PyString_FromString(Dep.DepType());
}

static PyObject *DependencyGetDepTypeUntranslated(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   return PyString_FromString(UntranslatedDepTypes[Dep->Type]);
}

static PyObject *DependencyGetDepTypeEnum(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   return MkPyNumber(Dep->Type);
}

static PyObject *DependencyGetID(PyObject *Self,void*)
{
   pkgCache::DepIterator &Dep = GetCpp<pkgCache::DepIterator>(Self);
   return MkPyNumber(Dep->ID);
}

static PyGetSetDef DependencyGetSet[] = {
   {"comp_type",DependencyGetCompType,0,
    "The type of comparison, as a string (one of '<', '<=', '=', '>=', '>')."},
   {"dep_type",DependencyGetDepType,0,
    "The type of the dependency; may be translated"},
   {"dep_type_untranslated",DependencyGetDepTypeUntranslated,0,
    "Same as dep_type, but guaranteed to be untranslated."},
   {"dep_type_enum",DependencyGetDepTypeEnum,0,
    "Same as dep_type, but with a numeric value instead of a string. Can\n"
    "be compared against the TYPE_ constants defined in this class."},
   {"id",DependencyGetID,0,
    "The numeric ID of this dependency object."},
   {"parent_pkg",DependencyGetParentPkg,0,
    "The apt_pkg.Package object of the package which depends."},
   {"parent_ver",DependencyGetParentVer,0,
    "The apt_pkg.Version object of the package which depends."},
   {"target_pkg",DependencyGetTargetPkg,0,
    "The apt_pkg.Package object of the package depended upon"},
   {"target_ver",DependencyGetTargetVer,0,
    "The version of the package depended upon as a string"},
   {}
};

static const char *dependency_doc =
    "Represent a dependency from one package version to a package,\n"
    "and (optionally) a version relation (e.g. >= 1). Dependency\n"
    "objects also provide useful functions like all_targets() or\n"
    "smart_target_pkg() for selecting packages to satisfy the\n"
    "dependency.";

PyTypeObject PyDependency_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.Dependency",                // tp_name
   sizeof(CppPyObject<pkgCache::DepIterator>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDealloc<pkgCache::DepIterator>,          // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   DependencyRepr,                      // tp_repr
   0,                                   // tp_as_number
   0,		                        // tp_as_sequence
   0,			                // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC, // tp_flags
   dependency_doc,                      // tp_doc
   CppTraverse<pkgCache::DepIterator>, // tp_traverse
   CppClear<pkgCache::DepIterator>, // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   DependencyMethods,                   // tp_methods
   0,                                   // tp_members
   DependencyGetSet,                    // tp_getset
};

									/*}}}*/
									/*}}}*/
// Reverse Dependency List Class					/*{{{*/
// ---------------------------------------------------------------------
static Py_ssize_t RDepListLen(PyObject *Self)
{
   return GetCpp<RDepListStruct>(Self).Len;
}

static PyObject *RDepListItem(PyObject *iSelf,Py_ssize_t Index)
{
   RDepListStruct &Self = GetCpp<RDepListStruct>(iSelf);
   if (Index < 0 || (unsigned)Index >= Self.Len)
   {
      PyErr_SetNone(PyExc_IndexError);
      return 0;
   }

   if ((unsigned)Index < Self.LastIndex)
   {
      Self.LastIndex = 0;
      Self.Iter = Self.Start;
   }

   while ((unsigned)Index > Self.LastIndex)
   {
      Self.LastIndex++;
      Self.Iter++;
      if (Self.Iter.end() == true)
      {
	 PyErr_SetNone(PyExc_IndexError);
	 return 0;
      }
   }

   return CppPyObject_NEW<pkgCache::DepIterator>(GetOwner<RDepListStruct>(iSelf),
						      &PyDependency_Type,Self.Iter);
}

static PySequenceMethods RDepListSeq =
{
   RDepListLen,
   0,                // concat
   0,                // repeat
   RDepListItem,
   0,                // slice
   0,                // assign item
   0                 // assign slice
};

static const char *dependencylist_doc =
    "A simple list-like type for representing multiple dependency\n"
    "objects in an efficient manner; without having to generate\n"
    "all Dependency objects in advance.";
PyTypeObject PyDependencyList_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.DependencyList",             // tp_name
   sizeof(CppPyObject<RDepListStruct>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDealloc<RDepListStruct>,      // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   0,                                   // tp_repr
   0,                                   // tp_as_number
   &RDepListSeq,                         // tp_as_sequence
   0,			                // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   0,                                   // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC, // tp_flags
   dependencylist_doc,             // tp_doc
   CppTraverse<RDepListStruct>,    // tp_traverse
   CppClear<RDepListStruct>,       // tp_clear
};

									/*}}}*/


#ifdef COMPAT_0_7
PyObject *TmpGetCache(PyObject *Self,PyObject *Args)
{
    PyErr_WarnEx(PyExc_DeprecationWarning, "apt_pkg.GetCache() is deprecated. "
                 "Please see apt_pkg.Cache() for the replacement.", 1);
    return PkgCacheNew(&PyCache_Type,Args,0);
}
#endif
