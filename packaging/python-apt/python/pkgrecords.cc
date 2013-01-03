// -*- mode: cpp; mode: fold -*-
// Description								/*{{{*/
// $Id: pkgrecords.cc,v 1.3 2002/02/26 01:36:15 mdz Exp $
/* ######################################################################

   Package Records - Wrapper for the package records functions

   ##################################################################### */
									/*}}}*/
// Include Files							/*{{{*/
#include "generic.h"
#include "apt_pkgmodule.h"
#include "pkgrecords.h"


#include <Python.h>
									/*}}}*/


// PkgRecords Class							/*{{{*/
// ---------------------------------------------------------------------


static PyObject *PkgRecordsLookup(PyObject *Self,PyObject *Args)
{
   PkgRecordsStruct &Struct = GetCpp<PkgRecordsStruct>(Self);

   PyObject *PkgFObj;
   long int Index;
   if (PyArg_ParseTuple(Args,"(O!l)",&PyPackageFile_Type,&PkgFObj,&Index) == 0)
      return 0;

   // Get the index and check to make sure it is reasonable
   pkgCache::PkgFileIterator &PkgF = GetCpp<pkgCache::PkgFileIterator>(PkgFObj);
   pkgCache *Cache = PkgF.Cache();
   if (Cache->DataEnd() <= Cache->VerFileP + Index + 1 ||
       Cache->VerFileP[Index].File != PkgF.Index())
   {
      PyErr_SetNone(PyExc_IndexError);
      return 0;
   }

   // Do the lookup
   Struct.Last = &Struct.Records.Lookup(pkgCache::VerFileIterator(*Cache,Cache->VerFileP+Index));

   // always return true (to make it consistent with the pkgsrcrecords object
   return PyBool_FromLong(1);
}

static PyMethodDef PkgRecordsMethods[] =
{
   {"lookup",PkgRecordsLookup,METH_VARARGS,
    "lookup((packagefile: apt_pkg.PackageFile, index: int)) -> bool\n\n"
    "Changes to a new package"},
   {}
};

/**
 * Get the PkgSrcRecordsStruct from a PyObject. If no package has been looked
 * up, set an AttributeError using the given name.
 */
static inline PkgRecordsStruct &GetStruct(PyObject *Self,char *name) {
   PkgRecordsStruct &Struct = GetCpp<PkgRecordsStruct>(Self);
   if (Struct.Last == 0)
      PyErr_SetString(PyExc_AttributeError,name);
   return Struct;
}

static PyObject *PkgRecordsGetFileName(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"FileName");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->FileName()) : 0;
}
static PyObject *PkgRecordsGetMD5Hash(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"MD5Hash");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->MD5Hash()) : 0;
}
static PyObject *PkgRecordsGetSHA1Hash(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"SHA1Hash");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->SHA1Hash()) : 0;
}
static PyObject *PkgRecordsGetSHA256Hash(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"SHA256Hash");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->SHA256Hash()) : 0;
}
static PyObject *PkgRecordsGetSourcePkg(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"SourcePkg");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->SourcePkg()) : 0;
}
static PyObject *PkgRecordsGetSourceVer(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"SourceVer");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->SourceVer()) : 0;
}
static PyObject *PkgRecordsGetMaintainer(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"Maintainer");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->Maintainer()) : 0;
}
static PyObject *PkgRecordsGetShortDesc(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"ShortDesc");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->ShortDesc()) : 0;
}
static PyObject *PkgRecordsGetLongDesc(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"LongDesc");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->LongDesc()) : 0;
}
static PyObject *PkgRecordsGetName(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"Name");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->Name()) : 0;
}
static PyObject *PkgRecordsGetHomepage(PyObject *Self,void*) {
   PkgRecordsStruct &Struct = GetStruct(Self,"Homepage");
   return (Struct.Last != 0) ? CppPyString(Struct.Last->Homepage()) : 0;
}
static PyObject *PkgRecordsGetRecord(PyObject *Self,void*) {
   const char *start, *stop;
   PkgRecordsStruct &Struct = GetStruct(Self,"Record");
   if (Struct.Last == 0)
      return 0;
   Struct.Last->GetRec(start, stop);
   return PyString_FromStringAndSize(start,stop-start);
}
static PyGetSetDef PkgRecordsGetSet[] = {
   {"filename",PkgRecordsGetFileName,0,
    "The filename of the package, as stored in the 'Filename' field."},
   {"homepage",PkgRecordsGetHomepage,0,
    "The homepage of the package, as stored in the 'Homepage' field."},
   {"long_desc",PkgRecordsGetLongDesc,0,
    "The long description of the packages; i.e. all lines in the\n"
    "'Description' field except for the first one."},
   {"md5_hash",PkgRecordsGetMD5Hash,0,
    "The MD5 hash value of the package, as stored in the 'MD5Sum' field."},
   {"maintainer",PkgRecordsGetMaintainer,0,
    "The maintainer of the package, as stored in the 'Maintainer' field."},
   {"name",PkgRecordsGetName,0,
    "The name of the package, as stored in the 'Package' field."},
   {"record",PkgRecordsGetRecord,0,
    "The raw record, suitable for parsing by apt_pkg.TagSection."},
   {"sha1_hash",PkgRecordsGetSHA1Hash,0,
    "The SHA1 hash value, as stored in the 'SHA1' field."},
   {"sha256_hash",PkgRecordsGetSHA256Hash,0,
    "The SHA256 hash value, as stored in the 'SHA256' field."},
   {"short_desc",PkgRecordsGetShortDesc,0,
    "The short description of the package, i.e. the first line of the\n"
    "'Description' field."},
   {"source_pkg",PkgRecordsGetSourcePkg,0,
    "The name of the source package, if different from the name of the\n"
    "binary package. This information is retrieved from the 'Source' field."},
   {"source_ver",PkgRecordsGetSourceVer,0,
    "The version of the source package, if it differs from the version\n"
    "of the binary package. Just like 'source_pkg', this information\n"
    "is retrieved from the 'Source' field."},
   {}
};

static PyObject *PkgRecordsNew(PyTypeObject *type,PyObject *Args,PyObject *kwds)
{
   PyObject *Owner;
   char *kwlist[] = {"cache",0};
   if (PyArg_ParseTupleAndKeywords(Args,kwds,"O!",kwlist,&PyCache_Type,
                                   &Owner) == 0)
      return 0;

   return HandleErrors(CppPyObject_NEW<PkgRecordsStruct>(Owner,type,
							      GetCpp<pkgCache *>(Owner)));
}

static const char *packagerecords_doc =
    "PackageRecords(cache: apt_pkg.Cache)\n\n"
    "Package Records contain information about packages. Those objects\n"
    "can be used to retrieve information such as maintainer or filename\n"
    "of a package. They can also be used to retrieve the raw records\n"
    "of the packages (i.e. those stanzas stored in Packages files).";

PyTypeObject PyPackageRecords_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.PackageRecords",                          // tp_name
   sizeof(CppPyObject<PkgRecordsStruct>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDealloc<PkgRecordsStruct>,   // tp_dealloc
   0,                                   // tp_print
   0,                                    // tp_getattr
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
   (Py_TPFLAGS_DEFAULT |                // tp_flags
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_GC),
   packagerecords_doc,                  // tp_doc
   CppTraverse<PkgRecordsStruct>,       // tp_traverse
   CppClear<PkgRecordsStruct>,          // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   PkgRecordsMethods,                   // tp_methods
   0,                                   // tp_members
   PkgRecordsGetSet,                    // tp_getset
   0,                                   // tp_base
   0,                                   // tp_dict
   0,                                   // tp_descr_get
   0,                                   // tp_descr_set
   0,                                   // tp_dictoffset
   0,                                   // tp_init
   0,                                   // tp_alloc
   PkgRecordsNew,                       // tp_new
};

									/*}}}*/


#ifdef COMPAT_0_7
PyObject *GetPkgRecords(PyObject *Self,PyObject *Args)
{
    PyErr_WarnEx(PyExc_DeprecationWarning, "apt_pkg.GetPkgRecords() is "
                 "deprecated. Please see apt_pkg.Records() for the "
                 "replacement.", 1);
    return PkgRecordsNew(&PyPackageRecords_Type,Args,0);
}
#endif
