// -*- mode: cpp; mode: fold -*-
// Description								/*{{{*/
// $Id: indexfile.cc,v 1.2 2003/12/26 17:04:22 mdz Exp $
/* ######################################################################

   pkgIndexFile - Wrapper for the pkgIndexFilefunctions

   ##################################################################### */
									/*}}}*/
// Include Files							/*{{{*/
#include "generic.h"
#include "apt_pkgmodule.h"

#include <apt-pkg/indexfile.h>

#include <Python.h>

static PyObject *IndexFileArchiveURI(PyObject *Self,PyObject *Args)
{
   pkgIndexFile *File = GetCpp<pkgIndexFile*>(Self);
   char *path;

   if (PyArg_ParseTuple(Args, "s",&path) == 0)
      return 0;
   return HandleErrors(Safe_FromString(File->ArchiveURI(path).c_str()));
}

static PyMethodDef IndexFileMethods[] =
{
   {"archive_uri",IndexFileArchiveURI,METH_VARARGS,
    "archive_uri(path: str) -> str\n\n"
    "Return the URI to the given path in the archive."},
   {}
};

#define File (GetCpp<pkgIndexFile*>(Self))
static PyObject *IndexFileGetLabel(PyObject *Self,void*) {
   return Safe_FromString(File->GetType()->Label);
}
static PyObject *IndexFileGetDescribe(PyObject *Self,void*) {
   return Safe_FromString(File->Describe().c_str());
}
static PyObject *IndexFileGetExists(PyObject *Self,void*) {
   return PyBool_FromLong((File->Exists()));
}
static PyObject *IndexFileGetHasPackages(PyObject *Self,void*) {
   return PyBool_FromLong((File->HasPackages()));
}
static PyObject *IndexFileGetSize(PyObject *Self,void*) {
   return MkPyNumber((File->Size()));
}
static PyObject *IndexFileGetIsTrusted(PyObject *Self,void*) {
   return PyBool_FromLong((File->IsTrusted()));
}
#undef File

#define S(x) (x ? x : "")
static PyObject *IndexFileRepr(PyObject *Self)
{
   pkgIndexFile *File = GetCpp<pkgIndexFile*>(Self);
   return PyString_FromFormat("<pkIndexFile object: "
			"Label:'%s' Describe='%s' Exists='%i' "
	                "HasPackages='%i' Size='%lu'  "
 	                "IsTrusted='%i' ArchiveURI='%s'>",
	    S(File->GetType()->Label),  File->Describe().c_str(), File->Exists(),
	    File->HasPackages(), File->Size(),
            File->IsTrusted(), File->ArchiveURI("").c_str());
}
#undef S

static PyGetSetDef IndexFileGetSet[] = {
    {"describe",IndexFileGetDescribe,0,
     "A string describing the index file."},
    {"exists",IndexFileGetExists,0,
     "A boolean value determining whether the index file exists."},
    {"has_packages",IndexFileGetHasPackages,0,
     "A boolean value determining whether the index file has packages."},
    {"is_trusted",IndexFileGetIsTrusted,0,
     "A boolean value determining whether the file can be trusted; e.g.\n"
     "because it is from a source with a GPG signed Release file."},
    {"label",IndexFileGetLabel,0,
     "The label of the index file."},
    {"size",IndexFileGetSize,0,
     "The size of the files, measured in bytes."},
    {}
};

static const char *indexfile_doc =
    "Represent an index file, i.e. package indexes, translation indexes,\n"
    "and source indexes.";

PyTypeObject PyIndexFile_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.IndexFile",          // tp_name
   sizeof(CppPyObject<pkgIndexFile*>),   // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   // Not ..Ptr, because the pointer is managed somewhere else.
   CppDeallocPtr<pkgIndexFile*>,   // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   IndexFileRepr,                // tp_repr
   0,                                   // tp_as_number
   0,                                   // tp_as_sequence
   0,                                   // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC, // tp_flags
   indexfile_doc,                       // tp_doc
   CppTraverse<pkgIndexFile*>,     // tp_traverse
   CppClear<pkgIndexFile*>,        // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   IndexFileMethods,             // tp_methods
   0,                                   // tp_members
   IndexFileGetSet,              // tp_getset
};




