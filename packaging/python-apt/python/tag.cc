// -*- mode: cpp; mode: fold -*-
// Description								/*{{{*/
// $Id: tag.cc,v 1.3 2002/02/26 01:36:15 mdz Exp $
/* ######################################################################

   Tag - Binding for the RFC 822 tag file parser

   Upon reflection I have make the TagSection wrapper look like a map..
   The other option was to use a sequence (which nicely matches the internal
   storage) but really makes no sense to a Python Programmer.. One
   specialized lookup is provided, the FindFlag lookup - as well as the
   usual set of duplicate things to match the C++ interface.

   The TagFile interface is also slightly different, it has a built in
   internal TagSection object that is used. Do not hold onto a reference
   to a TagSection and let TagFile go out of scope! The underlying storage
   for the section will go away and it will seg.

   ##################################################################### */
									/*}}}*/
// Include Files							/*{{{*/
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "generic.h"
#include "apt_pkgmodule.h"

#include <apt-pkg/tagfile.h>
#include <apt-pkg/fileutl.h>

#include <stdio.h>
#include <iostream>
#include <Python.h>

using namespace std;
									/*}}}*/
/* We need to keep a private copy of the data.. */
struct TagSecData : public CppPyObject<pkgTagSection>
{
   char *Data;
   bool Bytes;
#if PY_MAJOR_VERSION >= 3
   PyObject *Encoding;
#endif
};

// The owner of the TagFile is a Python file object.
struct TagFileData : public CppPyObject<pkgTagFile>
{
   TagSecData *Section;
   FileFd Fd;
   bool Bytes;
#if PY_MAJOR_VERSION >= 3
   PyObject *Encoding;
#endif
};

// Traversal and Clean for owned objects
int TagFileTraverse(PyObject *self, visitproc visit, void* arg) {
    Py_VISIT(((TagFileData *)self)->Section);
    Py_VISIT(((TagFileData *)self)->Owner);
    return 0;
}

int TagFileClear(PyObject *self) {
    Py_CLEAR(((TagFileData *)self)->Section);
    Py_CLEAR(((TagFileData *)self)->Owner);
    return 0;
}

// Helpers to return Unicode or bytes as appropriate.
#if PY_MAJOR_VERSION < 3
#define TagSecString_FromStringAndSize(self, v, len) \
    PyString_FromStringAndSize((v), (len))
#define TagSecString_FromString(self, v) PyString_FromString(v)
#else
static PyObject *TagSecString_FromStringAndSize(PyObject *self, const char *v,
	 				 Py_ssize_t len) {
   TagSecData *Self = (TagSecData *)self;
   if (Self->Bytes)
      return PyBytes_FromStringAndSize(v, len);
   else if (Self->Encoding)
      return PyUnicode_Decode(v, len, PyUnicode_AsString(Self->Encoding), 0);
   else
      return PyUnicode_FromStringAndSize(v, len);
}

static PyObject *TagSecString_FromString(PyObject *self, const char *v) {
   TagSecData *Self = (TagSecData *)self;
   if (Self->Bytes)
      return PyBytes_FromString(v);
   else if (Self->Encoding)
      return PyUnicode_Decode(v, strlen(v),
			      PyUnicode_AsString(Self->Encoding), 0);
   else
      return PyUnicode_FromString(v);
}
#endif


									/*}}}*/
// TagSecFree - Free a Tag Section					/*{{{*/
// ---------------------------------------------------------------------
/* */
void TagSecFree(PyObject *Obj)
{
   TagSecData *Self = (TagSecData *)Obj;
   delete [] Self->Data;
   CppDealloc<pkgTagSection>(Obj);
}
									/*}}}*/
// TagFileFree - Free a Tag File					/*{{{*/
// ---------------------------------------------------------------------
/* */
void TagFileFree(PyObject *Obj)
{
   #ifdef ALLOC_DEBUG
   std::cerr << "=== DEALLOCATING " << Obj->ob_type->tp_name << "^ ===\n";
   #endif
   TagFileData *Self = (TagFileData *)Obj;
   Py_CLEAR(Self->Section);
   Self->Object.~pkgTagFile();
   Self->Fd.~FileFd();
   Py_CLEAR(Self->Owner);
   Obj->ob_type->tp_free(Obj);
}
									/*}}}*/

// Tag Section Wrappers							/*{{{*/
static char *doc_Find =
    "find(name: str[, default = None]) -> str\n\n"
    "Find the key given by 'name' and return the value. If the key can\n"
    "not be found, return 'default'.";
static PyObject *TagSecFind(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   char *Default = 0;
   if (PyArg_ParseTuple(Args,"s|z",&Name,&Default) == 0)
      return 0;

   const char *Start;
   const char *Stop;
   if (GetCpp<pkgTagSection>(Self).Find(Name,Start,Stop) == false)
   {
      if (Default == 0)
	 Py_RETURN_NONE;
      return TagSecString_FromString(Self,Default);
   }
   return TagSecString_FromStringAndSize(Self,Start,Stop-Start);
}

static char *doc_FindRaw =
    "find_raw(name: str[, default = None] -> str\n\n"
    "Same as find(), but returns the complete 'key: value' field; instead of\n"
    "just the value.";
static PyObject *TagSecFindRaw(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   char *Default = 0;
   if (PyArg_ParseTuple(Args,"s|z",&Name,&Default) == 0)
      return 0;

   unsigned Pos;
   if (GetCpp<pkgTagSection>(Self).Find(Name,Pos) == false)
   {
      if (Default == 0)
	 Py_RETURN_NONE;
      return TagSecString_FromString(Self,Default);
   }

   const char *Start;
   const char *Stop;
   GetCpp<pkgTagSection>(Self).Get(Start,Stop,Pos);

   return TagSecString_FromStringAndSize(Self,Start,Stop-Start);
}

static char *doc_FindFlag =
    "find_flag(name: str) -> int\n\n"
    "Return 1 if the key's value is 'yes' or a similar value describing\n"
    "a boolean true. If the field does not exist, or does not have such a\n"
    "value, return 0.";
static PyObject *TagSecFindFlag(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   if (PyArg_ParseTuple(Args,"s",&Name) == 0)
      return 0;

   unsigned long Flag = 0;
   if (GetCpp<pkgTagSection>(Self).FindFlag(Name,Flag,1) == false)
   {
      Py_INCREF(Py_None);
      return Py_None;
   }
   return PyBool_FromLong(Flag);
}

// Map access, operator []
static PyObject *TagSecMap(PyObject *Self,PyObject *Arg)
{
   const char *Name = PyObject_AsString(Arg);
   if (Name == 0)
      return 0;
   const char *Start;
   const char *Stop;
   if (GetCpp<pkgTagSection>(Self).Find(Name,Start,Stop) == false)
   {
      PyErr_SetString(PyExc_KeyError,Name);
      return 0;
   }

   return TagSecString_FromStringAndSize(Self,Start,Stop-Start);
}

// len() operation
static Py_ssize_t TagSecLength(PyObject *Self)
{
   pkgTagSection &Sec = GetCpp<pkgTagSection>(Self);
   return Sec.Count();
}

// Look like a mapping
static char *doc_Keys =
    "keys() -> list\n\n"
    "Return a list of all keys.";
static PyObject *TagSecKeys(PyObject *Self,PyObject *Args)
{
   pkgTagSection &Tags = GetCpp<pkgTagSection>(Self);
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   // Convert the whole configuration space into a list
   PyObject *List = PyList_New(0);
   for (unsigned int I = 0; I != Tags.Count(); I++)
   {
      const char *Start;
      const char *Stop;
      Tags.Get(Start,Stop,I);
      const char *End = Start;
      for (; End < Stop && *End != ':'; End++);

      PyObject *Obj;
      PyList_Append(List,Obj = PyString_FromStringAndSize(Start,End-Start));
      Py_DECREF(Obj);
   }
   return List;
}

#if PY_MAJOR_VERSION < 3
static char *doc_Exists =
    "has_key(name: str) -> bool\n\n"
    "Return True if the key given by 'name' exists, False otherwise.";
static PyObject *TagSecExists(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   if (PyArg_ParseTuple(Args,"s",&Name) == 0)
      return 0;

   const char *Start;
   const char *Stop;
   return PyBool_FromLong(GetCpp<pkgTagSection>(Self).Find(Name,Start,Stop));
}
#endif

static int TagSecContains(PyObject *Self,PyObject *Arg)
{
   const char *Name = PyObject_AsString(Arg);
   if (Name == 0)
      return 0;
   const char *Start;
   const char *Stop;
   if (GetCpp<pkgTagSection>(Self).Find(Name,Start,Stop) == false)
      return 0;
   return 1;
}

static char *doc_Bytes =
    "bytes() -> int\n\n"
    "Return the number of bytes this section is large.";
static PyObject *TagSecBytes(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   return MkPyNumber(GetCpp<pkgTagSection>(Self).size());
}

static PyObject *TagSecStr(PyObject *Self)
{
   const char *Start;
   const char *Stop;
   GetCpp<pkgTagSection>(Self).GetSection(Start,Stop);
   return TagSecString_FromStringAndSize(Self,Start,Stop-Start);
}
									/*}}}*/
// TagFile Wrappers							/*{{{*/
static char *doc_Step =
    "step() -> bool\n\n"
    "Step forward in the file";
static PyObject *TagFileStep(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   TagFileData &Obj = *(TagFileData *)Self;
   if (Obj.Object.Step(Obj.Section->Object) == false)
      return HandleErrors(PyBool_FromLong(0));

   return HandleErrors(PyBool_FromLong(1));
}

// TagFile Wrappers							/*{{{*/
static PyObject *TagFileNext(PyObject *Self)
{
   TagFileData &Obj = *(TagFileData *)Self;
   // Replace the section.
   Py_CLEAR(Obj.Section);
   Obj.Section = (TagSecData*)(&PyTagSection_Type)->tp_alloc(&PyTagSection_Type, 0);
   new (&Obj.Section->Object) pkgTagSection();
   Obj.Section->Owner = Self;
   Py_INCREF(Obj.Section->Owner);
   Obj.Section->Data = 0;
   Obj.Section->Bytes = Obj.Bytes;
#if PY_MAJOR_VERSION >= 3
   // We don't need to incref Encoding as the previous Section object already
   // held a reference to it.
   Obj.Section->Encoding = Obj.Encoding;
#endif
   if (Obj.Object.Step(Obj.Section->Object) == false)
      return HandleErrors(NULL);

   // Bug-Debian: http://bugs.debian.org/572596
   // Duplicate the data here and scan the duplicated section data; in order
   // to not use any shared storage.
   // TODO: Provide an API in apt-pkg to do this; this is really ugly.

   // Fetch old section data
   const char *Start;
   const char *Stop;
   Obj.Section->Object.GetSection(Start,Stop);
   // Duplicate the data
   Obj.Section->Data = new char[Stop-Start];
   strncpy(Obj.Section->Data, Start, Stop-Start);
   // Rescan it
   Obj.Section->Object.Scan(Obj.Section->Data, Stop-Start);

   Py_INCREF(Obj.Section);
   return HandleErrors(Obj.Section);
}

static PyObject *TagFileIter(PyObject *Self) {
    Py_INCREF(Self);
    return Self;
}

static char *doc_Offset =
    "offset() -> int\n\n"
    "Return the current offset.";
static PyObject *TagFileOffset(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;
   return MkPyNumber(((TagFileData *)Self)->Object.Offset());
   
}

static char *doc_Jump =
    "jump(offset: int) -> bool\n\n"
    "Jump to the given offset; return True on success. Note that jumping to\n"
    "an offset is not very reliable, and the 'section' attribute may point\n"
    "to an unexpected section.";
static PyObject *TagFileJump(PyObject *Self,PyObject *Args)
{
   int Offset;
   if (PyArg_ParseTuple(Args,"i",&Offset) == 0)
      return 0;

   TagFileData &Obj = *(TagFileData *)Self;
   if (Obj.Object.Jump(Obj.Section->Object,Offset) == false)
      return HandleErrors(PyBool_FromLong(0));

   return HandleErrors(PyBool_FromLong(1));
}
									/*}}}*/
// ParseSection - Parse a single section from a tag file		/*{{{*/
// ---------------------------------------------------------------------
static PyObject *TagSecNew(PyTypeObject *type,PyObject *Args,PyObject *kwds) {
   char *Data;
   int Len;
   char Bytes = 0;
   char *kwlist[] = {"text", "bytes", 0};

   // this allows reading "byte" types from python3 - but we don't
   // make (much) use of it yet
   if (PyArg_ParseTupleAndKeywords(Args,kwds,"s#|b",kwlist,&Data,&Len,&Bytes) == 0)
      return 0;

   // Create the object..
   TagSecData *New = (TagSecData*)type->tp_alloc(type, 0);
   new (&New->Object) pkgTagSection();
   New->Data = new char[strlen(Data)+2];
   snprintf(New->Data,strlen(Data)+2,"%s\n",Data);
   New->Bytes = Bytes;
#if PY_MAJOR_VERSION >= 3
   New->Encoding = 0;
#endif

   if (New->Object.Scan(New->Data,strlen(New->Data)) == false)
   {
      cerr << New->Data << endl;
      Py_DECREF((PyObject *)New);
      PyErr_SetString(PyExc_ValueError,"Unable to parse section data");
      return 0;
   }

   New->Object.Trim();

   return New;
}

#ifdef COMPAT_0_7
char *doc_ParseSection = "ParseSection(Text) -> TagSection()\n\nDeprecated.";
PyObject *ParseSection(PyObject *self,PyObject *Args)
{
   PyErr_WarnEx(PyExc_DeprecationWarning, "apt_pkg.ParseSection() is "
                "deprecated. Please see apt_pkg.TagSection() for the "
                "replacement.", 1);
    return TagSecNew(&PyTagSection_Type,Args,0);
}
#endif
									/*}}}*/
// ParseTagFile - Parse a tagd file					/*{{{*/
// ---------------------------------------------------------------------
/* This constructs the parser state. */

static PyObject *TagFileNew(PyTypeObject *type,PyObject *Args,PyObject *kwds)
{
   TagFileData *New;
   PyObject *File = 0;
   char Bytes = 0;

   char *kwlist[] = {"file", "bytes", 0};
   if (PyArg_ParseTupleAndKeywords(Args,kwds,"O|b",kwlist,&File,&Bytes) == 0)
      return 0;

   // check if we got a filename or a file object
   int fileno = -1;
   const char *filename = NULL;
   filename = PyObject_AsString(File);
   if (filename == NULL) {
      PyErr_Clear();
      fileno = PyObject_AsFileDescriptor(File);
   }

   // handle invalid arguments
   if (fileno == -1 && filename == NULL)
   {
      PyErr_SetString(PyExc_TypeError, 
                      "Argument must be string, fd or have a fileno() method");
      return 0;
   }

   New = (TagFileData*)type->tp_alloc(type, 0);
   if (fileno != -1)
   {
#ifdef APT_HAS_GZIP
      new (&New->Fd) FileFd();
      New->Fd.OpenDescriptor(fileno, FileFd::ReadOnlyGzip, false);
#else
      new (&New->Fd) FileFd(fileno,false);
#endif
   }
   else 
   {
      // FileFd::Extension got added in this revision
#if (APT_PKG_MAJOR >= 4 && APT_PKG_MINOR >= 12)
      new (&New->Fd) FileFd(filename, FileFd::ReadOnly, FileFd::Extension, false);
#else
      new (&New->Fd) FileFd(filename, FileFd::ReadOnly, false);
#endif
   } 
   New->Bytes = Bytes;
   New->Owner = File;
   Py_INCREF(New->Owner);
#if PY_MAJOR_VERSION >= 3
   if (fileno != -1) {
      New->Encoding = PyObject_GetAttr(File, PyUnicode_FromString("encoding"));
      if (New->Encoding && !PyUnicode_Check(New->Encoding))
         New->Encoding = 0;
   } else
      New->Encoding = 0;
   Py_XINCREF(New->Encoding);
#endif
   new (&New->Object) pkgTagFile(&New->Fd);

   // Create the section
   New->Section = (TagSecData*)(&PyTagSection_Type)->tp_alloc(&PyTagSection_Type, 0);
   new (&New->Section->Object) pkgTagSection();
   New->Section->Owner = New;
   Py_INCREF(New->Section->Owner);
   New->Section->Data = 0;
   New->Section->Bytes = Bytes;
#if PY_MAJOR_VERSION >= 3
   New->Section->Encoding = New->Encoding;
   Py_XINCREF(New->Section->Encoding);
#endif

   return HandleErrors(New);
}
#ifdef COMPAT_0_7
char *doc_ParseTagFile = "ParseTagFile(file) -> TagFile()\n\nDeprecated.";
PyObject *ParseTagFile(PyObject *self,PyObject *Args) {
   PyErr_WarnEx(PyExc_DeprecationWarning, "apt_pkg.ParseTagFile() is "
                "deprecated. Please see apt_pkg.TagFile() for the "
                "replacement.", 1);
    return TagFileNew(&PyTagFile_Type,Args,0);
}
#endif
									/*}}}*/
// RewriteSection - Rewrite a section..					/*{{{*/
// ---------------------------------------------------------------------
/* An interesting future extension would be to add a user settable
   order list */
char *doc_RewriteSection =
"rewrite_section(section: TagSection, order: list, rewrite_list: list) -> str\n"
"\n"
"Rewrite the section given by 'section' using 'rewrite_list', and order the\n"
"fields according to 'order'.\n\n"
"The parameter 'order' is a list object containing the names of the fields\n"
"in the order they should appear in the rewritten section.\n"
"apt_pkg.REWRITE_PACKAGE_ORDER and apt_pkg.REWRITE_SOURCE_ORDER are two\n"
"predefined lists for rewriting package and source sections, respectively\n\n"
"The parameter 'rewrite_list' is a list of tuples of the form\n"
"'(tag, newvalue[, renamed_to])', where 'tag' describes the field which\n"
"should be changed, 'newvalue' the value which should be inserted or None\n"
"to delete the field, and the optional renamed_to can be used to rename the\n"
"field.";
PyObject *RewriteSection(PyObject *self,PyObject *Args)
{
   PyObject *Section;
   PyObject *Order;
   PyObject *Rewrite;
   if (PyArg_ParseTuple(Args,"O!O!O!",&PyTagSection_Type,&Section,
			&PyList_Type,&Order,&PyList_Type,&Rewrite) == 0)
      return 0;

   // Convert the order list
   const char **OrderList = ListToCharChar(Order,true);

   // Convert the Rewrite list.
   TFRewriteData *List = new TFRewriteData[PySequence_Length(Rewrite)+1];
   memset(List,0,sizeof(*List)*(PySequence_Length(Rewrite)+1));
   for (int I = 0; I != PySequence_Length(Rewrite); I++)
   {
      List[I].NewTag = 0;
      if (PyArg_ParseTuple(PySequence_GetItem(Rewrite,I),"sz|s",
			  &List[I].Tag,&List[I].Rewrite,&List[I].NewTag) == 0)
      {
	 delete [] OrderList;
	 delete [] List;
	 return 0;
      }
   }

   /* This is a glibc extension.. If not running on glibc I'd just take
      this whole function out, it is probably infrequently used */
   char *bp = 0;
   size_t size;
   FILE *F = open_memstream (&bp, &size);

   // Do the rewrite
   bool Res = TFRewrite(F,GetCpp<pkgTagSection>(Section),OrderList,List);
   delete [] OrderList;
   delete [] List;
   fclose(F);

   if (Res == false)
   {
      free(bp);
      return HandleErrors();
   }

   // Return the string
   PyObject *ResObj = TagSecString_FromStringAndSize(Section,bp,size);
   free(bp);
   return HandleErrors(ResObj);
}
									/*}}}*/

// Method table for the Tag Section object
static PyMethodDef TagSecMethods[] =
{
   // Query
   {"find",TagSecFind,METH_VARARGS,doc_Find},
   {"find_raw",TagSecFindRaw,METH_VARARGS,doc_FindRaw},
   {"find_flag",TagSecFindFlag,METH_VARARGS,doc_FindFlag},
   {"bytes",TagSecBytes,METH_VARARGS,doc_Bytes},

   // Python Special
   {"keys",TagSecKeys,METH_VARARGS,doc_Keys},
#if PY_MAJOR_VERSION < 3
   {"has_key",TagSecExists,METH_VARARGS,doc_Exists},
#endif
   {"get",TagSecFind,METH_VARARGS,doc_Find},
   {}
};


PySequenceMethods TagSecSeqMeth = {0,0,0,0,0,0,0,TagSecContains,0,0};
PyMappingMethods TagSecMapMeth = {TagSecLength,TagSecMap,0};


static char *doc_TagSec = "TagSection(text: str, [bytes: bool = False])\n\n"
   "Provide methods to access RFC822-style header sections, like those\n"
   "found in debian/control or Packages files.\n\n"
   "TagSection() behave like read-only dictionaries and also provide access\n"
   "to the functions provided by the C++ class (e.g. find).\n\n"
   "By default, text read from files is treated as strings (binary data in\n"
   "Python 2, Unicode strings in Python 3). Use bytes=True to cause all\n"
   "header values read from this TagSection to be bytes even in Python 3.\n"
   "Header names are always treated as Unicode.";
PyTypeObject PyTagSection_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.TagSection",                // tp_name
   sizeof(TagSecData),                  // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   TagSecFree,                          // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   0,                                   // tp_repr
   0,                                   // tp_as_number
   &TagSecSeqMeth,                      // tp_as_sequence
   &TagSecMapMeth,                      // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   TagSecStr,                           // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   (Py_TPFLAGS_DEFAULT |                // tp_flags
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_GC),
   doc_TagSec,                          // tp_doc
   CppTraverse<pkgTagSection>,     // tp_traverse
   CppClear<pkgTagSection>,         // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   TagSecMethods,                       // tp_methods
   0,                                   // tp_members
   0,                                   // tp_getset
   0,                                   // tp_base
   0,                                   // tp_dict
   0,                                   // tp_descr_get
   0,                                   // tp_descr_set
   0,                                   // tp_dictoffset
   0,                                   // tp_init
   0,                                   // tp_alloc
   TagSecNew,                           // tp_new
};

// Method table for the Tag File object
static PyMethodDef TagFileMethods[] =
{
   // Query
   {"step",TagFileStep,METH_VARARGS,doc_Step},
   {"offset",TagFileOffset,METH_VARARGS,doc_Offset},
   {"jump",TagFileJump,METH_VARARGS,doc_Jump},

   {}
};

// Return the current section.
static PyObject *TagFileGetSection(PyObject *Self,void*) {
   PyObject *Obj = ((TagFileData *)Self)->Section;
   Py_INCREF(Obj);
   return Obj;
}

static PyGetSetDef TagFileGetSet[] = {
    {"section",TagFileGetSection,0,
     "The current section, as a TagSection object.",0},
    {}
};


static char *doc_TagFile = "TagFile(file, [bytes: bool = False])\n\n"
   "TagFile() objects provide access to debian control files, which consist\n"
   "of multiple RFC822-style sections.\n\n"
   "To provide access to those sections, TagFile objects provide an iterator\n"
   "which yields TagSection objects for each section.\n\n"
   "TagFile objects also provide another API which uses a shared TagSection\n"
   "object in the 'section' member. The functions step() and jump() can be\n"
   "used to navigate within the file; offset() returns the current\n"
   "position.\n\n"
   "It is important to not mix the use of both APIs, because this can have\n"
   "unwanted effects.\n\n"
   "The parameter 'file' refers to an object providing a fileno() method or\n"
   "a file descriptor (an integer).\n\n"
   "By default, text read from files is treated as strings (binary data in\n"
   "Python 2, Unicode strings in Python 3). Use bytes=True to cause all\n"
   "header values read from this TagFile to be bytes even in Python 3.\n"
   "Header names are always treated as Unicode.";

// Type for a Tag File
PyTypeObject PyTagFile_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.TagFile",                   // tp_name
   sizeof(TagFileData),                 // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   TagFileFree,                         // tp_dealloc
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
   (Py_TPFLAGS_DEFAULT |                // tp_flags
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_GC),
   doc_TagFile,                         // tp_doc
   TagFileTraverse,                     // tp_traverse
   TagFileClear,                        // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   TagFileIter,                         // tp_iter
   TagFileNext,                         // tp_iternext
   TagFileMethods,                      // tp_methods
   0,                                   // tp_members
   TagFileGetSet,                       // tp_getset
   0,                                   // tp_base
   0,                                   // tp_dict
   0,                                   // tp_descr_get
   0,                                   // tp_descr_set
   0,                                   // tp_dictoffset
   0,                                   // tp_init
   0,                                   // tp_alloc
   TagFileNew,                          // tp_new

};
