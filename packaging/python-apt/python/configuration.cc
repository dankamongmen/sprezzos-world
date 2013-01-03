// -*- mode: cpp; mode: fold -*-
// Description								/*{{{*/
// $Id: configuration.cc,v 1.4 2003/06/03 03:22:27 mdz Exp $
/* ######################################################################

   Configuration - Binding for the configuration object.

   The Configuration object can have an owner (a parent Configuration object),
   and it always uses a pointer.

   The wrapping is mostly 1:1 with the C++ code, but there are additions to
   wrap the linked tree walking into nice flat sequence walking.

   ##################################################################### */
									/*}}}*/
// Include Files							/*{{{*/
#include "generic.h"
#include "apt_pkgmodule.h"

#include <apt-pkg/configuration.h>
#include <apt-pkg/cmndline.h>
#include <sstream>
#include <Python.h>
									/*}}}*/

// GetSelf - Convert PyObject to Configuration				/*{{{*/
// ---------------------------------------------------------------------
/* */
static inline Configuration &GetSelf(PyObject *Obj)
{
   return *GetCpp<Configuration*>(Obj);
}
									/*}}}*/

// Method Wrappers							/*{{{*/
static const char *doc_Find =
    "find(key: str[, default: str = '']) -> str\n\n"
    "Find the value for the given key and return it. If the\n"
    "given key does not exist, return default instead.";
static PyObject *CnfFind(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   char *Default = 0;
   if (PyArg_ParseTuple(Args,"s|s",&Name,&Default) == 0)
      return 0;
   return CppPyString(GetSelf(Self).Find(Name,Default));
}

static const char *doc_FindFile =
    "find_file(key: str[, default: str = '']) -> str\n\n"
    "Same as find(), but for filenames. In the APT configuration, there\n"
    "is a special section Dir:: for storing filenames. find_file() locates\n"
    "the given key and then goes up and prepends the directory names to the\n"
    "return value. For example, for:\n"
    "\n"
    "    apt_pkg.config['Dir'] = 'a'\n"
    "    apt_pkg.config['Dir::D'] = 'b'\n"
    "    apt_pkg.config['Dir::D::F'] = 'c'\n"
    "\n"
    "find_file('Dir::D::F') returns 'a/b/c'. There is also a special\n"
    "configuration setting RootDir which will always be prepended to the\n"
    "result (the default being ''). Thus, if RootDir is 'x', the example\n"
    "would return 'x/a/b/c'.";
static PyObject *CnfFindFile(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   char *Default = 0;
   if (PyArg_ParseTuple(Args,"s|s",&Name,&Default) == 0)
      return 0;
   return CppPyString(GetSelf(Self).FindFile(Name,Default));
}

static const char *doc_FindDir =
    "find_dir(key: str[, default: str = '']) -> str\n\n"
    "Same as find_file(), but for directories. The difference is\n"
    "that this function adds a trailing slash to the result.";
static PyObject *CnfFindDir(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   char *Default = 0;
   if (PyArg_ParseTuple(Args,"s|s",&Name,&Default) == 0)
      return 0;
   return CppPyString(GetSelf(Self).FindDir(Name,Default));
}

static const char *doc_FindI =
    "find_i(key: str[, default: int = 0]) -> int\n\n"
    "Same as find, but for integer values.";
static PyObject *CnfFindI(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   int Default = 0;
   if (PyArg_ParseTuple(Args,"s|i",&Name,&Default) == 0)
      return 0;
   return MkPyNumber(GetSelf(Self).FindI(Name,Default));
}

static const char *doc_FindB =
    "find_i(key: str[, default: bool = False]) -> bool\n\n"
    "Same as find, but for boolean values; returns False on unknown values.";
static PyObject *CnfFindB(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   int Default = 0;
   if (PyArg_ParseTuple(Args,"s|i",&Name,&Default) == 0)
      return 0;
   return PyBool_FromLong(GetSelf(Self).FindB(Name,(Default == 0?false:true)));
}

static const char *doc_Set =
    "set(key: str, value: str)\n\n"
    "Set the given key to the given value. To set int or bool values,\n"
    "encode them using str(value) and then use find_i()/find_b()\n"
    "to retrieve their value again.";
static PyObject *CnfSet(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   char *Value = 0;
   if (PyArg_ParseTuple(Args,"ss",&Name,&Value) == 0)
      return 0;

   GetSelf(Self).Set(Name,Value);
   Py_INCREF(Py_None);
   return Py_None;
}

static const char *doc_Exists =
    "exists(key: str) -> bool\n\n"
    "Check whether the given key exists.";
static PyObject *CnfExists(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   if (PyArg_ParseTuple(Args,"s",&Name) == 0)
      return 0;
   return PyBool_FromLong((int)GetSelf(Self).Exists(Name));
}

static int CnfContains(PyObject *Self,PyObject *Arg)
{
   return (int)GetSelf(Self).Exists(PyString_AsString(Arg));
}

static const char *doc_Clear = 
    "clear(key: str)\n\n"
    "Remove the specified option and all sub-options.";
static PyObject *CnfClear(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   if (PyArg_ParseTuple(Args,"s",&Name) == 0)
      return 0;

   GetSelf(Self).Clear(Name);

   Py_INCREF(Py_None);
   return Py_None;
}

// The amazing narrowing search ability!
static const char *doc_SubTree =
    "subtree(key: str) -> apt_pkg.Configuration\n\n"
    
    "Return a new apt_pkg.Configuration object with the given option\n"
    "as its root. Example:\n\n"
    "    apttree = config.subtree('APT')\n"
    "    apttree['Install-Suggests'] = config['APT::Install-Suggests']";
static PyObject *CnfSubTree(PyObject *Self,PyObject *Args)
{
   char *Name;
   if (PyArg_ParseTuple(Args,"s",&Name) == 0)
      return 0;
   const Configuration::Item *Itm = GetSelf(Self).Tree(Name);
   if (Itm == 0)
   {
      PyErr_SetString(PyExc_KeyError,Name);
      return 0;
   }

   return CppPyObject_NEW<Configuration*>(Self,&PyConfiguration_Type,
                                               new Configuration(Itm));
}

// Return a list of items at a specific level
static char *doc_List =
    "list([root: str]) -> list\n\n"
    "Return a list of all items at the given root, using their full\n"
    "name. For example, in a configuration object where the options A,\n"
    "B, and B::C are set, the following expressions evaluate to True:\n\n"
    "   conf.list() == ['A', 'B']\n"
    "   conf.list('A') == ['']\n"
    "   conf.list('B') == ['B::C']\n";
static PyObject *CnfList(PyObject *Self,PyObject *Args)
{
   char *RootName = 0;
   if (PyArg_ParseTuple(Args,"|s",&RootName) == 0)
      return 0;

   // Convert the whole configuration space into a list
   PyObject *List = PyList_New(0);
   const Configuration::Item *Top = GetSelf(Self).Tree(RootName);
   if (!GetSelf(Self).Tree(0))
    return List;
   const Configuration::Item *Root = GetSelf(Self).Tree(0)->Parent;
   if (Top != 0 && RootName != 0)
      Top = Top->Child;
   for (; Top != 0; Top = Top->Next)
   {
      PyObject *Obj;
      PyList_Append(List,Obj = CppPyString(Top->FullTag(Root)));
      Py_DECREF(Obj);
   }

   return List;
}

/* Return a list of values of items at a specific level.. This is used to
   get out value lists */
static char *doc_ValueList =
    "value_list([root: str]) -> list\n\n"
    "Same as list(), but instead of returning the keys, return the values.";
static PyObject *CnfValueList(PyObject *Self,PyObject *Args)
{
   char *RootName = 0;
   if (PyArg_ParseTuple(Args,"|s",&RootName) == 0)
      return 0;

   // Convert the whole configuration space into a list
   PyObject *List = PyList_New(0);
   const Configuration::Item *Top = GetSelf(Self).Tree(RootName);
   if (Top != 0 && RootName != 0)
      Top = Top->Child;
   for (; Top != 0; Top = Top->Next)
   {
      PyObject *Obj;
      PyList_Append(List,Obj = CppPyString(Top->Value));
      Py_DECREF(Obj);
   }

   return List;
}

static char *doc_MyTag =
    "my_tag() -> str\n\n"
    "Return the tag of the root of this Configuration object. For the\n"
    "default object, this is an empty string. For a subtree('APT') of\n"
    "such an object, it would be 'APT' (given as an example).";
static PyObject *CnfMyTag(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   const Configuration::Item *Top = GetSelf(Self).Tree(0);
   if (Top == 0)
      return Py_BuildValue("s","");
   return CppPyString(Top->Parent->Tag);
}

static char *doc_Dump =
    "dump() -> str\n\n"
    "Return a string dump this Configuration object.";
static PyObject *CnfDump(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   std::stringstream ss;
   GetSelf(Self).Dump(ss);
   return CppPyString(ss.str());
}


// Look like a mapping
static char *doc_Keys =
    "keys([root: str]) -> list\n\n"
    "Return a list of all keys in the configuration object. If 'root'\n"
    "is given, limit the list to those below the root.";
static PyObject *CnfKeys(PyObject *Self,PyObject *Args)
{
   char *RootName = 0;
   if (PyArg_ParseTuple(Args,"|s",&RootName) == 0)
      return 0;

   // Convert the whole configuration space into a list
   PyObject *List = PyList_New(0);
   const Configuration::Item *Top = GetSelf(Self).Tree(RootName);
   const Configuration::Item *Stop = Top;
   const Configuration::Item *Root = 0;
   if (RootName == 0)
      Stop = 0;
   if (Top != 0 && GetSelf(Self).Tree(0))
      Root = GetSelf(Self).Tree(0)->Parent;
   for (; Top != 0;)
   {
      PyObject *Obj;
      PyList_Append(List,Obj = CppPyString(Top->FullTag(Root)));
      Py_DECREF(Obj);

      if (Top->Child != 0)
      {
	 Top = Top->Child;
	 continue;
      }

      while (Top != 0 && Top->Next == 0 && Top != Root &&
	     Top->Parent != Stop)
	 Top = Top->Parent;
      if (Top != 0)
	 Top = Top->Next;
   }

   return List;
}

// Map access, operator []
static PyObject *CnfMap(PyObject *Self,PyObject *Arg)
{
   if (PyString_Check(Arg) == 0)
   {
      PyErr_SetNone(PyExc_TypeError);
      return 0;
   }

   if (GetSelf(Self).Exists(PyString_AsString(Arg)) == false)
   {
      PyErr_SetString(PyExc_KeyError,PyString_AsString(Arg));
      return 0;
   }

   return CppPyString(GetSelf(Self).Find(PyString_AsString(Arg)));
}

// Assignment with operator []
static int CnfMapSet(PyObject *Self,PyObject *Arg,PyObject *Val)
{
   if (PyString_Check(Arg) == 0 || (Val != NULL && PyString_Check(Val) == 0))
   {
      PyErr_SetNone(PyExc_TypeError);
      return -1;
   }

   if (Val == NULL)
      GetSelf(Self).Clear(PyString_AsString(Arg));
   else
      GetSelf(Self).Set(PyString_AsString(Arg),PyString_AsString(Val));
   return 0;
}
									/*}}}*/
// Config file loaders							/*{{{*/
char *doc_LoadConfig =
    "read_config_file(configuration: apt_pkg.Configuration, filename: str)\n\n"
    "Read the configuration file 'filename' and set the appropriate\n"
    "options in the configuration object.";
PyObject *LoadConfig(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   if (PyArg_ParseTuple(Args,"Os",&Self,&Name) == 0)
      return 0;
   if (PyConfiguration_Check(Self)== 0)
   {
      PyErr_SetString(PyExc_TypeError,"argument 1: expected Configuration.");
      return 0;
   }

   if (ReadConfigFile(GetSelf(Self),Name,false) == false)
     return HandleErrors();

   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}
char *doc_LoadConfigISC =
    "read_config_file_isc(configuration: apt_pkg.Configuration, filename: str)\n\n"
    "Like read_config_file(), but for configuration files like bind's\n"
    "named.conf. They have a slightly different format than APT\n"
    "configuration files.";
PyObject *LoadConfigISC(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   if (PyArg_ParseTuple(Args,"Os",&Self,&Name) == 0)
      return 0;
   if (PyConfiguration_Check(Self)== 0)
   {
      PyErr_SetString(PyExc_TypeError,"argument 1: expected Configuration.");
      return 0;
   }

   if (ReadConfigFile(GetSelf(Self),Name,true) == false)
      return HandleErrors();

   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}
char *doc_LoadConfigDir =
    "read_config_dir(configuration: apt_pkg.Configuration, dirname: str)\n\n"
    "Read all configuration files in the dir given by 'dirname' in the\n"
    "correct order.";
PyObject *LoadConfigDir(PyObject *Self,PyObject *Args)
{
   char *Name = 0;
   if (PyArg_ParseTuple(Args,"Os",&Self,&Name) == 0)
      return 0;
   if (PyConfiguration_Check(Self)== 0)
   {
      PyErr_SetString(PyExc_TypeError,"argument 1: expected Configuration.");
      return 0;
   }

   if (ReadConfigDir(GetSelf(Self),Name,false) == false)
     return HandleErrors();

   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}
									/*}}}*/

// ParseCommandLine - Wrapper for the command line interface		/*{{{*/
// ---------------------------------------------------------------------
char *doc_ParseCommandLine =
"parse_commandLine(config: Configuration, options: list, argv: list) -> list\n"
"\n"
"Parse the command line in 'argv' into the configuration space. The\n"
"list 'options' contains a list of 3-tuples or 4-tuples in the form:\n"
"\n"
"   (short_option: str, long_option: str, variable: str[, type: str])\n"
"\n"
"The element 'short_option' is one character, the 'long_option' element\n"
"is the name of the long option, the element 'variable' the name of the\n"
"configuration option the result will be stored in and type is one of\n"
"'HasArg', 'IntLevel', 'Boolean', 'InvBoolean', 'ConfigFile',\n"
"'ArbItem'. The default type is 'Boolean'. Read the online documentation\n"
"in python-apt-doc and its tutorial on writing an apt-cdrom clone for more\n"
"details.";
PyObject *ParseCommandLine(PyObject *Self,PyObject *Args)
{
   PyObject *POList;
   PyObject *Pargv;
   if (PyArg_ParseTuple(Args,"OO!O!",&Self,
			&PyList_Type,&POList,&PyList_Type,&Pargv) == 0)
      return 0;
   if (PyConfiguration_Check(Self)== 0)
   {
      PyErr_SetString(PyExc_TypeError,"argument 1: expected Configuration.");
      return 0;
   }

   if (PySequence_Length(Pargv) < 1) {
      PyErr_SetString(PyExc_ValueError,"argv is an empty sequence");
      return 0;
   }
   // Convert the option list
   int Length = PySequence_Length(POList);
   CommandLine::Args *OList = new CommandLine::Args[Length+1];
   OList[Length].ShortOpt = 0;
   OList[Length].LongOpt = 0;

   for (int I = 0; I != Length; I++)
   {
      char *Type = 0;
      #if PY_MAJOR_VERSION >= 3
      if (PyArg_ParseTuple(PySequence_GetItem(POList,I),"Czs|s",
      #else
      if (PyArg_ParseTuple(PySequence_GetItem(POList,I),"czs|s",
      #endif
			   &OList[I].ShortOpt,&OList[I].LongOpt,
			   &OList[I].ConfName,&Type) == 0)
      {
	 delete [] OList;
	 return 0;
      }
      OList[I].Flags = 0;

      // Convert the type over to flags..
      if (Type != 0)
      {
	 if (strcasecmp(Type,"HasArg") == 0)
	    OList[I].Flags = CommandLine::HasArg;
	 else if (strcasecmp(Type,"IntLevel") == 0)
	    OList[I].Flags = CommandLine::IntLevel;
	 else if (strcasecmp(Type,"Boolean") == 0)
	    OList[I].Flags = CommandLine::Boolean;
	 else if (strcasecmp(Type,"InvBoolean") == 0)
	    OList[I].Flags = CommandLine::InvBoolean;
	 else if (strcasecmp(Type,"ConfigFile") == 0)
	    OList[I].Flags = CommandLine::ConfigFile;
	 else if (strcasecmp(Type,"ArbItem") == 0)
	    OList[I].Flags = CommandLine::ArbItem;
      }
   }

   // Convert the argument list into a char **
   const char **argv = ListToCharChar(Pargv);
   if (argv == 0)
   {
      delete [] OList;
      return 0;
   }

   // Do the command line processing
   PyObject *List = 0;
   {
      CommandLine CmdL(OList,&GetSelf(Self));
      if (CmdL.Parse(PySequence_Length(Pargv),argv) == false)
      {
	 delete [] argv;
	 delete [] OList;
	 return HandleErrors();
      }

      // Convert the file listing into a python sequence
      for (Length = 0; CmdL.FileList[Length] != 0; Length++);
      List = PyList_New(Length);
      for (int I = 0; CmdL.FileList[I] != 0; I++)
      {
	 PyList_SetItem(List,I,PyString_FromString(CmdL.FileList[I]));
      }
   }

   delete [] argv;
   delete [] OList;
   return HandleErrors(List);
}
									/*}}}*/

// Method table for the Configuration object
static PyMethodDef CnfMethods[] =
{
   // Query
   {"find",CnfFind,METH_VARARGS,doc_Find},
   {"find_file",CnfFindFile,METH_VARARGS,doc_FindFile},
   {"find_dir",CnfFindDir,METH_VARARGS,doc_FindDir},
   {"find_i",CnfFindI,METH_VARARGS,doc_FindI},
   {"find_b",CnfFindB,METH_VARARGS,doc_FindB},

   // Others
   {"set",CnfSet,METH_VARARGS,doc_Set},
   {"exists",CnfExists,METH_VARARGS,doc_Exists},
   {"subtree",CnfSubTree,METH_VARARGS,doc_SubTree},
   {"list",CnfList,METH_VARARGS,doc_List},
   {"value_list",CnfValueList,METH_VARARGS,doc_ValueList},
   {"my_tag",CnfMyTag,METH_VARARGS,doc_MyTag},
   {"clear",CnfClear,METH_VARARGS,doc_Clear},
   {"dump",CnfDump,METH_VARARGS,doc_Dump},
   // Python Special
   {"keys",CnfKeys,METH_VARARGS,doc_Keys},
   #if PY_MAJOR_VERSION < 3
   {"has_key",CnfExists,METH_VARARGS,doc_Exists},
   #endif
   {"get",CnfFind,METH_VARARGS,doc_Find},
   {}
};

static PyObject *CnfNew(PyTypeObject *type, PyObject *args, PyObject *kwds) {
    char *kwlist[] = {NULL};
    if (PyArg_ParseTupleAndKeywords(args,kwds,"",kwlist) == 0)
        return 0;
    return CppPyObject_NEW<Configuration*>(NULL, type, new Configuration());
}

// Type for a Normal Configuration object
static PySequenceMethods ConfigurationSeq = {0,0,0,0,0,0,0,CnfContains,0,0};
static PyMappingMethods ConfigurationMap = {0,CnfMap,CnfMapSet};

static const char *configuration_doc =
    "Configuration()\n\n"
    "Represent the configuration of APT by mapping option keys to\n"
    "values and storing configuration parsed from files like\n"
    "/etc/apt/apt.conf. The most important Configuration object\n"
    "is apt_pkg.config which points to the global configuration\n"
    "object. Other top-level Configuration objects can be created\n"
    "by calling the constructor, but there is usually no reason to.";

PyTypeObject PyConfiguration_Type =
{
   PyVarObject_HEAD_INIT(&PyType_Type, 0)
   "apt_pkg.Configuration",             // tp_name
   sizeof(CppPyObject<Configuration*>),  // tp_basicsize
   0,                                   // tp_itemsize
   // Methods
   CppDeallocPtr<Configuration*>,  // tp_dealloc
   0,                                   // tp_print
   0,                                   // tp_getattr
   0,                                   // tp_setattr
   0,                                   // tp_compare
   0,                                   // tp_repr
   0,                                   // tp_as_number
   &ConfigurationSeq,                   // tp_as_sequence
   &ConfigurationMap,                   // tp_as_mapping
   0,                                   // tp_hash
   0,                                   // tp_call
   0,                                   // tp_str
   _PyAptObject_getattro,               // tp_getattro
   0,                                   // tp_setattro
   0,                                   // tp_as_buffer
   (Py_TPFLAGS_DEFAULT |                // tp_flags
    Py_TPFLAGS_BASETYPE),
   configuration_doc,                   // tp_doc
   0,                                   // tp_traverse
   0,                                   // tp_clear
   0,                                   // tp_richcompare
   0,                                   // tp_weaklistoffset
   0,                                   // tp_iter
   0,                                   // tp_iternext
   CnfMethods,                          // tp_methods
   0,                                   // tp_members
   0,                                   // tp_getset
   0,                                   // tp_base
   0,                                   // tp_dict
   0,                                   // tp_descr_get
   0,                                   // tp_descr_set
   0,                                   // tp_dictoffset
   0,                                   // tp_init
   0,                                   // tp_alloc
   CnfNew,                              // tp_new
};

