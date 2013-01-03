// -*- mode: cpp; mode: fold -*-
// Description								/*{{{*/
// $Id: string.cc,v 1.3 2002/01/08 06:53:04 jgg Exp $
/* ######################################################################

   string - Mappings for the string functions that are worthwile for
            Python users

   ##################################################################### */
									/*}}}*/
// Include Files							/*{{{*/
#include "apt_pkgmodule.h"
#include "generic.h"

#include <apt-pkg/strutl.h>

#include <Python.h>
									/*}}}*/

// Templated function							/*{{{*/
/* Macro for the generic string in string out function */
#define MkStr(Python,CFunc) \
PyObject *Python(PyObject *Self,PyObject *Args) \
{ \
   char *Str = 0; \
   if (PyArg_ParseTuple(Args,"s",&Str) == 0) \
      return 0; \
   return CppPyString(CFunc(Str)); \
}

#define MkInt(Python,CFunc, ctype, pytype) \
PyObject *Python(PyObject *Self,PyObject *Args) \
{ \
   ctype Val = 0; \
   if (PyArg_ParseTuple(Args,pytype,&Val) == 0) \
      return 0; \
   return CppPyString(CFunc(Val)); \
}

MkStr(StrDeQuote,DeQuoteString);

/*
 * Input bytes(Py3k)/str(Py2), output str.
 */
PyObject *StrBase64Encode(PyObject *Self,PyObject *Args) {
   char *Str = 0;
   #if PY_MAJOR_VERSION >= 3
   if (PyArg_ParseTuple(Args,"y",&Str) == 0)
   #else
   if (PyArg_ParseTuple(Args,"s",&Str) == 0)
   #endif
      return 0;
   return CppPyString(Base64Encode(Str));
}

MkStr(StrURItoFileName,URItoFileName);

//MkFloat(StrSizeToStr,SizeToStr);
MkInt(StrTimeToStr,TimeToStr, unsigned long, "k");
MkInt(StrTimeRFC1123,TimeRFC1123, long long, "L");
									/*}}}*/

// Other String functions						/*{{{*/
PyObject *StrSizeToStr(PyObject *Self,PyObject *Args)
{
   PyObject *Obj;
   double value;

   if (PyArg_ParseTuple(Args,"O",&Obj) == 0)
      return 0;
   // In Python 3, PyInt_Check is aliased to PyLong_Check and PyInt_AsLong is
   // aliased to PyLong_AsLong.  Therefore we do the actual long checks first
   // so that if it is a long in Python 3, the value will be converted to a
   // double rather than a long.  This avoids OverflowError regressions in
   // Python 3.  LP: #1030278
   if (PyLong_Check(Obj))
      value = PyLong_AsDouble(Obj);
   else if (PyInt_Check(Obj))
      value = PyInt_AsLong(Obj);
   else if (PyFloat_Check(Obj))
      value = PyFloat_AsDouble(Obj);
   else {
      PyErr_SetString(PyExc_TypeError,"Only understand integers and floats");
      return 0;
   }
   // Check for OverflowErrors or other exceptions during conversion.
   if (PyErr_Occurred())
      return 0;
   return CppPyString(SizeToStr(value));
}

PyObject *StrQuoteString(PyObject *Self,PyObject *Args)
{
   char *Str = 0;
   char *Bad = 0;
   if (PyArg_ParseTuple(Args,"ss",&Str,&Bad) == 0)
      return 0;
   return CppPyString(QuoteString(Str,Bad));
}

PyObject *StrStringToBool(PyObject *Self,PyObject *Args)
{
   char *Str = 0;
   if (PyArg_ParseTuple(Args,"s",&Str) == 0)
      return 0;
   return MkPyNumber(StringToBool(Str));
}

PyObject *StrStrToTime(PyObject *Self,PyObject *Args)
{
   char *Str = 0;
   if (PyArg_ParseTuple(Args,"s",&Str) == 0)
      return 0;

   time_t Result;
   if (StrToTime(Str,Result) == false)
   {
      Py_INCREF(Py_None);
      return Py_None;
   }

   return MkPyNumber(Result);
}

PyObject *StrCheckDomainList(PyObject *Self,PyObject *Args)
{
   char *Host = 0;
   char *List = 0;
   if (PyArg_ParseTuple(Args,"ss",&Host,&List) == 0)
      return 0;
   return PyBool_FromLong(CheckDomainList(Host,List));
}

									/*}}}*/
