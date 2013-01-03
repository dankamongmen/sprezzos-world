/*
 * orderlist.cc - Wrapper around pkgOrderList
 *
 * Copyright 2011 Julian Andres Klode <jak@debian.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 */

#include <Python.h>
#include "apt_pkgmodule.h"
#include "generic.h"
#include <apt-pkg/orderlist.h>

struct PyOrderList : CppPyObject<pkgOrderList*> {
    pkgCache::PkgIterator current;
    int nextIndex;
};

static PyObject *order_list_new(PyTypeObject *type,PyObject *args,
                                  PyObject *kwds)
{
    PyObject *pyDepCache = NULL;
    char *kwlist[] = {"depcache", NULL};
    if (PyArg_ParseTupleAndKeywords(args, kwds, "O!", kwlist,
                                    &PyDepCache_Type, &pyDepCache)
                                    == 0)
        return 0;

    pkgDepCache *depCache = PyDepCache_ToCpp(pyDepCache);
    return PyOrderList_FromCpp(new pkgOrderList(depCache), true, 
                               pyDepCache);
}

static const char order_list_append_doc[] =
    "append(pkg: Package)\n\n"
    "Append a package to the end of the list.";
static PyObject *order_list_append(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    PyObject *pyPackage = NULL;
    if (PyArg_ParseTuple(args, "O!", &PyPackage_Type, &pyPackage) == 0)
        return 0;

    list->push_back(PyPackage_ToCpp(pyPackage));
    Py_RETURN_NONE;
}

static const char order_list_score_doc[] =
    "score(pkg: Package) -> int\n\n"
    "Return the score of the package.";
static PyObject *order_list_score(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    PyObject *pyPackage = NULL;
    if (PyArg_ParseTuple(args, "O!", &PyPackage_Type, &pyPackage) == 0)
        return 0;

    return MkPyNumber(list->Score(PyPackage_ToCpp(pyPackage)));
}

static const char order_list_order_critical_doc[] =
    "order_critical()\n\n"
    "Order by PreDepends only (critical unpack order).";
static PyObject *order_list_order_critical(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    if (PyArg_ParseTuple(args, "") == 0)
        return 0;

    list->OrderCritical();
    
    Py_INCREF(Py_None);
    return HandleErrors(Py_None);
}

static const char order_list_order_unpack_doc[] =
    "order_unpack()\n\n"
    "Order the packages for unpacking (see Debian Policy).";
static PyObject *order_list_order_unpack(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    if (PyArg_ParseTuple(args, "") == 0)
        return 0;

    list->OrderUnpack();
    Py_INCREF(Py_None);
    return HandleErrors(Py_None);
}

static const char order_list_order_configure_doc[] =
    "order_configure()\n\n"
    "Order the packages for configuration (see Debian Policy).";
static PyObject *order_list_order_configure(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    if (PyArg_ParseTuple(args, "") == 0)
        return 0;

    list->OrderConfigure();
    
    Py_INCREF(Py_None);
    return HandleErrors(Py_None);
}

static bool valid_flags(unsigned int flags) {
	return (flags & ~pkgOrderList::Added
	              & ~pkgOrderList::AddPending
	              & ~pkgOrderList::Immediate
	              & ~pkgOrderList::Loop
	              & ~pkgOrderList::UnPacked
	              & ~pkgOrderList::Configured
	              & ~pkgOrderList::Removed
	              & ~pkgOrderList::InList
	              & ~pkgOrderList::After
	              & ~pkgOrderList::States) == 0;
}

static const char order_list_flag_doc[] =
    "flag(pkg: Package, flag: int[, unset_flags: int])\n\n"
    "Flag the package, set flags in 'flag' and remove flags in\n"
    "'unset_flags'.";
static PyObject *order_list_flag(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    
    PyObject *pyPkg = NULL;
    unsigned int flags = 0;
    unsigned int unset_flags = 0;
    if (PyArg_ParseTuple(args, "O!I|I", &PyPackage_Type, &pyPkg,
                         &flags, &unset_flags) == 0)
        return 0;

	if (!valid_flags(flags))
		return PyErr_Format(PyExc_ValueError, "flags (%u) is"
		                    " not a valid combination of flags.",
		                    flags);
	if (!valid_flags(unset_flags))
		return PyErr_Format(PyExc_ValueError, "unset_flags (%u) is"
		                    " not a valid combination of flags.",
		                    unset_flags);

    list->Flag(PyPackage_ToCpp(pyPkg), flags, unset_flags);
    
    Py_RETURN_NONE;
}

static const char order_list_is_flag_doc[] =
    "is_flag(pkg: Package, flag: int)\n\n"
    "Check if the flag(s) are set.";
static PyObject *order_list_is_flag(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    PyObject *pyPkg = NULL;
    unsigned int flags = 0;
    if (PyArg_ParseTuple(args, "O!I", &PyPackage_Type, &pyPkg,
                         &flags) == 0)
        return 0;

	if (!valid_flags(flags))
		return PyErr_Format(PyExc_ValueError, "flags (%u) is"
		                    " not a valid combination of flags.",
		                    flags);

    return PyBool_FromLong(list->IsFlag(PyPackage_ToCpp(pyPkg), flags));
}

static const char order_list_wipe_flags_doc[] =
    "wipe_flags(flags: int)\n\n"
    "Remove the flags in 'flags' from all packages in this list";
static PyObject *order_list_wipe_flags(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    unsigned int flags = 0;
    if (PyArg_ParseTuple(args, "I", &flags) == 0)
        return 0;

	if (!valid_flags(flags))
		return PyErr_Format(PyExc_ValueError, "flags (%u) is"
		                    " not a valid combination of flags.",
		                    flags);

    list->WipeFlags(flags);
    Py_RETURN_NONE;
}

static const char order_list_is_now_doc[] =
    "is_now(pkg: Package)\n\n"
    "Check if the package is flagged for any state but removal.";
static PyObject *order_list_is_now(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    PyObject *pyPkg = NULL;
    if (PyArg_ParseTuple(args, "O!", &PyPackage_Type, &pyPkg) == 0)
        return 0;

    return PyBool_FromLong(list->IsNow(PyPackage_ToCpp(pyPkg)));
}

static const char order_list_is_missing_doc[] =
    "is_now(pkg: Package)\n\n"
    "Check if the package is marked for install.";
static PyObject *order_list_is_missing(PyObject *self,PyObject *args)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    PyObject *pyPkg = NULL;
    if (PyArg_ParseTuple(args, "O!", &PyPackage_Type, &pyPkg) == 0)
        return 0;

    return PyBool_FromLong(list->IsMissing(PyPackage_ToCpp(pyPkg)));
}


#define METHOD(name) {#name, order_list_##name, METH_VARARGS,\
                      order_list_##name##_doc}

static PyMethodDef order_list_methods[] = {
	METHOD(append),
	METHOD(score),
	METHOD(order_critical),
	METHOD(order_unpack),
	METHOD(order_configure),
	METHOD(flag),
	METHOD(is_flag),
	METHOD(is_now),
	METHOD(is_missing),
	METHOD(wipe_flags),
	{}
};

static PyObject *order_list_seq_item(PyObject *self,Py_ssize_t index)
{
    pkgOrderList *list = GetCpp<pkgOrderList*>(self);
    PyObject *owner = GetOwner<pkgOrderList*>(self);
    PyObject *pycache = GetOwner<pkgOrderList*>(owner);
    pkgCache *cache = PyCache_ToCpp(pycache);

    if (index < 0 || index >= list->size())
        return PyErr_Format(PyExc_IndexError, "Out of range: %zd", index);

    return PyPackage_FromCpp(pkgCache::PkgIterator(*cache,
                                *(list->begin() + index)),
                             true, owner);
}

Py_ssize_t order_list_seq_length(PyObject *self)
{
    return GetCpp<pkgOrderList*>(self)->size();
}

static PySequenceMethods order_list_as_sequence =
{
   order_list_seq_length, // sq_length
   0,                     // sq_concat
   0,                     // sq_repeat
   order_list_seq_item,   // sq_item
   0,                     // sq_ass_item
   0,                     // sq_contains
   0,                     // sq_inplace_concat
   0                      // sq_inplace_repeat
};

static const char order_list_doc[] = "OrderList(depcache: DepCache)\n\n"
    "Sequence type for packages with special ordering methods.";
PyTypeObject PyOrderList_Type = {
    PyVarObject_HEAD_INIT(&PyType_Type, 0)
    "apt_pkg.OrderList",                 // tp_name
    sizeof(CppPyObject<pkgOrderList*>),  // tp_basicsize
    0,                                   // tp_itemsize
    // Methods
    CppDeallocPtr<pkgOrderList*>,        // tp_dealloc
    0,                                   // tp_print
    0,                                   // tp_getattr
    0,                                   // tp_setattr
    0,                                   // tp_compare
    0,                                   // tp_repr
    0,                                   // tp_as_number
    &order_list_as_sequence,             // tp_as_sequence
    0,                                   // tp_as_mapping
    0,                                   // tp_hash
    0,                                   // tp_call
    0,                                   // tp_str
    0,                                   // tp_getattro
    0,                                   // tp_setattro
    0,                                   // tp_as_buffer
    Py_TPFLAGS_DEFAULT,                  // tp_flags
    order_list_doc,                      // tp_doc
    0,                                   // tp_traverse
    0,                                   // tp_clear
    0,                                   // tp_richcompare
    0,                                   // tp_weaklistoffset
    0,                                   // tp_iter
    0,                                   // tp_iternext
    order_list_methods,                  // tp_methods
    0,                                   // tp_members
    0,                                   // tp_getset
    0,                                   // tp_base
    0,                                   // tp_dict
    0,                                   // tp_descr_get
    0,                                   // tp_descr_set
    0,                                   // tp_dictoffset
    0,                                   // tp_init
    0,                                   // tp_alloc
    order_list_new,                      // tp_new
};
