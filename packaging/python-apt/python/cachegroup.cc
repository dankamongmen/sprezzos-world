/*
 * cachegroup.cc - Wrapper around pkgCache::GrpIterator
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
#include <apt-pkg/pkgcache.h>

struct PyGroup : CppPyObject<pkgCache::GrpIterator> {
    pkgCache::PkgIterator current;
    int nextIndex;
};

static PyObject *group_new(PyTypeObject *type,PyObject *args,
                                  PyObject *kwds)
{
    PyObject *pyCache;
    char *name;
    char *kwlist[] = {"cache", "name", NULL};
    if (PyArg_ParseTupleAndKeywords(args, kwds, "O!s", kwlist,
                                    &PyCache_Type, &pyCache,
                                    &name) == 0)
        return 0;

    pkgCache *cache = GetCpp<pkgCache *>(pyCache);

    pkgCache::GrpIterator grp = cache->FindGrp(name);

    if (!grp.end()) {
        return PyGroup_FromCpp(grp, true, pyCache);
    } else {
        PyErr_SetString(PyExc_KeyError, name);
        return NULL;
    }
}

static const char group_find_package_doc[] =
    "find_package(architecture: str) -> Package\n\n"
    "Return a package for the given architecture, or None if none exists";
static PyObject *group_find_package(PyObject *self,PyObject *args)
{
    pkgCache::GrpIterator grp = GetCpp<pkgCache::GrpIterator>(self);
    PyObject *owner = GetOwner<pkgCache::GrpIterator>(self);
    
    char *architecture;
    if (PyArg_ParseTuple(args, "s", &architecture) == 0)
        return 0;

    pkgCache::PkgIterator pkg = grp.FindPkg(architecture);

    if (pkg.end()) {
        Py_RETURN_NONE;
    } else {
        return PyPackage_FromCpp(pkg, true, owner ? owner : self);
    }
}

static const char group_find_preferred_package_doc[] =
    "find_preferred_package(prefer_non_virtual: bool = True) -> Package\n\n"
    "Return a package for the best architecture, either the native one\n"
    "or the first found one. If none exists, return None. If non_virtual\n"
    "is True, prefer non-virtual packages over virtual ones.";
static PyObject *group_find_preferred_package(PyObject *self,PyObject *args,
                                              PyObject *kwds)
{
    pkgCache::GrpIterator grp = GetCpp<pkgCache::GrpIterator>(self);
    PyObject *owner = GetOwner<pkgCache::GrpIterator>(self);
    char nonvirtual = 1;
    char *kwlist[] = {"prefer_non_virtual", NULL};
    if (PyArg_ParseTupleAndKeywords(args, kwds, "|b", kwlist, &nonvirtual) == 0)
        return 0;
    pkgCache::PkgIterator pkg = grp.FindPreferredPkg(nonvirtual);

    if (pkg.end()) {
        Py_RETURN_NONE;
    } else {
        return PyPackage_FromCpp(pkg, true, owner);
    }
}

static PyMethodDef group_methods[] = {
    {"find_package",group_find_package,METH_VARARGS,group_find_package_doc},
    {"find_preferred_package",(PyCFunction) group_find_preferred_package,
     METH_VARARGS|METH_KEYWORDS,group_find_preferred_package_doc},
    {}
};

static PyObject *group_seq_item(PyObject *pySelf,Py_ssize_t index)
{
    PyGroup *self = static_cast<PyGroup *>(pySelf);
    pkgCache::GrpIterator grp = GetCpp<pkgCache::GrpIterator>(self);
    PyObject *owner = GetOwner<pkgCache::GrpIterator>(self);

    if (self->nextIndex > index || self->nextIndex == 0)  {
        self->nextIndex = 1;
        new (&self->current) pkgCache::PkgIterator(grp.PackageList());
    }
        
    if (self->nextIndex != index + 1) {
        while (self->nextIndex <= index && !self->current.end()) {
            self->current = grp.NextPkg(self->current);
            self->nextIndex++;
        }
    }

    if (self->current.end())
        return PyErr_Format(PyExc_IndexError, "Out of range: %zd", index);

    return PyPackage_FromCpp(self->current, true, owner);
}


static PySequenceMethods group_as_sequence =
{
   0,
   0,                // concat
   0,                // repeat
   group_seq_item,
   0,                // slice
   0,                // assign item
   0                 // assign slice
};


static const char group_doc[] = "Group(cache, name)\n\n"
    "Group of packages with the same name.\n\n"
    "Provides access to all packages sharing a name. Can be used this\n"
    "like a list, or by using the special find_*() methods. If you use\n"
    "it as a sequence, make sure to access it linearly, as this uses a\n"
    "linked list internally.";
PyTypeObject PyGroup_Type = {
    PyVarObject_HEAD_INIT(&PyType_Type, 0)
    "apt_pkg.Group",                     // tp_name
    sizeof(PyGroup),                     // tp_basicsize
    0,                                   // tp_itemsize
    // Methods
    CppDealloc<pkgCache::GrpIterator>,   // tp_dealloc
    0,                                   // tp_print
    0,                                   // tp_getattr
    0,                                   // tp_setattr
    0,                                   // tp_compare
    0,                                   // tp_repr
    0,                                   // tp_as_number
    &group_as_sequence,                  // tp_as_sequence
    0,                                   // tp_as_mapping
    0,                                   // tp_hash
    0,                                   // tp_call
    0,                                   // tp_str
    0,                                   // tp_getattro
    0,                                   // tp_setattro
    0,                                   // tp_as_buffer
    Py_TPFLAGS_DEFAULT,                  // tp_flags
    group_doc,                           // tp_doc
    0,                                   // tp_traverse
    0,                                   // tp_clear
    0,                                   // tp_richcompare
    0,                                   // tp_weaklistoffset
    0,                                   // tp_iter
    0,                                   // tp_iternext
    group_methods,                       // tp_methods
    0,                                   // tp_members
    0,                                   // tp_getset
    0,                                   // tp_base
    0,                                   // tp_dict
    0,                                   // tp_descr_get
    0,                                   // tp_descr_set
    0,                                   // tp_dictoffset
    0,                                   // tp_init
    0,                                   // tp_alloc
    group_new,                           // tp_new
};
