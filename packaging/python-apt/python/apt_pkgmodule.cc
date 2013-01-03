// -*- mode: cpp; mode: fold -*-
// Description								/*{{{*/
// $Id: apt_pkgmodule.cc,v 1.5 2003/07/23 02:20:24 mdz Exp $
/* ######################################################################

   apt_pkgmodule - Top level for the python module. Create the internal
                   structures for the module in the interpriter.

   ##################################################################### */
									/*}}}*/
// Include Files							/*{{{*/
#include "apt_pkgmodule.h"
#include "generic.h"

#include <apt-pkg/configuration.h>
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/packagemanager.h>
#include <apt-pkg/version.h>
#include <apt-pkg/deblistparser.h>
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/md5.h>
#include <apt-pkg/sha1.h>
#include <apt-pkg/sha256.h>
#include <apt-pkg/init.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/orderlist.h>
#include <apt-pkg/aptconfiguration.h>
#include <apt-pkg/fileutl.h>

#include <sys/stat.h>
#include <libintl.h>
#include <unistd.h>
#include <Python.h>
									/*}}}*/

/**
 * A Python->C->Python gettext() function.
 *
 * Python's gettext() ignores setlocale() which causes a strange behavior
 * because the values received from apt-pkg respect setlocale(). We circumvent
 * this problem by calling the C version of gettext(). This is also much
 * faster.
 */
static PyObject *py_gettext(PyObject *self, PyObject *Args) {
    const char *msg;
    char *domain = "python-apt";
    if (PyArg_ParseTuple(Args,"s|s:gettext",&msg, &domain) == 0)
        return 0;

    return PyString_FromString(dgettext(domain, msg));
}

// newConfiguration - Build a new configuration class			/*{{{*/
// ---------------------------------------------------------------------
#ifdef COMPAT_0_7
static char *doc_newConfiguration = "Construct a configuration instance";
static PyObject *newConfiguration(PyObject *self,PyObject *args)
{
   PyErr_WarnEx(PyExc_DeprecationWarning, "apt_pkg.newConfiguration() is "
                "deprecated. Use apt_pkg.Configuration() instead.", 1);
   return CppPyObject_NEW<Configuration*>(NULL, &PyConfiguration_Type, new Configuration());
}
#endif
									/*}}}*/

// Version Wrappers							/*{{{*/
// These are kind of legacy..
static char *doc_VersionCompare =
    "version_compare(a: str, b: str) -> int\n\n"
    "Compare the given versions; return -1 if 'a' is smaller than 'b',\n"
    "0 if they are equal, and 2 if 'a' is larger than 'b'.";
static PyObject *VersionCompare(PyObject *Self,PyObject *Args)
{
   char *A;
   char *B;
   int LenA;
   int LenB;

   if (PyArg_ParseTuple(Args,"s#s#",&A,&LenA,&B,&LenB) == 0)
      return 0;

   if (_system == 0)
   {
      PyErr_SetString(PyExc_ValueError,"_system not initialized");
      return 0;
   }

   return MkPyNumber(_system->VS->DoCmpVersion(A,A+LenA,B,B+LenB));
}

static char *doc_CheckDep =
    "check_dep(pkg_ver: str, dep_op: str, dep_ver: str) -> bool\n\n"
    "Check that the given requirement is fulfilled; i.e. that the version\n"
    "string given by 'pkg_ver' matches the version string 'dep_ver' under\n"
    "the condition specified by the operator 'dep_op' (<,<=,=,>=,>).\n\n"
    "Return True if 'pkg_ver' matches 'dep_ver' under the\n"
    "condition 'dep_op'; for example, this returns True:\n\n"
    "    apt_pkg.check_dep('1', '<=', '2')";
static PyObject *CheckDep(PyObject *Self,PyObject *Args)
{
   char *A;
   char *B;
   char *OpStr;
   unsigned int Op = 0;

   if (PyArg_ParseTuple(Args,"sss",&A,&OpStr,&B) == 0)
      return 0;

   if (strcmp(OpStr, ">") == 0) OpStr = ">>";
   if (strcmp(OpStr, "<") == 0) OpStr = "<<";
   if (*debListParser::ConvertRelation(OpStr,Op) != 0)
   {
      PyErr_SetString(PyExc_ValueError,"Bad comparision operation");
      return 0;
   }

   if (_system == 0)
   {
      PyErr_SetString(PyExc_ValueError,"_system not initialized");
      return 0;
   }

   return PyBool_FromLong(_system->VS->CheckDep(A,Op,B));
}

#ifdef COMPAT_0_7
static char *doc_CheckDepOld = "CheckDep(PkgVer,DepOp,DepVer) -> bool";
static PyObject *CheckDepOld(PyObject *Self,PyObject *Args)
{
   char *A;
   char *B;
   char *OpStr;
   unsigned int Op = 0;

   if (PyArg_ParseTuple(Args,"sss",&A,&OpStr,&B) == 0)
      return 0;
   if (*debListParser::ConvertRelation(OpStr,Op) != 0)
   {
      PyErr_SetString(PyExc_ValueError,"Bad comparision operation");
      return 0;
   }

   if (_system == 0)
   {
      PyErr_SetString(PyExc_ValueError,"_system not initialized");
      return 0;
   }

   return PyBool_FromLong(_system->VS->CheckDep(A,Op,B));
}
#endif

static char *doc_UpstreamVersion =
    "upstream_version(ver: str) -> str\n\n"
    "Return the upstream version for the package version given by 'ver'.";
static PyObject *UpstreamVersion(PyObject *Self,PyObject *Args)
{
   char *Ver;
   if (PyArg_ParseTuple(Args,"s",&Ver) == 0)
      return 0;
   return CppPyString(_system->VS->UpstreamVersion(Ver));
}

static const char *doc_ParseDepends =
"parse_depends(s: str) -> list\n"
"\n"
"Parse the dependencies given by 's' and return a list of lists. Each of\n"
"these lists represents one or more options for an 'or' dependency in\n"
"the form of '(pkg, ver, comptype)' tuples. The tuple element 'pkg'\n"
"is the name of the package; the element 'ver' is the version, or ''\n"
"if no version was requested. The element 'ver' is a comparison\n"
"operator ('<', '<=', '=', '>=', or '>').";

static const char *parse_src_depends_doc =
"parse_src_depends(s: str) -> list\n"
"\n"
"Parse the dependencies given by 's' and return a list of lists. Each of\n"
"these lists represents one or more options for an 'or' dependency in\n"
"the form of '(pkg, ver, comptype)' tuples. The tuple element 'pkg'\n"
"is the name of the package; the element 'ver' is the version, or ''\n"
"if no version was requested. The element 'ver' is a comparison\n"
"operator ('<', '<=', '=', '>=', or '>')."
"\n\n"
"Dependencies may be restricted to certain architectures and the result\n"
"only contains those dependencies for the architecture set in the\n"
"configuration variable APT::Architecture";
static PyObject *RealParseDepends(PyObject *Self,PyObject *Args,
                                  bool ParseArchFlags, std::string name,
                                  bool debStyle=false)
{
   std::string Package;
   std::string Version;
   unsigned int Op;
   bool StripMultiArch=true;

   const char *Start;
   const char *Stop;
   int Len;

   if (PyArg_ParseTuple(Args,(char *)("s#|b:" + name).c_str(), 
                        &Start, &Len, &StripMultiArch) == 0)
      return 0;
   Stop = Start + Len;
   PyObject *List = PyList_New(0);
   PyObject *LastRow = 0;
   while (1)
   {
      if (Start == Stop)
	 break;

      Start = debListParser::ParseDepends(Start,Stop,Package,Version,Op,
					  ParseArchFlags, StripMultiArch);
      if (Start == 0)
      {
	 PyErr_SetString(PyExc_ValueError,"Problem Parsing Dependency");
	 Py_DECREF(List);
	 return 0;
      }

      if (LastRow == 0)
	 LastRow = PyList_New(0);

      if (Package.empty() == false)
      {
	 PyObject *Obj;
	 PyList_Append(LastRow,Obj = Py_BuildValue("sss",Package.c_str(),
						   Version.c_str(),
						debStyle ? pkgCache::CompTypeDeb(Op) : pkgCache::CompType(Op)));
	 Py_DECREF(Obj);
      }

      // Group ORd deps into a single row..
      if ((Op & pkgCache::Dep::Or) != pkgCache::Dep::Or)
      {
	 if (PyList_Size(LastRow) != 0)
	    PyList_Append(List,LastRow);
	 Py_DECREF(LastRow);
	 LastRow = 0;
      }
   }
   return List;
}
static PyObject *ParseDepends(PyObject *Self,PyObject *Args)
{
   return RealParseDepends(Self, Args, false, "parse_depends");
}
static PyObject *ParseSrcDepends(PyObject *Self,PyObject *Args)
{
   return RealParseDepends(Self, Args, true, "parse_src_depends");
}
#ifdef COMPAT_0_7
static PyObject *ParseDepends_old(PyObject *Self,PyObject *Args)
{
   return RealParseDepends(Self, Args, false, "ParseDepends", true);
}
static PyObject *ParseSrcDepends_old(PyObject *Self,PyObject *Args)
{
   return RealParseDepends(Self, Args, true, "ParseSrcDepends", true);
}
#endif
									/*}}}*/
// md5sum - Compute the md5sum of a file or string			/*{{{*/
// ---------------------------------------------------------------------
static const char *doc_md5sum =
    "md5sum(object) -> str\n\n"
    "Return the md5sum of the object. 'object' may either be a string, in\n"
    "which case the md5sum of the string is returned, or a file() object\n"
    "(or file descriptor), in which case the md5sum of its contents is\n"
    "returned.";
static PyObject *md5sum(PyObject *Self,PyObject *Args)
{
   PyObject *Obj;
   if (PyArg_ParseTuple(Args,"O",&Obj) == 0)
      return 0;

   // Digest of a string.
   if (PyBytes_Check(Obj) != 0)
   {
      char *s;
      Py_ssize_t len;
      MD5Summation Sum;
      PyBytes_AsStringAndSize(Obj, &s, &len);
      Sum.Add((const unsigned char*)s, len);
      return CppPyString(Sum.Result().Value());
   }

   // Digest of a file
   int Fd = PyObject_AsFileDescriptor(Obj);
   if (Fd != -1)
   {
      MD5Summation Sum;
      struct stat St;
      if (fstat(Fd,&St) != 0 ||
	  Sum.AddFD(Fd,St.st_size) == false)
      {
	 PyErr_SetFromErrno(PyExc_SystemError);
	 return 0;
      }

      return CppPyString(Sum.Result().Value());
   }

   PyErr_SetString(PyExc_TypeError,"Only understand strings and files");
   return 0;
}
									/*}}}*/
// sha1sum - Compute the sha1sum of a file or string			/*{{{*/
// ---------------------------------------------------------------------
static const char *doc_sha1sum =
    "sha1sum(object) -> str\n\n"
    "Return the sha1sum of the object. 'object' may either be a string, in\n"
    "which case the sha1sum of the string is returned, or a file() object\n"
    "(or file descriptor), in which case the sha1sum of its contents is\n"
    "returned.";
static PyObject *sha1sum(PyObject *Self,PyObject *Args)
{
   PyObject *Obj;
   if (PyArg_ParseTuple(Args,"O",&Obj) == 0)
      return 0;

   // Digest of a string.
   if (PyBytes_Check(Obj) != 0)
   {
      char *s;
      Py_ssize_t len;
      SHA1Summation Sum;
      PyBytes_AsStringAndSize(Obj, &s, &len);
      Sum.Add((const unsigned char*)s, len);
      return CppPyString(Sum.Result().Value());
   }

   // Digest of a file
   int Fd = PyObject_AsFileDescriptor(Obj);
   if (Fd != -1)
   {
      SHA1Summation Sum;
      struct stat St;
      if (fstat(Fd,&St) != 0 ||
	  Sum.AddFD(Fd,St.st_size) == false)
      {
	 PyErr_SetFromErrno(PyExc_SystemError);
	 return 0;
      }

      return CppPyString(Sum.Result().Value());
   }

   PyErr_SetString(PyExc_TypeError,"Only understand strings and files");
   return 0;
}
									/*}}}*/
// sha256sum - Compute the sha1sum of a file or string			/*{{{*/
// ---------------------------------------------------------------------
static const char *doc_sha256sum =
    "sha256sum(object) -> str\n\n"
    "Return the sha256sum of the object. 'object' may either be a string, in\n"
    "which case the sha256sum of the string is returned, or a file() object\n"
    "(or file descriptor), in which case the sha256sum of its contents is\n"
    "returned.";;
static PyObject *sha256sum(PyObject *Self,PyObject *Args)
{
   PyObject *Obj;
   if (PyArg_ParseTuple(Args,"O",&Obj) == 0)
      return 0;

   // Digest of a string.
   if (PyBytes_Check(Obj) != 0)
   {
      char *s;
      Py_ssize_t len;
      SHA256Summation Sum;
      PyBytes_AsStringAndSize(Obj, &s, &len);
      Sum.Add((const unsigned char*)s, len);
      return CppPyString(Sum.Result().Value());
   }

   // Digest of a file
   int Fd = PyObject_AsFileDescriptor(Obj);
   if (Fd != -1)
   {
      SHA256Summation Sum;
      struct stat St;
      if (fstat(Fd,&St) != 0 ||
	  Sum.AddFD(Fd,St.st_size) == false)
      {
	 PyErr_SetFromErrno(PyExc_SystemError);
	 return 0;
      }

      return CppPyString(Sum.Result().Value());
   }

   PyErr_SetString(PyExc_TypeError,"Only understand strings and files");
   return 0;
}
									/*}}}*/
// get_architectures - return the list of architectures			/*{{{*/
// ---------------------------------------------------------------------
static const char *doc_GetArchitectures =
    "get_architectures() -> list\n\n"
    "Return the list of supported architectures on this system. On a \n"
    "multiarch system this can be more than one. The main architectures\n"
    "is the first item in the list.";;
static PyObject *GetArchitectures(PyObject *Self,PyObject *Args)
{
   PyObject *Obj;
   if (PyArg_ParseTuple(Args,"",&Obj) == 0)
      return 0;

   PyObject *List = PyList_New(0);
   std::vector<std::string> arches = APT::Configuration::getArchitectures();
   std::vector<std::string>::const_iterator I;
   for (I = arches.begin(); I != arches.end(); I++) 
   {
      PyList_Append(List, CppPyString(*I));
   }

   return List;
}
									/*}}}*/
// init - 3 init functions						/*{{{*/
// ---------------------------------------------------------------------
static char *doc_Init =
"init()\n\n"
"Shorthand for doing init_config() and init_system(). When working\n"
"with command line arguments, first call init_config() then parse\n"
"the command line and finally call init_system().";
static PyObject *Init(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   pkgInitConfig(*_config);
   pkgInitSystem(*_config,_system);

   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}

static char *doc_InitConfig =
"init_config()\n\n"
"Load the default configuration and the config file.";
static PyObject *InitConfig(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   pkgInitConfig(*_config);

   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}

static char *doc_InitSystem =
"init_system()\n\n"
"Construct the apt_pkg system.";
static PyObject *InitSystem(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   pkgInitSystem(*_config,_system);

   Py_INCREF(Py_None);
   return HandleErrors(Py_None);
}
									/*}}}*/

// fileutils.cc: GetLock						/*{{{*/
// ---------------------------------------------------------------------
static char *doc_GetLock =
"get_lock(file: str, errors: bool) -> int\n\n"
"Create an empty file of the given name and lock it. If the locking\n"
"succeeds, return the file descriptor of the lock file. Afterwards,\n"
"locking the file from another process will fail and thus cause\n"
"get_lock() to return -1 or raise an Error (if 'errors' is True).\n\n"
"From Python 2.6 on, it is recommended to use the context manager\n"
"provided by apt_pkg.FileLock instead using the with-statement.";
static PyObject *GetLock(PyObject *Self,PyObject *Args)
{
   const char *file;
   char errors = false;
   if (PyArg_ParseTuple(Args,"s|b",&file,&errors) == 0)
      return 0;

   int fd = GetLock(file, errors);

   return HandleErrors(MkPyNumber(fd));
}

static char *doc_PkgSystemLock =
"pkgsystem_lock() -> bool\n\n"
"Acquire the global lock for the package system by using /var/lib/dpkg/lock\n"
"to do the locking. From Python 2.6 on, the apt_pkg.SystemLock context\n"
"manager is available and should be used instead.";
static PyObject *PkgSystemLock(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   bool res = _system->Lock();

   Py_INCREF(Py_None);
   return HandleErrors(PyBool_FromLong(res));
}

static char *doc_PkgSystemUnLock =
"pkgsystem_unlock() -> bool\n\n"
"Release the global lock for the package system.";
static PyObject *PkgSystemUnLock(PyObject *Self,PyObject *Args)
{
   if (PyArg_ParseTuple(Args,"") == 0)
      return 0;

   bool res = _system->UnLock();

   Py_INCREF(Py_None);
   return HandleErrors(PyBool_FromLong(res));
}

									/*}}}*/

// initapt_pkg - Core Module Initialization				/*{{{*/
// ---------------------------------------------------------------------
/* */
static PyMethodDef methods[] =
{
   // Constructors
   {"init",Init,METH_VARARGS,doc_Init},
   {"init_config",InitConfig,METH_VARARGS,doc_InitConfig},
   {"init_system",InitSystem,METH_VARARGS,doc_InitSystem},

   // Internationalization.
   {"gettext",py_gettext,METH_VARARGS,
    "gettext(msg: str[, domain: str = 'python-apt']) -> str\n\n"
    "Translate the given string. This is much faster than Python's version\n"
    "and only does translations after setlocale() has been called."},

   // Tag File
   {"rewrite_section",RewriteSection,METH_VARARGS,doc_RewriteSection},

   // Locking
   {"get_lock",GetLock,METH_VARARGS,doc_GetLock},
   {"pkgsystem_lock",PkgSystemLock,METH_VARARGS,doc_PkgSystemLock},
   {"pkgsystem_unlock",PkgSystemUnLock,METH_VARARGS,doc_PkgSystemUnLock},

   // Command line
   {"read_config_file",LoadConfig,METH_VARARGS,doc_LoadConfig},
   {"read_config_dir",LoadConfigDir,METH_VARARGS,doc_LoadConfigDir},
   {"read_config_file_isc",LoadConfigISC,METH_VARARGS,doc_LoadConfig},
   {"parse_commandline",ParseCommandLine,METH_VARARGS,doc_ParseCommandLine},

   // Versioning
   {"version_compare",VersionCompare,METH_VARARGS,doc_VersionCompare},
   {"check_dep",CheckDep,METH_VARARGS,doc_CheckDep},
   {"upstream_version",UpstreamVersion,METH_VARARGS,doc_UpstreamVersion},

   // Depends
   {"parse_depends",ParseDepends,METH_VARARGS,doc_ParseDepends},
   {"parse_src_depends",ParseSrcDepends,METH_VARARGS,parse_src_depends_doc},

   // Stuff
   {"md5sum",md5sum,METH_VARARGS,doc_md5sum},
   {"sha1sum",sha1sum,METH_VARARGS,doc_sha1sum},
   {"sha256sum",sha256sum,METH_VARARGS,doc_sha256sum},

   // multiarch
   {"get_architectures", GetArchitectures, METH_VARARGS, doc_GetArchitectures},

   // Strings
   {"check_domain_list",StrCheckDomainList,METH_VARARGS,
    "check_domain_list(host: str, domains: str) -> bool\n\n"
    "Check if the host given by 'host' belongs to one of the domains\n"
    "specified in the comma separated string 'domains'. An example\n"
    "would be:\n\n"
    "    check_domain_list('alioth.debian.org','debian.net,debian.org')\n\n"
    "which would return True because alioth belongs to debian.org."},
   {"quote_string",StrQuoteString,METH_VARARGS,
    "quote_string(string: str, repl: str) -> str\n\n"
    "Escape the string 'string', replacing any character not allowed in a URL"
    "or specified by 'repl' with its ASCII value preceded by a percent sign"
    "(so for example ' ' becomes '%20')."},
   {"dequote_string",StrDeQuote,METH_VARARGS,
    "dequote_string(string: str) -> str\n\n"
    "Dequote the given string by replacing all HTTP encoded values such\n"
    "as '%20' with their decoded value (in this case, ' ')."},
   {"size_to_str",StrSizeToStr,METH_VARARGS,
    "size_to_str(bytes: int) -> str\n\n"
    "Return a string describing the size in a human-readable manner using\n"
    "SI prefix and base-10 units, e.g. '1k' for 1000, '1M' for 1000000, etc."},
   {"time_to_str",StrTimeToStr,METH_VARARGS,
    "time_to_str(seconds: int) -> str\n\n"
    "Return a string describing the number of seconds in a human\n"
    "readable manner using days, hours, minutes and seconds."},
   {"uri_to_filename",StrURItoFileName,METH_VARARGS,
    "uri_to_filename(uri: str) -> str\n\n"
    "Return a filename based on the given URI after replacing some\n"
    "parts not suited for filenames (e.g. '/')."},
   {"base64_encode",StrBase64Encode,METH_VARARGS,
    "base64_encode(value: bytes) -> str\n\n"
    "Encode the given bytestring into Base64. The input may not\n"
    "contain a null byte character (use the base64 module for this)."},
   {"string_to_bool",StrStringToBool,METH_VARARGS,
    "string_to_bool(string: str) -> int\n\n"
    "Return 1 if the string is a value such as 'yes', 'true', '1';\n"
    "0 if the string is a value such as 'no', 'false', '0'; -1 if\n"
    "the string is not recognized."},
   {"time_rfc1123",StrTimeRFC1123,METH_VARARGS,
    "time_rfc1123(unixtime: int) -> str\n\n"
    "Format the given Unix time according to the requirements of\n"
    "RFC 1123."},
   {"str_to_time",StrStrToTime,METH_VARARGS,
    "str_to_time(rfc_time: str) -> int\n\n"
    "Convert the given RFC 1123 formatted string to a Unix timestamp."},

   // DEPRECATED
   #ifdef COMPAT_0_7
   {"CheckDep",CheckDepOld,METH_VARARGS,doc_CheckDepOld},
   {"newConfiguration",newConfiguration,METH_VARARGS,doc_newConfiguration},
   {"InitConfig",InitConfig,METH_VARARGS,doc_InitConfig},
   {"InitSystem",InitSystem,METH_VARARGS,doc_InitSystem},

   {"ParseSection",ParseSection,METH_VARARGS,doc_ParseSection},
   {"ParseTagFile",ParseTagFile,METH_VARARGS,doc_ParseTagFile},
   {"RewriteSection",RewriteSection,METH_VARARGS,doc_RewriteSection},

   {"GetLock",GetLock,METH_VARARGS,doc_GetLock},
   {"PkgSystemLock",PkgSystemLock,METH_VARARGS,doc_PkgSystemLock},
   {"PkgSystemUnLock",PkgSystemUnLock,METH_VARARGS,doc_PkgSystemUnLock},

   {"ReadConfigFile",LoadConfig,METH_VARARGS,doc_LoadConfig},
   {"ReadConfigDir",LoadConfigDir,METH_VARARGS,doc_LoadConfigDir},
   {"ReadConfigFileISC",LoadConfigISC,METH_VARARGS,doc_LoadConfig},
   {"ParseCommandLine",ParseCommandLine,METH_VARARGS,doc_ParseCommandLine},

   {"VersionCompare",VersionCompare,METH_VARARGS,doc_VersionCompare},
   {"UpstreamVersion",UpstreamVersion,METH_VARARGS,doc_UpstreamVersion},

   {"ParseDepends",ParseDepends_old,METH_VARARGS,doc_ParseDepends},
   {"ParseSrcDepends",ParseSrcDepends_old,METH_VARARGS,doc_ParseDepends},

   {"CheckDomainList",StrCheckDomainList,METH_VARARGS,
    "CheckDomainList(String,String) -> Bool"},
   {"QuoteString",StrQuoteString,METH_VARARGS,"QuoteString(String,String) -> String"},
   {"DeQuoteString",StrDeQuote,METH_VARARGS,"DeQuoteString(String) -> String"},
   {"SizeToStr",StrSizeToStr,METH_VARARGS,"SizeToStr(int) -> String"},
   {"TimeToStr",StrTimeToStr,METH_VARARGS,"TimeToStr(int) -> String"},
   {"URItoFileName",StrURItoFileName,METH_VARARGS,"URItoFileName(String) -> String"},
   {"Base64Encode",StrBase64Encode,METH_VARARGS,"Base64Encode(String) -> String"},
   {"StringToBool",StrStringToBool,METH_VARARGS,"StringToBool(String) -> int"},
   {"TimeRFC1123",StrTimeRFC1123,METH_VARARGS,"TimeRFC1123(int) -> String"},
   {"StrToTime",StrStrToTime,METH_VARARGS,"StrToTime(String) -> Int"},

   {"GetCache",TmpGetCache,METH_VARARGS,"GetCache() -> PkgCache"},
   {"GetDepCache",GetDepCache,METH_VARARGS,"GetDepCache(Cache) -> DepCache"},
   {"GetPkgRecords",GetPkgRecords,METH_VARARGS,"GetPkgRecords(Cache) -> PkgRecords"},
   {"GetPkgSrcRecords",GetPkgSrcRecords,METH_VARARGS,"GetPkgSrcRecords() -> PkgSrcRecords"},
   {"GetPkgSourceList",GetPkgSourceList,METH_VARARGS,"GetPkgSourceList() -> PkgSourceList"},
   {"GetPkgProblemResolver",GetPkgProblemResolver,METH_VARARGS,"GetDepProblemResolver(DepCache) -> PkgProblemResolver"},
   {"GetPkgActionGroup",GetPkgActionGroup,METH_VARARGS,"GetPkgActionGroup(DepCache) -> PkgActionGroup"},
   {"GetCdrom",GetCdrom,METH_VARARGS,"GetCdrom() -> Cdrom"},
   {"GetAcquire",GetAcquire,METH_VARARGS,"GetAcquire() -> Acquire"},
   {"GetPkgAcqFile",(PyCFunction)GetPkgAcqFile,METH_KEYWORDS|METH_VARARGS, doc_GetPkgAcqFile},
   {"GetPackageManager",GetPkgManager,METH_VARARGS,"GetPackageManager(DepCache) -> PackageManager"},
   #endif

   {}
};

static struct _PyAptPkgAPIStruct API = {
   &PyAcquire_Type,           // acquire_type
   &PyAcquire_FromCpp,        // acquire_fromcpp
   &PyAcquire_ToCpp,          // acquire_tocpp
   &PyAcquireFile_Type,       // acquirefile_type
   &PyAcquireFile_FromCpp,    // acquirefile_fromcpp
   &PyAcquireFile_ToCpp,      // acquirefile_tocpp
   &PyAcquireItem_Type,       // acquireitem_type
   &PyAcquireItem_FromCpp,    // acquireitem_fromcpp
   &PyAcquireItem_ToCpp,      // acquireitem_type
   &PyAcquireItemDesc_Type,   // acquireitemdesc_type
   &PyAcquireItemDesc_FromCpp,// acquireitemdesc_fromcpp
   &PyAcquireItemDesc_ToCpp,  // acquireitemdesc_tocpp
   &PyAcquireWorker_Type,     // acquireworker_type
   &PyAcquireWorker_FromCpp,  // acquireworker_fromcpp
   &PyAcquireWorker_ToCpp,    // acquireworker_tocpp
   &PyActionGroup_Type,       // actiongroup_type
   &PyActionGroup_FromCpp,    // actiongroup_fromcpp
   &PyActionGroup_ToCpp,      // actiongroup_tocpp
   &PyCache_Type,             // cache_type
   &PyCache_FromCpp,          // cache_fromcpp
   &PyCache_ToCpp,            // cache_tocpp
   &PyCacheFile_Type,         // cachefile_type
   &PyCacheFile_FromCpp,      // cachefile_fromcpp
   &PyCacheFile_ToCpp,        // cachefile_tocpp
   &PyCdrom_Type,             // cdrom_type
   &PyCdrom_FromCpp,          // cdrom_fromcpp
   &PyCdrom_ToCpp,            // cdrom_tocpp
   &PyConfiguration_Type,     // configuration_type
   &PyConfiguration_FromCpp,  // configuration_fromcpp
   &PyConfiguration_ToCpp,    // configuration_tocpp
   &PyDepCache_Type,          // depcache_type
   &PyDepCache_FromCpp,       // depcache_fromcpp
   &PyDepCache_ToCpp,         // depcache_tocpp
   &PyDependency_Type,        // dependency_type
   &PyDependency_FromCpp,     // dependency_fromcpp
   &PyDependency_ToCpp,       // dependency_tocpp
   &PyDependencyList_Type,    // dependencylist_type
   0,                         // FIXME: dependencylist_fromcpp
   0,                         // FIXME: dependencylist_tocpp
   &PyDescription_Type,       // description_type
   &PyDescription_FromCpp,    // description_fromcpp
   &PyDescription_ToCpp,      // description_tocpp
   &PyHashes_Type,            // hashes_type
   &PyHashes_FromCpp,         // hashes_fromcpp
   &PyHashes_ToCpp,           // hashes_tocpp
   &PyHashString_Type,        // hashstring_type
   &PyHashString_FromCpp,     // hashstring_fromcpp
   &PyHashString_ToCpp,       // hashstring_tocpp
   &PyIndexRecords_Type,      // indexrecords_type
   &PyIndexRecords_FromCpp,     // indexrecords_tocpp
   &PyIndexRecords_ToCpp,     // indexrecords_tocpp
   &PyMetaIndex_Type,         // metaindex_type
   &PyMetaIndex_FromCpp,        // metaindex_tocpp
   &PyMetaIndex_ToCpp,        // metaindex_tocpp
   &PyPackage_Type,           // package_type
   &PyPackage_FromCpp,          // package_tocpp
   &PyPackage_ToCpp,          // package_tocpp
   &PyPackageFile_Type,       // packagefile_type
   &PyPackageFile_FromCpp,      // packagefile_tocpp
   &PyPackageFile_ToCpp,      // packagefile_tocpp
   &PyIndexFile_Type,         // packageindexfile_type
   &PyIndexFile_FromCpp,        // packageindexfile_tocpp
   &PyIndexFile_ToCpp,        // packageindexfile_tocpp
   &PyPackageList_Type,       // packagelist_type
   0,                         // FIXME: packagelist_fromcpp
   0,                         // FIXME: packagelist_tocpp
   &PyPackageManager_Type,    // packagemanager_type
   &PyPackageManager_FromCpp,   // packagemanager_type
   &PyPackageManager_ToCpp,   // packagemanager_type
   &PyPackageRecords_Type,    // packagerecords_type
   0,                         // FIXME: packagerecords_fromcpp
   0,                         // FIXME: packagerecords_tocpp
   &PyPolicy_Type,            // policy_type
   &PyPolicy_FromCpp,           // policy_tocpp
   &PyPolicy_ToCpp,           // policy_tocpp
   &PyProblemResolver_Type,   // problemresolver_type
   &PyProblemResolver_FromCpp,  // problemresolver_tocpp
   &PyProblemResolver_ToCpp,  // problemresolver_tocpp
   &PySourceList_Type,        // sourcelist_type
   &PySourceList_FromCpp,       // sourcelist_tocpp
   &PySourceList_ToCpp,       // sourcelist_tocpp
   &PySourceRecords_Type,     // sourcerecords_type
   0,                         // FIXME: sourcerecords_fromcpp
   0,                         // FIXME: sourcerecords_tocpp
   &PyTagFile_Type,           // tagfile_type
   &PyTagFile_FromCpp,          // tagfile_tocpp
   &PyTagFile_ToCpp,          // tagfile_tocpp
   &PyTagSection_Type,        // tagsection_type
   &PyTagSection_FromCpp,       // tagsection_tocpp
   &PyTagSection_ToCpp,       // tagsection_tocpp
   &PyVersion_Type,           // version_type
   &PyVersion_FromCpp,          // version_tocpp
   &PyVersion_ToCpp,          // version_tocpp
   &PyGroup_Type,             // group_type
   &PyGroup_FromCpp,          // group_fromcpp
   &PyGroup_ToCpp,            // group_tocpp
   &PyOrderList_Type,         // orderlist_type
   &PyOrderList_FromCpp,      // orderlist_fromcpp
   &PyOrderList_ToCpp         // orderlist_tocpp
};


#define ADDTYPE(mod,name,type) { \
    if (PyType_Ready(type) == -1) INIT_ERROR; \
    Py_INCREF(type); \
    PyModule_AddObject(mod,name,(PyObject *)type); }


static const char *apt_pkg_doc =
    "Classes and functions wrapping the apt-pkg library.\n\n"
    "The apt_pkg module provides several classes and functions for accessing\n"
    "the functionality provided by the apt-pkg library. Typical uses might\n"
    "include reading APT index files and configuration files and installing\n"
    "or removing packages.";

#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef moduledef = {
        PyModuleDef_HEAD_INIT,
        "apt_pkg",
        apt_pkg_doc,
        -1,
        methods,
        0,
        0,
        0,
        0,
};

#define INIT_ERROR return 0
extern "C" PyObject * PyInit_apt_pkg()
#else
#define INIT_ERROR return
extern "C" void initapt_pkg()
#endif
{
   // Finalize our types to add slots, etc.
   if (PyType_Ready(&PyConfiguration_Type) == -1) INIT_ERROR;
   if (PyType_Ready(&PyCacheFile_Type) == -1) INIT_ERROR;

   // Initialize the module
   #if PY_MAJOR_VERSION >= 3
   PyObject *Module = PyModule_Create(&moduledef);
   #else
   PyObject *Module = Py_InitModule3("apt_pkg",methods, apt_pkg_doc);
   #endif

   // Global variable linked to the global configuration class
   CppPyObject<Configuration*> *Config = CppPyObject_NEW<Configuration*>(NULL, &PyConfiguration_Type);
   Config->Object = _config;
   // Global configuration, should never be deleted.
   Config->NoDelete = true;
   PyModule_AddObject(Module,"config",Config);
   #ifdef COMPAT_0_7
   Py_INCREF(Config);
   PyModule_AddObject(Module,"Config",Config);
   #endif




   // Add our classes.
   /* ============================ tag.cc ============================ */
   ADDTYPE(Module,"TagSection",&PyTagSection_Type);
   ADDTYPE(Module,"TagFile",&PyTagFile_Type);
   /* ============================ acquire.cc ============================ */
   ADDTYPE(Module,"Acquire",&PyAcquire_Type);
   ADDTYPE(Module,"AcquireFile",&PyAcquireFile_Type);
   ADDTYPE(Module,"AcquireItem",&PyAcquireItem_Type); // NO __new__()
   ADDTYPE(Module,"AcquireWorker",&PyAcquireWorker_Type); // NO __new__()
   /* ============================ cache.cc ============================ */
   ADDTYPE(Module,"Cache",&PyCache_Type);
   ADDTYPE(Module,"Dependency",&PyDependency_Type); // NO __new__()
   ADDTYPE(Module,"Description",&PyDescription_Type); // NO __new__()
   ADDTYPE(Module,"PackageFile",&PyPackageFile_Type); // NO __new__()
   ADDTYPE(Module,"PackageList",&PyPackageList_Type);  // NO __new__(), internal
   ADDTYPE(Module,"DependencyList",&PyDependencyList_Type); // NO __new__(), internal
   ADDTYPE(Module,"Package",&PyPackage_Type); // NO __new__()
   ADDTYPE(Module,"Version",&PyVersion_Type); // NO __new__()
   ADDTYPE(Module,"Group", &PyGroup_Type);
   ADDTYPE(Module,"GroupList", &PyGroupList_Type);
   /* ============================ cdrom.cc ============================ */
   ADDTYPE(Module,"Cdrom",&PyCdrom_Type);
   /* ========================= configuration.cc ========================= */
   ADDTYPE(Module,"Configuration",&PyConfiguration_Type);
   /* ========================= depcache.cc ========================= */
   ADDTYPE(Module,"ActionGroup",&PyActionGroup_Type);
   ADDTYPE(Module,"DepCache",&PyDepCache_Type);
   ADDTYPE(Module,"ProblemResolver",&PyProblemResolver_Type);
   /* ========================= indexfile.cc ========================= */
   ADDTYPE(Module,"IndexFile",&PyIndexFile_Type); // NO __new__()
   /* ========================= metaindex.cc ========================= */
   ADDTYPE(Module,"MetaIndex",&PyMetaIndex_Type); // NO __new__()
   /* ========================= pkgmanager.cc ========================= */
   ADDTYPE(Module,"_PackageManager",&PyPackageManager_Type);
   ADDTYPE(Module,"PackageManager",&PyPackageManager2_Type);
   /* ========================= pkgrecords.cc ========================= */
   ADDTYPE(Module,"PackageRecords",&PyPackageRecords_Type);
   /* ========================= pkgsrcrecords.cc ========================= */
   ADDTYPE(Module,"SourceRecords",&PySourceRecords_Type);
   /* ========================= sourcelist.cc ========================= */
   ADDTYPE(Module,"SourceList",&PySourceList_Type);
   ADDTYPE(Module,"IndexRecords",&PyIndexRecords_Type);
   ADDTYPE(Module,"HashString",&PyHashString_Type);
   ADDTYPE(Module,"Policy",&PyPolicy_Type);
   ADDTYPE(Module,"Hashes",&PyHashes_Type);
   ADDTYPE(Module,"AcquireItemDesc",&PyAcquireItemDesc_Type);
   ADDTYPE(Module,"SystemLock",&PySystemLock_Type);
   ADDTYPE(Module,"FileLock",&PyFileLock_Type);
   ADDTYPE(Module,"OrderList",&PyOrderList_Type);
   // Tag file constants
   PyModule_AddObject(Module,"REWRITE_PACKAGE_ORDER",
                      CharCharToList(TFRewritePackageOrder));

   PyModule_AddObject(Module,"REWRITE_SOURCE_ORDER",
                      CharCharToList(TFRewriteSourceOrder));

   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_ADDED", MkPyNumber(pkgOrderList::Added));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_ADD_PENDIG", MkPyNumber(pkgOrderList::AddPending));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_IMMEDIATE", MkPyNumber(pkgOrderList::Immediate));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_LOOP", MkPyNumber(pkgOrderList::Loop));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_UNPACKED", MkPyNumber(pkgOrderList::UnPacked));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_CONFIGURED", MkPyNumber(pkgOrderList::Configured));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_REMOVED", MkPyNumber(pkgOrderList::Removed));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_IN_LIST", MkPyNumber(pkgOrderList::InList));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_AFTER", MkPyNumber(pkgOrderList::After));
   PyDict_SetItemString(PyOrderList_Type.tp_dict, "FLAG_STATES_MASK", MkPyNumber(pkgOrderList::States));

   // Acquire constants.
   // some constants
   PyDict_SetItemString(PyAcquire_Type.tp_dict, "RESULT_CANCELLED",
                        MkPyNumber(pkgAcquire::Cancelled));
   PyDict_SetItemString(PyAcquire_Type.tp_dict, "RESULT_CONTINUE",
                        MkPyNumber(pkgAcquire::Continue));
   PyDict_SetItemString(PyAcquire_Type.tp_dict, "RESULT_FAILED",
                        MkPyNumber(pkgAcquire::Failed));
#ifdef COMPAT_0_7
   PyDict_SetItemString(PyAcquire_Type.tp_dict, "ResultCancelled",
                        MkPyNumber(pkgAcquire::Cancelled));
   PyDict_SetItemString(PyAcquire_Type.tp_dict, "ResultContinue",
                        MkPyNumber(pkgAcquire::Continue));
   PyDict_SetItemString(PyAcquire_Type.tp_dict, "ResultFailed",
                        MkPyNumber(pkgAcquire::Failed));
#endif
    // Dependency constants
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_DEPENDS",
                        MkPyNumber(pkgCache::Dep::Depends));
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_PREDEPENDS",
                        MkPyNumber(pkgCache::Dep::PreDepends));
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_SUGGESTS",
                        MkPyNumber(pkgCache::Dep::Suggests));
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_RECOMMENDS",
                        MkPyNumber(pkgCache::Dep::Recommends));
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_CONFLICTS",
                        MkPyNumber(pkgCache::Dep::Conflicts));
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_REPLACES",
                        MkPyNumber(pkgCache::Dep::Replaces));
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_OBSOLETES",
                        MkPyNumber(pkgCache::Dep::Obsoletes));
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_DPKG_BREAKS",
                        MkPyNumber(pkgCache::Dep::DpkgBreaks));
   PyDict_SetItemString(PyDependency_Type.tp_dict, "TYPE_ENHANCES",
                        MkPyNumber(pkgCache::Dep::Enhances));


   // PackageManager constants
   PyDict_SetItemString(PyPackageManager_Type.tp_dict, "RESULT_COMPLETED",
                        MkPyNumber(pkgPackageManager::Completed));
   PyDict_SetItemString(PyPackageManager_Type.tp_dict, "RESULT_FAILED",
                        MkPyNumber(pkgPackageManager::Failed));
   PyDict_SetItemString(PyPackageManager_Type.tp_dict, "RESULT_INCOMPLETE",
                        MkPyNumber(pkgPackageManager::Incomplete));

#ifdef COMPAT_0_7
   PyDict_SetItemString(PyPackageManager_Type.tp_dict, "ResultCompleted",
                        MkPyNumber(pkgPackageManager::Completed));
   PyDict_SetItemString(PyPackageManager_Type.tp_dict, "ResultFailed",
                        MkPyNumber(pkgPackageManager::Failed));
   PyDict_SetItemString(PyPackageManager_Type.tp_dict, "ResultIncomplete",
                        MkPyNumber(pkgPackageManager::Incomplete));
#endif

   PyDict_SetItemString(PyVersion_Type.tp_dict, "MULTI_ARCH_NONE",
                        MkPyNumber(pkgCache::Version::None));
   PyDict_SetItemString(PyVersion_Type.tp_dict, "MULTI_ARCH_ALL",
                        MkPyNumber(pkgCache::Version::All));
   PyDict_SetItemString(PyVersion_Type.tp_dict, "MULTI_ARCH_FOREIGN",
                        MkPyNumber(pkgCache::Version::Foreign));
   PyDict_SetItemString(PyVersion_Type.tp_dict, "MULTI_ARCH_SAME",
                        MkPyNumber(pkgCache::Version::Same));
   PyDict_SetItemString(PyVersion_Type.tp_dict, "MULTI_ARCH_ALLOWED",
                        MkPyNumber(pkgCache::Version::Allowed));
   PyDict_SetItemString(PyVersion_Type.tp_dict, "MULTI_ARCH_ALL_FOREIGN",
                        MkPyNumber(pkgCache::Version::AllForeign));
   PyDict_SetItemString(PyVersion_Type.tp_dict, "MULTI_ARCH_ALL_ALLOWED",
                        MkPyNumber(pkgCache::Version::AllAllowed));
   // AcquireItem Constants.
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "STAT_IDLE",
                        MkPyNumber(pkgAcquire::Item::StatIdle));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "STAT_FETCHING",
                        MkPyNumber(pkgAcquire::Item::StatFetching));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "STAT_DONE",
                        MkPyNumber(pkgAcquire::Item::StatDone));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "STAT_TRANSIENT_NETWORK_ERROR",
                        MkPyNumber(pkgAcquire::Item::StatTransientNetworkError));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "STAT_ERROR",
                        MkPyNumber(pkgAcquire::Item::StatError));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "STAT_AUTH_ERROR",
                        MkPyNumber(pkgAcquire::Item::StatAuthError));

#ifdef COMPAT_0_7
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "StatIdle",
                        MkPyNumber(pkgAcquire::Item::StatIdle));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "StatFetching",
                        MkPyNumber(pkgAcquire::Item::StatFetching));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "StatDone",
                        MkPyNumber(pkgAcquire::Item::StatDone));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "StatError",
                        MkPyNumber(pkgAcquire::Item::StatError));
   PyDict_SetItemString(PyAcquireItem_Type.tp_dict, "StatAuthError",
                        MkPyNumber(pkgAcquire::Item::StatAuthError));
#endif

#if PY_MAJOR_VERSION >= 3 && PY_MINOR_VERSION >= 1
   PyObject *PyCapsule = PyCapsule_New(&API, "apt_pkg._C_API", NULL);
#else
   PyObject *PyCapsule = PyCObject_FromVoidPtr(&API, NULL);
#endif
   PyModule_AddObject(Module, "_C_API", PyCapsule);
   // Version..
   PyModule_AddStringConstant(Module,"VERSION",(char *)pkgVersion);
   PyModule_AddStringConstant(Module,"LIB_VERSION",(char *)pkgLibVersion);
   PyModule_AddStringConstant(Module,"DATE",__DATE__);
   PyModule_AddStringConstant(Module,"TIME",__TIME__);

   // My constants
   PyModule_AddIntConstant(Module,"PRI_IMPORTANT",pkgCache::State::Important);
   PyModule_AddIntConstant(Module,"PRI_REQUIRED",pkgCache::State::Required);
   PyModule_AddIntConstant(Module,"PRI_STANDARD",pkgCache::State::Standard);
   PyModule_AddIntConstant(Module,"PRI_OPTIONAL",pkgCache::State::Optional);
   PyModule_AddIntConstant(Module,"PRI_EXTRA",pkgCache::State::Extra);
   // CurState
   PyModule_AddIntConstant(Module,"CURSTATE_NOT_INSTALLED",pkgCache::State::NotInstalled);
   PyModule_AddIntConstant(Module,"CURSTATE_UNPACKED",pkgCache::State::UnPacked);
   PyModule_AddIntConstant(Module,"CURSTATE_HALF_CONFIGURED",pkgCache::State::HalfConfigured);
   PyModule_AddIntConstant(Module,"CURSTATE_HALF_INSTALLED",pkgCache::State::HalfInstalled);
   PyModule_AddIntConstant(Module,"CURSTATE_CONFIG_FILES",pkgCache::State::ConfigFiles);
   PyModule_AddIntConstant(Module,"CURSTATE_INSTALLED",pkgCache::State::Installed);
   // SelState
   PyModule_AddIntConstant(Module,"SELSTATE_UNKNOWN",pkgCache::State::Unknown);
   PyModule_AddIntConstant(Module,"SELSTATE_INSTALL",pkgCache::State::Install);
   PyModule_AddIntConstant(Module,"SELSTATE_HOLD",pkgCache::State::Hold);
   PyModule_AddIntConstant(Module,"SELSTATE_DEINSTALL",pkgCache::State::DeInstall);
   PyModule_AddIntConstant(Module,"SELSTATE_PURGE",pkgCache::State::Purge);
   // InstState
   PyModule_AddIntConstant(Module,"INSTSTATE_OK",pkgCache::State::Ok);
   PyModule_AddIntConstant(Module,"INSTSTATE_REINSTREQ",pkgCache::State::ReInstReq);
   PyModule_AddIntConstant(Module,"INSTSTATE_HOLD",pkgCache::State::Hold);
   PyModule_AddIntConstant(Module,"INSTSTATE_HOLD_REINSTREQ",pkgCache::State::HoldReInstReq);


   // DEPRECATED API
   #ifdef COMPAT_0_7
   PyModule_AddObject(Module,"RewritePackageOrder",
                      CharCharToList(TFRewritePackageOrder));
   PyModule_AddObject(Module,"RewriteSourceOrder",
                      CharCharToList(TFRewriteSourceOrder));
   PyModule_AddStringConstant(Module,"LibVersion",(char *)pkgLibVersion);
   PyModule_AddStringConstant(Module,"Date",__DATE__);
   PyModule_AddStringConstant(Module,"Time",__TIME__);
   PyModule_AddIntConstant(Module,"PriImportant",pkgCache::State::Important);
   PyModule_AddIntConstant(Module,"PriRequired",pkgCache::State::Required);
   PyModule_AddIntConstant(Module,"PriStandard",pkgCache::State::Standard);
   PyModule_AddIntConstant(Module,"PriOptional",pkgCache::State::Optional);
   PyModule_AddIntConstant(Module,"PriExtra",pkgCache::State::Extra);
   PyModule_AddIntConstant(Module,"CurStateNotInstalled",pkgCache::State::NotInstalled);
   PyModule_AddIntConstant(Module,"CurStateUnPacked",pkgCache::State::UnPacked);
   PyModule_AddIntConstant(Module,"CurStateHalfConfigured",pkgCache::State::HalfConfigured);
   PyModule_AddIntConstant(Module,"CurStateHalfInstalled",pkgCache::State::HalfInstalled);
   PyModule_AddIntConstant(Module,"CurStateConfigFiles",pkgCache::State::ConfigFiles);
   PyModule_AddIntConstant(Module,"CurStateInstalled",pkgCache::State::Installed);
   PyModule_AddIntConstant(Module,"SelStateUnknown",pkgCache::State::Unknown);
   PyModule_AddIntConstant(Module,"SelStateInstall",pkgCache::State::Install);
   PyModule_AddIntConstant(Module,"SelStateHold",pkgCache::State::Hold);
   PyModule_AddIntConstant(Module,"SelStateDeInstall",pkgCache::State::DeInstall);
   PyModule_AddIntConstant(Module,"SelStatePurge",pkgCache::State::Purge);
   PyModule_AddIntConstant(Module,"InstStateOk",pkgCache::State::Ok);
   PyModule_AddIntConstant(Module,"InstStateReInstReq",pkgCache::State::ReInstReq);
   PyModule_AddIntConstant(Module,"InstStateHold",pkgCache::State::Hold);
   PyModule_AddIntConstant(Module,"InstStateHoldReInstReq",pkgCache::State::HoldReInstReq);
   PyModule_AddIntConstant(Module,"_COMPAT_0_7",1);
   #else
   PyModule_AddIntConstant(Module,"_COMPAT_0_7",0);
   #endif
   #if PY_MAJOR_VERSION >= 3
   return Module;
   #endif
}
									/*}}}*/

