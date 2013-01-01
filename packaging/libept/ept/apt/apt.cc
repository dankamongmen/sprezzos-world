/** \file
 * High-level front-end to libapt-pkg, as a data provider for the ept framework.
 */

/* 
 * Copyright (C) 2007,2008  Enrico Zini <enrico@enricozini.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

#include <ept/apt/apt.h>

#include <apt-pkg/error.h>
#include <apt-pkg/init.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/pkgcachegen.h>
#include <apt-pkg/policy.h>
#include <apt-pkg/cachefile.h>

#include <wibble/sys/fs.h>
#include <sys/stat.h>

#include <vector>
#include <algorithm>

#include <iostream>

using namespace std;

namespace ept {
namespace apt {

static time_t aptTimestamp()
{
    namespace wfs = wibble::sys::fs;

    std::auto_ptr<struct stat> st = wfs::stat(
        _config->FindFile( "Dir::Cache::pkgcache" ) );
    time_t t1 = st.get() == NULL ? 0 : st->st_mtime;
 
    std::auto_ptr<struct stat> st1 = wfs::stat(
        _config->FindFile( "Dir::State::status" ) );
    time_t t2 = st1.get() == NULL ? 0 : st1->st_mtime;
 
    return t1 > t2 ? t1 : t2;
}

Exception::Exception(const std::string& context) throw ()
	: Generic(context)
{
	// Concatenate all errors and warnings found
	string err;
	while (!_error->empty())
	{
		bool type = _error->PopMessage(err);
		if (type)
			m_message += "E: " + err + "\n";
		else
			m_message += "W: " + err + "\n";
   }
}

static void aptInit ()
{
    if (_config->FindB("Initialized"))
        return;

    if (!pkgInitConfig (*_config))
		throw Exception("initialising apt configuration");

    _config->Set("Initialized", 1);

	/*
    _config->Set("Dir", CACHE_DIR);
    _config->Set("Dir::Cache", "cache");
    _config->Set("Dir::State", "state");
    _config->Set("Dir::Etc", "etc");
    _config->Set("Dir::State::status", CACHE_DIR "dpkg-status");
	*/
    if (!pkgInitSystem (*_config, _system))
		throw Exception("initialising apt system");
}

struct AptImplementation
{
	pkgSourceList* m_list;
	MMap *m;
	OpProgress progress;
	pkgCache* m_cache;
	pkgPolicy* m_policy;
	pkgCacheFile* m_depcache;
	time_t m_open_timestamp;
	
	AptImplementation() : m_list(0), m(0), m_cache(0), m_policy(0), m_depcache(0), m_open_timestamp(0)
	{
		// Init the apt library if needed
		aptInit();

		m_open_timestamp = aptTimestamp();

		m_list = new pkgSourceList;
		if (!m_list->ReadMainList())
			throw Exception("reading list of sources");

		bool res = pkgMakeStatusCache(*m_list, progress, &m, true);
		progress.Done();
		if (!res)
			throw Exception("Reading the package lists or status file");

		m_cache = new pkgCache(m);
		m_policy = new pkgPolicy(m_cache);
		if (!ReadPinFile(*m_policy))
			throw Exception("Reading the policy pin file");
	}

	~AptImplementation()
	{
		if (m_depcache) delete m_depcache;
		if (m_policy) delete m_policy;
		if (m_cache) delete m_cache;
		if (m) delete m;
		if (m_list) delete m_list;
	}

	pkgCache& cache()
	{
		return *m_cache;
	}

	pkgPolicy& policy()
	{
		return *m_policy;
	}

	pkgCacheFile& depcache()
	{
		if (!m_depcache)
		{
			m_depcache = new pkgCacheFile;
			if (!m_depcache->Open(progress, false))
				throw Exception("Opening the cache file");
		}
		return *m_depcache;
	}
};

// Sort a version list by package file locality
bool localityCompare(const pkgCache::VerFile* a, const pkgCache::VerFile* b)
{
   if (a == 0 && b == 0)
      return false;
   if (a == 0)
      return true;
   if (b == 0)
      return false;
   
   if (a->File == b->File)
      return a->Offset < b->Offset;
   return a->File < b->File;
}

// Iterate records using the algorithm used by apt-cache dumpavail
struct RecordIteratorImpl
{
	mutable int _ref;
	AptImplementation& apt;
	vector<pkgCache::VerFile*> vflist;
	pkgCache::PkgFileIterator lastFile;
	FileFd file;
	size_t lastOffset;

	RecordIteratorImpl(AptImplementation& apt) : _ref(0), apt(apt)
	{
		// We already have an estimate of how many versions we're about to find
		vflist.reserve(apt.cache().HeaderP->PackageCount + 1);

		// Populate the vector of versions to print
		for (pkgCache::PkgIterator pi = apt.cache().PkgBegin(); !pi.end(); ++pi)
		{    
			if (pi->VersionList == 0)
				continue;

			/* Get the candidate version or fallback on the installed version,
			 * as usual */
			pkgCache::VerIterator vi = apt.policy().GetCandidateVer(pi);
			if (vi.end() == true)
			{
				if (pi->CurrentVer == 0)
					continue;
				vi = pi.CurrentVer();
			}

			// Choose a valid file that contains the record for this version
			pkgCache::VerFileIterator vfi = vi.FileList();
			for ( ; !vfi.end(); ++vfi)
				if ((vfi.File()->Flags & pkgCache::Flag::NotSource) == 0)
					break;

			// Handle packages whose candidate version is currently installed
			// from outside the archives (like from a locally built .deb
			if (vfi.end() == true)
			{
				for (pkgCache::VerIterator cur = pi.VersionList(); cur.end() != true; cur++)
				{
					for (vfi = cur.FileList(); vfi.end() == false; vfi++)
					{	 
						if ((vfi.File()->Flags & pkgCache::Flag::NotSource) == 0)
						{
							vfi = vi.FileList();
							break;
						}
					}

					if (vfi.end() == false)
						break;
				}
			}
			if (!vfi.end())
				vflist.push_back(vfi);
		}

		//cerr << vflist.size() << " versions found" << endl;

		sort(vflist.begin(), vflist.end(), localityCompare);

		//for (size_t i = 0; i < vflist.size(); ++i)
		//{
		//	pkgCache::PkgFileIterator fi(apt.cache(), vflist[i]->File + apt.cache().PkgFileP);
		//	cerr << i << ": " << fi.FileName() << ":" << vflist[i]->Offset << "-" << vflist[i]->Size << endl;
		//}
		//cerr << "Done indexing." << endl;
	}

	~RecordIteratorImpl()
	{
		if (file.IsOpen())
			file.Close();
	}

	void ref() { ++_ref; }
	bool unref() { return --_ref == 0; }

	size_t size() { return vflist.size(); }

	string record(size_t idx)
	{
		//cerr << "Access record " << idx << endl;
		//cerr << "lastfile: " << (lastFile.Cache() != 0) << endl;
		// We can't reuse the file that was already open: open the new one
		if ((lastFile.Cache() == 0) || vflist[idx]->File + apt.cache().PkgFileP != lastFile)
		{
			//cerr << "Needs open/reopen" << endl;
			lastFile = pkgCache::PkgFileIterator(apt.cache(), vflist[idx]->File + apt.cache().PkgFileP);
			if (!lastFile.IsOk())
				throw Exception(string("Reading the data record for a package from file ") + lastFile.FileName());
			//cerr << "Ok for " << lastFile.FileName() << endl;
			if (file.IsOpen())
				file.Close();
			if (!file.Open(lastFile.FileName(), FileFd::ReadOnly))
				throw Exception(string("Opening file ") + lastFile.FileName());
			//cerr << "Opened " << lastFile.FileName() << endl;
			lastOffset = 0;
		}

		//cerr << "Reading from " << lastFile.FileName() << ":" << vflist[idx]->Offset << "-" << vflist[idx]->Size << " (lastOffset: " << lastOffset << ")" << endl;

		// If we start near were we ended, avoid a seek and enlarge the read a bit
		size_t slack = vflist[idx]->Offset - lastOffset;
		//cerr << "Slack: " << slack << endl;
		if (slack > 8)
		{
			//cerr << "Slack too big: seek to " << vflist[idx]->Offset << endl;
			slack = 0;
			if (!file.Seek(vflist[idx]->Offset))
				throw Exception(string("Cannot seek to package record in file ") + lastFile.FileName());
		}

		char buffer[vflist[idx]->Size + slack + 1];
		if (!file.Read(buffer, vflist[idx]->Size + slack))
			throw Exception(string("Cannot read package record in file ") + lastFile.FileName());
		buffer[vflist[idx]->Size + slack] = '\n';
		//cerr << "Data read (slack: " << slack << ")" << endl;

		lastOffset = vflist[idx]->Offset + vflist[idx]->Size;

		return string(buffer+slack, vflist[idx]->Size);
	}
};

Apt::Iterator::Iterator(const Iterator& i)
{
	if (i.cur)
	{
		pkgCache::PkgIterator* p = new pkgCache::PkgIterator;
		*p = *static_cast<pkgCache::PkgIterator*>(i.cur);
		cur = p;
	} else
		cur = 0;
}

Apt::Iterator& Apt::Iterator::operator=(const Iterator& i)
{
	if (cur != i.cur)
	{
		if (cur) delete static_cast<pkgCache::PkgIterator*>(cur);
		if (i.cur)
		{
			pkgCache::PkgIterator* p = new pkgCache::PkgIterator;
			*p = *static_cast<pkgCache::PkgIterator*>(i.cur);
			cur = p;
		} else
			cur = 0;
	}
	return *this;
}

Apt::Iterator::~Iterator()
{
	if (cur) delete static_cast<pkgCache::PkgIterator*>(cur);
}
std::string Apt::Iterator::operator*()
{
	return static_cast<pkgCache::PkgIterator*>(cur)->Name();
}
Apt::Iterator& Apt::Iterator::operator++()
{
	pkgCache::PkgIterator* iter = static_cast<pkgCache::PkgIterator*>(cur);
	++*iter;
	while (!iter->end() && (*iter)->VersionList == 0)
		++*iter;
	if (iter->end())
	{
		delete iter;
		cur = 0;
	}
	return *this;
}
bool Apt::Iterator::operator==(const Iterator& i) const
{
	if (cur == 0 && i.cur == 0)
		return true;
	if (cur == 0 || i.cur == 0)
		return false;
	pkgCache::PkgIterator* iter1 = static_cast<pkgCache::PkgIterator*>(cur);
	pkgCache::PkgIterator* iter2 = static_cast<pkgCache::PkgIterator*>(i.cur);
	return *iter1 == *iter2;
}
bool Apt::Iterator::operator!=(const Iterator& i) const
{
	if (cur == 0 && i.cur == 0)
		return false;
	if (cur == 0 || i.cur == 0)
		return true;
	pkgCache::PkgIterator* iter1 = static_cast<pkgCache::PkgIterator*>(cur);
	pkgCache::PkgIterator* iter2 = static_cast<pkgCache::PkgIterator*>(i.cur);
	return *iter1 != *iter2;
}


Apt::RecordIterator::RecordIterator(RecordIteratorImpl* impl, size_t pos)
	: impl(impl), pos(pos), cur_pos(pos)
{
	if (impl)
	{
		impl->ref();
		cur = impl->record(pos);
		cur_pos = pos;
	}
}
Apt::RecordIterator::RecordIterator(const RecordIterator& r)
	: impl(r.impl), pos(r.pos), cur(r.cur), cur_pos(r.cur_pos)
{
	if (impl)
		impl->ref();
}
Apt::RecordIterator::~RecordIterator()
{
	if (impl && impl->unref())
		delete impl;
}
std::string Apt::RecordIterator::operator*()
{
	if (cur_pos != pos)
	{
		cur = impl->record(pos);
		cur_pos = pos;
	}
	return cur;
}
std::string* Apt::RecordIterator::operator->()
{
	if (cur_pos != pos)
	{
		cur = impl->record(pos);
		cur_pos = pos;
	}
	return &cur;
}
Apt::RecordIterator& Apt::RecordIterator::operator++()
{
	++pos;
	if (pos >= impl->size())
	{
		// If we reach the end, we become an end iterator
		if (impl && impl->unref())
			delete impl;
		impl = 0;
		pos = 0;
	}
	return *this;
}
Apt::RecordIterator& Apt::RecordIterator::operator=(const RecordIterator& r)
{
	// Increment first, to avoid it reaching zero on assignment to self
	if (r.impl) r.impl->ref();
	if (impl && impl->unref())
		delete impl;
	impl = r.impl;
	pos = r.pos;
	cur = r.cur;
	cur_pos = r.cur_pos;
	return *this;
}
bool Apt::RecordIterator::operator==(const RecordIterator& ri) const
{
	return impl == ri.impl && pos == ri.pos;
}
bool Apt::RecordIterator::operator!=(const RecordIterator& ri) const
{
	return impl != ri.impl || pos != ri.pos;
}


Apt::Apt() : impl(new AptImplementation()) {}
Apt::~Apt() { delete impl; }

Apt::iterator Apt::begin() const
{
	pkgCache::PkgIterator* p = new pkgCache::PkgIterator;
	*p = impl->cache().PkgBegin();
	return Apt::Iterator(p);
}

Apt::iterator Apt::end() const
{
	return Apt::Iterator();
}

Apt::record_iterator Apt::recordBegin() const
{
	return Apt::RecordIterator(new RecordIteratorImpl(*impl));
}

Apt::record_iterator Apt::recordEnd() const
{
	return Apt::RecordIterator();
}

size_t Apt::size() const
{
   	return impl->cache().HeaderP->PackageCount;
}

time_t Apt::timestamp()
{
	return aptTimestamp();
}

bool Apt::isValid(const std::string& pkg) const
{
	pkgCache::PkgIterator pi = impl->cache().FindPkg(pkg);
	return !pi.end();
}

Version Apt::validate(const Version& ver) const
{
	pkgCache::PkgIterator pi = impl->cache().FindPkg(ver.name());
	if (pi.end()) return Version();
	for (pkgCache::VerIterator vi = pi.VersionList(); !vi.end(); vi++)
	{
		const char* v = vi.VerStr();
		if (v == 0) continue;
		if (ver.version() == v)
			return ver;
	}
	return Version();
}

Version Apt::candidateVersion(const std::string& pkg) const
{
	pkgCache::PkgIterator pi = impl->cache().FindPkg(pkg);
	if (pi.end()) return Version();
	pkgCache::VerIterator vi = impl->policy().GetCandidateVer(pi);
	if (vi.end()) return Version();
	return Version(pkg, vi.VerStr());
}

Version Apt::installedVersion(const std::string& pkg) const
{
	pkgCache::PkgIterator pi = impl->cache().FindPkg(pkg);
	if (pi.end()) return Version();
	if (pi->CurrentVer == 0) return Version();
	pkgCache::VerIterator vi = pi.CurrentVer();
	if (vi.end()) return Version();
	return Version(pkg, vi.VerStr());
}

Version Apt::anyVersion(const std::string& pkg) const
{
	pkgCache::PkgIterator pi = impl->cache().FindPkg(pkg);
	if (pi.end()) return Version();

	pkgCache::VerIterator vi = impl->policy().GetCandidateVer(pi);
	if (vi.end())
	{
		if (pi->CurrentVer == 0) return Version();
		vi = pi.CurrentVer();
		if (vi.end()) return Version();
	}
	return Version(pkg, vi.VerStr());
}

PackageState Apt::state(const std::string& pkg) const
{
	pkgCache::PkgIterator pi = impl->cache().FindPkg(pkg);
	if (pi.end()) return PackageState();
	pkgDepCache::StateCache sc = impl->depcache()[pi];

	unsigned int flags = PackageState::Valid;

	// Check if the package is installed
    if (pi->CurrentState != pkgCache::State::ConfigFiles &&
		pi->CurrentState != pkgCache::State::NotInstalled &&
		pi->CurrentVer != 0)
	{
		// Try to get a VerIterator to the installed version
		pkgCache::VerIterator inst = pi.CurrentVer();
		if (!inst.end())
		{
			// If we made it so far, it is installed
			flags |= PackageState::Installed;

			// Now check if it is upgradable
			pkgCache::VerIterator cand = impl->policy().GetCandidateVer(pi);

			// If the candidate version is different than the installed one, then
			// it is installable
			if (!cand.end() && inst != cand)
				flags |= PackageState::Upgradable;
		}
	}
    if (sc.Install())
        flags |= PackageState::Install;
    if ((sc.iFlags & pkgDepCache::ReInstall) == pkgDepCache::ReInstall)
        flags |= PackageState::ReInstall;
    if (sc.Keep())
        flags |= PackageState::Keep;
    if (sc.Delete())
        flags |= PackageState::Remove;
    if ((sc.iFlags & pkgDepCache::Purge) == pkgDepCache::Purge)
        flags |= PackageState::Purge;
    if (sc.NowBroken())
        flags |= PackageState::NowBroken;
    if (sc.InstBroken())
        flags |= PackageState::WillBreak;

	return PackageState(flags);
}

std::string Apt::rawRecord(const std::string& ver) const
{
	// TODO: possibly reimplement using a single lump of apt code, to avoid
	// repeating lookups
	return rawRecord(anyVersion(ver));
}

std::string Apt::rawRecord(const Version& ver) const
{
	pkgCache::PkgIterator pi = impl->cache().FindPkg(ver.name());
	if (pi.end()) return std::string();
	for (pkgCache::VerIterator vi = pi.VersionList(); !vi.end(); vi++)
	{
		const char* v = vi.VerStr();
		if (v == 0) continue;
		if (ver.version() == v)
		{
			// Code taken and adapted from apt-cache's DisplayRecord

			// Find an appropriate file
			pkgCache::VerFileIterator vfi = vi.FileList();
			for (; !vfi.end(); vfi++)
				if ((vfi.File()->Flags & pkgCache::Flag::NotSource) == 0)
					break;
			if (vfi.end())
				vfi = vi.FileList();

			// Check and load the package list file
			pkgCache::PkgFileIterator pfi = vfi.File();
			if (!pfi.IsOk())
				throw Exception(string("Reading the data record for a package version from file ") + pfi.FileName());

			FileFd pkgf(pfi.FileName(), FileFd::ReadOnly);
			if (_error->PendingError() == true)
				return std::string();

			// Read the record and then write it out again.
			char* buffer = new char[vfi->Size+1];
			buffer[vfi->Size] = '\n';
			if (!pkgf.Seek(vfi->Offset) || !pkgf.Read(buffer, vfi->Size))
			{
				delete[] buffer;
				return std::string();
			}

			std::string res(buffer, vfi->Size);
			delete[] buffer;
			return res;
		}
	}
	return std::string();
}


const pkgCache* Apt::aptPkgCache() const 
{
	return impl->m_cache;
}


void Apt::checkCacheUpdates()
{
	if (impl->m_open_timestamp < timestamp())
	{
		// Crudely reopen everything
		delete impl;
		impl = new AptImplementation;
	}
}

void Apt::invalidateTimestamp()
{
	impl->m_open_timestamp = 0;
}

}
}

// vim:set ts=4 sw=4:
