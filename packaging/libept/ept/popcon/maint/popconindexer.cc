#include <ept/popcon/popcon.h>
#include <ept/popcon/maint/popconindexer.h>
#include <ept/popcon/maint/path.h>

#include <wibble/exception.h>
#include <wibble/sys/fs.h>

#include <tagcoll/diskindex/mmap.h>

#include <unistd.h>
#include <set>
#include <string>
#include <cstdio>
#include <cstring>

using namespace std;

namespace ept {
namespace popcon {

template<typename STRUCT>
struct StructIndexer : public tagcoll::diskindex::MMapIndexer
{
	const STRUCT& data;
	StructIndexer(const STRUCT& data) : data(data) {}

	int encodedSize() const { return sizeof(STRUCT); }
	void encode(char* buf) const { *(STRUCT*)buf = data; }
};

/// MMapIndexer that indexes the package names
struct PopconGenerator : public tagcoll::diskindex::MMapIndexer
{
	// Sorted set of all available package names and data
	std::map<std::string, Score> data;

	int encodedSize() const
	{
		int size = data.size() * sizeof(Score);
		for (std::map<std::string, Score>::const_iterator i = data.begin();
				i != data.end(); ++i)
			size += i->first.size() + 1;
		return tagcoll::diskindex::MMap::align(size);
	}

	void encode(char* buf) const
	{
		int pos = data.size() * sizeof(Score);
		int idx = 0;
		for (std::map<std::string, Score>::const_iterator i = data.begin();
				i != data.end(); ++i)
		{
			((Score*)buf)[idx] = i->second;
			((Score*)buf)[idx].offset = pos;
			memcpy(buf + pos, i->first.c_str(), i->first.size() + 1);
			pos += i->first.size() + 1;
			++idx;
		}
	}
};


PopconIndexer::PopconIndexer()
	: mainSource(Path::popconSourceDir()),
	  userSource(Path::popconUserSourceDir())
{
	rescan();
}

void PopconIndexer::rescan()
{
	ts_main_src = mainSource.timestamp();
	ts_user_src = userSource.timestamp();
	ts_main_sco = Path::timestamp(Path::scores());
	ts_user_sco = Path::timestamp(Path::userScores());
	ts_main_idx = Path::timestamp(Path::scoresIndex());
	ts_user_idx = Path::timestamp(Path::userScoresIndex());
}

bool PopconIndexer::needsRebuild() const
{
	// If there are no indexes of any kind, then we need rebuilding
	if (ts_user_sco == 0 || ts_main_sco == 0 || ts_user_idx == 0 && ts_main_idx == 0)
		return true;

	// If the user index is ok, then we are fine
	if (ts_user_sco >= sourceTimestamp() && ts_user_idx >= sourceTimestamp())
		return false;

	// If there are user sources, then we cannot use the system index
	if (ts_user_src > 0)
		return true;

	// If there are no user sources, then we can fallback on the system
	// indexes in case the user indexes are not up to date
	if (ts_main_sco >= sourceTimestamp() && ts_main_idx >= sourceTimestamp())
		return false;

	return true;
}

bool PopconIndexer::userIndexIsRedundant() const
{
	// If there is no user index, then it is not redundant
	if (ts_user_idx == 0)
		return false;

	// If the system index is not up to date, then the user index is not
	// redundant
	if (ts_main_idx < sourceTimestamp())
		return false;

	return true;
}

bool PopconIndexer::rebuild(const std::string& scofname, const std::string& idxfname)
{
	PopconGenerator gen;
	InfoStruct is;
	is.submissions = 0;
	if (!mainSource.readScores(gen.data, is.submissions))
		userSource.readScores(gen.data, is.submissions);
	if (gen.data.empty())
		return false;

	StructIndexer<InfoStruct> infoStruct(is);

	// Create the index
	tagcoll::diskindex::MasterMMapIndexer master(idxfname);
	master.append(gen);
	master.append(infoStruct);
	master.commit();

//	for (map<string, Score>::const_iterator i = gen.data.begin(); i != gen.data.end(); ++i)
//	{
//		fprintf(stderr, "%s   %d   %f\n", i->first.c_str(), i->second.offset, i->second.score);
//	}

	// Create the score file
	FILE* out = fopen(scofname.c_str(), "wt");
	if (out == NULL)
		throw wibble::exception::File(scofname, "opening and truncating file for writing");
	for (map<string, Score>::const_iterator i = gen.data.begin();
			i != gen.data.end(); ++i)
	{
		fprintf(out, "%s %f\n", i->first.c_str(), i->second.score);
	}
	fclose(out);
	return true;
}

bool PopconIndexer::rebuildIfNeeded()
{
	if (needsRebuild())
	{
		// Decide if we rebuild the user index or the system index
		if (Path::access(Path::popconIndexDir(), W_OK) == 0)
		{
			// Since we can write on the system index directory, we rebuild
			// the system index
			if (!rebuild(Path::scores(), Path::scoresIndex()))
				return false;
			ts_main_sco = Path::timestamp(Path::scores());
			ts_main_idx = Path::timestamp(Path::scoresIndex());
			if (Path::scores() == Path::userScores())
				ts_user_sco = ts_main_sco;
			if (Path::scoresIndex() == Path::userScoresIndex())
				ts_user_idx = ts_main_idx;
		} else {
			wibble::sys::fs::mkFilePath(Path::userScores());
			wibble::sys::fs::mkFilePath(Path::userScoresIndex());
			if (!rebuild(Path::userScores(), Path::userScoresIndex()))
				return false;
			ts_user_sco = Path::timestamp(Path::userScores());
			ts_user_idx = Path::timestamp(Path::userScoresIndex());
		}
		return true;
	}
	return false;
}

bool PopconIndexer::deleteRedundantUserIndex()
{
	if (userIndexIsRedundant())
	{
		// Delete the user indexes if they exist
		if (Path::scores() != Path::userScores())
		{
			unlink(Path::userScores().c_str());
			ts_user_sco = 0;
		}
		if (Path::scoresIndex() != Path::userScoresIndex())
		{
			unlink(Path::userScoresIndex().c_str());
			ts_user_idx = 0;
		}
		return true;
	}
	return false;
}

bool PopconIndexer::getUpToDatePopcon(std::string& scofname, std::string& idxfname)
{
	// If there are no indexes of any kind, then we have nothing to return
	if (ts_user_sco == 0 && ts_main_sco == 0 && ts_user_idx == 0 && ts_main_idx == 0)
		return false;

	// If the user index is up to date, use it
	if (ts_user_sco >= sourceTimestamp() &&
	    ts_user_idx >= sourceTimestamp())
	{
		scofname = Path::userScores();
		idxfname = Path::userScoresIndex();
		return true;
	}

	// If the user index is not up to date and we have user sources, we cannot
	// fall back to the system index
	if (ts_user_src != 0)
		return false;

	// Fallback to the system index
	if (ts_main_sco >= sourceTimestamp() &&
	    ts_main_idx >= sourceTimestamp())
	{
		scofname = Path::scores();
		idxfname = Path::scoresIndex();
		return true;
	}

	return false;
}


bool PopconIndexer::obtainWorkingPopcon(std::string& scofname, std::string& idxfname)
{
	PopconIndexer indexer;

	indexer.rebuildIfNeeded();
	indexer.deleteRedundantUserIndex();
	return indexer.getUpToDatePopcon(scofname, idxfname);
}


}
}

// vim:set ts=4 sw=4:
