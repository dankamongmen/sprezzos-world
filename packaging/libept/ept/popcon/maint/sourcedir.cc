#include <ept/popcon/maint/sourcedir.h>
#include <ept/popcon/maint/path.h>

#include <wibble/string.h>

#include <tagcoll/input/zlib.h>
#include <tagcoll/input/stdio.h>

#include <cstdlib>

using namespace std;
using namespace wibble;

namespace ept {
namespace popcon {

SourceDir::FileType SourceDir::fileType(const std::string& name)
{
	if (name[0] == '.') return SKIP;

	if (name == "all-popcon-results.txt") return RAW;
	if (name == "all-popcon-results.txt.gz") return RAWGZ;

	return SKIP;
}

time_t SourceDir::timestamp()
{
	if (!valid()) return 0;

	time_t max = 0;
	for (const_iterator d = begin(); d != end(); ++d)
	{
		FileType type = fileType(d->d_name);
		if (type == SKIP) continue;

		time_t ts = Path::timestamp(str::joinpath(path(), d->d_name));
		if (ts > max) max = ts;
	}

	return max;
}

bool readLine(tagcoll::input::Input& in, string& str)
{
	str.clear();
	int c;
	while ((c = in.nextChar()) != tagcoll::input::Input::Eof && c != '\n')
		str += c;
	return c != tagcoll::input::Input::Eof;
}

static void parseScores(tagcoll::input::Input& in, map<std::string, Score>& out, size_t& submissions)
{
	string line;
	while (readLine(in, line))
	{
		if (line.size() < 10)
			continue;
		if (line.substr(0, 13) == "Submissions: ")
		{
			submissions = strtoul(line.substr(13).c_str(), 0, 10);
			continue;
		}
		if (line.substr(0, 9) != "Package: ")
			continue;
		size_t start = 9;
		size_t end = line.find(' ', start);
		if (end == string::npos)
			continue;
		string name = line.substr(start, end-start);
		// Skip packages not in the apt index
		//if (!apt.isValid(name))
			//continue;

		start = line.find_first_not_of(' ', end);
		if (start == string::npos) continue;
		end = line.find(' ', start);
		if (end == string::npos) continue;
		string vote = line.substr(start, end-start);

		start = line.find_first_not_of(' ', end);
		if (start == string::npos) continue;
		end = line.find(' ', start);
		if (end == string::npos) continue;
		string old = line.substr(start, end-start);

		start = line.find_first_not_of(' ', end);
		if (start == string::npos) continue;
		end = line.find(' ', start);
		if (end == string::npos) continue;
		string recent = line.substr(start, end-start);

		start = line.find_first_not_of(' ', end);
		if (start == string::npos) continue;
		end = line.find(' ', start);
		if (end == string::npos) end = line.size();
		string nofiles = line.substr(start, end-start);

		float score = (float)strtoul(vote.c_str(), NULL, 10) 
					+ (float)strtoul(recent.c_str(), NULL, 10) * 0.5f
			        + (float)strtoul(old.c_str(), NULL, 10) * 0.3f
			        + (float)strtoul(nofiles.c_str(), NULL, 10) * 0.8f;

		if (score > 0)
			out.insert(make_pair(name, Score(score)));
	}
}

bool SourceDir::readScores(map<std::string, Score>& out, size_t& submissions)
{
	if (!valid()) return false;
	bool done = false;

	for (const_iterator d = begin(); d != end(); ++d)
	{
		FileType type = fileType(d->d_name);
		if (type == RAW)
		{
			// Read uncompressed data
			tagcoll::input::Stdio in(str::joinpath(path(), d->d_name));

			// Read the scores
			parseScores(in, out, submissions);
			done = true;
		}
		else if (type == RAWGZ)
		{
			// Read compressed data
			tagcoll::input::Zlib in(str::joinpath(path(), d->d_name));

			// Read the scores
			parseScores(in, out, submissions);
			done = true;
		}
	}
	return done;
}

}
}

// vim:set ts=4 sw=4:
