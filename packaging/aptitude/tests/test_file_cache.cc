#include <boost/format.hpp>
#include <boost/function.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/test/unit_test.hpp>

#include <generic/util/file_cache.h>
#include <generic/util/temp.h>

#include <sys/stat.h>

#include <apt-pkg/fileutl.h>

#include <fstream>

#include <libgen.h>

using aptitude::util::file_cache;

class usingTemp
{
public:
  usingTemp()
  {
    temp::initialize("testFileCache");
  }

  ~usingTemp()
  {
    temp::shutdown();
  }
};

bool exists(const std::string &s)
{
  // Slightly lame way to test that a file exists.  Should be enough
  // for our purposes.
  struct stat buf;

  return stat(s.c_str(), &buf) == 0;
}

BOOST_FIXTURE_TEST_CASE(createFileCache, usingTemp)
{
  {
    temp::name tn("cache");

    BOOST_CHECK(!exists(tn.get_name()));

    {
      boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 10000, 10000));
      BOOST_CHECK(exists(tn.get_name()));
    }
  }

  {
    temp::name tn("cache");

    {
      boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 0, 10000));
      BOOST_CHECK(exists(tn.get_name()));
    }
  }

  {
    temp::name tn("cache");

    {
      boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 10000, 0));
      BOOST_CHECK(!exists(tn.get_name()));
    }
  }
}

class raw_istream_iterator
{
  std::istream *in;
  char c;
  bool finished;

public:
  raw_istream_iterator(std::istream &_in)
    : in(&_in),
      c(0),
      finished(!in->good())
  {
    if(!finished)
      {
	in->get(c);
	finished = in->eof();
      }
  }

  raw_istream_iterator()
    : in(0), c(0), finished(true)
  {
  }

  bool operator==(const raw_istream_iterator &other) const
  {
    // Only end iterators are equivalent.
    return finished && other.finished;
  }

  bool operator!=(const raw_istream_iterator &other) const
  {
    return !(*this == other);
  }

  const char &operator*() const
  {
    BOOST_REQUIRE(!finished);

    return c;
  }

  raw_istream_iterator &operator++()
  {
    if(in->good())
      {
	in->get(c);
	finished = in->eof();
      }
    else
      finished = true;

    return *this;
  }
};

#define CHECK_FILE_CONTENTS(filename, collection)			\
  do {									\
    std::ifstream ___tmp_stream(filename.c_str());			\
    BOOST_CHECK_EQUAL_COLLECTIONS(raw_istream_iterator(___tmp_stream), raw_istream_iterator(), \
				  collection.begin(), collection.end()); \
  } while(0)

#define CHECK_CACHED_VALUE(cache, key, collection, mtime)			\
  do {									\
    time_t     ___cached_mtime(-1);					\
    temp::name ___cached_name(cache->getItem(key, ___cached_mtime));	\
    BOOST_CHECK_MESSAGE(___cached_name.valid(), "The key " << key << " does not exist in the cache."); \
    if(___cached_name.valid())						\
      {									\
	BOOST_CHECK_EQUAL(___cached_mtime, (mtime));			\
	CHECK_FILE_CONTENTS(___cached_name.get_name(), collection);	\
      }									\
  } while(0)

struct fileCacheTestInfo
{
  temp::name infilename1;
  temp::name infilename2;
  temp::name infilename3;

  std::vector<char> infileData1;
  std::vector<char> infileData2;
  std::vector<char> infileData3;

  std::string key1;
  std::string key2;
  std::string key3;

  time_t time1;
  time_t time2;
  time_t time3;
};

// Creates a file cache containing three files whose sizes add up to 1,000 bytes.
void setupFileCacheTest(const boost::shared_ptr<file_cache> cache,
			fileCacheTestInfo &testInfo)
{
  // Store three files whose sizes add up to 1,000 bytes.
  testInfo.infilename1 = temp::name("testInFile");
  testInfo.infilename2 = temp::name("testInFile");
  testInfo.infilename3 = temp::name("testInFile");

  std::ofstream
    infile1(testInfo.infilename1.get_name().c_str()),
    infile2(testInfo.infilename2.get_name().c_str()),
    infile3(testInfo.infilename3.get_name().c_str());

  for(int i = 0; i < 333; ++i)
    testInfo.infileData1.push_back((char)i);

  for(int i = 333; i < 666; ++i)
    testInfo.infileData2.push_back((char)i);

  for(int i = 666; i < 1000; ++i)
    testInfo.infileData3.push_back((char)i);

  testInfo.time1 = 100;
  testInfo.time2 = 200;
  testInfo.time3 = 300;

  infile1.write(&testInfo.infileData1.front(), testInfo.infileData1.size());
  infile2.write(&testInfo.infileData2.front(), testInfo.infileData2.size());
  infile3.write(&testInfo.infileData3.front(), testInfo.infileData3.size());

  infile1.close();
  infile2.close();
  infile3.close();


  {
    // Sanity-check the raw iterator class.
    std::ifstream
      testFile1(testInfo.infilename1.get_name().c_str()),
      testFile2(testInfo.infilename2.get_name().c_str()),
      testFile3(testInfo.infilename3.get_name().c_str());

    BOOST_REQUIRE_EQUAL_COLLECTIONS(raw_istream_iterator(testFile1), raw_istream_iterator(),
				    testInfo.infileData1.begin(), testInfo.infileData1.end());
    BOOST_REQUIRE_EQUAL_COLLECTIONS(raw_istream_iterator(testFile2), raw_istream_iterator(),
				    testInfo.infileData2.begin(), testInfo.infileData2.end());
    BOOST_REQUIRE_EQUAL_COLLECTIONS(raw_istream_iterator(testFile3), raw_istream_iterator(),
				    testInfo.infileData3.begin(), testInfo.infileData3.end());
  }


  testInfo.key1 = "key1";
  testInfo.key2 = "key2";
  testInfo.key3 = "key3";

  BOOST_CHECK(!cache->getItem(testInfo.key1).valid());
  BOOST_CHECK(!cache->getItem(testInfo.key2).valid());
  BOOST_CHECK(!cache->getItem(testInfo.key3).valid());


  cache->putItem(testInfo.key1, testInfo.infilename1.get_name(), testInfo.time1);

  CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1, testInfo.time1);
  BOOST_CHECK(!cache->getItem(testInfo.key2).valid());
  BOOST_CHECK(!cache->getItem(testInfo.key3).valid());


  cache->putItem(testInfo.key2, testInfo.infilename2.get_name(), testInfo.time2);

  CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1, testInfo.time1);
  CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2, testInfo.time2);
  BOOST_CHECK(!cache->getItem(testInfo.key3).valid());


  cache->putItem(testInfo.key3, testInfo.infilename3.get_name(), testInfo.time3);

  CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1, testInfo.time1);
  CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2, testInfo.time2);
  CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3, testInfo.time3);
}

BOOST_FIXTURE_TEST_CASE(fileCacheStoreDisk, usingTemp)
{
  temp::name tn("cache");

  boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 0, 1000));
  BOOST_CHECK(exists(tn.get_name()));

  fileCacheTestInfo testInfo;
  setupFileCacheTest(cache, testInfo);
}

BOOST_FIXTURE_TEST_CASE(fileCacheStoreMemory, usingTemp)
{
  temp::name tn("cache");

  boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 1000, 0));

  fileCacheTestInfo testInfo;
  setupFileCacheTest(cache, testInfo);
}

BOOST_FIXTURE_TEST_CASE(fileCacheStoreDiskAndMemory, usingTemp)
{
  temp::name tn("cache");

  boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 333, 1000));
  BOOST_CHECK(exists(tn.get_name()));

  fileCacheTestInfo testInfo;
  setupFileCacheTest(cache, testInfo);
}

BOOST_FIXTURE_TEST_CASE(fileCacheModifiedTime, usingTemp)
{
  temp::name tn("cache");

  boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 333, 1000));
  BOOST_CHECK(exists(tn.get_name()));

  fileCacheTestInfo testInfo;
  setupFileCacheTest(cache, testInfo);


  time_t mtime1 = 0, mtime2 = 0, mtime3 = 0;

  BOOST_CHECK(cache->getItem(testInfo.key1, mtime1).valid());
  BOOST_CHECK(cache->getItem(testInfo.key2, mtime2).valid());
  BOOST_CHECK(cache->getItem(testInfo.key3, mtime3).valid());

  BOOST_CHECK_EQUAL(mtime1, testInfo.time1);
  BOOST_CHECK_EQUAL(mtime2, testInfo.time2);
  BOOST_CHECK_EQUAL(mtime3, testInfo.time3);
}

void runDropLeastRecentlyUsedTest(const boost::function<boost::shared_ptr<file_cache> (std::string)> &cache_k)
{
  // Check that we can control which of the three entries is dropped
  // when we add a fourth entry.


  // Drop key3.
  {
    temp::name tn("cache");
    boost::shared_ptr<file_cache> cache(cache_k(tn.get_name()));

    fileCacheTestInfo testInfo;
    setupFileCacheTest(cache, testInfo);

    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3, testInfo.time3);
    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1, testInfo.time1);
    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2, testInfo.time2);

    cache->putItem("key4", testInfo.infilename1.get_name(), testInfo.time1);


    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1, testInfo.time1);
    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2, testInfo.time2);
    CHECK_CACHED_VALUE(cache, "key4", testInfo.infileData1, testInfo.time1);
    BOOST_CHECK(!cache->getItem(testInfo.key3).valid());
  }


  // Drop key2.
  {
    temp::name tn("cache");
    boost::shared_ptr<file_cache> cache(cache_k(tn.get_name()));

    fileCacheTestInfo testInfo;
    setupFileCacheTest(cache, testInfo);

    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2, testInfo.time2);
    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1, testInfo.time1);
    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3, testInfo.time3);

    cache->putItem("key4", testInfo.infilename1.get_name(), testInfo.time1);


    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1, testInfo.time1);
    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3, testInfo.time3);
    CHECK_CACHED_VALUE(cache, "key4", testInfo.infileData1, testInfo.time1);
    BOOST_CHECK(!cache->getItem(testInfo.key2).valid());
  }


  // Drop key1.
  {
    temp::name tn("cache");
    boost::shared_ptr<file_cache> cache(cache_k(tn.get_name()));

    fileCacheTestInfo testInfo;
    setupFileCacheTest(cache, testInfo);

    CHECK_CACHED_VALUE(cache, testInfo.key1, testInfo.infileData1, testInfo.time1);
    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3, testInfo.time3);
    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2, testInfo.time2);

    cache->putItem("key4", testInfo.infilename1.get_name(), testInfo.time1);


    CHECK_CACHED_VALUE(cache, testInfo.key2, testInfo.infileData2, testInfo.time2);
    CHECK_CACHED_VALUE(cache, testInfo.key3, testInfo.infileData3, testInfo.time3);
    CHECK_CACHED_VALUE(cache, "key4", testInfo.infileData1, testInfo.time1);
    BOOST_CHECK(!cache->getItem(testInfo.key1).valid());
  }
}

BOOST_FIXTURE_TEST_CASE(fileCacheDropLeastRecentlyUsedDisk, usingTemp)
{
  temp::name tn("cache");

  runDropLeastRecentlyUsedTest(boost::lambda::bind(&file_cache::create,
						   boost::lambda::_1, 0, 1000));
}

// The changelog that's expected to be in the upgrade test database.
const std::string expectedZenityChangelog = "Source: zenity\n\
Version: 2.28.0-1\n\
Distribution: unstable\n\
Urgency: low\n\
Maintainer: Andrea Veri <andrea.veri89@gmail.com>\n\
Date: Thu, 24 Sep 2009 18:47:12 +0200\n\
Changes: \n\
 zenity (2.28.0-1) unstable; urgency=low\n\
 .\n\
   * New upstream release.\n\
   * debian/control:\n\
     - Bumped standards-version to 3.8.3. No changes needed.\n\
     - libglade2-dev B-D removed, it's no more needed as per\n\
       configure.ac requirements.\n\
     - libglib2.0-dev B-D added as per configure.ac requirements.\n\
   * debian/copyright:\n\
     - added missing copyright holders.\n\
   * debian/patches/01_focus_windows:\n\
     - removed, applied upstream\n\
   * debian/rules:\n\
     - simple-patchsys include removed as far as we have no\n\
       patches to get applied anymore.\n\
\n\
Source: zenity\n\
Version: 2.26.0-2\n\
Distribution: unstable\n\
Urgency: low\n\
Maintainer: Josselin Mouette <joss@debian.org>\n\
Date: Tue, 18 Aug 2009 18:23:10 +0200\n\
Closes: 528455 533867\n\
Changes: \n\
 zenity (2.26.0-2) unstable; urgency=low\n\
 .\n\
   * Only conflict with libgtkada-bin << 2.12.0-4, add replaces.\n\
     Closes: #533867.\n\
   * 01_focus_windows.patch: stolen upstream. Focus zenity windows by\n\
     default. Closes: #528455.\n\
";

// The versions to test upgrades from.  Version 1 was an unreleased
// cache format and didn't provide an upgrade path, so it isn't
// included in the test.
const int min_database_test_upgrade_version = 2;
const int max_database_test_upgrade_version = 3;

extern char *argv0;

void testCacheUpgradeFrom(int version)
{
  std::string argv0_dirname;

  // dirname modifies its argument, so we need to work on a copy.
  {
    char *argv0_copy = strdup(argv0);
    argv0_dirname = dirname(argv0_copy);
    free(argv0_copy);
  }

  const std::string srcdir = SRCDIR;
  std::string inputFilename = (boost::format(std::string("%s/") + srcdir + "/file_caches/ver%d_cache.db") % argv0_dirname % version).str();

  // Make a temporary copy, since the upgrade is in-place (don't want
  // to modify the test data!).
  temp::name tn("cache");


  namespace io = boost::iostreams;


  {
    io::file_source input(inputFilename);
    io::file_sink outputSink(tn.get_name());

    BOOST_REQUIRE_MESSAGE(input.is_open(), "Unable to open the input test file " << inputFilename);
    BOOST_REQUIRE_MESSAGE(outputSink.is_open(), "Unable to open the output temporary database " << tn.get_name());

    std::streamsize copySize =
      io::copy(io::file_source(inputFilename),
	       io::file_sink(tn.get_name()));

    BOOST_REQUIRE_GT(copySize, 0);
  }

  boost::shared_ptr<file_cache> cache(file_cache::create(tn.get_name(), 333, 1000));

  temp::name found(cache->getItem("delta-changelog://zenity/2.26.0-2/2.28.0-1"));

  CHECK_CACHED_VALUE(cache, "delta-changelog://zenity/2.26.0-2/2.28.0-1",
		     expectedZenityChangelog, 0);
}

// This should use parameterized test cases via BOOST_PARAM_TEST_CASE,
// but you can't use those unless you manually register test cases,
// which would mean that I'd have to have the main .cc file know all
// the parameterized test cases in the world (yuck).
BOOST_FIXTURE_TEST_CASE(testCacheUpgrade, usingTemp)
{
  for(int version = min_database_test_upgrade_version;
      version <= max_database_test_upgrade_version;
      ++version)
    {
      testCacheUpgradeFrom(version);
    }
}
