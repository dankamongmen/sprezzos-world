#include <boost/test/unit_test.hpp>

#include <generic/util/sqlite.h>

using namespace aptitude::sqlite;

// Allocates a database in memory for testing purposes.
struct memory_db_fixture
{
  boost::shared_ptr<db> tmpdb;

  memory_db_fixture()
    : tmpdb(db::create(":memory:"))
  {
  }
};

// Allocates a database in memory for testing purposes and populates
// it with some test data.
//
// Creates a table "test" with columns A (primary key), B, C, and
// inserts some values:
//
// A       B            C
// 50      "aardvark"   -5
// 51      "balderdash" -5
// 52      "contusion"  x'5412'
struct test_db_fixture : public memory_db_fixture
{
  test_db_fixture()
  {
    statement::prepare(*tmpdb, "create table test(A integer primary key, B text, C integer)")->exec();
    statement::prepare(*tmpdb, "insert into test (A, B, C) values (50, 'aardvark', -5)")->exec();
    statement::prepare(*tmpdb, "insert into test (A, B, C) values (51, 'balderdash', -5)")->exec();
    statement::prepare(*tmpdb, "insert into test (A, B, C) values (52, 'contusion', X'5412')")->exec();
  }
};

BOOST_AUTO_TEST_CASE(cantOpenDb)
{
  // Test that a failed open throws an exception (don't know how to
  // test that it doesn't leak, which is the other thing we want).
  //
  // Note: This test will fail if the ridiculous name it uses exists;
  // a nice enhancement would be to generate names on the fly with a
  // RNG.

  BOOST_REQUIRE_THROW(db::create("ridiculous-and-not-existing-database-name-foo-12983474yf4yrt1839y4vcf8913bh4fiuv",
				 SQLITE_OPEN_READWRITE),
		      exception);
}

BOOST_FIXTURE_TEST_CASE(prepareStatement, memory_db_fixture)
{
  statement::prepare(*tmpdb, "create table foo(bar int)");
  statement::prepare(*tmpdb, std::string("create table foo(bar int)"));
}

BOOST_FIXTURE_TEST_CASE(prepareStatementFail, memory_db_fixture)
{
  BOOST_REQUIRE_THROW(statement::prepare(*tmpdb, "select * from bar"),
		      exception);
}

// Test that we can create the test DB and do nothing else.
BOOST_FIXTURE_TEST_CASE(testSetupDb, test_db_fixture)
{
}

BOOST_FIXTURE_TEST_CASE(testGetBlob, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select A, B, C from test where A = 52");

  {
    statement::execution ex(*stmt);
    BOOST_REQUIRE(ex.step());

    int len = -1;
    const void *val;

    val = stmt->get_blob(0, len);
    const char * const fiftytwo = "52";
    BOOST_CHECK_EQUAL(len, strlen(fiftytwo));
    BOOST_CHECK_EQUAL_COLLECTIONS(fiftytwo, fiftytwo + strlen(fiftytwo),
				  reinterpret_cast<const char *>(val),
				  reinterpret_cast<const char *>(val) + len);

    val = stmt->get_blob(1, len);
    const char * const contusion = "contusion";
    BOOST_CHECK_EQUAL(len, strlen(contusion));
    BOOST_CHECK_EQUAL_COLLECTIONS(contusion, contusion + strlen(contusion),
				  reinterpret_cast<const char *>(val),
				  reinterpret_cast<const char *>(val) + len);

    val = stmt->get_blob(2, len);
    const char arr[2] = { 0x54, 0x12 };
    BOOST_CHECK_EQUAL(len, sizeof(arr));
    BOOST_CHECK_EQUAL_COLLECTIONS(arr, arr + sizeof(arr),
				  reinterpret_cast<const char *>(val),
				  reinterpret_cast<const char *>(val) + len);

    BOOST_CHECK(!ex.step());
    BOOST_CHECK_THROW(stmt->get_blob(2, len),
		      exception);
  }
}

BOOST_FIXTURE_TEST_CASE(testGetDouble, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select C from test where A = 51");

  {
    statement::execution ex(*stmt);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(stmt->get_double(0), -5);

    BOOST_CHECK(!ex.step());
    BOOST_CHECK_THROW(stmt->get_double(0),
		      exception);
  }
}

BOOST_FIXTURE_TEST_CASE(testGetInt, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select A from test where A <> 51 order by A");

  {
    statement::execution ex(*stmt);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(stmt->get_int(0), 50);

    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(stmt->get_int(0), 52);

    BOOST_CHECK(!ex.step());
    BOOST_CHECK_THROW(stmt->get_int(0),
		      exception);
  }
}

BOOST_FIXTURE_TEST_CASE(testGetInt64, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select A from test where A <> 51 order by A");

  {
    statement::execution ex(*stmt);
    BOOST_REQUIRE(ex.step());
    BOOST_CHECK_EQUAL(stmt->get_int64(0), 50);

    BOOST_REQUIRE(ex.step());
    BOOST_CHECK_EQUAL(stmt->get_int64(0), 52);

    BOOST_CHECK(!ex.step());
    BOOST_CHECK_THROW(stmt->get_int64(0),
		      exception);
  }
}

BOOST_FIXTURE_TEST_CASE(testGetString, test_db_fixture)
{
  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select B from test where C = -5 order by A");

  {
    statement::execution ex(*stmt);
    BOOST_REQUIRE(ex.step());
    BOOST_CHECK_EQUAL(stmt->get_string(0), "aardvark");

    BOOST_REQUIRE(ex.step());
    BOOST_CHECK_EQUAL(stmt->get_string(0), "balderdash");

    BOOST_CHECK(!ex.step());
    BOOST_CHECK_THROW(stmt->get_string(0),
		      exception);
  }
}

BOOST_FIXTURE_TEST_CASE(testGetCachedStatement, memory_db_fixture)
{
  tmpdb->set_statement_cache_limit(2);
  db::statement_proxy p1(tmpdb->get_cached_statement("create table foo(bar int)"));
  db::statement_proxy p2(tmpdb->get_cached_statement("create table foo(bar int)"));
  p2.reset();
  p1.reset();

  db::statement_proxy p3(tmpdb->get_cached_statement("create table foo(bar int)"));
  db::statement_proxy p4(tmpdb->get_cached_statement("create table bar(foo int)"));

  // Test that statements are being reused.
  statement * const stmt1(&*p4);

  p3.reset();
  p4.reset();

  db::statement_proxy p5(tmpdb->get_cached_statement("create table bar(foo int)"));

  statement * const stmt2(&*p5);

  BOOST_CHECK_EQUAL(stmt1, stmt2);

  // Use the statements for some trivial operations.
  db::statement_proxy p6(tmpdb->get_cached_statement("create table foo(bar int)"));
  p6->exec();

  db::statement_proxy p7(tmpdb->get_cached_statement("insert into foo (bar) values (5)"));
  db::statement_proxy p8(tmpdb->get_cached_statement("select bar from foo"));

  p7->exec();

  {
    statement::execution ex(*p8);
    BOOST_REQUIRE(ex.step());
    BOOST_CHECK_EQUAL(p8->get_int(0), 5);
    BOOST_CHECK(!ex.step());
  }
}

BOOST_FIXTURE_TEST_CASE(getCachedStatementFail, memory_db_fixture)
{
  BOOST_REQUIRE_THROW(statement::prepare(*tmpdb, "select * from bar"),
		      exception);
  db::statement_proxy p1(tmpdb->get_cached_statement("create table foo(bar int)"));
  db::statement_proxy p2(tmpdb->get_cached_statement("create table foo(bar int)"));
}

struct parameter_binding_test : public test_db_fixture
{
  boost::shared_ptr<statement> get_C_statement;
  boost::shared_ptr<statement> put_statement;
  static const int test_A = 10;

  parameter_binding_test()
  {
    get_C_statement = statement::prepare(*tmpdb, "select C from test where A = ?");
    put_statement = statement::prepare(*tmpdb, "insert into test (A, B, C) values (?, NULL, ?)");
  }
};

BOOST_FIXTURE_TEST_CASE(testBindBlob, parameter_binding_test)
{
  put_statement->bind_int(1, test_A);

  const unsigned char data[8] = { 0x54, 0x10, 0x20, 0x67,
			 0xd9, 0x45, 0xbd, 0x1a };
  BOOST_CHECK_THROW(put_statement->bind_blob(3, data, sizeof(data)),
		    exception);
  put_statement->bind_blob(2, data, sizeof(data));
  put_statement->exec();

  // Test that the data is still there.
  get_C_statement->bind_int(1, test_A);
  {
    statement::execution ex(*get_C_statement);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(get_C_statement->get_column_type(0), SQLITE_BLOB);

    int blob_bytes;
    const void *blob(get_C_statement->get_blob(0, blob_bytes));
    BOOST_CHECK_EQUAL(blob_bytes, sizeof(data));
    BOOST_CHECK_EQUAL_COLLECTIONS(reinterpret_cast<const unsigned char *>(blob),
				  reinterpret_cast<const unsigned char *>(blob) + blob_bytes,
				  data, data + sizeof(data));

    BOOST_CHECK(!ex.step());
  }
}

BOOST_FIXTURE_TEST_CASE(testBindDouble, parameter_binding_test)
{
  put_statement->bind_int(1, test_A);

  const double data = 9876543.123;

  BOOST_CHECK_THROW(put_statement->bind_double(3, data),
		    exception);
  put_statement->bind_double(2, data);
  put_statement->exec();

  get_C_statement->bind_int(1, test_A);
  {
    statement::execution ex(*get_C_statement);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(get_C_statement->get_column_type(0), SQLITE_FLOAT);

    BOOST_CHECK_EQUAL(get_C_statement->get_double(0), data);

    BOOST_CHECK(!ex.step());
  }
}

BOOST_FIXTURE_TEST_CASE(testBindInt, parameter_binding_test)
{
  put_statement->bind_int(1, test_A);

  const int data = 0x948291ff;

  BOOST_CHECK_THROW(put_statement->bind_int(3, data),
		    exception);
  put_statement->bind_int(2, data);
  put_statement->exec();

  get_C_statement->bind_int(1, test_A);
  {
    statement::execution ex(*get_C_statement);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(get_C_statement->get_column_type(0), SQLITE_INTEGER);

    BOOST_CHECK_EQUAL(get_C_statement->get_int(0), data);

    BOOST_CHECK(!ex.step());
  }
}

BOOST_FIXTURE_TEST_CASE(testBindInt64, parameter_binding_test)
{
  put_statement->bind_int(1, test_A);

  const sqlite_int64 data = 0x948291ff01234567LL;

  BOOST_CHECK_THROW(put_statement->bind_int64(3, data),
		    exception);
  put_statement->bind_int64(2, data);
  put_statement->exec();

  get_C_statement->bind_int(1, test_A);
  {
    statement::execution ex(*get_C_statement);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(get_C_statement->get_column_type(0), SQLITE_INTEGER);

    BOOST_CHECK_EQUAL(get_C_statement->get_int64(0), data);

    BOOST_CHECK(!ex.step());
  }
}

BOOST_FIXTURE_TEST_CASE(testBindNull, parameter_binding_test)
{
  put_statement->bind_int(1, test_A);

  BOOST_CHECK_THROW(put_statement->bind_null(3),
		    exception);
  put_statement->bind_null(2);
  put_statement->exec();

  get_C_statement->bind_int(1, test_A);
  {
    statement::execution ex(*get_C_statement);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(get_C_statement->get_column_type(0), SQLITE_NULL);

    BOOST_CHECK(!ex.step());
  }
}

BOOST_FIXTURE_TEST_CASE(testBindString, parameter_binding_test)
{
  put_statement->bind_int(1, test_A);

  const std::string data("abracadabra");

  BOOST_CHECK_THROW(put_statement->bind_string(3, data),
		    exception);
  put_statement->bind_string(2, data);
  put_statement->exec();

  get_C_statement->bind_int(1, test_A);
  {
    statement::execution ex(*get_C_statement);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(get_C_statement->get_column_type(0), SQLITE_TEXT);

    BOOST_CHECK_EQUAL(get_C_statement->get_string(0), data);

    BOOST_CHECK(!ex.step());
  }
}

BOOST_FIXTURE_TEST_CASE(testBindZeroBlob, parameter_binding_test)
{
  put_statement->bind_int(1, test_A);

  const char testBlobData[7] = { 0, 0, 0, 0, 0, 0, 0 };

  BOOST_CHECK_THROW(put_statement->bind_zeroblob(3, sizeof(testBlobData)),
		    exception);
  put_statement->bind_zeroblob(2, sizeof(testBlobData));
  put_statement->exec();

  get_C_statement->bind_int(1, test_A);
  {
    statement::execution ex(*get_C_statement);
    BOOST_REQUIRE(ex.step());

    BOOST_CHECK_EQUAL(get_C_statement->get_column_type(0), SQLITE_BLOB);

    int blob_bytes;
    const void *blob(get_C_statement->get_blob(0, blob_bytes));
    BOOST_CHECK_EQUAL(blob_bytes, sizeof(testBlobData));
    BOOST_CHECK_EQUAL_COLLECTIONS(reinterpret_cast<const char *>(blob),
				  reinterpret_cast<const char *>(blob) + blob_bytes,
				  testBlobData, testBlobData + sizeof(testBlobData));

    BOOST_CHECK(!ex.step());
  }
}

struct test_blob_fixture : public test_db_fixture
{
  sqlite_int64 blob_rowid;

  test_blob_fixture()
  {
    boost::shared_ptr<statement> s(statement::prepare(*tmpdb, "select ROWID from test where A = 52"));

    {
      statement::execution ex(*s);
      BOOST_REQUIRE(ex.step());
      blob_rowid = s->get_int64(0);
      BOOST_CHECK(!ex.step());
    }
  }
};

BOOST_FIXTURE_TEST_CASE(testOpenBlob, test_blob_fixture)
{
  blob::open(*tmpdb,
	     "main",
	     "test",
	     "C",
	     blob_rowid);

  BOOST_CHECK_THROW(blob::open(*tmpdb,
			       "nosuchdb",
			       "test",
			       "C",
			       blob_rowid),
		    exception);
  BOOST_CHECK_THROW(blob::open(*tmpdb,
			       "main",
			       "nosuchtable",
			       "C",
			       blob_rowid),
		    exception);
  BOOST_CHECK_THROW(blob::open(*tmpdb,
			       "main",
			       "test",
			       "NOSUCHCOLUMN",
			       blob_rowid),
		    exception);
  // This rowid is guaranteed not to map to anything because the rowid
  // is always equal to the primary key.
  BOOST_CHECK_THROW(blob::open(*tmpdb,
			       "main",
			       "test",
			       "C",
			       100),
		    exception);
}

BOOST_FIXTURE_TEST_CASE(testBlobSize, test_blob_fixture)
{
  boost::shared_ptr<blob> b = blob::open(*tmpdb,
					 "main",
					 "test",
					 "C",
					 blob_rowid);

  BOOST_CHECK_EQUAL(b->size(), 2);
}

BOOST_FIXTURE_TEST_CASE(testBlobRead, test_blob_fixture)
{
  boost::shared_ptr<blob> b = blob::open(*tmpdb,
					 "main",
					 "test",
					 "C",
					 blob_rowid);

  char contents[3];
  const char expected[] = { 0x54, 0x12 };
  b->read(0, contents, 2);
  BOOST_CHECK_EQUAL_COLLECTIONS(contents, contents + 2,
				expected, expected + 2);
  b->read(0, contents, 2);
  BOOST_CHECK_EQUAL_COLLECTIONS(contents, contents + 2,
				expected, expected + 2);

  BOOST_CHECK_THROW(b->read(0, contents, 3), exception);
}

BOOST_FIXTURE_TEST_CASE(testBlobWrite, test_blob_fixture)
{
  const char data[2] = { 0x54, 0x11 };
  {
    boost::shared_ptr<blob> b = blob::open(*tmpdb,
					   "main",
					   "test",
					   "C",
					   blob_rowid);

    b->write(1, data + 1, 1);
    BOOST_CHECK_THROW(b->write(1, data, 2), exception);
  }

  boost::shared_ptr<statement> stmt =
    statement::prepare(*tmpdb, "select C from test where rowid = ?");
  stmt->bind_int64(1, blob_rowid);
  {
    statement::execution ex(*stmt);
    BOOST_REQUIRE(ex.step());

    int len = -1;
    const void *val = stmt->get_blob(0, len);
    BOOST_CHECK_EQUAL(len, sizeof(data));
    BOOST_CHECK_EQUAL_COLLECTIONS(data, data + sizeof(data),
				  reinterpret_cast<const char *>(val),
				  reinterpret_cast<const char *>(val) + len);

    BOOST_CHECK(!ex.step());
  }
}
