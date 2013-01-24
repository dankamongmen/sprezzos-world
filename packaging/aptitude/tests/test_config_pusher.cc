// test_config_pusher.cc
//
//   Copyright (C) 2007 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include <cppunit/extensions/HelperMacros.h>

#include <generic/apt/config_signal.h>

class ConfigPusherTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(ConfigPusherTest);

  CPPUNIT_TEST(testConfigPush);

  CPPUNIT_TEST_SUITE_END();

private:
  void testConfigPush()
  {
    static const char * const testTree = "TestTree::CfgPusher";

    Configuration cfg;
    CPPUNIT_ASSERT_EQUAL(std::string("asdf"), cfg.Find(testTree, "asdf"));

    {
      config_change_pusher pusher(testTree, 6, cfg);

      CPPUNIT_ASSERT_EQUAL(6, cfg.FindI(testTree, 0));
    }

    CPPUNIT_ASSERT_EQUAL(std::string("asdf"), cfg.Find(testTree, "asdf"));

    {
      config_change_pusher pusher(testTree, "Blah", cfg);

      CPPUNIT_ASSERT_EQUAL(std::string("Blah"), cfg.Find(testTree, ""));
    }

    CPPUNIT_ASSERT_EQUAL(std::string("asdf"), cfg.Find(testTree, "asdf"));

    {
      config_change_pusher pusher(testTree, true, cfg);

      CPPUNIT_ASSERT_EQUAL(true, cfg.FindB(testTree, false));
    }

    CPPUNIT_ASSERT_EQUAL(std::string("asdf"), cfg.Find(testTree, "asdf"));


    cfg.Set(testTree, 10);
    CPPUNIT_ASSERT_EQUAL(10, cfg.FindI(testTree, 0));

    {
      config_change_pusher pusher(testTree, 6, cfg);

      CPPUNIT_ASSERT_EQUAL(6, cfg.FindI(testTree, 0));
    }

    CPPUNIT_ASSERT_EQUAL(10, cfg.FindI(testTree, 0));

    cfg.Set(testTree, "Tada");
    CPPUNIT_ASSERT_EQUAL(std::string("Tada"), cfg.Find(testTree, ""));

    {
      config_change_pusher pusher(testTree, "Blah", cfg);

      CPPUNIT_ASSERT_EQUAL(std::string("Blah"), cfg.Find(testTree, ""));
    }

    CPPUNIT_ASSERT_EQUAL(std::string("Tada"), cfg.Find(testTree, ""));

    cfg.Set(testTree, false);
    CPPUNIT_ASSERT_EQUAL(false, cfg.FindB(testTree, false));

    {
      config_change_pusher pusher(testTree, true, cfg);

      CPPUNIT_ASSERT_EQUAL(true, cfg.FindB(testTree, false));
    }

    CPPUNIT_ASSERT_EQUAL(false, cfg.FindB(testTree, false));
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(ConfigPusherTest);
