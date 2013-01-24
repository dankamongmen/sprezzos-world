/** \file test_search_input.cc */

// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include <generic/apt/matching/compare_patterns.h>
#include <generic/apt/matching/pattern.h>
#include <generic/controllers/search_input.h>
#include <generic/views/mocks/search_input.h>

#include <boost/make_shared.hpp>
#include <boost/test/unit_test.hpp>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

namespace ctrls = aptitude::controllers;
namespace views = aptitude::views;
namespace mocks = aptitude::views::mocks;

using aptitude::matching::compare_patterns;
using aptitude::matching::pattern;
using boost::make_shared;
using boost::shared_ptr;
using cwidget::util::ref_ptr;
using testing::AnyNumber;
using testing::Expectation;
using testing::InSequence;
using testing::NiceMock;
using testing::Return;
using testing::StrictMock;

namespace
{
  MATCHER_P(PatternsEqual,
            expected,
            "")
  {
    return compare_patterns(arg, expected) == 0;
  }

  // Mock class that lets us capture the controller's outgoing
  // signals.
  class controller_callbacks
  {
  public:
    MOCK_METHOD2(activated, void(std::wstring, cwidget::util::ref_ptr<aptitude::matching::pattern>));
  };

  struct SearchInputTest
  {
    boost::shared_ptr<mocks::search_input> view;
    boost::shared_ptr<mocks::search_input> viewNice;
    boost::shared_ptr<mocks::search_input> viewStrict;

    controller_callbacks callbacks;
    controller_callbacks callbacksNice;
    controller_callbacks callbacksStrict;

    // Controllers are initialized lazily, to give us a chance to
    // register expectations with the views first.
  private:
    boost::shared_ptr<ctrls::search_input> controller;
    boost::shared_ptr<ctrls::search_input> controllerNice;
    boost::shared_ptr<ctrls::search_input> controllerStrict;

  public:
    boost::shared_ptr<ctrls::search_input> get_controller()
    {
      if(controller.get() == NULL)
        {
          controller = ctrls::create_search_input(view);
          controller->connect_activated(sigc::mem_fun(callbacks,
                                                      &controller_callbacks::activated));
        }
      return controller;
    }

    boost::shared_ptr<ctrls::search_input> get_controllerNice()
    {
      if(controllerNice.get() == NULL)
        {
          controllerNice = ctrls::create_search_input(view);
          controllerNice->connect_activated(sigc::mem_fun(callbacksNice,
                                                          &controller_callbacks::activated));
        }
      return controllerNice;
    }

    boost::shared_ptr<ctrls::search_input> get_controllerStrict()
    {
      if(controllerStrict.get() == NULL)
        {
          controllerStrict = ctrls::create_search_input(view);
          controllerStrict->connect_activated(sigc::mem_fun(callbacksStrict,
                                                            &controller_callbacks::activated));
        }
      return controllerStrict;
    }

    SearchInputTest()
      : view(make_shared<mocks::search_input>()),
        viewNice(make_shared<NiceMock<mocks::search_input> >()),
        viewStrict(make_shared<StrictMock<mocks::search_input> >())
    {
    }
  };
}

BOOST_FIXTURE_TEST_CASE(testEnteringCorrectTextSearches, SearchInputTest)
{
  ref_ptr<pattern> p = pattern::make_installed();
  std::wstring p_text = L"?installed";

  // The controller isn't required to call set_error_message(), but it
  // should set it to "" if it does.
  EXPECT_CALL(*view, set_error_message(std::wstring())).Times(AnyNumber());

  Expectation set_search_text;
  {
    InSequence dummy;

    set_search_text = EXPECT_CALL(*view, set_search_text(p_text));
    EXPECT_CALL(callbacks, activated(p_text, PatternsEqual(p)));
  }

  // After setting the search text, get_search_text() will return the
  // new text.
  EXPECT_CALL(*view, get_search_text())
    .After(set_search_text)
    .WillRepeatedly(Return(p_text));

  get_controller()->enter_text(p_text);
}
