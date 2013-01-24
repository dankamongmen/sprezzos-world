// test_matching.cc
//
//   Copyright (C) 2008-2009, 2011 Daniel Burrows
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

#include <generic/apt/matching/compare_patterns.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/matching/serialize.h>

#include <cwidget/generic/util/ssprintf.h>

#include <apt-pkg/error.h>

using namespace aptitude::matching;
using cwidget::util::ref_ptr;
using cwidget::util::ssprintf;

namespace
{
  // We test this several ways.
  //
  // We test that the expected pattern equals itself.
  //
  // We test that patterns which don't have the same expected
  // serialization don't equal each other.
  //
  // We test that the comparison function is a total order on the test
  // set.
  //
  // We test that the input pattern parses to the expected pattern.
  //
  // We test that the expected pattern and the input pattern's parse
  // serialize to the expected serialization.
  //
  // We test that the expected serialization parses to the expected
  // pattern.
  struct pattern_test
  {
    std::string input_pattern;
    std::string expected_serialization;
    ref_ptr<pattern> expected_pattern;
  };

  pattern_test test_patterns[] = {
    { "?for x: ?=x ?for y: ?depends(?depends(?=y) ?depends(?=x))",
      "?for x: ?=x ?for y: ?depends(?depends(?=y) ?depends(?=x))",
      pattern::make_for("x",
			pattern::make_and(pattern::make_equal(0),
					  pattern::make_for("y",
							    pattern::make_depends(pkgCache::Dep::Depends, false,
										  pattern::make_and(pattern::make_depends(pkgCache::Dep::Depends, false, pattern::make_equal(1)),
												    pattern::make_depends(pkgCache::Dep::Depends, false, pattern::make_equal(0))))))) },

    { "?not(!~nfoo)", "!(!?name(\"foo\"))",
      pattern::make_not(pattern::make_not(pattern::make_name("foo"))) },

    { "?not(?false)", "!?false",
      pattern::make_not(pattern::make_false()) },


    { "?action(install)", "?action(install)",
      pattern::make_action(pattern::action_install) },

    { "?action(upgrade)", "?action(upgrade)",
      pattern::make_action(pattern::action_upgrade) },

    { "?action(downgrade)", "?action(downgrade)",
      pattern::make_action(pattern::action_downgrade) },

    { "?action(remove)", "?action(remove)",
      pattern::make_action(pattern::action_remove) },

    { "?action(purge)", "?action(purge)",
      pattern::make_action(pattern::action_purge) },

    { "?action(reinstall)", "?action(reinstall)",
      pattern::make_action(pattern::action_reinstall) },

    { "?action(hold)", "?action(hold)",
      pattern::make_action(pattern::action_hold) },

    { "?action(keep)", "?action(keep)",
      pattern::make_action(pattern::action_keep) },

    { "~ainstall", "?action(install)",
      pattern::make_action(pattern::action_install) },

    { "~aupgrade", "?action(upgrade)",
      pattern::make_action(pattern::action_upgrade) },

    { "~adowngrade", "?action(downgrade)",
      pattern::make_action(pattern::action_downgrade) },

    { "~aremove", "?action(remove)",
      pattern::make_action(pattern::action_remove) },

    { "~apurge", "?action(purge)",
      pattern::make_action(pattern::action_purge) },

    { "~areinstall", "?action(reinstall)",
      pattern::make_action(pattern::action_reinstall) },

    { "~ahold", "?action(hold)",
      pattern::make_action(pattern::action_hold) },

    { "~akeep", "?action(keep)",
      pattern::make_action(pattern::action_keep) },

    { "?all-versions(~nelba~|a~\"ble)", "?all-versions(?name(\"elba|a\\\"ble\"))",
      pattern::make_all_versions(pattern::make_name("elba|a\"ble")) },

    { "?all-versions(~c)", "?all-versions(?config-files)",
      pattern::make_all_versions(pattern::make_config_files()) },

    { "?and(?maintainer(xyz), ?broken ?broken-depends, ?archive(stable))",
      "?maintainer(\"xyz\") (?broken ?broken-depends) ?archive(\"stable\")",
      pattern::make_and(pattern::make_maintainer("xyz"),
			pattern::make_and(pattern::make_broken(),
					  pattern::make_broken_type(pkgCache::Dep::Depends)),
			pattern::make_archive("stable")) },

    { "?any-version(~Tasdf)", "?any-version(?true ?name(\"asdf\"))",
      pattern::make_any_version(pattern::make_and(pattern::make_true(),
						  pattern::make_name("asdf"))) },

    { "?any-version(~i)", "?any-version(?installed)",
      pattern::make_any_version(pattern::make_installed()) },

    { "~A \"^asdf.*asdf$\"", "?archive(\"^asdf.*asdf$\")",
      pattern::make_archive("^asdf.*asdf$") },

    { "?archive(^asdf.*asdf$)", "?archive(\"^asdf.*asdf$\")",
      pattern::make_archive("^asdf.*asdf$") },

    { "?automatic test", "?automatic ?name(\"test\")",
      pattern::make_and(pattern::make_automatic(),
			pattern::make_name("test")) },

    { "~M test", "?automatic ?name(\"test\")",
      pattern::make_and(pattern::make_automatic(),
			pattern::make_name("test")) },

    { "?for x: ?depends(?for y: ?bind(y, ?source-package(argle~~)))",
      "?for x: ?depends(?for y: ?bind(y, ?source-package(\"argle~\")))",
      pattern::make_for("x",
			pattern::make_depends(pkgCache::Dep::Depends,
					      false,
					      pattern::make_for("y",
								pattern::make_bind(1, pattern::make_source_package("argle~"))))) },

    { "?for x: ?depends(?for y: ?bind(y, ?source-version(argle~~)))",
      "?for x: ?depends(?for y: ?bind(y, ?source-version(\"argle~\")))",
      pattern::make_for("x",
			pattern::make_depends(pkgCache::Dep::Depends,
					      false,
					      pattern::make_for("y",
								pattern::make_bind(1, pattern::make_source_version("argle~"))))) },

    { "?for x: ?true", "?for x: ?true",
      pattern::make_for("x", pattern::make_true()) },

    { "?for x: ?false", "?for x: ?false",
      pattern::make_for("x", pattern::make_false()) },

    { "?broken", "?broken", pattern::make_broken() },

    { "~basdf", "?broken ?name(\"asdf\")",
      pattern::make_and(pattern::make_broken(),
			pattern::make_name("asdf")) },

    { "?broken-depends", "?broken-depends",
      pattern::make_broken_type(pkgCache::Dep::Depends) },

    { "?broken-predepends", "?broken-predepends",
      pattern::make_broken_type(pkgCache::Dep::PreDepends) },

    { "?broken-recommends", "?broken-recommends",
      pattern::make_broken_type(pkgCache::Dep::Recommends) },

    { "?broken-suggests", "?broken-suggests",
      pattern::make_broken_type(pkgCache::Dep::Suggests) },

    { "?broken-breaks", "?broken-breaks",
      pattern::make_broken_type(pkgCache::Dep::DpkgBreaks) },

    { "?broken-conflicts", "?broken-conflicts",
      pattern::make_broken_type(pkgCache::Dep::Conflicts) },

    { "?broken-replaces", "?broken-replaces",
      pattern::make_broken_type(pkgCache::Dep::Replaces) },

    { "~Bdepends", "?broken-depends",
      pattern::make_broken_type(pkgCache::Dep::Depends) },

    { "~Bpredepends", "?broken-predepends",
      pattern::make_broken_type(pkgCache::Dep::PreDepends) },

    { "~Brecommends", "?broken-recommends",
      pattern::make_broken_type(pkgCache::Dep::Recommends) },

    { "~Bsuggests", "?broken-suggests",
      pattern::make_broken_type(pkgCache::Dep::Suggests) },

    { "~Bbreaks", "?broken-breaks",
      pattern::make_broken_type(pkgCache::Dep::DpkgBreaks) },

    { "~Bconflicts", "?broken-conflicts",
      pattern::make_broken_type(pkgCache::Dep::Conflicts) },

    { "~Breplaces", "?broken-replaces",
      pattern::make_broken_type(pkgCache::Dep::Replaces) },

    // Test that all the broken and dependency-type variations work.

    { "?depends(?true)", "?depends(?true)",
      pattern::make_depends(pkgCache::Dep::Depends,
			    false,
			    pattern::make_true()) },

    // Repeated so we can test that just changing the sub-pattern
    // causes a mismatch.
    { "?depends(?false)", "?depends(?false)",
      pattern::make_depends(pkgCache::Dep::Depends,
			    false,
			    pattern::make_false()) },

    { "?predepends(?true)", "?predepends(?true)",
      pattern::make_depends(pkgCache::Dep::PreDepends,
			    false,
			    pattern::make_true()) },

    { "?suggests(?true)", "?suggests(?true)",
      pattern::make_depends(pkgCache::Dep::Suggests,
			    false,
			    pattern::make_true()) },

    { "?recommends(?true)", "?recommends(?true)",
      pattern::make_depends(pkgCache::Dep::Recommends,
			    false,
			    pattern::make_true()) },

    { "?conflicts(?true)", "?conflicts(?true)",
      pattern::make_depends(pkgCache::Dep::Conflicts,
			    false,
			    pattern::make_true()) },

    { "?replaces(?true)", "?replaces(?true)",
      pattern::make_depends(pkgCache::Dep::Replaces,
			    false,
			    pattern::make_true()) },

    { "?breaks(?true)", "?breaks(?true)",
      pattern::make_depends(pkgCache::Dep::DpkgBreaks,
			    false,
			    pattern::make_true()) },

    // ?conflicts is handled below, with ?depends.

    { "?config-files", "?config-files", pattern::make_config_files() },

    { "~c", "?config-files", pattern::make_config_files() },

    // Test short forms.

    { "~D ?true", "?depends(?true)",
      pattern::make_depends(pkgCache::Dep::Depends,
			    false,
			    pattern::make_true()) },

    { "~Ddepends: ?true", "?depends(?true)",
      pattern::make_depends(pkgCache::Dep::Depends,
			    false,
			    pattern::make_true()) },

    { "~Dpredepends: ?true", "?predepends(?true)",
      pattern::make_depends(pkgCache::Dep::PreDepends,
			    false,
			    pattern::make_true()) },

    { "~Dsuggests: ?true", "?suggests(?true)",
      pattern::make_depends(pkgCache::Dep::Suggests,
			    false,
			    pattern::make_true()) },

    { "~Drecommends: ?true", "?recommends(?true)",
      pattern::make_depends(pkgCache::Dep::Recommends,
			    false,
			    pattern::make_true()) },

    { "~Dconflicts: ?true", "?conflicts(?true)",
      pattern::make_depends(pkgCache::Dep::Conflicts,
			    false,
			    pattern::make_true()) },

    { "~C~T", "?conflicts(?true)",
      pattern::make_depends(pkgCache::Dep::Conflicts,
			    false,
			    pattern::make_true()) },

    { "~Dreplaces: ?true", "?replaces(?true)",
      pattern::make_depends(pkgCache::Dep::Replaces,
			    false,
			    pattern::make_true()) },

    { "~Dbreaks: ?true", "?breaks(?true)",
      pattern::make_depends(pkgCache::Dep::DpkgBreaks,
			    false,
			    pattern::make_true()) },

    // Test ?broken-* forms.

    { "?broken-depends(?true)", "?broken-depends(?true)",
      pattern::make_depends(pkgCache::Dep::Depends,
			    true,
			    pattern::make_true()) },

    { "?broken-predepends(?true)", "?broken-predepends(?true)",
      pattern::make_depends(pkgCache::Dep::PreDepends,
			    true,
			    pattern::make_true()) },

    { "?broken-suggests(?true)", "?broken-suggests(?true)",
      pattern::make_depends(pkgCache::Dep::Suggests,
			    true,
			    pattern::make_true()) },

    { "?broken-recommends(?true)", "?broken-recommends(?true)",
      pattern::make_depends(pkgCache::Dep::Recommends,
			    true,
			    pattern::make_true()) },

    { "?broken-conflicts(?true)", "?broken-conflicts(?true)",
      pattern::make_depends(pkgCache::Dep::Conflicts,
			    true,
			    pattern::make_true()) },

    { "?broken-replaces(?true)", "?broken-replaces(?true)",
      pattern::make_depends(pkgCache::Dep::Replaces,
			    true,
			    pattern::make_true()) },

    { "?broken-breaks(?true)", "?broken-breaks(?true)",
      pattern::make_depends(pkgCache::Dep::DpkgBreaks,
			    true,
			    pattern::make_true()) },

    // Test ~DB forms.

    { "~DB ?true", "?broken-depends(?true)",
      pattern::make_depends(pkgCache::Dep::Depends,
			    true,
			    pattern::make_true()) },

    { "~DBdepends: ?true", "?broken-depends(?true)",
      pattern::make_depends(pkgCache::Dep::Depends,
			    true,
			    pattern::make_true()) },

    { "~DBpredepends: ?true", "?broken-predepends(?true)",
      pattern::make_depends(pkgCache::Dep::PreDepends,
			    true,
			    pattern::make_true()) },

    { "~DBsuggests: ?true", "?broken-suggests(?true)",
      pattern::make_depends(pkgCache::Dep::Suggests,
			    true,
			    pattern::make_true()) },

    { "~DBrecommends: ?true", "?broken-recommends(?true)",
      pattern::make_depends(pkgCache::Dep::Recommends,
			    true,
			    pattern::make_true()) },

    { "~DBconflicts: ?true", "?broken-conflicts(?true)",
      pattern::make_depends(pkgCache::Dep::Conflicts,
			    true,
			    pattern::make_true()) },

    { "~DBreplaces: ?true", "?broken-replaces(?true)",
      pattern::make_depends(pkgCache::Dep::Replaces,
			    true,
			    pattern::make_true()) },

    { "~DBbreaks: ?true", "?broken-breaks(?true)",
      pattern::make_depends(pkgCache::Dep::DpkgBreaks,
			    true,
			    pattern::make_true()) },

    { "?description(All the news that's fit to print)",
      "?description(\"All the news that's fit to print\")",
      pattern::make_description("All the news that's fit to print") },

    { "~d \"All the news that's fit to print\"",
      "?description(\"All the news that's fit to print\")",
      pattern::make_description("All the news that's fit to print") },

    { "?essential", "?essential", pattern::make_essential() },

    { "~E", "?essential", pattern::make_essential() },

    { "?exact-name( foo)", "?exact-name(\"foo\")",
      pattern::make_exact_name("foo") },

    { "?false", "?false", pattern::make_false() },

    { "~F", "?false", pattern::make_false() },

    // No test for ?for; it was tested above.

    { "?garbage asdf", "?garbage ?name(\"asdf\")",
      pattern::make_and(pattern::make_garbage(),
			pattern::make_name("asdf")) },

    { "~g asdf", "?garbage ?name(\"asdf\")",
      pattern::make_and(pattern::make_garbage(),
			pattern::make_name("asdf")) },

    { "?installed asdf", "?installed ?name(\"asdf\")",
      pattern::make_and(pattern::make_installed(),
			pattern::make_name("asdf")) },

    { "~i asdf", "?installed ?name(\"asdf\")",
      pattern::make_and(pattern::make_installed(),
			pattern::make_name("asdf")) },

    { "?maintainer(dburrows@debian.org)", "?maintainer(\"dburrows@debian.org\")",
      pattern::make_maintainer("dburrows@debian.org") },

    { "~m\tdburrows@debian.org asdf", "?maintainer(\"dburrows@debian.org\") ?name(\"asdf\")",
      pattern::make_and(pattern::make_maintainer("dburrows@debian.org"),
			pattern::make_name("asdf")) },

    { "?narrow(?broken, ?version(5\\.43\\.2))",
      "?narrow(?broken, ?version(\"5\\\\.43\\\\.2\"))",
      pattern::make_narrow(pattern::make_broken(),
			   pattern::make_version("5\\.43\\.2")) },

    { "~S  ?broken   ((?version(999)))",
      "?narrow(?broken, ?version(\"999\"))",
      pattern::make_narrow(pattern::make_broken(),
			   pattern::make_version("999")) },

    { "?name(elsi nore)", "?name(\"elsi nore\")",
      pattern::make_name("elsi nore") },

    { "~nelsi~ nore", "?name(\"elsi nore\")",
      pattern::make_name("elsi nore") },

    { "?new FOO", "?new ?name(\"FOO\")",
      pattern::make_and(pattern::make_new(),
			pattern::make_name("FOO")) },

    { "~N FOO", "?new ?name(\"FOO\")",
      pattern::make_and(pattern::make_new(),
			pattern::make_name("FOO")) },

    { "?obsolete", "?obsolete", pattern::make_obsolete() },

    { "~o~o", "?obsolete ?obsolete",
      pattern::make_and(pattern::make_obsolete(),
			pattern::make_obsolete()) },

    { "?and(?term(apt), ?or(?term(gui), ?installed))",
      "?term(\"apt\") (?term(\"gui\") | ?installed)",
      pattern::make_and(pattern::make_term("apt"),
			pattern::make_or(pattern::make_term("gui"),
					 pattern::make_installed())) },

    { "?and(?or(?term(gui), ?installed), ?term(apt))",
      "(?term(\"gui\") | ?installed) ?term(\"apt\")",
      pattern::make_and(pattern::make_or(pattern::make_term("gui"),
					 pattern::make_installed()),
			pattern::make_term("apt")) },

    { "?or(~nasdf,   ?new)", "?name(\"asdf\") | ?new",
      pattern::make_or(pattern::make_name("asdf"),
		       pattern::make_new()) },

    { "~nfdsa  |  ?new", "?name(\"fdsa\") | ?new",
      pattern::make_or(pattern::make_name("fdsa"),
		       pattern::make_new()) },

    { "?origin(Debian)", "?origin(\"Debian\")",
      pattern::make_origin("Debian") },

    { "~O Debian", "?origin(\"Debian\")",
      pattern::make_origin("Debian") },

    { "?provides(?true)", "?provides(?true)",
      pattern::make_provides(pattern::make_true()) },

    { "~P ?false", "?provides(?false)",
      pattern::make_provides(pattern::make_false()) },

    { "?priority(important)", "?priority(important)",
      pattern::make_priority(pkgCache::State::Important) },

    { "?priority(required)", "?priority(required)",
      pattern::make_priority(pkgCache::State::Required) },

    { "?priority(standard)", "?priority(standard)",
      pattern::make_priority(pkgCache::State::Standard) },

    { "?priority(optional)", "?priority(optional)",
      pattern::make_priority(pkgCache::State::Optional) },

    { "?priority(extra)", "?priority(extra)",
      pattern::make_priority(pkgCache::State::Extra) },

    { "~p  important", "?priority(important)",
      pattern::make_priority(pkgCache::State::Important) },

    { "~p  required", "?priority(required)",
      pattern::make_priority(pkgCache::State::Required) },

    { "~p  standard", "?priority(standard)",
      pattern::make_priority(pkgCache::State::Standard) },

    { "~p  optional", "?priority(optional)",
      pattern::make_priority(pkgCache::State::Optional) },

    { "~p  extra", "?priority(extra)",
      pattern::make_priority(pkgCache::State::Extra) },


    // Test that all the broken and dependency-type variations work.

    { "?reverse-depends(?true)", "?reverse-depends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Depends,
				    false,
				    pattern::make_true()) },

    // Test that changing the sub-pattern gives us an inequal result.
    { "?reverse-depends(?false)", "?reverse-depends(?false)",
      pattern::make_reverse_depends(pkgCache::Dep::Depends,
				    false,
				    pattern::make_false()) },

    { "?reverse-predepends(?true)", "?reverse-predepends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::PreDepends,
				    false,
				    pattern::make_true()) },

    { "?reverse-suggests(?true)", "?reverse-suggests(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Suggests,
				    false,
				    pattern::make_true()) },

    { "?reverse-recommends(?true)", "?reverse-recommends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Recommends,
				    false,
				    pattern::make_true()) },

    { "?reverse-conflicts(?true)", "?reverse-conflicts(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Conflicts,
				    false,
				    pattern::make_true()) },

    { "?reverse-replaces(?true)", "?reverse-replaces(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Replaces,
				    false,
				    pattern::make_true()) },

    { "?reverse-breaks(?true)", "?reverse-breaks(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::DpkgBreaks,
				    false,
				    pattern::make_true()) },

    // Test short forms.

    { "~Rdepends: ?true", "?reverse-depends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Depends,
				    false,
				    pattern::make_true()) },

    { "~R ?true", "?reverse-depends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Depends,
				    false,
				    pattern::make_true()) },

    { "~Rpredepends: ?true", "?reverse-predepends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::PreDepends,
				    false,
				    pattern::make_true()) },

    { "~Rsuggests: ?true", "?reverse-suggests(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Suggests,
				    false,
				    pattern::make_true()) },

    { "~Rrecommends: ?true", "?reverse-recommends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Recommends,
				    false,
				    pattern::make_true()) },

    { "~Rconflicts: ?true", "?reverse-conflicts(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Conflicts,
				    false,
				    pattern::make_true()) },

    { "~Rreplaces: ?true", "?reverse-replaces(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Replaces,
				    false,
				    pattern::make_true()) },

    { "~Rbreaks: ?true", "?reverse-breaks(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::DpkgBreaks,
				    false,
				    pattern::make_true()) },

    // Test ?broken-reverse-* forms.

    { "?broken-reverse-depends(?true)", "?broken-reverse-depends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Depends,
				    true,
				    pattern::make_true()) },

    { "?broken-reverse-predepends(?true)", "?broken-reverse-predepends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::PreDepends,
				    true,
				    pattern::make_true()) },

    { "?broken-reverse-suggests(?true)", "?broken-reverse-suggests(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Suggests,
				    true,
				    pattern::make_true()) },

    { "?broken-reverse-recommends(?true)", "?broken-reverse-recommends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Recommends,
				    true,
				    pattern::make_true()) },

    { "?broken-reverse-conflicts(?true)", "?broken-reverse-conflicts(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Conflicts,
				    true,
				    pattern::make_true()) },

    { "?broken-reverse-replaces(?true)", "?broken-reverse-replaces(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Replaces,
				    true,
				    pattern::make_true()) },

    { "?broken-reverse-breaks(?true)", "?broken-reverse-breaks(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::DpkgBreaks,
				    true,
				    pattern::make_true()) },

    // Test ?reverse-broken-* forms.

    { "?reverse-broken-depends(?true)", "?broken-reverse-depends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Depends,
				    true,
				    pattern::make_true()) },

    { "?reverse-broken-predepends(?true)", "?broken-reverse-predepends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::PreDepends,
				    true,
				    pattern::make_true()) },

    { "?reverse-broken-suggests(?true)", "?broken-reverse-suggests(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Suggests,
				    true,
				    pattern::make_true()) },

    { "?reverse-broken-recommends(?true)", "?broken-reverse-recommends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Recommends,
				    true,
				    pattern::make_true()) },

    { "?reverse-broken-conflicts(?true)", "?broken-reverse-conflicts(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Conflicts,
				    true,
				    pattern::make_true()) },

    { "?reverse-broken-replaces(?true)", "?broken-reverse-replaces(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Replaces,
				    true,
				    pattern::make_true()) },

    { "?reverse-broken-breaks(?true)", "?broken-reverse-breaks(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::DpkgBreaks,
				    true,
				    pattern::make_true()) },

    // Test ~RB forms.

    { "~RB ?true", "?broken-reverse-depends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Depends,
				    true,
				    pattern::make_true()) },

    { "~RBdepends: ?true", "?broken-reverse-depends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Depends,
				    true,
				    pattern::make_true()) },

    { "~RBpredepends: ?true", "?broken-reverse-predepends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::PreDepends,
				    true,
				    pattern::make_true()) },

    { "~RBsuggests: ?true", "?broken-reverse-suggests(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Suggests,
				    true,
				    pattern::make_true()) },

    { "~RBrecommends: ?true", "?broken-reverse-recommends(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Recommends,
				    true,
				    pattern::make_true()) },

    { "~RBconflicts: ?true", "?broken-reverse-conflicts(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Conflicts,
				    true,
				    pattern::make_true()) },

    { "~RBreplaces: ?true", "?broken-reverse-replaces(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::Replaces,
				    true,
				    pattern::make_true()) },

    { "~RBbreaks: ?true", "?broken-reverse-breaks(?true)",
      pattern::make_reverse_depends(pkgCache::Dep::DpkgBreaks,
				    true,
				    pattern::make_true()) },

    { "?section(main)", "?section(\"main\")",
      pattern::make_section("main") },

    { "~smain", "?section(\"main\")",
      pattern::make_section("main") },

    { "?source-package(\"asdf\")", "?source-package(\"asdf\")",
      pattern::make_source_package("asdf") },

    { "?source-package(\"fdsa\")", "?source-package(\"fdsa\")",
      pattern::make_source_package("fdsa") },

    { "?source-version(\"asdf\")", "?source-version(\"asdf\")",
      pattern::make_source_version("asdf") },

    { "?source-version(\"fdsa\")", "?source-version(\"fdsa\")",
      pattern::make_source_version("fdsa") },

    { "?tag(type::foo)", "?tag(\"type::foo\")",
      pattern::make_tag("type::foo") },

    { "~G type::bar", "?tag(\"type::bar\")",
      pattern::make_tag("type::bar") },

    { "?true", "?true", pattern::make_true() },

    { "?task(desktop)", "?task(\"desktop\")",
      pattern::make_task("desktop") },

    { "~t gnome", "?task(\"gnome\")",
      pattern::make_task("gnome") },

    { "?term(development)", "?term(\"development\")",
      pattern::make_term("development") },

    { "?term-prefix(aptitu)", "?term-prefix(\"aptitu\")",
      pattern::make_term_prefix("aptitu") },

    { "?upgradable", "?upgradable", pattern::make_upgradable() },

    { "~U", "?upgradable", pattern::make_upgradable() },

    { "?user-tag(asdf)", "?user-tag(\"asdf\")", pattern::make_user_tag("asdf") },

    { "?user-tag(fdsa)", "?user-tag(\"fdsa\")", pattern::make_user_tag("fdsa") },

    { "?version(1.0)", "?version(\"1.0\")",
      pattern::make_version("1.0") },

    { "~V2.0", "?version(\"2.0\")",
      pattern::make_version("2.0") },

    { "~v" , "?virtual", pattern::make_virtual() },

    { "?widen(?true)", "?widen(?true)",
      pattern::make_widen(pattern::make_true()) },

    { "?widen(?false)", "?widen(?false)",
      pattern::make_widen(pattern::make_false()) },

    { "~", "?name(\"~\")",
      pattern::make_name("~") },

    { "abc", "?name(\"abc\")",
      pattern::make_name("abc") },
  };

  const int num_test_patterns = sizeof(test_patterns) / sizeof(test_patterns[0]);
}

class MatchingTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(MatchingTest);

  CPPUNIT_TEST(testTotalOrder);
  CPPUNIT_TEST(testParse);
  CPPUNIT_TEST(testParseThenSerialize);
  CPPUNIT_TEST(testSerialize);
  CPPUNIT_TEST(testSerializationParse);

  CPPUNIT_TEST_SUITE_END();

  class intArray2d
  {
    int *entries;
    int num_rows;
    int num_cols;

    intArray2d(const intArray2d &);

  public:
    intArray2d(int _num_rows, int _num_cols)
      : entries(new int[_num_rows * _num_cols]),
	num_rows(_num_rows), num_cols(_num_cols)
    {
    }

    ~intArray2d()
    {
      delete[] entries;
    }

    int &operator()(int row, int col)
    {
      return entries[row * num_cols + col];
    }

    const int &operator()(int row, int col) const
    {
      return entries[row * num_cols + col];
    }
  };

public:
  void testTotalOrder()
  {
    intArray2d comparisons(num_test_patterns, num_test_patterns);
    for(int i = 0; i < num_test_patterns; ++i)
      for(int j = 0; j < num_test_patterns; ++j)
	comparisons(i, j) = compare_patterns(test_patterns[i].expected_pattern,
					     test_patterns[j].expected_pattern);

    // We need to verify three properties:
    //
    // (1) Reflexivity: identical patterns (ones
    //     that have identical serialization) should
    //     be equal.
    // (2) Antisymmetry: if a < b then b > a.  If
    //     a = b then b = a and the patterns are
    //     identical.
    // (3) Transitivity: if a <= b and b <= c,
    //     then a <= c.  if a >= b and b >= c,
    //     then a >= c.

    // Reflexivity.
    for(int i = 0; i < num_test_patterns; ++i)
      for(int j = 0; j < num_test_patterns; ++j)
	{
	  if(test_patterns[i].expected_serialization ==
	     test_patterns[j].expected_serialization)
	    CPPUNIT_ASSERT_EQUAL(0, comparisons(i, j));
	  else
	    CPPUNIT_ASSERT_MESSAGE(ssprintf("Comparing %s to %s",
					    serialize_pattern(test_patterns[i].expected_pattern).c_str(),
					    serialize_pattern(test_patterns[j].expected_pattern).c_str()),
			   0 != comparisons(i, j));
	}

    // Antisymmetry.
    for(int i = 0; i < num_test_patterns; ++i)
      for(int j = 0; j < num_test_patterns; ++j)
	{
	  const int ij = comparisons(i, j);
	  const int ji = comparisons(j, i);

	  if(ij < 0)
	    CPPUNIT_ASSERT_MESSAGE(ssprintf("Comparing %s to %s",
					    serialize_pattern(test_patterns[j].expected_pattern).c_str(),
					    serialize_pattern(test_patterns[i].expected_pattern).c_str()),
				   ji > 0);
	  else if(ij > 0)
	    CPPUNIT_ASSERT_MESSAGE(ssprintf("Comparing %s to %s",
					    serialize_pattern(test_patterns[i].expected_pattern).c_str(),
					    serialize_pattern(test_patterns[j].expected_pattern).c_str()),
				   ji < 0);
	  else
	    CPPUNIT_ASSERT_EQUAL(test_patterns[i].expected_serialization,
				 test_patterns[j].expected_serialization);
	}

    for(int i = 0; i < num_test_patterns; ++i)
      for(int j = 0; j < num_test_patterns; ++j)
	for(int k = 0; k < num_test_patterns; ++k)
	  {
	    int ij = comparisons(i, j);
	    int jk = comparisons(j, k);
	    int ik = comparisons(i, k);

	    if(ij <= 0 && jk <= 0)
	      CPPUNIT_ASSERT_MESSAGE(ssprintf("Comparing %s to %s",
					      serialize_pattern(test_patterns[i].expected_pattern).c_str(),
					      serialize_pattern(test_patterns[k].expected_pattern).c_str()),
			     ik <= 0);

	    if(ij >= 0 && jk >= 0)
	      CPPUNIT_ASSERT_MESSAGE(ssprintf("Comparing %s to %s",
					      serialize_pattern(test_patterns[i].expected_pattern).c_str(),
					      serialize_pattern(test_patterns[k].expected_pattern).c_str()),
			     ik >= 0);
	  }
  }

  void testParse()
  {
    for(int i = 0; i < num_test_patterns; ++i)
      {
	const pattern_test &test(test_patterns[i]);

	ref_ptr<pattern> parsed(parse(test.input_pattern));
	_error->DumpErrors();
	CPPUNIT_ASSERT(parsed.valid());

	CPPUNIT_ASSERT_EQUAL_MESSAGE(ssprintf("Comparing %s to %s",
					      serialize_pattern(parsed).c_str(),
					      serialize_pattern(test.expected_pattern).c_str()),
				     0, compare_patterns(parsed,
							 test.expected_pattern));
      }
  }

  void testParseThenSerialize()
  {
    for(int i = 0; i < num_test_patterns; ++i)
      {
	const pattern_test &test(test_patterns[i]);

	ref_ptr<pattern> parsed(parse(test.input_pattern));
	_error->DumpErrors();
	CPPUNIT_ASSERT(parsed.valid());

	std::string serialized(serialize_pattern(parsed));

	CPPUNIT_ASSERT_EQUAL(test.expected_serialization,
			     serialized);
      }
  }

  void testSerialize()
  {
    for(int i = 0; i < num_test_patterns; ++i)
      {
	const pattern_test &test(test_patterns[i]);

	std::string serialized(serialize_pattern(test.expected_pattern));

	CPPUNIT_ASSERT_EQUAL(test.expected_serialization,
			     serialized);
      }
  }

  void testSerializationParse()
  {
    for(int i = 0; i < num_test_patterns; ++i)
      {
	const pattern_test &test(test_patterns[i]);

	ref_ptr<pattern> parsed(parse(test.expected_serialization));
	_error->DumpErrors();
	CPPUNIT_ASSERT(parsed.valid());

	CPPUNIT_ASSERT_EQUAL_MESSAGE(ssprintf("Comparing %s and %s",
					      serialize_pattern(parsed).c_str(),
					      serialize_pattern(test.expected_pattern).c_str()),
				     0,
				     compare_patterns(parsed,
						      test.expected_pattern));
      }
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(MatchingTest);
