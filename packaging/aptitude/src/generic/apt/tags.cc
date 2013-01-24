// tags.cc
//
//   Copyright (C) 2005, 2007-2008, 2010 Daniel Burrows
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

#include "tags.h"

#ifndef HAVE_EPT

#include <aptitude.h>

#include "apt.h"
#include "config_signal.h"

#include <algorithm>
#include <map>
#include <utility>

#include <cctype>
#include <string.h>

#include <sigc++/functors/mem_fun.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/tagfile.h>

#include <cwidget/generic/util/eassert.h>

using namespace std;
using aptitude::apt::tag;

class tag_list
{
  // The string to parse.
  std::string s;
public:
  class const_iterator
  {
    std::string::const_iterator start, finish, limit;
  public:
    const_iterator(const std::string::const_iterator &_start,
		   const std::string::const_iterator &_finish,
		   const std::string::const_iterator &_limit)
      :start(_start), finish(_finish), limit(_limit)
    {
    }

    const_iterator operator=(const const_iterator &other)
    {
      start = other.start;
      finish = other.finish;
      limit = other.limit;

      return *this;
    }

    bool operator==(const const_iterator &other)
    {
      return other.start == start && other.finish == finish && other.limit == limit;
    }

    bool operator!=(const const_iterator &other)
    {
      return other.start != start || other.finish != finish || other.limit != limit;
    }

    const_iterator &operator++();

    tag operator*()
    {
      return tag(start, finish);
    }
  };

  tag_list(const char *start, const char *finish)
    :s(start, finish)
  {
  }

  tag_list &operator=(const tag_list &other)
  {
    s=other.s;

    return *this;
  }

  const_iterator begin() const;
  const_iterator end() const
  {
    return const_iterator(s.end(), s.end(), s.end());
  }
};

tag_list::const_iterator &tag_list::const_iterator::operator++()
{
  start = finish;

  while(start != limit && (*start) != ',')
    ++start;

  if(start != limit) // Push past the comma.
    ++start;

  while(start != limit && isspace(*start))
    ++start;

  if(start == limit)
    finish = limit;
  else
    {
      // Eat everything up to the next comma.
      finish = start+1;
      while(finish != limit && (*finish) != ',')
	++finish;
    }

  return *this;
}

tag_list::const_iterator tag_list::begin() const
{
  std::string::const_iterator endfirst=s.begin();
  while(endfirst != s.end() && (*endfirst) != ',')
    ++endfirst;

  const_iterator rval(s.begin(), endfirst, s.end());
  return rval;
}

typedef set<tag> db_entry;

// The database is built eagerly, since the common use case is
// to scan everything in sight right away and this makes it easy
// to provide a progress bar to the user.
db_entry *tagDB;

static void insert_tags(std::set<tag> &tags,
                        const char *start,
                        const char *finish)
{
  const tag_list lst(start, finish);

  for(tag_list::const_iterator t=lst.begin(); t!=lst.end(); ++t)
    tags.insert(*t);
}

static void insert_tags(const pkgCache::VerIterator &ver,
			const pkgCache::VerFileIterator &vf)
{
  set<tag> *tags = tagDB + ver.ParentPkg().Group()->ID;

  const char *recstart=0, *recend=0;
  const char *tagstart, *tagend;
  pkgTagSection sec;

  eassert(apt_package_records);
  eassert(tagDB);

  apt_package_records->Lookup(vf).GetRec(recstart, recend);
  if(!recstart || !recend)
    return;
  if(!sec.Scan(recstart, recend-recstart+1))
    return;

  if(!sec.Find("Tag", tagstart, tagend))
    return;

  insert_tags((*tags), tagstart, tagend);
}

static void reset_tags()
{
  delete[] tagDB;
  tagDB = NULL;
}

const std::set<tag> aptitude::apt::get_tags(const pkgCache::PkgIterator &pkg)
{
  if(!apt_cache_file || !tagDB)
    return std::set<tag>();

  return tagDB[pkg.Group()->ID];
}

static bool read_debtags_package_tags(OpProgress *progress,
                                      const std::string &filename)
{
  _error->PushToStack();
  FileFd F(filename, FileFd::ReadOnly);
  _error->RevertToStack();

  if(!F.IsOpen())
    {
      // _error->Warning(_("Unable to load debtags package tags, perhaps"
      //                   " debtags is not installed?"));
      // Fail silently; debtags need not be installed.
      return false;
    }

  const unsigned long m = (*apt_cache_file)->Head().GroupCount;
  unsigned long n = 0;

  if(progress != NULL)
    progress->OverallProgress(0, m, 1,
                             _("Building tag database"));

  const unsigned long long buf_size = 4096;
  char buf[buf_size];
  while(F.ReadLine(buf, buf_size) != NULL)
    {
      if(progress != NULL)
        progress->OverallProgress(++n, m, 1,
                                  _("Building tag database"));

      const char *sep = strstr(buf, ": ");
      if(sep == NULL)
        continue;

      const string pkg_name(buf, sep - buf);
      const pkgCache::GrpIterator grp((*apt_cache_file)->FindGrp(pkg_name));
      if(grp.end() == true)
        continue;

      set<tag> *tags = tagDB + grp->ID;

      const char *tagstart = sep + 2;
      const char *tagend = tagstart;
      while((*tagend) != '\0')
        ++tagend;

      insert_tags((*tags), tagstart, tagend);
    }

  if(progress != NULL)
    progress->Done();

  return true;
}

bool initialized_reset_signal;
void aptitude::apt::load_tags(OpProgress *progress)
{
  eassert(apt_cache_file && apt_package_records);

  if(!initialized_reset_signal)
    {
      cache_closed.connect(sigc::ptr_fun(reset_tags));
      cache_reload_failed.connect(sigc::ptr_fun(reset_tags));
      initialized_reset_signal = true;
    }

  tagDB = new db_entry[(*apt_cache_file)->Head().GroupCount];

  // Try to load from the debtags database
  {
    const string filename(aptcfg->FindFile("Debtags::Package-Tags",
                                           "/var/lib/debtags/package-tags"));
    if(read_debtags_package_tags(progress, filename) == true)
      return;
  }

  // Otherwise fall back to reading the Packages files directly
  std::vector<loc_pair> verfiles;

  for(pkgCache::PkgIterator p = (*apt_cache_file)->PkgBegin();
      !p.end(); ++p)
    for(pkgCache::VerIterator v = p.VersionList(); !v.end(); ++v)
      for(pkgCache::VerFileIterator vf = v.FileList();
	  !vf.end(); ++vf)
	verfiles.push_back(loc_pair(v, vf));

  sort(verfiles.begin(), verfiles.end(), location_compare());

  if(progress != NULL)
    progress->OverallProgress(0, verfiles.size(), 1,
                             _("Building tag database"));
  size_t n=0;
  for(std::vector<loc_pair>::iterator i=verfiles.begin();
      i!=verfiles.end(); ++i)
    {
      insert_tags(i->first, i->second);
      ++n;
      if(progress != NULL)
        progress->OverallProgress(n, verfiles.size(), 1,
                                  _("Building tag database"));
    }

  if(progress != NULL)
    progress->Done();
}

// TAG VOCABULARY FILE
typedef map<string, string> facet_description_map;
typedef map<string, string> tag_description_map;

facet_description_map *facet_descriptions;
tag_description_map *tag_descriptions;

static void init_vocabulary()
{
  if(facet_descriptions != NULL)
    {
      eassert(tag_descriptions != NULL);
      return;
    }

  facet_descriptions = new facet_description_map;
  tag_descriptions = new tag_description_map;

  _error->PushToStack();
  FileFd F(aptcfg->FindFile("DebTags::Vocabulary", "/var/lib/debtags/vocabulary"),
	   FileFd::ReadOnly);
  _error->RevertToStack();

  if(!F.IsOpen())
    {
      //_error->Warning(_("Unable to load debtags vocabulary, perhaps debtags is not installed?"));
      // Fail silently; debtags need not be installed.
      return;
    }

  pkgTagFile tagfile(&F);

  pkgTagSection sec;

  while(tagfile.Step(sec))
    {
      string facet;
      const char *start, *end;

      if(sec.Find("Facet", start, end))
	{
	  facet.assign(start, end-start);

	  if(sec.Find("Description", start, end))
	    (*facet_descriptions)[facet] = string(start, end-start);
	}
      else if(sec.Find("Tag", start, end))
	{
	  string tag(start, end-start);

	  if(sec.Find("Description", start, end))
	    (*tag_descriptions)[tag] = string(start, end-start);
	}
    }
}

std::string aptitude::apt::get_facet_long_description(const tag &t)
{
  init_vocabulary();

  const string facet(get_facet_name(t));

  facet_description_map::const_iterator found =
    facet_descriptions->find(facet);

  if(found == facet_descriptions->end())
    return string();
  else
    return found->second;
}

// FIXME: This code to split long/short descriptions is repeated in
// too many places.
std::string aptitude::apt::get_facet_short_description(const tag &t)
{
  const string desc(get_facet_long_description(t));
  return string(desc, 0, desc.find('\n'));
}

std::string aptitude::apt::get_tag_long_description(const tag &t)
{
  init_vocabulary();

  tag_description_map::const_iterator found =
    tag_descriptions->find(t);

  if(found == tag_descriptions->end())
    return string();
  else
    return found->second;
}

std::string aptitude::apt::get_tag_short_description(const tag &t)
{
  const string desc = get_tag_long_description(t);
  return string(desc, 0, desc.find('\n'));
}

#else // HAVE_EPT

#if defined(HAVE_EPT_DEBTAGS_VOCABULARY_FACET_DATA) || defined(HAVE_EPT_DEBTAGS_VOCABULARY_TAG_DATA)
#define USE_VOCABULARY 1
#endif

#include <ept/debtags/debtags.h>
#ifdef USE_VOCABULARY
#include <ept/debtags/vocabulary.h>
#endif
#include "apt.h"

#include <aptitude.h>

#include <boost/format.hpp>

namespace aptitude
{
  namespace apt
  {
    const ept::debtags::Debtags *debtagsDB;

#ifdef USE_VOCABULARY
    const ept::debtags::Vocabulary *debtagsVocabulary;
#endif

    void reset_tags()
    {
      delete debtagsDB;
      debtagsDB = NULL;

#ifdef USE_VOCABULARY
      delete debtagsVocabulary;
      debtagsVocabulary = NULL;
#endif
    }

    bool initialized_reset_signal;
    void load_tags(OpProgress *progress)
    {
      if(!initialized_reset_signal)
	{
	  cache_closed.connect(sigc::ptr_fun(reset_tags));
	  cache_reload_failed.connect(sigc::ptr_fun(reset_tags));
	  initialized_reset_signal = true;
	}

      try
	{
	  debtagsDB = new ept::debtags::Debtags;
#ifdef USE_VOCABULARY
          debtagsVocabulary = new ept::debtags::Vocabulary;
#endif
	}
      catch(std::exception &ex)
	{
	  // If debtags failed to initialize, just leave it
	  // uninitialized.
	  debtagsDB = NULL;
#ifdef USE_VOCABULARY
          debtagsVocabulary = NULL;
#endif
	}
    }

    const std::set<tag> get_tags(const pkgCache::PkgIterator &pkg)
    {
      if(!apt_cache_file || !debtagsDB)
	return std::set<tag>();

      // TODO: handle !hasData() here.
      try
	{
	  return debtagsDB->getTagsOfItem(pkg.Name());
	}
      catch(std::exception &ex)
	{
	  return std::set<tag>();
	}
    }

#ifdef HAVE_EPT_DEBTAGS_VOCABULARY_FACET_DATA
    std::string get_facet_long_description(const tag &t)
    {
      if(debtagsVocabulary == NULL)
        return _("No tag descriptions are available.");

      const ept::debtags::voc::FacetData * const fd =
        debtagsVocabulary->facetData(get_facet_name(t));

      if(fd == NULL)
        return (boost::format(_("No description available for %s."))
                % get_facet_name(t)).str();
      else
        return fd->longDescription();
    }

    std::string get_facet_short_description(const tag &t)
    {
      if(debtagsVocabulary == NULL)
        return _("No tag descriptions are available.");

      const ept::debtags::voc::FacetData * const fd =
        debtagsVocabulary->facetData(get_facet_name(t));

      if(fd == NULL)
        return (boost::format(_("No description available for %s."))
                % get_facet_name(t)).str();
      else
        return fd->shortDescription();
    }
#else
#error "Don't know how to retrieve facet descriptions."
#endif

#ifdef HAVE_EPT_DEBTAGS_VOCABULARY_TAG_DATA
    std::string get_tag_long_description(const tag &t)
    {
      if(debtagsVocabulary == NULL)
        return _("No tag descriptions are available.");

      const ept::debtags::voc::TagData * const td =
        debtagsVocabulary->tagData(t);

      if(td == NULL)
        return (boost::format(_("No description available for %s."))
                % get_fullname(t)).str();
      else
        return td->longDescription();
    }

    std::string get_tag_short_description(const tag &t)
    {
      if(debtagsVocabulary == NULL)
        return _("No tag descriptions are available.");

      const ept::debtags::voc::TagData * const td =
        debtagsVocabulary->tagData(get_fullname(t));

      if(td == NULL)
        return (boost::format(_("No description available for %s."))
                % get_fullname(t)).str();
      else
        return td->shortDescription();
    }
#else
#error "Don't know how to retrieve tag descriptions."
#endif
  }
}

#endif // HAVE_EPT

namespace aptitude
{
  namespace apt
  {
    std::string get_facet_name(const tag &t)
    {
      const std::string name = get_fullname(t);
      std::size_t split_pos = name.find("::");
      if(split_pos == std::string::npos)
        return _("legacy");
      else
        return std::string(name, 0, split_pos);
    }

    std::string get_tag_name(const tag &t)
    {
      const std::string name = get_fullname(t);
      std::size_t split_pos = name.find("::");
      if(split_pos == std::string::npos)
        return name;
      else
        return std::string(name, split_pos + 2);
    }
  }
}
