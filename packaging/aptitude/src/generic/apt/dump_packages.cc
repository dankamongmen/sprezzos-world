// dump_packages.cc     -*-c++-*-
//
//   Copyright (C) 2007, 2009 Daniel Burrows
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

#include "dump_packages.h"

#include "apt.h"

#include <apt-pkg/configuration.h>
#include <apt-pkg/error.h>
#include <apt-pkg/tagfile.h>

#include <cwidget/generic/util/eassert.h>
#include <cwidget/generic/util/exception.h>
#include <cwidget/generic/util/ssprintf.h>

#include <aptitude.h>
#include <generic/util/dirent_safe.h>
#include <generic/util/temp.h>

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <fstream>

namespace aptitude
{
  namespace apt
  {
    void dump_verfile(const pkgCache::VerFileIterator &vf,
		      std::ostream &out)
    {
      eassert(apt_cache_file != NULL);
      eassert(apt_package_records != NULL);

      pkgRecords::Parser &p = apt_package_records->Lookup(vf);
      const char *start, *stop;
      p.GetRec(start, stop);

      out.write(start, stop - start);
    }

    void dump_version(const pkgCache::VerIterator &ver,
		      std::ostream &out)
    {
      eassert(apt_cache_file != NULL);
      eassert(apt_package_records != NULL);

      bool first = true;
      for(pkgCache::VerFileIterator vf = ver.FileList();
	  !vf.end(); ++vf)
	{
	  if(first)
	    first = false;
	  else
	    out << std::endl;
	  dump_verfile(vf, out);
	}
    }

    void dump_versions(const std::vector<pkgCache::VerIterator> &packages,
		       std::ostream &out)
    {
      eassert(apt_cache_file != NULL);
      eassert(apt_package_records != NULL);

      for(std::vector<pkgCache::VerIterator>::const_iterator it =
	    packages.begin(); it != packages.end(); ++it)
	{
	  dump_version(*it, out);
	  out << std::endl;
	}
    }

    namespace
    {
      class dep_target
      {
	std::string name;
	/** \brief Either an empty string or the contents of the parens.
	 *
	 *  This is used instead of interpreting the version
	 *  information because we don't need it and this reduces
	 *  the burden on our parser.
	 */
	std::string version_information;

      public:
	dep_target(const std::string &_name,
		   const std::string &_version_information)
	  : name(_name),
	    version_information(_version_information)
	{
	}

	const std::string &get_name() const { return name; }
	const std::string &get_version_information() const { return version_information; }
      };

      /** \brief Internal notion of one element of a dependency. */
      class dep_element
      {
	/** \brief The elements of the OR. */
	std::vector<dep_target> targets;

      public:
	dep_element(const std::vector<dep_target> &_targets)
	  : targets(_targets)
	{
	}

	const std::vector<dep_target> &get_targets() const
	{
	  return targets;
	}
      };

      class ParseException : public cwidget::util::Exception
      {
	std::string msg;

      public:
	ParseException(const std::string &_msg)
	  : msg(_msg)
	{
	}

	std::string errmsg() const { return msg; }
      };

      // Note: this parser might be a bit more lenient than the dpkg
      // parser..
      void parse_deps(const char *&start, const char *stop,
		      std::vector<dep_element> &out)
      {
	while(start != stop)
	  {
	    if(isspace(*start))
	      {
		if(*start == '\n')
		  {
		    // Quit when we hit a newline that ends
		    // the dep group.
		    ++start;
		    if(start == stop || !isspace(*start))
		      return;
		  }
		else
		  ++start;
	      }
	    else
	      {
		std::vector<dep_target> targets;

		if(*start == ',' || *start == '(' || *start == '|')
		  throw ParseException("Unexpected metacharacter in package name.");

		while(start != stop && *start != ',')
		  {
		    std::string pkg_name;
		    while(start != stop && !isspace(*start) &&
			  *start != ',' && *start != '(' && *start != '|')
		      {
			pkg_name += *start;
			++start;
		      }

		    while(start != stop && isspace(*start))
		      {
			if(*start == '\n')
			  {
			    ++start;
			    if(start == stop || !isspace(*start))
			      {
				// End of the Depends element -- save
				// our place and stop.
				targets.push_back(dep_target(pkg_name, ""));
				out.push_back(dep_element(targets));
				return;
			      }
			  }
			++start;
		      }

		    std::string version_information;
		    if(start != stop && *start == '(')
		      {
			++start;
			while(start != stop && *start != ')')
			  {
			    version_information += *start;
			    ++start;
			  }

			if(start == stop)
			  throw ParseException("Unexpected EOF parsing version information.");
			else
			  ++start;
		      }

		    targets.push_back(dep_target(pkg_name, version_information));

		    // NB: we'll ignore consecutive pipe characters.
		    // That's ok, this isn't a strict parser.
		    while(start != stop && (*start == '|' || isspace(*start)))
		      {
			if(*start == '\n')
			  {
			    ++start;
			    if(start == stop || !isspace(*start))
			      {
				out.push_back(dep_element(targets));
				return;
			      }
			  }
			else
			  ++start;
		      }
		  }

		if(start != stop)
		  {
		    eassert(*start == ',');
		    out.push_back(dep_element(targets));
		    ++start;
		  }
	      }
	  }
      }

      // Functor matching dep targets whose target is not in the set
      // of visited packages (or provided by a version of a visited
      // package!).
      bool is_irrelevant_dep(const dep_target &target,
			     const std::set<pkgCache::PkgIterator> &visited_packages)
      {
	pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(target.get_name());
	if(pkg.end())
	  return true;
	else if(visited_packages.find(pkg) != visited_packages.end())
	  return false;
	else
	  {
	    // Also return false if anything providing the target
	    // package is in the live set.
	    for(pkgCache::PrvIterator prvIt = pkg.ProvidesList();
		!prvIt.end(); ++prvIt)
	      {
		if(visited_packages.find(prvIt.OwnerPkg()) != visited_packages.end())
		  return false;
	      }

	    return true;
	  }
      }

      // Drop targets of dependencies that are irrelevant;
      // drop dependencies with only irrelevant targets.
      void filter_deps(const std::vector<dep_element> &in_elements,
		       const std::set<pkgCache::PkgIterator> &visited_packages,
		       std::vector<dep_element> &out_elements)
      {
	for(std::vector<dep_element>::const_iterator inIt = in_elements.begin();
	    inIt != in_elements.end(); ++inIt)
	  {
	    std::vector<dep_target> targets;

	    for(std::vector<dep_target>::const_iterator targetIt = inIt->get_targets().begin();
		targetIt != inIt->get_targets().end(); ++targetIt)
	      {
		if(!is_irrelevant_dep(*targetIt, visited_packages))
		  targets.push_back(*targetIt);
	      }

	    if(!targets.empty())
	      out_elements.push_back(dep_element(targets));
	  }
      }

      void dump_dep_line(const std::vector<dep_element> &elements,
			 std::ostream &out)
      {
	bool first_element = true;
	for(std::vector<dep_element>::const_iterator eltIt = elements.begin();
	    eltIt != elements.end(); ++eltIt)
	  {
	    if(first_element)
	      first_element = false;
	    else
	      out << ", ";

	    bool first_target = true;
	    for(std::vector<dep_target>::const_iterator targetIt =
		  eltIt->get_targets().begin();
		targetIt != eltIt->get_targets().end(); ++targetIt)
	      {
		if(first_target)
		  first_target = false;
		else
		  out << " | ";

		out << targetIt->get_name();
		if(!targetIt->get_version_information().empty())
		  {
		    out << " (" << targetIt->get_version_information() << ")";
		  }
	      }
	  }

	out << std::endl;
      }

      void dump_truncated_section(const char *start,
				  const char *stop,
				  const std::set<pkgCache::PkgIterator> &visited_packages,
				  std::ostream &out)
      {
	eassert(apt_cache_file != NULL);
	eassert(apt_package_records != NULL);

	while(start != stop)
	  {
	    const char *line_end =
	      reinterpret_cast<const char *>(memchr(start, '\n', stop - start));

	    if(isspace(*start))
	      {
		if(line_end == NULL)
		  {
		    out.write(start, stop - start);
		    start = stop;
		  }
		else
		  // We expect that lines starting with blanks
		  // should be copied exactly (if not, they should
		  // be handled below).
		  {
		    out.write(start, line_end - start + 1);
		    start = line_end + 1;
		  }
	      }
	    else
	      {
		// Two cases of interest:
		//  (1) Not a dependency line.  In this case we
		//      just output the line and all its
		//      successors literally.
		//  (2) A dependency line.  In this case we parse
		//      the line and its successors (eek) and
		//      build our own internal notion of the result.
		const char * const end   = line_end == NULL  ? stop : line_end;
		const char * const next  = line_end == NULL  ? stop : line_end + 1;
		const char * const colon = reinterpret_cast<const char *>(memchr(start, ':', end - start));

		if(colon == NULL) // ??
		  {
		    out.write(start, end - start);
		    start = next;
		  }
		else
		  {
		    static const char *depends_fields[] = {
		      "Depends",
		      "Recommends",
		      "Suggests",
		      "Enhances",
		      "Pre-Depends",
		      "Conflicts",
		      "Replaces",
		      "Obsoletes",
		      "Breaks",
		      NULL
		    };

		    // Check whether any of these match the field
		    // we've encountered.
		    bool found = false;
		    for(const char **fld = depends_fields; !found && *fld != NULL; ++fld)
		      {
			if(static_cast<size_t>(colon - start)  !=  strlen(*fld))
			  continue;

			if(strncasecmp(start, *fld, colon - start) == 0)
			  found = true;
		      }

		    if(!found)
		      {
			// Write to *next* so we include the newline
			// if any.
			out.write(start, next - start);
			start = next;
		      }
		    else
		      {
			const char *newStart = colon + 1;
			std::vector<dep_element> deps;
			parse_deps(newStart, stop, deps);
			std::vector<dep_element> filtered_deps;
			filter_deps(deps, visited_packages, filtered_deps);

			if(!filtered_deps.empty())
			  {
			    // Write out everything up to **and including**
			    // the colon.
			    out.write(start, colon - start + 1);
			    out << " ";
			    dump_dep_line(filtered_deps, out);
			  }

			start = newStart;
		      }
		  }
	      }
	  }
      }

      // Use pkgTagFile to copy all the entries of fd that are tagged
      // with 'Package' and a package whose name appears in the given
      // set to the output stream.
      //
      // Note that this **assumes** that the file has to do with
      // describing the current cache state.  This is necessary in order
      // to look up providers of names (if it weren't true, we'd have to
      // load all the files to copy and interpret them by hand before
      // doing anything with them!).
      void copy_truncated(FileFd &fd,
			  std::ostream &out,
			  const std::set<pkgCache::PkgIterator> &visited_packages)
      {
	pkgTagFile tag_file(&fd);

	pkgTagSection section;
	bool first = true;
	while(tag_file.Step(section))
	  {
	    // Look for a Package tag.
	    std::string pkg_name = section.FindS("Package");

	    if(pkg_name.empty())
	      continue;

	    pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(pkg_name);
	    if(pkg.end())
	      continue;

	    if(visited_packages.find(pkg) == visited_packages.end())
	      continue;

	    // Write out a separator if we already write something.
	    if(first)
	      first = false;
	    else
	      out << std::endl;

	    // Whee, write out the section.
	    const char *start;
	    const char *stop;
	    section.GetSection(start, stop);
	    dump_truncated_section(start, stop, visited_packages, out);
	  }
      }
    }

    std::string dirname(const std::string &name)
    {
      int len = name.size();
      while(len > 0 && name[len - 1] != '/')
	--len;
      while(len > 0 && name[len - 1] == '/')
	--len;
      if(len == 0)
	return "/";
      else
	return std::string(name, 0, len);
    }

    int make_directory_and_parents(const std::string &dir)
    {
      // Trivial code to make a directory.  HERE BE RACES!
      int result = mkdir(dir.c_str(), 0777);
      if(result != 0 && errno == ENOENT)
	{
	  make_directory_and_parents(dirname(dir));
	  result = mkdir(dir.c_str(), 0777);
	}

      if(result != 0 && errno == EEXIST)
	{
	  // Check that the path is a directory.  We follow
	  // symlinks, which is probably what you want.
	  struct stat buf;
	  int stat_result = stat(dir.c_str(), &buf);
	  if(stat_result != 0)
	    return stat_result;

	  if(S_ISDIR(buf.st_mode))
	    return 0;
	  else
	    return result;
	}
      else
	return result;
    }

    void copy_truncated(const std::string &inFileName,
			const std::string &outFileName,
			const std::set<pkgCache::PkgIterator> &visited_packages)
    {
      int infd = open(inFileName.c_str(), O_RDONLY);
      if(infd == -1)
	return;
      FileFd infile(infd);
      if(!infile.IsOpen() || infile.Failed())
	return;

      struct stat buf;
      if(fstat(infile.Fd(), &buf) != 0)
	{
	  _error->Errno("stat", _("Unable to stat %s."), inFileName.c_str());
	  return;
	}

      if(S_ISDIR(buf.st_mode))
	return;

      if(make_directory_and_parents(dirname(outFileName)) != 0)
	return;

      std::ofstream outfile(outFileName.c_str());
      if(!outfile)
	return;

      copy_truncated(infile, outfile, visited_packages);
    }

    void get_directory_files(const std::string &dir,
			     std::vector<std::string> &out)
    {
      DIR *d = opendir(dir.c_str());
      if(d == NULL) // Assume no dir means no files to copy.
	return;

      struct dirent *tmp;
      dirent_safe dir_entry;
      for(int readdir_result = readdir_r(d, &dir_entry.d, &tmp);
	  readdir_result == 0 && tmp != NULL;
	  readdir_result = readdir_r(d, &dir_entry.d, &tmp))
	out.push_back(dir_entry.d.d_name);
    }

    void copy_dir_truncated(const std::string &dir,
			    const std::string &to,
			    std::set<pkgCache::PkgIterator> visited_packages)
    {
      std::vector<std::string> dir_files;
      get_directory_files(dir, dir_files);

      // Don't even create a destination directory with an empty
      // input.
      if(dir_files.empty())
	return;

      if(make_directory_and_parents(to) != 0)
	return;

      for(std::vector<std::string>::const_iterator it = dir_files.begin();
	  it != dir_files.end(); ++it)
	{
	  if(*it == "." || *it == "..")
	    continue;

	  const std::string inFileName = dir + "/" + *it;
	  const std::string outFileName = to + "/" + *it;
	  copy_truncated(inFileName, outFileName, visited_packages);
	}
    }

    void recursive_copy_dir(const std::string &from,
			    const std::string &to)
     {
       std::vector<std::string> dir_files;
       get_directory_files(from, dir_files);

       // Don't even create a destination directory with an emtpy
       // input.
       if(dir_files.empty())
	 return;

       if(make_directory_and_parents(to) != 0)
	 return;

       for(std::vector<std::string>::const_iterator it = dir_files.begin();
	   it != dir_files.end(); ++it)
	 {
	   if(*it == "." || *it == "..")
	     continue;

	   std::string infilename = from + "/" + *it;
	   int infd = open(infilename.c_str(), O_RDONLY);
	   if(infd == -1)
	     continue;
	   FileFd infile(infd);
	   if(!infile.IsOpen() || infile.Failed())
	     continue;

	   struct stat buf;
	   if(fstat(infile.Fd(), &buf) != 0)
	     {
	       _error->Errno("stat", _("Unable to stat %s."), infilename.c_str());
	       continue;
	     }

	   if(S_ISDIR(buf.st_mode))
	     {
	       recursive_copy_dir(infilename, to + "/" + *it);
	     }
	   else
	     {
	       int outfd = creat((to + "/" + *it).c_str(), 0666);
	       if(outfd == -1)
		 continue;
	       FileFd outfile(outfd);
	       if(outfile.IsOpen() && !outfile.Failed())
		 CopyFile(infile, outfile);
	    }
	}
    }

    void copy_file(const std::string &in, const std::string &out)
    {
      // Open the file ourselves to avoid bogus error spewage.
      int infd = open(in.c_str(), O_RDONLY);
      if(infd == -1)
	return;
      FileFd inFile(infd);

      if(make_directory_and_parents(dirname(out)) != 0)
	return;

      int outfd = open(out.c_str(), O_WRONLY | O_CREAT | O_TRUNC, 0666);
      if(outfd == -1)
	return;
      FileFd outFile(outfd);

      CopyFile(inFile, outFile);
    }

    void make_truncated_cache(const std::set<pkgCache::PkgIterator> &packages,
			      const std::string &out_directory)
    {
      if(make_directory_and_parents(out_directory) != 0)
	{
	  std::string tmp = cwidget::util::sstrerror(errno);
	  _error->Error(_("Unable to create truncated cache: %s."),
			tmp.c_str());
	  return;
	}
    }

    void dump_truncated_apt_extended_states(const std::set<pkgCache::PkgIterator> &visited_packages,
					    const std::string &outDir)
    {
      temp::dir tmp_dir("aptitude-dump-directory");
      std::string oldStateDir(_config->Find("Dir::State"));

      try
	{
	  _config->Set("Dir::State", tmp_dir.get_name());
	  OpProgress dummy_progress;
	  (*apt_cache_file)->writeStateFile(&dummy_progress, false);
	  _config->Set("Dir::State", oldStateDir);
	}
      catch(...)
	{
	  _config->Set("Dir::State", oldStateDir);
	  throw;
	}

      make_directory_and_parents(outDir);

      copy_truncated(tmp_dir.get_name() + "/extended_states",
		     outDir + "/extended_states",
		     visited_packages);

      unlink((tmp_dir.get_name() + "/extended_states").c_str());
    }

    void dump_truncated_aptitude_states(const std::set<pkgCache::PkgIterator> &visited_packages,
					const std::string &outDir)
    {
      temp::dir tmp_dir("aptitude-dump-directory");

      // Declaring aptitude_states_filename is a workaround for
      // strange behavior in g++ 4.3.
      std::string aptitude_states_filename("aptitude_states");
      temp::name tmp_states(aptitude_states_filename);
      OpProgress progress;

      if(!(*apt_cache_file)->save_selection_list(progress,
						 tmp_states.get_name().c_str()))
	return;

      const std::string aptitude_state_dir =
	_config->FindDir("Dir::Aptitude::state", STATEDIR);
      make_directory_and_parents(aptitude_state_dir);

      const std::string state_file = aptitude_state_dir + "pkgstates";
      const std::string out_file = outDir + "/" + state_file;
      copy_truncated(tmp_states.get_name(),
		     out_file,
		     visited_packages);
    }

    // Make a truncated state snapshot.  We need to copy:
    //
    //   $(Dir::Aptitude::state)/pkgstates -- but a newly written
    //                                        version.
    //   $(Dir::State::lists)/*
    //   $(Dir::State::status)/*
    //   $(Dir::Etc::sourcelist)
    //   $(Dir::Etc::sourceparts)/*
    //   $(Dir::Etc::vendorlist)
    //   $(Dir::Etc::vendorparts)/*
    //   $(Dir::Etc::main)
    //   $(Dir::Etc::parts)/*
    //   $(Dir::Etc::preferences)
    //   $(Dir::Etc::preferencesparts)/*
    //
    // Dir::State::* are truncated copies; the others are copied
    // literally.
    void make_truncated_state_copy(const std::string &outDir,
				   const std::set<pkgCache::PkgIterator> &visited_packages)
    {
      {
	dump_truncated_aptitude_states(visited_packages, outDir);
      }

      {
	const std::string state_dir = _config->FindDir("Dir::state");

	if(!state_dir.empty())
	  dump_truncated_apt_extended_states(visited_packages,
					     outDir + "/" + state_dir);
      }

      {
	const std::string lists = _config->FindDir("Dir::State::lists");
	if(!lists.empty())
	  copy_dir_truncated(lists, outDir + "/" + lists,
			     visited_packages);
      }

      {
	const std::string status = _config->FindFile("Dir::State::status");
	if(!status.empty())
	  copy_truncated(status, outDir + "/" + status,
			 visited_packages);
      }

      {
	const std::string lists = _config->FindFile("Dir::State::lists");
	if(!lists.empty())
	  copy_dir_truncated(lists, outDir + "/" + lists,
			     visited_packages);
      }

      {
	// Could do a recursive copy of Dir::Etc, which would pick
	// up files not explicitly called out here, but that's an
	// awfully broad brush and fails if any of these have been
	// renamed.

	const std::string sourceList = _config->FindFile("Dir::Etc::status");

	if(!sourceList.empty())
	  copy_file(sourceList, outDir + "/" + sourceList);
      }

      {
	const std::string sourceList = _config->FindFile("Dir::Etc::sourcelist");
	if(!sourceList.empty())
	  copy_file(sourceList, outDir + "/" + sourceList);
      }

      {
	const std::string sourceParts = _config->FindDir("Dir::Etc::sourceparts");
	if(!sourceParts.empty())
	  recursive_copy_dir(sourceParts, outDir + "/" + sourceParts);
      }

      {
	const std::string vendorList = _config->FindFile("Dir::Etc::vendorlist");
	if(!vendorList.empty())
	  copy_file(vendorList, outDir + "/" + vendorList);
      }

      {
	const std::string vendorParts = _config->FindDir("Dir::Etc::vendorparts");
	if(!vendorParts.empty())
	  recursive_copy_dir(vendorParts, outDir + "/" + vendorParts);
      }

      {
	const std::string main = _config->FindFile("Dir::Etc::main");
	if(!main.empty())
	  copy_file(main, outDir + "/" + main);
      }

      {
	const std::string parts = _config->FindDir("Dir::Etc::parts");
	if(!parts.empty())
	  recursive_copy_dir(parts, outDir + "/" + parts);
      }

      {
	const std::string preferences = _config->FindFile("Dir::Etc::preferences");
	if(!preferences.empty())
	  copy_truncated(preferences, outDir + "/" + preferences,
			 visited_packages);
      }

      {
	const std::string preferencesParts = _config->FindDir("Dir::Etc::preferencesparts");
	if(!preferencesParts.empty())
	  recursive_copy_dir(preferencesParts, outDir + "/" + preferencesParts);
      }
    }

    void dump_truncated_packages(const std::set<pkgCache::PkgIterator> &packages,
				 std::ostream &out)
    {
      try
	{
	  bool first = true;
	  for(std::set<pkgCache::PkgIterator>::const_iterator it = packages.begin();
	      it != packages.end(); ++it)
	    {
	      for(pkgCache::VerIterator vIt = it->VersionList(); !vIt.end(); ++vIt)
		{
		  for(pkgCache::VerFileIterator vfIt = vIt.FileList();
		      !vfIt.end(); ++vfIt)
		    {
		      if(first)
			first = false;
		      else
			out << std::endl;

		      pkgRecords::Parser &p = apt_package_records->Lookup(vfIt);
		      const char *start, *stop;
		      p.GetRec(start, stop);

		      dump_truncated_section(start, stop, packages, out);
		    }
		}
	    }
	}
      catch(const cwidget::util::Exception &e)
	{
	  out << std::endl << "Uncaught exception: "
	      << e.errmsg() << std::endl;
	}
    }
  }
}
