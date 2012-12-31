/*
 * Copyright (C) 2009-2012 Dr. Christoph L. Spiel
 *
 * This file is part of Enblend.
 *
 * Enblend is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Enblend is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Enblend; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */


// Life is tough and then you die.  -- Jack Dempsey


#include <list>
#include <string>

#if _WIN32
#ifndef HAVE_BOOST_FILESYSTEM
#include <ctype.h>  // isalpha
#endif
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "filenameparse.h"

#if defined(_MSC_VER) || defined(HAVE_WINDOWS_H)
#define PATH_SEPARATOR "\\"
#else
#define PATH_SEPARATOR "/"
#endif

#define DOT "."
#define DOTDOT ".."


#ifdef HAVE_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>

#if BOOST_FILESYSTEM_VERSION < 3
typedef boost::filesystem::basic_path<std::string, boost::filesystem::path_traits> basic_path;
#define GETPATHSTRING(x) x
#else
typedef boost::filesystem::path basic_path;
#define GETPATHSTRING(x) (x).string()
#endif
#endif


namespace enblend {

bool
isRelativePath(const std::string& aFilename)
{
#ifdef HAVE_BOOST_FILESYSTEM
    const basic_path path(aFilename);
    return !path.has_root_directory();
#else
    const std::string::size_type separator = aFilename.find(PATH_SEPARATOR);
#if defined(_MSC_VER) || defined(HAVE_WINDOWS_H)
    return !(aFilename.size() >= 3 &&
             isalpha(aFilename[0]) &&
             aFilename[1] == ':' &&
             separator == 2);
#else
    return separator != 0;
#endif
#endif
}


std::string
extractDirname(const std::string& aFilename)
{
#ifdef HAVE_BOOST_FILESYSTEM
    const basic_path path(aFilename);
    const std::string directory(path.branch_path().string());
    return directory.empty() ? DOT : directory;
#else
    const std::string::size_type separator = aFilename.rfind(PATH_SEPARATOR);
    return (separator == std::string::npos) ? DOT : aFilename.substr(0, separator);
#endif
}


std::string
extractBasename(const std::string& aFilename)
{
#ifdef HAVE_BOOST_FILESYSTEM
    const basic_path path(aFilename);
    return GETPATHSTRING(path.leaf());
#else
    const std::string::size_type separator = aFilename.rfind(PATH_SEPARATOR);
    return
        (separator == std::string::npos) ?
        aFilename :
        aFilename.substr(separator + 1, aFilename.length() - separator - 1);
#endif
}


std::string
extractFilename(const std::string& aFilename)
{
#ifdef HAVE_BOOST_FILESYSTEM
    const basic_path path(aFilename);
    return basename(path);
#else
    const std::string::size_type separator = aFilename.rfind(PATH_SEPARATOR);
    const std::string::size_type dot = aFilename.rfind(DOT);
    if (separator == std::string::npos)
    {
        return (dot == std::string::npos) ? aFilename : aFilename.substr(0, dot);
    }
    else
    {
        return
            (dot == std::string::npos) ?
            aFilename.substr(separator + 1, aFilename.length() - separator - 1) :
            aFilename.substr(separator + 1, dot - separator - 1);
    }
#endif
}


std::string
extractExtension(const std::string& aFilename)
{
#ifdef HAVE_BOOST_FILESYSTEM
    const basic_path path(aFilename);
    return extension(path);
#else
    const std::string::size_type dot = aFilename.rfind(DOT);
    return
        (dot == std::string::npos) ?
        "" :
        aFilename.substr(dot, aFilename.length() - dot);
#endif
}


typedef std::list<std::string> list_t;


#ifdef HAVE_BOOST_FILESYSTEM

inline basic_path
removeDotsBoost(const basic_path& aPath)
{
    basic_path result;
    for (basic_path::const_iterator p = aPath.begin(); p != aPath.end(); ++p)
    {
        if (*p != DOT)
        {
            result /= *p;
        }
    }
    return result;
}


inline basic_path
removeDotDotsBoost(const basic_path& aPath)
{
    list_t directories;
    for (basic_path::const_iterator p = aPath.begin(); p != aPath.end(); ++p)
    {
        if (*p == DOTDOT &&
            !directories.empty() && directories.back() != DOTDOT)
        {
            directories.pop_back();
        }
        else
        {
            directories.push_back(GETPATHSTRING(*p));
        }
    }
    basic_path result;
    for (list_t::const_iterator p = directories.begin(); p != directories.end(); ++p)
    {
        result /= *p;
    }
    return result;
}

#else

inline
std::string
removeDotsCxx(const std::string& aPathname)
{
    std::string path(aPathname);
    std::string::size_type predecessor = std::string::npos;
    std::string::size_type separator = path.find(PATH_SEPARATOR);
    while (separator != std::string::npos)
    {
        const std::string::size_type begin =
            predecessor == std::string::npos ? 0 : predecessor + 1;
        const std::string component =
            path.substr(begin, separator - predecessor - 1);
        if (component == DOT)
        {
            path.erase(begin, 2);
        }
        else
        {
            predecessor = separator;
        }
        separator = path.find(PATH_SEPARATOR, predecessor + 1);
    }
    if (predecessor == std::string::npos)
    {
        if (path == DOT)
        {
            path.clear();
        }
    }
    else
    {
        const std::string component = path.substr(predecessor + 1);
        if (component == DOT)
        {
            path.erase(predecessor);
        }
    }
    return path;
}


inline std::string
removeDotDotsCxx(const std::string& aPathname)
{
    std::string path(aPathname);
    list_t directories;
    std::string::size_type predecessor = std::string::npos;
    std::string::size_type separator = path.find(PATH_SEPARATOR);
    while (separator != std::string::npos)
    {
        const std::string::size_type begin =
            predecessor == std::string::npos ? 0 : predecessor + 1;
        const std::string component =
            path.substr(begin, separator - predecessor - 1);
        if (component == DOTDOT &&
            !directories.empty() && directories.back() != DOTDOT)
        {
            directories.pop_back();
        }
        else
        {
            directories.push_back(component);
        }

        predecessor = separator;
        separator = path.find(PATH_SEPARATOR, predecessor + 1);
    }
    if (predecessor == std::string::npos)
    {
        directories.push_back(path);
    }
    else
    {
        const std::string component = path.substr(predecessor + 1);
        if (component == DOTDOT &&
            !directories.empty() && directories.back() != DOTDOT)
        {
            directories.pop_back();
        }
        else
        {
            directories.push_back(component);
        }
    }
    std::string result;
    for (list_t::const_iterator p = directories.begin(); p != directories.end(); ++p)
    {
        if (p != directories.begin())
        {
            result.append(PATH_SEPARATOR);
        }
        result.append(*p);
    }
    return result;
}
#endif


std::string
canonicalizePath(const std::string& aPathname, bool keepDot)
{
#ifdef HAVE_BOOST_FILESYSTEM
    const basic_path result =
        removeDotDotsBoost(removeDotsBoost(basic_path(aPathname)));
    if (keepDot && result.empty())
    {
        return std::string(DOT);
    }
    else
    {
        return result.string();
    }
#else
    std::string result = removeDotDotsCxx(removeDotsCxx(aPathname));

    // For compatability with the Boost implementation: Remove a
    // trailing PATH_SEPARATOR unless we reference the root directory.
    const size_t size = result.size();
    if (size >= 2 && result.substr(size - 1, 1) == PATH_SEPARATOR)
    {
        result.erase(size - 1, 1);
    }
    if (keepDot && result.empty())
    {
        return std::string(DOT);
    }
    else
    {
        return result;
    }
#endif
}


std::string
concatPath(const std::string& aPathname, const std::string& anotherPathname)
{
#ifdef HAVE_BOOST_FILESYSTEM
    basic_path path(aPathname);
    basic_path leaf(anotherPathname);
    path /= leaf;
    return path.string();
#else
    if (aPathname.empty())
    {
        return anotherPathname;
    }
    else if (anotherPathname.empty())
    {
        return aPathname;
    }
    else
    {
        const std::string::size_type end = aPathname.find_last_not_of(PATH_SEPARATOR);
        const std::string path =
            (end == std::string::npos) ?
            aPathname :
            aPathname.substr(0, end + 1);
        const std::string::size_type begin = anotherPathname.find_first_not_of(PATH_SEPARATOR);
        const std::string leaf =
            (begin == std::string::npos) ?
            anotherPathname :
            anotherPathname.substr(begin);
        return path + PATH_SEPARATOR + leaf;
    }
#endif
}

} // namespace enblend

// Local Variables:
// mode: c++
// End:
