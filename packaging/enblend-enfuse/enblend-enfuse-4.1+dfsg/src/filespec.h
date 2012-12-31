/*
 * Copyright (C) 2009-2012 Christoph L. Spiel
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
#ifndef __FILESPEC_H__
#define __FILESPEC_H__

#include <list>
#include <string>

#include "selector.h"


#define RESPONSE_FILE_PREFIX_CHAR '@'
#define RESPONSE_FILE_COMMENT_CHAR '#'
#define WHITESPACE_CHARS "\n\r\t "


namespace enblend
{
    typedef std::list<std::string> FileNameList;

    typedef std::pair<std::string, unsigned> FilePosition; /** Filename, line number pairs */
    typedef std::list<FilePosition> FilePositionTrace;     /** Traceback to a file position */

    /** Print the (back-)trace of all response files opened so far. */
    void unroll_trace(const FilePositionTrace& a_trace);


    class TraceableFileName
    {
    public:
        TraceableFileName(const std::string& a_filename,
                          const FilePositionTrace& a_trace,
                          selector::Abstract* a_selector) :
            filename_(a_filename), trace_(a_trace), selector_(a_selector) {}

        virtual ~TraceableFileName() {}

        const std::string& filename() const {return filename_;}
        const FilePositionTrace& trace() const {return trace_;}
        void unroll_trace() const {enblend::unroll_trace(trace_);}
        selector::Abstract* selector() const {return selector_;}

    private:
        const std::string filename_;
        const FilePositionTrace trace_;
        selector::Abstract* const selector_;
    };


    typedef std::list<TraceableFileName> TraceableFileNameList; /** */


    /** Recursively unfold filename which may be a literal name or a
     *  response file.  The result is a list of only literal
     *  filenames. */
    void unfold_filename(TraceableFileNameList& result, const std::string& filename);

    /** Answer whether we suspect filename is a response file. */
    bool maybe_response_file(const std::string& filename);


    /** List of algorithm name / algorithm description pairs */
    typedef std::list<std::pair<std::string, std::string> > algorithm_list;

    /** Answer a list of all globbing algorithms including aliases
     *  that are known, i.e., have been compiled in. */
    algorithm_list known_globbing_algorithms();
}

#endif /* __FILESPEC_H__ */

// Local Variables:
// mode: c++
// End:
