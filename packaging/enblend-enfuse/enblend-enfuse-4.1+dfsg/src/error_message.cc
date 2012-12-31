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


#include <cstring>
#include <sstream>

#include <boost/scoped_ptr.hpp>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error_message.h"

namespace enblend
{
namespace detail
{
    std::string
    noErrorMessageAvailable(int anErrorNumber)
    {
        std::ostringstream oss;
        oss << "No detailed error message available; decimal error #" << anErrorNumber;
        return oss.str();
    }
}


std::string
errorMessage(int anErrorNumber)
{
#if defined(HAVE_STRERROR) || defined(HAVE_STRERROR_R)
    char* message;
    int return_code;
#if defined(HAVE_STRERROR_R)
    const size_t buffer_size = 65536;
    boost::scoped_ptr<char> message_buffer(new char[buffer_size]);
#if defined(STRERROR_R_CHAR_P)
    message = strerror_r(anErrorNumber, message_buffer.get(), buffer_size);
    return_code = 0;
#else
    message = message_buffer.get();
    return_code = strerror_r(anErrorNumber, message, buffer_size);
#endif // STRERROR_R_CHAR_P
#elif defined(HAVE_STRERROR)
    message = strerror(anErrorNumber);
    return_code = 0;
#endif // HAVE_STRERROR_R, HAVE_STRERROR

    if (return_code != 0)
    {
        std::ostringstream oss;
        oss <<
            "Conversion of decimal error #" << anErrorNumber <<
            " to an error message failed with decimal return code " << return_code;
        return oss.str();
    }
    else if (strlen(message) == 0)
    {
        return detail::noErrorMessageAvailable(anErrorNumber);
    }
    else
    {
        return std::string(message);
    }
#else
    return detail::noErrorMessageAvailable(anErrorNumber);
#endif // HAVE_STRERROR || HAVE_STRERROR_R
}

}
