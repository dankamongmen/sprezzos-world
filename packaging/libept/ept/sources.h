/** -*- C++ -*-
	@file ept/sources.h
	@author Peter Rockai <me@mornfall.net>
*/

#include <iterator>
#include <iostream>
#include <sstream>
#include <string>

#include <wibble/range.h>
#include <wibble/mixin.h>

#ifndef EPT_SOURCES_H
#define EPT_SOURCES_H

namespace ept {

struct Sources {
    struct Entry : wibble::mixin::Comparable< Entry > {
        typedef wibble::Range< std::string > StringRange;
        typedef wibble::Consumer< std::string > StringConsumer;
        typedef std::vector< std::string > StringVector;
        struct Word { std::string s; };
        enum Type { Binary, Source, Comment };
        Entry( bool e = false, Type t = Comment,
               std::string u = "", std::string d = "",
               StringRange c = wibble::range( *new StringVector ) )
            : m_enabled( e ), m_type( t ), m_url( u ), m_dist( d )
        {
            c.output( wibble::consumer( m_components ) );
        }

        Entry( const Entry &e )
            : m_enabled( e.m_enabled ), m_type( e.m_type ), m_url( e.m_url ),
              m_dist( e.m_dist ), m_comment( e.m_comment )
        {
            wibble::range( e.m_components ).output( wibble::consumer( m_components ) );
        }

        bool operator< ( const Entry &o ) const {
            if (type() < o.type())
                return true;
            if (enabled() < o.enabled())
                return true;
            if (url() < o.url())
                return true;
            if (distribution() < o.distribution())
                return true;
            if (components() < o.components())
                return true;
            if (comment() < o.comment())
                return true;
            return false;
        }

        bool operator== ( const Entry &e ) const {
            return not ( ( *this < e ) || ( e < *this ) );
        }

        std::string components() const {
            std::ostringstream s;
            std::copy( m_components.begin(), m_components.end(),
                       std::ostream_iterator< std::string >( s, " " ) );
            return s.str();
        }

        void setComponents( const std::string &s ) {
            std::istringstream i( s );
            m_components.clear();
            std::copy( std::istream_iterator< std::string >( i ),
                       std::istream_iterator< std::string >(),
                       wibble::consumer( m_components ) );
        }

        std::string typeString() const {
			switch (type())
			{
				case Binary: return "deb";
				case Source: return "deb-src";
				case Comment: return "comment";
			}
        }

        void setTypeString( const std::string &s ) {
            if (s == "deb") setType( Binary );
            if (s == "deb-src") setType( Source );
            if (s == "comment") setType( Comment );
        }

        std::string distribution() const { return m_dist; }
        void setDistribution( const std::string &s ) { m_dist = s; }

        std::string url() const { return m_url; }
        void setUrl( const std::string &s ) { m_url = s; }

        bool enabled() const { return m_enabled; }
        void setEnabled( bool e ) { m_enabled = e; }

        std::string comment() const { return m_comment; }
        void setComment( const std::string &s ) { m_comment = s; }

        Type type() const { return m_type; }
        void setType( Type t ) {
            m_type = t;
            if (t == Comment) setEnabled( false );
        }

        friend std::istream &operator >>( std::istream &i, Entry &s );

        protected:

        bool m_enabled;
        Type m_type;
        std::string m_url;
        std::string m_dist;
        StringVector m_components;
        std::string m_comment;
    };
    void add( const Entry &e ) {
        wibble::consumer( m_entries ).consume( e );
    }
    void clear() { m_entries.clear(); }
    void disable( const Entry & );
    void enable( const Entry & );
    wibble::Range< Entry > entries() const {
        return wibble::range( m_entries );
    }
    friend std::istream &operator >>( std::istream &i, Sources &s );
protected:
    std::vector< Entry > m_entries;
};

inline std::istream &operator >>( std::istream &i, Sources::Entry::Word &w )
{
    bool bracket = false, quote = false, started = false;
    char c;
    w.s = "";
    while (!i.eof()) {
        c = i.get();
        if (started && !quote && !bracket && isspace( c ))
            break;
        if (!isspace( c ))
            started = true;
        if (started)
            w.s += c;
        if (bracket && c == ']')
            bracket = false;
        if (quote && c == '"')
            quote = false;
        if (!quote && c == '[')
            bracket = true;
        if (!bracket && c == '"')
            quote = true;
    }
    return i;
}

inline std::istream &operator >>( std::istream &i, Sources::Entry &e ) {
    std::string line, tmp;
    std::getline( i, line );
    std::istringstream l( line );
    // std::cerr << "parsing line: " << line << std::endl;
    l >> tmp;
    e.setEnabled( true );
    if (tmp[0] == '#') {
        if (tmp.size() > 1)
            tmp = tmp.substr(1);
        else
            l >> tmp;
        e.setEnabled( false );
    }
    // std::cerr << "type: " << tmp << std::endl;
    if (tmp == "deb" || tmp == "deb-src") {
        e.setTypeString( tmp );
    } else {
        // std::cerr << "comment: '" << line << "'" << std::endl;
        e.setType( Sources::Entry::Comment );
        e.setEnabled( false );
        e.setComment( line );
        return i;
    }
    Sources::Entry::Word w;
    l >> w; e.m_url = w.s;
    l >> w; e.m_dist = w.s;
    e.m_components.clear();
    std::copy( std::istream_iterator< std::string >( l ),
               std::istream_iterator< std::string >(),
               wibble::consumer( e.m_components ) );
    return i;
}

inline std::ostream &operator <<( std::ostream &o, const Sources::Entry &e )
{
    if (e.type() == Sources::Entry::Comment)
        return o << e.comment();
    if (! e.enabled())
        o << "# ";
    o << e.typeString();
    o << " " << e.url() << " " << e.distribution() << " " << e.components();
    return o;
}

inline std::istream &operator >>( std::istream &i, Sources &s ) {
    std::copy( std::istream_iterator< Sources::Entry >( i ),
               std::istream_iterator< Sources::Entry >(),
               wibble::consumer( s.m_entries ) );
    return i;
}

inline std::ostream &operator <<( std::ostream &o, const Sources &s ) {
    std::copy( s.entries().begin(), s.entries().end(),
               std::ostream_iterator< Sources::Entry >( o, "\n" ) );
    return o;
}

}

#endif
// vim:set ts=4 sw=4:
