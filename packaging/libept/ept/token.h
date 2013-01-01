// -*- C++ -*-
#include <wibble/mixin.h>
#include <string>

#ifndef EPT_TOKEN_H
#define EPT_TOKEN_H

namespace ept {

struct Token : wibble::mixin::Comparable< Token > {
    std::string _id; // formatted as package[_version]
    std::string id() const { return _id; }

    Token() : _id( "" ) {}
    Token( std::string s ) : _id( s ) {}

    std::string version() const {
        return _id.find( '_' ) == std::string::npos ? "" :
            std::string( _id, _id.find( '_' ) + 1, _id.size() );
    }

    std::string package() const {
        return std::string( _id, 0,
                            _id.find( '_' ) == std::string::npos ?
                            _id.size() : _id.find( '_' ) );
    }

    bool isDesktop() const {
        return std::string( _id, 0, 8 ) == "desktop:";
    }

    std::string desktop() const {
        return isDesktop() ? std::string( _id, 8, _id.size() ) : "";
    }

    bool hasVersion() const {
        return version() != "";
    }

    bool valid() const {
        return _id != "";
    }

    bool operator<=( const Token &o ) const {
        return _id <= o._id;
    }
};

}

inline std::ostream &operator<<( std::ostream &o, const ept::Token &t ) {
    return o << t.id();
}

#endif
