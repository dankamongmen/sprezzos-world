// load_grouppolicy.h
//
//  Copyright 2001 Daniel Burrows
//

#ifndef LOAD_GROUPPOLICY_H
#define LOAD_GROUPPOLICY_H

#include <string>

/** \brief Routines to parse grouping policies.
 *  
 *  
 *  Grouping-policy configurations are written as follows:
 *  
 *  POLICY1,POLICY2(arg1,arg2,...,..,argN),POLICY3,...,POLICYN
 *  
 *  Each POLICY name specifies a way to sort at a particular level.  Obviously,
 *  the policy names may not contain commas.  Similarly, arg1..argN are arguments
 *  to the policies.  Different policies, just to be confusing, may implement
 *  different handling of their arguments.
 *  
 *  If this ever gets totally out of control, it may be worthwhile to write a
 *  routine allowing other modules to register parsers for an individual policy.
 *  Right now I don't think this is necessary.
 * 
 *  \file load_grouppolicy.h
 */

class pkg_grouppolicy_factory;

/** \brief Parses a chain of grouping policies.
 *
 *  \param s a string which specifies a grouping policy configuration.
 */
pkg_grouppolicy_factory *parse_grouppolicy(const std::string &s);

#endif
