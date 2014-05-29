/**
 * \file
 *
 * \brief Allows to print errors and warnings, key and keysets
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_PRINT_HPP
#define TOOLS_PRINT_HPP

#include <iosfwd>
#include <toolexception.hpp>

namespace kdb
{

class Key;
class KeySet;

void printError(kdb::Key error);
void printWarnings(kdb::Key error);

std::ostream & operator << (std::ostream & os, const kdb::Key &k);
std::ostream & operator << (std::ostream & os, const kdb::KeySet &k);

}

#endif
