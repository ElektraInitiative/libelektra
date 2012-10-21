#ifndef PRINT_HPP
#define PRINT_HPP

#include <iosfwd>

namespace kdb
{
	class Key;
	class KeySet;
}

void printError(kdb::Key error);
void printWarnings(kdb::Key error);

std::ostream & operator << (std::ostream & os, const kdb::Key &k);
std::ostream & operator << (std::ostream & os, const kdb::KeySet &k);

#endif
