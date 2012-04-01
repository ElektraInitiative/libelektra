#ifndef PRINT_HPP
#define PRINT_HPP

#include <key.hpp>

#include <ostream>

void printError(kdb::Key error);
void printWarnings(kdb::Key error);

std::ostream & operator << (std::ostream & os, const kdb::Key &k);

#endif
