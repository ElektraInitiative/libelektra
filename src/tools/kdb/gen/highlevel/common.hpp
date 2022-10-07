/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_KDB_GEN_HIGHLEVEL_COMMON_HPP
#define ELEKTRA_KDB_GEN_HIGHLEVEL_COMMON_HPP

#include <kdb.hpp>

#if __GNUC__ >= 12
#pragma GCC diagnostic error "-Wmaybe-uninitialized"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#include <regex>
#pragma GCC diagnostic pop
#pragma GCC diagnostic pop
#else
#include <regex>
#endif


std::string upCaseFirst (const std::string & str);

void escapeNonAlphaNum (std::string & str);

std::vector<std::string> getKeyParts (const kdb::Key & key);

bool hasType (const kdb::Key & key);

std::string getType (const kdb::Key & key);

std::string getTagName (std::string name);
std::string getTagName (const kdb::Key & key, const std::string & parentKey);

std::string snakeCaseToCamelCase (const std::string & s, bool upper = false);

std::string snakeCaseToPascalCase (const std::string & s);

std::string snakeCaseToMacroCase (const std::string & s);

std::string camelCaseToMacroCase (const std::string & s);

#endif // ELEKTRA_KDB_GEN_HIGHLEVEL_COMMON_HPP
