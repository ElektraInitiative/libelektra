/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_KDB_ELEKTRAGEN_COMMON_HPP
#define ELEKTRA_KDB_ELEKTRAGEN_COMMON_HPP

#include <kdb.hpp>
#include <regex>

void escapeNonAlphaNum (std::string & str);

std::vector<std::string> getKeyParts (const kdb::Key & key);

bool hasType (const kdb::Key & key);

std::string getType (const kdb::Key & key);

std::string getTagName (const kdb::Key & key, const std::string & parentKey);

std::string snakeCaseToCamelCase (const std::string & s, bool upper = false);

std::string snakeCaseToPascalCase (const std::string & s);

std::string snakeCaseToMacroCase (const std::string & s);

std::string camelCaseToMacroCase (const std::string & s);

#endif // ELEKTRA_KDB_ELEKTRAGEN_COMMON_HPP
