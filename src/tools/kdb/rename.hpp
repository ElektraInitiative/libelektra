/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDBRENAME_HPP
#define ELEKTRA_KDBRENAME_HPP

#include <key.hpp>

#include <iostream>
#include <string>

/** @return a renamed key
 */
inline kdb::Key rename_key (kdb::Key & k, std::string const & sourceName, std::string const & newDirName, bool verbose)
{
	std::string otherName = k.getName ();
	std::string baseName = otherName.substr (sourceName.length ());
	if (verbose) std::cout << "key: " << otherName << " will be renamed to: " << newDirName + baseName << std::endl;

	kdb::Key newKey = k.dup ();
	newKey.setName (newDirName + baseName);
	return newKey;
}

#endif
