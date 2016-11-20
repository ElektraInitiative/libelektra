/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <key.hpp>

#include <iostream>
#include <string>

/** @return a renamed key
  */
inline kdb::Key rename_key (kdb::Key k, std::string sourceName, std::string newDirName, bool verbose)
{
	std::string otherName = k.getName ();
	std::string baseName = otherName.substr (sourceName.length ());
	if (verbose) std::cout << "key: " << otherName << " will be renamed to: " << newDirName + baseName << std::endl;

	kdb::Key newKey = k.dup ();
	newKey.setName (newDirName + baseName);
	return newKey;
}
