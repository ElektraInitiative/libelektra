#ifndef ELEKTRA_KCONFIGPARSER_HPP
#define ELEKTRA_KCONFIGPARSER_HPP

#include "file_utility.hpp"
#include <elektra/kdb/errors.h>
#include <kdbplugin.hpp>

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

namespace kconfig
{
class KConfigParser
{
private:
	/* This FileUtility is used to read from the file */
	FileUtility & fileUtility;
	/* This KeySet is used to store the parsed values */
	CppKeySet & keySet;

	/**
	 * @brief This method parses a group from the file into an Elektra key
	 * 	Format: (\[keyname\])+[\[$meta\]]
	 * @param parent
	 * @return
	 */
	kdb::Key loadGroupNameFromFile (CppKey const & parent);

	/**
	 * @brief This method parses a key from the file into an Elektra key.
	 * 	Format: keyname[\[locale\]](\[$meta\])*[=value]
	 * @param parent
	 * @return
	 */
	kdb::Key loadKeyFromFile (CppKey const & parent);

	/**
	 * @brief This method is used to add a key to the KeySet only if it contains KConfig metadata
	 * @param key This is the key that we want to add
	 */
	void appendIfContainsMeta (CppKey const & key);

	/**
	 * @brief This method is used to add a key to the KeySet if it isn't the same as the group
	 * @param key This is the key that we want to add
	 * @param group This is the key that we don't want to add
	 */
	void appendIfNotGroup (CppKey const & key, CppKey const & group);

public:
	/**
	 * @brief This constructor is used to abstract the File and KeySet that we're working on as well as the parsing methods
	 * @param fileUtilityParam This FileUtility is where we read the file from
	 * @param keySetParam This KeySet is where we store the parsed keys
	 */
	KConfigParser (FileUtility & fileUtilityParam, CppKeySet & keySetParam);

	/**
	 * @brief This method is used to parses a KConfig file into an Elektra KeySet similarly to the KConfigIniBackend from the KConfig
	 * project
	 * @param parent the prefix to all the created keys
	 */
	void parse (CppKey const & parent);
};
}

#endif // ELEKTRA_KCONFIGPARSER_HPP
