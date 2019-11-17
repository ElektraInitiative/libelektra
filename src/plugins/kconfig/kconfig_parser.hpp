#ifndef ELEKTRA_KCONFIGPARSER_HPP
#define ELEKTRA_KCONFIGPARSER_HPP

#include "file_utility.hpp"
#include <kdbplugin.hpp>

class KconfigParser
{
private:
	FileUtility & fileUtility;
	kdb::KeySet & keySet;

	kdb::Key loadGroupNameFromFile (kdb::Key const & parent);
	kdb::Key loadKeyFromFile (kdb::Key const & parent);

	void appendIfContainsMeta (kdb::Key const & key);
	void appendIfNotGroup (kdb::Key const & key, kdb::Key const & group);

public:
	KconfigParser (FileUtility & fileUtilityParam, kdb::KeySet & keySetParam);
	void parse (kdb::Key const & parent);
};


#endif // ELEKTRA_KCONFIGPARSER_HPP
