/**
 * @file
 *
 * @brief model for snippet entry
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_MODEL_ENTRY_HPP
#define ELEKTRA_REST_MODEL_ENTRY_HPP

#include <algorithm>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/compare.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/tokenizer.hpp>

#include <config.hpp>
#include <exceptions.hpp>
#include <kdb_includes.hpp>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

/**
 * @brief namespace for models
 */
namespace model
{

/**
 * @brief model class for a configuration snippet entry
 * 
 * this class encapsulates all information that belongs to
 * a configuration snippet entry, including the snippet itself.
 */
class Entry : public kdb::Key
{

public:
	/**
     * @brief constructs an Entry object based on a kdb::Key object
	 * 
     * @param key An object of type kdb::Key
     */
	Entry (kdb::Key & k) : kdb::Key (k)
	{
	}

	/**
	 * @brief constructs an Entry based on its public key
	 * 
     * Constructs an Entry object based on a key name (string).
     * It implicitely constructs a kdb::Key object by super().
     * Adds the base config repository path to the entry name, so
     * it is a valid key to be used with KDB.
	 * 
     * @param keyName A string to be used as key name
     */
	Entry (std::string entryName)
	: kdb::Key (Config::instance ().getConfig ().get<std::string> ("kdb.path.configs") + std::string ("/") + entryName, KEY_END)
	{
	}

	/**
	 * @brief constructs an Entry based on its public key parts
	 * 
     * Constructs an Entry object based on four key parts (strings).
     * It implicitely constructs a kdb::Key object by super().
     * It also adds the base config repository path to the entry name, so
     * it is a valid key to be used with KDB.
	 * 
     * @param organization The organization for the Entry
     * @param application The application for the Entry
     * @param scope The scope for the Entry
     * @param slug The unique slug for the Entry
     */
	Entry (const std::string & organization, const std::string & application, const std::string & scope, const std::string & slug)
	: kdb::Key (Config::instance ().getConfig ().get<std::string> ("kdb.path.configs") + std::string ("/") + organization +
			    std::string ("/") + application + std::string ("/") + scope + std::string ("/") + slug,
		    KEY_END)
	{
	}

	/**
	 * @brief attempts to add a subkey to the entry key
	 * 
     * Adds a key as sub key. Does check if the given key is really
     * a sub key. If not, nothing is changed.
	 * 
	 * @param key The key do be added as sub key, if it is one.
     */
	void addSubkey (kdb::Key k)
	{
		if (k.isBelow (static_cast<kdb::Key &> (*this)))
		{
			m_subkeys.append (k);
		}
	}

	/**
	 * @brief attempts to add all keys of the keyset as subkey
	 * 
	 * @note other than the overloaded @ref addSubkeys function that takes
	 * iterators as arguments, this function will not stop trying to add keys
	 * if a mismatch is found.
	 * 
	 * @param ks a keyset that should be added to the entry subkeys
	 */
	void addSubkeys (kdb::KeySet & ks)
	{
		for (auto elem : ks)
		{
			this->addSubkey (elem);
		}
	}

	/**
	 * @brief attempts to add many keys as subkeys
	 * 
     * Adds several keys as sub key. Checks if the given keys
	 * are subkeys before they are added. Returns on the first
	 * non-subkey the iterator.
	 * 
     * @param iter_start A KeySet iterator from where on sub keys should be added
	 * @param iter_end The end of the KeySet until we can read
	 * @return The iterator of the first non-child element
     */
	kdb::KeySet::iterator addSubkeys (kdb::KeySet::iterator iter_start, kdb::KeySet::iterator iter_end)
	{
		kdb::Key & k = static_cast<kdb::Key &> (*this);
		auto elem = iter_start;
		while (elem != iter_end)
		{
			if (elem.get ().isBelow (k))
			{
				m_subkeys.append (elem.get ());
			}
			else
			{
				break; // because keyset is sorted, following keys can't be children
			}
			elem++;
		}
		return elem;
	}

	/**
	 * @brief getter for the subkeys keyset
	 * 
     * Getter for the kdb::KeySet containing all added sub keys.
     * Returned will be a reference. That means changes to the
     * subkeys vector will affect the entry. This allows for
     * removal of subkeys.
	 * 
     * @return kdb::KeySet with all sub keys
     */
	kdb::KeySet & getSubkeys ()
	{
		return m_subkeys;
	}

	/**
	 * @brief getter for the public part of the key name
	 * 
     * Getter for the public name of the entry. That is the key name
     * without the repository prefix. So if all keys are stored
     * under a certain path, this path is erased from the key name.
	 * 
     * @return Key name without the storage path prefix
     */
	std::string getPublicName () const
	{
		return this->getName ().erase (0, Config::instance ().getConfig ().get<std::string> ("kdb.path.configs").length () + 1);
	}

	/**
	 * @brief getter for the splitted public key part
	 * 
     * Getter for the public name parts of the entry. That is a
     * vector containing the four entry parts (organization,
     * application, scope and unique slug).
     * The public name is split at the separator "/" and the
     * resulting parts are added to the vector in chronological order,
     * so the first part of the entry name (key name) will also be
     * the first entry in the resulting vector.
	 * 
     * @return vector containing all four public key parts
     */
	std::vector<std::string> getPublicNameParts () const
	{
		std::vector<std::string> result;
		std::string publicName = this->getPublicName ();
		boost::split (result, publicName, boost::is_any_of ("/"));
		return result;
	}

	/**
	 * @brief getter for the organization part of the public name
	 * 
     * @return The organization of the entry
     */
	std::string getOrganization () const
	{
		return this->getPublicNameParts ().at (0);
	}

	/**
     * @brief getter for the application part of the public name
	 * 
     * @return The application of the entry
     */
	std::string getApplication () const
	{
		return this->getPublicNameParts ().at (1);
	}

	/**
     * @brief getter for the scope part of the public name
	 * 
	 * @return The scope of the entry
     */
	std::string getScope () const
	{
		return this->getPublicNameParts ().at (2);
	}

	/**
     * @brief getter for the unique slug part of the public name
	 * 
     * @return The unique slug of the entry
     */
	std::string getSlug () const
	{
		return this->getPublicNameParts ().at (3);
	}

	/**
     * @brief setter for the author of the entry
	 * 
     * @note the author is stored as meta data of the key.
	 * 
     * @param author The author as string
     */
	void setAuthor (const std::string & author)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR, author);
	}

	/**
     * @brief getter for the author of the entry
	 * 
     * @return The author as string
     */
	std::string getAuthor () const
	{
		if (this->hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR))
			return this->getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR);
		else
			return std::string ();
	}

	/**
     * @brief setter for the creation date of the entry
	 * 
     * @note the creation date is stored as timestamp as meta data of the key.
	 * 
     * @param created_at The creation date as long
     */
	void setCreatedAt (const long created_at)
	{
		this->setMeta<long> (ELEKTRA_REST_MODEL_ENTRY_META_CREATEDAT, created_at);
	}

	/**
     * @brief getter for the creation date of the entry
	 * 
     * @return The creation date as timestamp as long
     */
	long getCreatedAt () const
	{
		if (this->hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_CREATEDAT))
			return this->getMeta<long> (ELEKTRA_REST_MODEL_ENTRY_META_CREATEDAT);
		else
			return 0;
	}

	/**
     * @brief setter for the title of the entry
	 * 
     * @note the title is stored as meta data of the key.
	 * 
     * @param title The title as string
     */
	void setTitle (const std::string & title)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_TITLE, title);
	}

	/**
     * @brief getter for the title of the entry
	 * 
     * @return The title as string
     */
	std::string getTitle () const
	{
		if (this->hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_TITLE))
			return this->getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_TITLE);
		else
			return std::string ();
	}

	/**
     * @brief setter for the description of the entry
	 * 
     * @note the description is stored as meta data of the key.
	 * 
     * @param desc The description as string
     */
	void setDescription (const std::string & desc)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_DESCRIPTION, desc);
	}

	/**
     * @brief getter for the description of the entry
	 * 
     * @return The description as string
     */
	std::string getDescription () const
	{
		if (this->hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_DESCRIPTION))
			return this->getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_DESCRIPTION);
		else
			return std::string ();
	}

	/**
     * @brief setter for the tags of the entry
	 * 
     * @note the tags are stored as a single string as meta data of
     *       the key.
	 * 
     * @param tags A vector containing all tags as string
     */
	void setTags (const std::vector<std::string> & tags)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_TAGS, boost::algorithm::join (tags, " "));
	}

	/**
     * @brief getter for the tags of the entry
	 * 
     * @return A vector containing all tags as string
     */
	std::vector<std::string> getTags () const
	{
		if (this->hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_TAGS))
		{
			std::string tags = this->getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_TAGS);
			std::stringstream ss (tags);
			std::istream_iterator<std::string> ss_begin (ss);
			std::istream_iterator<std::string> ss_end;
			std::vector<std::string> vstrings (ss_begin, ss_end);
			return vstrings;
		}
		else
		{
			return std::vector<std::string> ();
		}
	}

	/**
     * @brief checks if the entry is tagged with a certain tag
	 * 
     * @param tag A string repesenting a tag to be checked for
     * @return True if the entry is tagged with the tag
     */
	bool hasTag (const std::string & tag) const
	{
		std::vector<std::string> tags = this->getTags ();
		return std::find (tags.begin (), tags.end (), tag) != tags.end ();
	}

	/**
	 * @brief setter for the upload plugin of the entry
	 * 
	 * @param format The plugin name as string
	 */
	void setUploadPlugin (const std::string & plugin)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_UPLOADPLUGIN, plugin);
	}

	/**
     * @brief getter for the upload plugin of the entry
	 * 
     * @return The plugin name as string
     */
	std::string getUploadPlugin () const
	{
		if (this->hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_UPLOADPLUGIN))
			return this->getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_UPLOADPLUGIN);
		else
			return std::string ();
	}

	/**
     * @brief setter for the number of views of the entry
	 * 
     * @param views The number of views as long
     */
	void setViews (const long views)
	{
		this->setMeta<long> (ELEKTRA_REST_MODEL_ENTRY_META_VIEWS, views);
	}

	/**
	 * @brief adds an amount of views to the current number of views
	 * 
	 * @param views The number of views to add
	 */
	void addViews (const long views)
	{
		this->setViews (this->getViews () + views);
	}

	/**
     * @brief getter for the number of views of the entry
	 * 
     * @return The number of views as long
     */
	long getViews () const
	{
		if (this->hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_VIEWS))
			return this->getMeta<long> (ELEKTRA_REST_MODEL_ENTRY_META_VIEWS);
		else
			return 0;
	}

	/**
     * @brief setter for the value of the entry
	 * 
     * @note the value is stored as the value of the key.
	 * 
     * @param content A string containing the value
     */
	void setValue (std::string & content)
	{
		this->set (content);
	}

	/**
     * @brief getter for the value of the entry
	 * 
     * @return A string containing the value
     */
	std::string getValue () const
	{
		return this->get<std::string> ();
	}

	/**
	 * @brief compares two entries based on their key
	 * 
	 * @param l left entry
	 * @param r right entry
	 * @return true if public key name of l < r
	 */
	static bool less_than_key (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getPublicName (), r.getPublicName (), boost::is_iless ());
	}

	/**
	 * @brief compares two entries based on their title
	 * 
	 * @param l left entry
	 * @param r right entry
	 * @return true if title of l < r
	 */
	static bool less_than_title (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getTitle (), r.getTitle (), boost::is_iless ());
	}

	/**
	 * @brief compares two entries based on their creation date
	 * 
	 * @param l left entry
	 * @param r right entry
	 * @return true if creation date of l < r
	 */
	static bool less_than_created_at (Entry & l, Entry & r)
	{
		return l.getCreatedAt () <= r.getCreatedAt ();
	}

	/**
	 * @brief compares two entries based on their author
	 * 
	 * @param l left entry
	 * @param r right entry
	 * @return true if author of l < r
	 */
	static bool less_than_author (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getAuthor (), r.getAuthor (), boost::is_iless ());
	}

	/**
	 * @brief compares two entries based on their organization
	 * 
	 * @param l left entry
	 * @param r right entry
	 * @return true if organization of l < r
	 */
	static bool less_than_organization (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getOrganization (), r.getOrganization (), boost::is_iless ());
	}

	/**
	 * @brief compares two entries based on their application
	 * 
	 * @param l left entry
	 * @param r right entry
	 * @return true if application of l < r
	 */
	static bool less_than_application (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getApplication (), r.getApplication (), boost::is_iless ());
	}

	/**
	 * @brief compares two entries based on their scope
	 * 
	 * @param l left entry
	 * @param r right entry
	 * @return true if scope of l < r
	 */
	static bool less_than_scope (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getScope (), r.getScope (), boost::is_iless ());
	}

	/**
	 * @brief compares two entries based on their slug
	 * 
	 * @param l left entry
	 * @param r right entry
	 * @return true if slug of l < r
	 */
	static bool less_than_slug (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getSlug (), r.getSlug (), boost::is_iless ());
	}

private:
	kdb::KeySet m_subkeys;
};

} // namespace model

} // namespace kdbrest

#endif
