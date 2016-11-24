#ifndef ELEKTRA_REST_MODEL_ENTRY_HEADER_GUARD
#define ELEKTRA_REST_MODEL_ENTRY_HEADER_GUARD

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

namespace kdbrest
{

namespace model
{

class Entry : public kdb::Key
{

public:
	/**
                 * Constructs an Entry object based on a kdb::Key object.
                 * It is an alternative for implicit upwards cast.
                 * @param key An object of type kdb::Key
                 */
	inline Entry (kdb::Key & k) : kdb::Key (k)
	{
	}
	/**
                 * Constructs an Entry object based on a key name (string).
                 * It implicitely constructs a kdb::Key object by super().
                 * Adds the base config repository path to the entry name, so
                 * it is a valid key to be used with KDB.
                 * @param keyName A string to be used as key name
                 */
	inline Entry (std::string entryName) : kdb::Key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/") + entryName, KEY_END)
	{
	}
	/**
         * Constructs an Entry object based on four key parts (strings).
         * It implicitely constructs a kdb::Key object by super().
         * It also adds the base config repository path to the entry name, so
         * it is a valid key to be used with KDB.
         * @param organization The organization for the Entry
         * @param application The application for the Entry
         * @param scope The scope for the Entry
         * @param slug The unique slug for the Entry
         */
	inline Entry (const std::string & organization, const std::string & application, const std::string & scope,
		      const std::string & slug)
	: kdb::Key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/") + organization + std::string ("/") + application +
			    std::string ("/") + scope + std::string ("/") + slug,
		    KEY_END)
	{
	}

	/**
                 * Adds a key as sub key. Does check if the given key is really
                 * a sub key. If not, nothing is changed.
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
                 * Adds several keys as sub key. Relies on addSubkey(), so the
                 * conditional checks are done also for the mass add function.
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
                 * Getter for the kdb::KeySet containing all added sub keys.
                 * Returned will be a reference. That means changes to the
                 * subkeys vector will affect the entry. This allows for
                 * removal of subkeys.
                 * @return kdb::KeySet with all sub keys
                 */
	kdb::KeySet & getSubkeys ()
	{
		return m_subkeys;
	}

	/**
                 * Getter for the public name of the entry. That is the key name
                 * without the repository prefix. So if all keys are stored
                 * under a certain path, this path is erased from the key name.
                 * @return Key name without the storage path prefix
                 */
	std::string getPublicName () const
	{
		return this->getName ().erase (0, sizeof (ELEKTRA_REST_CONFIG_REPOSITORY_PATH));
	}
	/**
                 * Getter for the public name parts of the entry. That is a
                 * vector containing the four entry parts (organization,
                 * application, scope and unique slug).
                 * The public name is split at the separator "/" and the
                 * resulting parts are added to the vector in chronological order,
                 * so the first part of the entry name (key name) will also be
                 * the first entry in the resulting vector.
                 * @return 
                 */
	std::vector<std::string> getPublicNameParts () const
	{
		std::vector<std::string> result;
		std::string publicName = this->getPublicName ();
		boost::split (result, publicName, boost::is_any_of ("/"));
		return result;
	}
	/**
                 * Getter for the organization part of the public name.
                 * @return The organization of the entry
                 */
	std::string getOrganization () const
	{
		return this->getPublicNameParts ().at (0);
	}
	/**
                 * Getter for the application part of the public name.
                 * @return The application of the entry
                 */
	std::string getApplication () const
	{
		return this->getPublicNameParts ().at (1);
	}
	/**
                 * Getter for the scope part of the public name.
                 * @return The scope of the entry
                 */
	std::string getScope () const
	{
		return this->getPublicNameParts ().at (2);
	}
	/**
                 * Getter for the unique slug part of the public name.
                 * @return The unique slug of the entry
                 */
	std::string getSlug () const
	{
		return this->getPublicNameParts ().at (3);
	}

	/**
                 * Setter for the author of the entry.
                 * The author is stored as meta data of the key.
                 * @param author The author as string
                 */
	void setAuthor (const std::string & author)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR, author);
	}
	/**
                 * Getter for the author of the entry.
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
                 * Setter for the creation date of the entry.
                 * The creation date is stored as timestamp as meta data of the key.
                 * @param created_at The creation date as long
                 */
	void setCreatedAt (const long created_at)
	{
		this->setMeta<long> (ELEKTRA_REST_MODEL_ENTRY_META_CREATEDAT, created_at);
	}
	/**
                 * Getter for the creation date of the entry.
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
                 * Setter for the title of the entry.
                 * The title is stored as meta data of the key.
                 * @param title The title as string
                 */
	void setTitle (const std::string & title)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_TITLE, title);
	}
	/**
                 * Getter for the title of the entry.
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
                 * Setter for the description of the entry.
                 * The description is stored as meta data of the key.
                 * @param desc The description as string
                 */
	void setDescription (const std::string & desc)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_DESCRIPTION, desc);
	}
	/**
                 * Getter for the description of the entry.
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
                 * Setter for the tags of the entry.
                 * The tags are stored as a single string as meta data of
                 * the key.
                 * @param tags A vector containing all tags as string
                 */
	void setTags (const std::vector<std::string> & tags)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_TAGS, boost::algorithm::join (tags, " "));
	}
	/**
                 * Getter for the tags of the entry.
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
                 * Checks if the entry is tagged with a certain tag.
                 * @param tag A string repesenting a tag to be checked for
                 * @return True if the entry is tagged with the tag
                 */
	bool hasTag (const std::string & tag) const
	{
		std::vector<std::string> tags = this->getTags ();
		return std::find (tags.begin (), tags.end (), tag) != tags.end ();
	}

	/**
	 * Setter for the upload plugin of the entry.
	 * @param format The plugin name as string
	 */
	void setUploadPlugin (const std::string & plugin)
	{
		this->setMeta (ELEKTRA_REST_MODEL_ENTRY_META_UPLOADPLUGIN, plugin);
	}
	/**
                 * Getter for the upload plugin of the entry.
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
                 * Setter for the number of views of the entry.
                 * @param views The number of views as long
                 */
	void setViews (const long views)
	{
		this->setMeta<long> (ELEKTRA_REST_MODEL_ENTRY_META_VIEWS, views);
	}
	/**
	 * Adds an amount of views to the current number of views.
	 * @param views The number of views to add
	 */
	void addViews (const long views)
	{
		this->setViews (this->getViews () + views);
	}
	/**
                 * Getter for the number of views of the entry.
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
                 * Setter for the value of the entry.
                 * The value is stored as the value of the key.
                 * @param content A string containing the value
                 */
	void setValue (std::string & content)
	{
		this->set (content);
	}
	/**
                 * Getter for the value of the entry.
                 * @return A string containing the value
                 */
	std::string getValue () const
	{
		return this->get<std::string> ();
	}

	static bool less_than_key (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getPublicName (), r.getPublicName (), boost::is_iless ());
	}

	static bool less_than_title (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getTitle (), r.getTitle (), boost::is_iless ());
	}

	static bool less_than_created_at (Entry & l, Entry & r)
	{
		return l.getCreatedAt () <= r.getCreatedAt ();
	}

	static bool less_than_author (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getAuthor (), r.getAuthor (), boost::is_iless ());
	}

	static bool less_than_organization (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getOrganization (), r.getOrganization (), boost::is_iless ());
	}

	static bool less_than_application (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getApplication (), r.getApplication (), boost::is_iless ());
	}

	static bool less_than_scope (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getScope (), r.getScope (), boost::is_iless ());
	}

	static bool less_than_slug (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (l.getSlug (), r.getSlug (), boost::is_iless ());
	}

	static bool greater_than_key (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (r.getPublicName (), l.getPublicName (), boost::is_iless ());
	}

	static bool greater_than_title (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (r.getTitle (), l.getTitle (), boost::is_iless ());
	}

	static bool greater_than_created_at (Entry & l, Entry & r)
	{
		return r.getCreatedAt () <= l.getCreatedAt ();
	}

	static bool greater_than_author (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (r.getAuthor (), l.getAuthor (), boost::is_iless ());
	}

	static bool greater_than_organization (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (r.getOrganization (), l.getOrganization (), boost::is_iless ());
	}

	static bool greater_than_application (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (r.getApplication (), l.getApplication (), boost::is_iless ());
	}

	static bool greater_than_scope (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (r.getScope (), l.getScope (), boost::is_iless ());
	}

	static bool greater_than_slug (Entry & l, Entry & r)
	{
		return boost::lexicographical_compare (r.getSlug (), l.getSlug (), boost::is_iless ());
	}

private:
	kdb::KeySet m_subkeys;
};

} // namespace model

} // namespace kdbrest

#endif
