/**
 * @file
 *
 * @brief model for user
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_MODEL_USER_HPP
#define ELEKTRA_REST_MODEL_USER_HPP

#include <map>
#include <string>
#include <utility>

#include <boost/algorithm/string/compare.hpp>
#include <boost/algorithm/string/predicate.hpp>

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
 * @brief model class for a REST service user
 * 
 * this class encapsulates all information that belongs to a user.
 */
class User : public kdb::Key
{

public:
	/**
     * @brief constructs an User object based on a kdb::Key object
	 * 
     * @param key A kdb::Key object
     */
	User (const kdb::Key & k) : kdb::Key (k)
	{
	}

	/**
     * @brief constructs an User object based on a username
	 * 
     * It implicitely constructs a kdb::Key object by super().
     * Adds the base user repository path to the username, so it
     * is a valid key to be use with KDB.
	 * 
     * @param username The username to be used for the key
     */
	User (const std::string username)
	: kdb::Key (Config::instance ().getConfig ().get<std::string> ("kdb.path.users") + std::string ("/") + username, KEY_END)
	{
	}

	/**
     * @brief attempts to add a key as subkey of the entry
	 * 
	 * Does check if the given key is really a sub key.
	 * If not, nothing is changed.
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
     * @brief getter for the kdb::KeySet containing all added sub keys
	 * 
     * Returned will be a reference. That means changes to the
     * subkeys keyset will affect the user. This allows for
     * removal of subkeys.
	 * 
     * @return kdb::KeySet with all sub keys
     */
	kdb::KeySet & getSubkeys ()
	{
		return this->m_subkeys;
	}

	/**
     * @brief getter for a sub key by name
	 * 
	 * If no sub key with the given name exists, an exception 
	 * will be thrown.
	 * 
     * @param name The name of the sub key to look for
     * @return The requested sub key
     * @throws kdbrest::exception::SubkeyNotFoundException
     */
	kdb::Key getSubkey (const std::string name) const
	{
		for (auto elem : this->m_subkeys)
		{
			if (elem.getName ().compare (this->getName () + std::string ("/") + name) == 0)
			{
				return elem;
			}
		}
		throw kdbrest::exception::SubkeyNotFoundException ();
	}

	/**
     * @brief getter for the username
	 * 
     * @return Username as string
     */
	std::string getUsername () const
	{
		return this->getName ().erase (0, Config::instance ().getConfig ().get<std::string> ("kdb.path.users").length () + 1);
	}

	/**
     * @brief setter for the password hash
	 * 
     * @param passwordHash Password hash as string
     */
	void setPasswordHash (std::string passwordHash)
	{
		this->set (passwordHash);
	}

	/**
     * @brief getter for the password hash
	 * 
     * @return Password hash as string
     */
	std::string getPasswordHash () const
	{
		return this->get<std::string> ();
	}

	/**
     * @brief setter for the email
	 * 
     * @param email Email as string
     */
	void setEmail (std::string email)
	{
		kdb::Key k;
		try
		{
			k = this->getSubkey (ELEKTRA_REST_MODEL_USER_META_EMAIL);
		}
		catch (kdbrest::exception::SubkeyNotFoundException & e)
		{
			k.setName (this->getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_EMAIL);
		}
		k.set<std::string> (email);
		this->addSubkey (k);
	}

	/**
     * @brief getter for the email
	 * 
     * @return Email as string
     */
	std::string getEmail () const
	{
		try
		{
			kdb::Key k = this->getSubkey (ELEKTRA_REST_MODEL_USER_META_EMAIL);
			return k.get<std::string> ();
		}
		catch (kdbrest::exception::SubkeyNotFoundException & e)
		{
			return "";
		}
	}

	/**
     * @brief setter for the rank
	 * 
     * The rank may be anything between (including)
	 *   ELEKTRA_REST_USER_MIN_RANK and
	 *   ELEKTRA_REST_USER_MAX_RANK
	 * 
     * @param rank Rank as integer
     */
	void setRank (int rank)
	{
		kdb::Key k;
		try
		{
			k = this->getSubkey (ELEKTRA_REST_MODEL_USER_META_RANK);
		}
		catch (kdbrest::exception::SubkeyNotFoundException & e)
		{
			k.setName (this->getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_RANK);
		}
		k.set<int> (rank);
		this->addSubkey (k);
	}

	/**
     * @brief getter for the rank
	 * 
	 * If no rank is set yet, the default rank for users
	 * will be returned.
	 *
     * @return Rank as integer
     */
	int getRank () const
	{
		try
		{
			kdb::Key k = this->getSubkey (ELEKTRA_REST_MODEL_USER_META_RANK);
			return k.get<int> ();
		}
		catch (kdbrest::exception::SubkeyNotFoundException & e)
		{
			return Config::instance ().getConfig ().get<int> ("permissions.rank.default"); // default is user rank
		}
	}

	/**
     * @brief setter for the creation date
	 * 
     * @param created_at Timestamp when the user has been created
     */
	void setCreatedAt (const long created_at)
	{
		kdb::Key k;
		try
		{
			k = this->getSubkey (ELEKTRA_REST_MODEL_USER_META_CREATEDAT);
		}
		catch (kdbrest::exception::SubkeyNotFoundException & e)
		{
			k.setName (this->getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_CREATEDAT);
		}
		k.set<long> (created_at);
		this->addSubkey (k);
	}

	/**
     * @brief getter for the creation date
	 * 
     * @return Timestamp when the user has been created
     */
	long getCreatedAt () const
	{
		try
		{
			kdb::Key k = this->getSubkey (ELEKTRA_REST_MODEL_USER_META_CREATEDAT);
			return k.get<long> ();
		}
		catch (kdbrest::exception::SubkeyNotFoundException & e)
		{
			return 0;
		}
	}

	/**
	 * @brief compares two users based on their username
	 * 
	 * @param l left user
	 * @param r right user
	 * @return true if the username of l < r
	 */
	static bool less_than_username (User & l, User & r)
	{
		return boost::lexicographical_compare (l.getUsername (), r.getUsername (), boost::is_iless ());
	}

	/**
	 * @brief compares two users based on their email
	 * 
	 * @param l left user
	 * @param r right user
	 * @return true if the email of l < r
	 */
	static bool less_than_email (User & l, User & r)
	{
		return boost::lexicographical_compare (l.getEmail (), r.getEmail (), boost::is_iless ());
	}

	/**
	 * @brief compares two users based on their creation date
	 * 
	 * @param l left user
	 * @param r right user
	 * @return true if the creation date of l < r
	 */
	static bool less_than_created_at (User & l, User & r)
	{
		return l.getCreatedAt () <= r.getCreatedAt ();
	}

	/**
	 * @brief compares two users based on their rank
	 * 
	 * @param l left user
	 * @param r right user
	 * @return true if the rank of l < r
	 */
	static bool less_than_rank (User & l, User & r)
	{
		return l.getRank () <= r.getRank ();
	}

private:
	kdb::KeySet m_subkeys;
};

} // namespace model

} // namespace kdbrest

#endif
