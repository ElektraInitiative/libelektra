/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDBMERGE_HPP_
#define KDBMERGE_HPP_

#include <kdb.hpp>
#include <merging/threewaymerge.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

/**
 * Provides a merging wrapper around a KDB instance. The wrapper allows to pass
 * a three way merger instance that is used to resolve conflicts during KDB set.
 */
class MergingKDB {
public:
	MergingKDB(KDB * kdb) : innerKdb(kdb) { }
	~MergingKDB() { }

	/**
	 * Behaves like the KDB get function
	 *
	 * @see KDB
	 */
	virtual int get (KeySet & returned, std::string const & keyname);

	/**
	 * Behaves like the KDB get function
	 *
	 * @see KDB
	 */
	virtual int get (KeySet & returned, Key & parentKey);

	/**
	 * If a conflict occurs during set, the supplied merger is used to resolve the conflict.
	 * If the conflict cannot be solved, an exception is thrown.
	 *
	 * @see KDB
	 * @throws MergingKDBException
	 */
	virtual int set (KeySet & returned, std::string const & keyname, ThreeWayMerge & merger);

	/**
	 * If a conflict occurs during set, the supplied merger is used to resolve the conflict.
	 * If the conflict cannot be solved, an exception is thrown.
	 *
	 * @see KDB
	 * @throws MergingKDBException
	 */
	virtual int set (KeySet & returned, Key & parentKey, ThreeWayMerge & merger);

private:
	KDB * innerKdb;
	KeySet base;
};

class MergingKDBException : public Exception
{
public:
	MergingKDBException (Key key, KeySet conflicts) : m_key (key), m_conflicts(conflicts)
	{
	}

	virtual ~MergingKDBException () throw ()
	{
	}

	virtual const char * what () const throw () override
	{
		return "Exception while merging conflicting KeySets";
	}

private:
	Key m_key;
	KeySet m_conflicts;
};


}
}
}


#endif /* KDBMERGE_HPP_ */


