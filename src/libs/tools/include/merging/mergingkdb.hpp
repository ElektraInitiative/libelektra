/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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
class MergingKDB : public KDB
{
public:
	MergingKDB ();
	explicit MergingKDB (KDB & kdb);
	virtual ~MergingKDB () throw ();

	/**
	 * Behaves like the KDB get function
	 *
	 * @see KDB
	 */
	int get (KeySet & returned, std::string const & keyname) override;

	/**
	 * Behaves like the KDB get function
	 *
	 * @see KDB
	 */
	int get (KeySet & returned, Key & parentKey) override;

	/**
	 * Synchronizes the file with the supplied KeySet.
	 * If a conflict occurs during set, the supplied merger is used to resolve the conflict.
	 * If the conflict cannot be solved, an exception is thrown. If the KeySet was successfully
	 * written (either by merging or due the absence of a conflict) the supplied KeySet is updated
	 * with the new content of the file.
	 *
	 * @see KDB
	 * @throws MergingKDBException
	 */
	virtual int synchronize (KeySet & returned, std::string const & keyname, ThreeWayMerge & merger);

	/**
	 * If a conflict occurs during set, the supplied merger is used to resolve the conflict.
	 * If the conflict cannot be solved, an exception is thrown. If the KeySet was successfully
	 * written (either by merging or due the absence of a conflict) the supplied KeySet is updated
	 * with the new content of the file.
	 *
	 * @see KDB
	 * @throws MergingKDBException
	 */
	virtual int synchronize (KeySet & returned, Key & parentKey, ThreeWayMerge & merger);

private:
	KeySet base;
};

class MergingKDBException : public KDBException
{
public:
	MergingKDBException (Key key, KeySet conflicts) : KDBException (key), m_conflicts (conflicts)
	{
	}

	virtual ~MergingKDBException () throw ()
	{
	}

	virtual const char * what () const throw () override
	{
		return "Exception while merging conflicting KeySets";
	}

	KeySet getConflicts () const
	{
		return m_conflicts;
	}

private:
	KeySet m_conflicts;
};
} // namespace merging
} // namespace tools
} // namespace kdb


#endif /* KDBMERGE_HPP_ */
