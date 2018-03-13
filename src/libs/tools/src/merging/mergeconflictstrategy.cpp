/**
 * @file
 *
 * @brief Implementation of MergeConflictStrategy
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <merging/mergeconflictstrategy.hpp>
#include <string>

using namespace std;

namespace kdb
{

namespace tools
{

namespace merging
{

ConflictOperation MergeConflictStrategy::getOurConflictOperation (const Key & conflictKey)
{
	string ourConflictName = conflictKey.getMeta<string> ("conflict/operation/our");
	ConflictOperation ourOperation = MergeConflictOperation::getFromName (ourConflictName);
	return ourOperation;
}

ConflictOperation MergeConflictStrategy::getTheirConflictOperation (const Key & conflictKey)
{
	string theirConflictName = conflictKey.getMeta<string> ("conflict/operation/their");
	ConflictOperation theirOperation = MergeConflictOperation::getFromName (theirConflictName);
	return theirOperation;
}

void MergeConflictStrategy::copyKeyValue (const Key & source, Key & destination)
{
	if (source && destination)
	{
		if (source.isString ())
		{
			destination.setString (source.getString ());
		}
		else
		{
			if (source.getValue () == nullptr)
			{
				destination.setBinary (nullptr, 0);
			}
			else
			{
				destination.setBinary (source.getBinary ().c_str (), source.getBinarySize ());
			}
		}
	}
}
} // namespace merging
} // namespace tools
} // namespace kdb
