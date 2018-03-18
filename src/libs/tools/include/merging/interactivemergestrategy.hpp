/**
 * @file
 *
 * @brief Interactive merge strategy asking for user input at each step
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_LIBTOOL_INTERACTIVEMERGESTRATEGY_HPP
#define ELEKTRA_LIBTOOL_INTERACTIVEMERGESTRATEGY_HPP

#include <merging/mergeconflictstrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

// This strategy can be used to interactively merging keys. It will ask
// the user for each conflict which key version should be used. All
// questions will be asked via the supplied input stream and results
// will only be printed to the supplied outputstream.
class InteractiveMergeStrategy : public MergeConflictStrategy
{
public:
	InteractiveMergeStrategy (std::istream & input, std::ostream & output) : inputStream (input), outputStream (output)
	{
	}

	virtual void resolveConflict (const MergeTask & task, Key & conflictKey, MergeResult & result) override;

private:
	std::istream & inputStream;
	std::ostream & outputStream;
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif
