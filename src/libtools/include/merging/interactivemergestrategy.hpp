/**
 * \file
 *
 * \brief Interactive merge strategy asking for user input at each step
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

class InteractiveMergeStrategy : public MergeConflictStrategy
{
public:

	InteractiveMergeStrategy(std::istream & input,
			std::ostream & output) :
		inputStream (input),
		outputStream (output)
	{
	}

	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result);

private:
	std::istream& inputStream;
	std::ostream& outputStream;
};

}
}
}

#endif
