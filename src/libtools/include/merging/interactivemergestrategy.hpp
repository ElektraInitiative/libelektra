/**
 * \file
 *
 * \brief Interactive merge strategy asking for user input at each step
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef INTERACTIVEMERGESTRATEGY_HPP_
#define INTERACTIVEMERGESTRATEGY_HPP_

#include <merging/mergeconflictstrategy.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace merging
{

class InteractiveMergeStrategy : public MergeConflictStrategy
{
public:

	InteractiveMergeStrategy(istream& _inputStream, ostream& _outputStream) :
			inputStream (_inputStream), outputStream (_outputStream)
	{
	}

	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result);

private:
	istream& inputStream;
	ostream& outputStream;
};

}
}
}

#endif /* INTERACTIVEMERGESTRATEGY_HPP_ */
