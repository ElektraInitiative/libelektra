/**
 * \file
 *
 * \brief Models a merge conflict
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef MERGECONFLICT_HPP_
#define MERGECONFLICT_HPP_

#include <string>
#include <stdexcept>

namespace kdb
{

namespace tools
{

namespace merging
{

enum ConflictOperation
{
	ADD, DELETE, MODIFY, META, SAME
};

class InvalidConflictOperation: public std::runtime_error
{
public:
	InvalidConflictOperation(std::string _message) :
			runtime_error (_message)
	{
	}
	;
};

class MergeConflictOperation
{
public:
	static std::string getFromTag(enum ConflictOperation operation)
	{
		switch (operation)
		{
		case ADD:
			return "add";
		case DELETE:
			return "delete";
		case MODIFY:
			return "modify";
		case META:
			return "meta";
		case SAME:
			return "same";
		}

		return "unknown";
	}

	static ConflictOperation getFromName(std::string name)
	{
		if (name == "add") return ADD;
		if (name == "delete") return DELETE;
		if (name == "modify") return MODIFY;
		if (name == "meta") return META;
		if (name == "same") return SAME;
		throw InvalidConflictOperation (
				"The conflict operation " + name + " is unknown");
	}
};

}
}
}

#endif /* MERGECONFLICT_HPP_ */
