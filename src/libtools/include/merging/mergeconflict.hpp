/**
 * \file
 *
 * \brief Implements a way to build and deal with a backend
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
	ADD, DELETE, MODIFY
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
		}
	}

	static ConflictOperation getFromName(std::string name)
	{
		if (name == "add") return ADD;
		if (name == "delete") return DELETE;
		if (name == "modify") return MODIFY;
		throw InvalidConflictOperation (
				"The conflict operation " + name + " is unknown");
	}
};

}
}
}

#endif /* MERGECONFLICT_HPP_ */
