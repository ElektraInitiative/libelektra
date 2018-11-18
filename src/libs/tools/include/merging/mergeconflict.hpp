/**
 * @file
 *
 * @brief Models a merge conflict
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef MERGECONFLICT_HPP_
#define MERGECONFLICT_HPP_

#include <string>
#include <toolexcept.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

class InvalidConflictOperation : public ToolException
{
public:
	explicit InvalidConflictOperation (std::string message) : ToolException (message){};
};

enum ConflictOperation
{
	CONFLICT_ADD, ///< Conflict because of adding key
	CONFLICT_DELETE,
	CONFLICT_MODIFY,
	CONFLICT_META,
	CONFLICT_SAME
};

class MergeConflictOperation
{
public:
	static std::string getFromTag (enum ConflictOperation operation)
	{
		switch (operation)
		{
		case CONFLICT_ADD:
			return "CONFLICT_ADD";
		case CONFLICT_DELETE:
			return "CONFLICT_DELETE";
		case CONFLICT_MODIFY:
			return "CONFLICT_MODIFY";
		case CONFLICT_META:
			return "CONFLICT_META";
		case CONFLICT_SAME:
			return "CONFLICT_SAME";
		}

		return "unknown";
	}

	static ConflictOperation getFromName (std::string name)
	{
		if (name == "CONFLICT_ADD") return CONFLICT_ADD;
		if (name == "CONFLICT_DELETE") return CONFLICT_DELETE;
		if (name == "CONFLICT_MODIFY") return CONFLICT_MODIFY;
		if (name == "CONFLICT_META") return CONFLICT_META;
		if (name == "CONFLICT_SAME") return CONFLICT_SAME;
		throw InvalidConflictOperation ("The conflict operation \"" + name + "\" is unknown");
	}
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* MERGECONFLICT_HPP_ */
