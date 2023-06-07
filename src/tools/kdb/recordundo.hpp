/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_RECORDUNDO_HPP
#define ELEKTRA_RECORDUNDO_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class RecordUndoCommand : public Command
{
public:
	RecordUndoCommand ();
	~RecordUndoCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	};

	virtual std::string getSynopsis () override
	{
		return "[parentKey]";
	};

	virtual std::string getShortHelpText () override
	{
		return "Undo what has been done in the recording.";
	}

	virtual std::string getLongHelpText () override
	{
		return "For example, \"kdb record-undo /test\" will undo everything under /test";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif // ELEKTRA_RECORDUNDO_HPP
