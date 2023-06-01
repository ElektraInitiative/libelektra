#ifndef EDITOR_HPP
#define EDITOR_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

using namespace std;

class EditorCommand : public Command
{
	std::string filename;

public:
	EditorCommand ();
	~EditorCommand ();

	virtual int execute (Cmdline const & cmdline);
	void tmpFile ();

	virtual std::string getShortOptions ()
	{
		return "sie";
	}

	virtual std::string getSynopsis ()
	{
		return "[options] key-name [format]";
	}

	virtual std::string getShortHelpText ()
	{
		return "Use your editor for editing KDB.";
	}

	virtual std::string getLongHelpText ()
	{
		return "See kdb help editor\n";
	}
};

#endif
