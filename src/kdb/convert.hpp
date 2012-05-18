#ifndef CONVERT_HPP
#define CONVERT_HPP

#include <command.hpp>
#include <kdb.hpp>

class ConvertCommand : public Command
{
	kdb::KeySet ks;

public:
	ConvertCommand();
	~ConvertCommand();

	virtual std::string getShortOptions()
	{
		return "v";
	}

	virtual std::string getShortHelpText()
	{
		return "Export configuration from the key database.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"[<import-format>] [<export-format>] [<import-file>] [export-file]\n"
			"The import utility allows you to convert\n"
			"any file format supported by elektra to any other.\n"
			"\n"
			"By default it uses dump as import and export,\n"
			"reads from stdin and writes to stdout.\n"
			"\n"
			"Example:\n"
			"cat sw.ecf | kdb convert dump xmltool > sw.xml\n"
			"To convert a elektra dump file to xml.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
