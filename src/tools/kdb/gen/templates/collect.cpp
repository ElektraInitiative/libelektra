/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <algorithm>
#include <cctype>
#include <fstream>
#include <iomanip>
#include <ios>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <unistd.h>
#include <vector>

std::string escapeForLiteral (std::istream & input)
{
	std::stringstream result;
	for (char c; input >> std::noskipws >> c;)
	{
		if (c == '"' || c == '\'' || c == '\\' || c == '?')
		{
			result << "\\" << c;
		}
		else if (c == '\n')
		{
			result << "\\n";
		}
		else if (c == '\r')
		{
			result << "\\r";
		}
		else if (c == '\t')
		{
			result << "\\t";
		}
		else if (isprint (c))
		{
			result << c;
		}
		else
		{
			std::ios init (nullptr);
			init.copyfmt (result);
			result << "\\x" << std::hex << std::setw (2) << std::setfill ('0') << int(c);
			result.copyfmt (init);
		}
	}

	return result.str ();
}

inline std::string convertToName (const std::string & str)
{
	std::string result (str);
	auto pos = result.find (".mustache");
	if (pos == result.length () - sizeof (".mustache") + 1)
	{
		result.erase (pos);
	}
	std::replace_if (result.begin (), result.end (), std::not1 (std::ptr_fun (isalnum)), '_');
	return result;
}

void process (std::ostream & out, std::vector<std::string> files)
{
	out << "#include <unordered_map>" << std::endl << "#include <string>" << std::endl << std::endl;

	for (const auto & file : files)
	{
		std::ifstream in (file);

		const auto name = convertToName (file);
		out << "static const char * const _kdbgentemplate_" << name << "= \"" << escapeForLiteral (in) << "\";" << std::endl;
	}

	out << std::endl;
	out << "static const std::unordered_map<std::string, std::string> kdbgenTemplates = {" << std::endl;
	for (const auto & file : files)
	{
		const auto name = convertToName (file);
		out << "\t{ \"" << name << "\", _kdbgentemplate_" << name << " }," << std::endl;
	}
	out << "};" << std::endl << std::endl;
}

int main (int argc, char const * argv[])
{
	if (argc < 2)
	{
		std::cerr << "Usage: " << argv[0] << " outfile [infiles...]" << std::endl
			  << "use -- as outfile to print to stdout" << std::endl;
		return 1;
	}

	std::vector<std::string> args (argv + 2, argv + argc);

	if (std::string (argv[1]) != "--")
	{
		std::ofstream file (argv[1]);
		if (!file.good ())
		{
			std::cerr << "Error: could not create outfile. Make sure the directory exists!" << std::endl;
			return 2;
		}
		process (file, args);
		file.flush ();
	}
	else
	{
		process (std::cout, args);
	}

	return 0;
}
