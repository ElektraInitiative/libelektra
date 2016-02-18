/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "parser.hpp"

#include <fstream>
#include <sstream>

using namespace std;

parse_t parse (std::string const & file)
{
	ifstream fin (file.c_str ());

	parse_t result;

	int number = 0;
	int linenr = 0;
	string line;
	string lastIdentifier;
	map<string, string> currentMap;

	while (getline (fin, line))
	{
		++linenr;

		if (line.empty ())
		{
			result.push_back (currentMap);
			currentMap.clear ();
			++number;
			continue;
		}

		size_t colonPos = line.find (':');
		if (colonPos == string::npos) throw parse_error ("No : found", linenr);
		std::string identifier = line.substr (0, colonPos);
		std::string text = line.substr (colonPos + 1);
		if (identifier.empty ()) identifier = lastIdentifier;
		if (identifier.empty ()) throw parse_error ("Line started with : but there was no previous identifier", linenr);

		if (identifier == "number")
		{
			int cmpNumber;
			std::istringstream istr (text);
			istr >> cmpNumber;

			if (number != cmpNumber) throw parse_error ("Given number for that entry wrong", linenr);
		}

		if (!currentMap[identifier].empty ()) currentMap[identifier] += "\n";
		currentMap[identifier] += text;

		lastIdentifier = identifier;
	}

	// if new newline at end of file
	if (!currentMap.empty ())
	{
		result.push_back (currentMap);
	}

	return result;
}
