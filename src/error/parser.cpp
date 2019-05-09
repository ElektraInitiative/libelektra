/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "parser.hpp"

#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>

using namespace std;
extern ostream cout;

parse_t parse (std::string const & file)
{
	ifstream fin (file.c_str ());

	parse_t result;

	int linenr = 0;
	string line;
	string lastIdentifier;
	map<string, string> currentMap;
	std::regex codeRegex ("^C[0-9A-Z]{5,5}$");


	while (getline (fin, line))
	{
		++linenr;

		if (line.empty ())
		{
			result.push_back (currentMap);
			currentMap.clear ();
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
			bool isHighlevelFile = (file.find ("highlevel") != string::npos);
			if (!std::regex_match (text, codeRegex) && !isHighlevelFile)
				throw parse_error ("Error code does not match regular expression C[0-9A-Z]{5,5}", linenr);
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
