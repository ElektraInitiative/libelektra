/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef PARSER_HPP
#define PARSER_HPP

#include <map>
#include <string>
#include <vector>

struct parse_error : std::exception
{
	std::string info;
	int linenr;

	parse_error (std::string info_, int linenr_) : info (std::move (info_)), linenr (linenr_) {}

	~parse_error () throw () {}

	virtual const char * what () const throw () override { return info.c_str (); }
};

typedef std::vector<std::map<std::string, std::string>> parse_t;

parse_t parse (std::string const & file);

#endif
