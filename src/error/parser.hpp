#ifndef PARSER_HPP
#define PARSER_HPP

#include <vector>
#include <string>
#include <map>

struct parse_error: std::exception
{
	std::string info;
	int linenr;

	parse_error(std::string info, int linenr) :
		info(info),
		linenr(linenr)
	{}

	~parse_error() throw()
	{}

	virtual const char* what() throw()
	{
		return info.c_str();
	}
};

typedef std::vector<std::map<std::string, std::string> > parse_t;

parse_t parse(std::string const& file);

#endif
