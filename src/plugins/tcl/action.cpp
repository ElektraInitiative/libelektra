#include <keyset>

#include <iostream>
#include <iterator>
#include <fstream>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>

#include <boost/bind.hpp>

#include "action.hpp"

using namespace std;

namespace elektra
{

using namespace kdb;

void serialize(ostream &ofs, KeySet & output)
{

	ofs << '{';
	while (Key k = output.next())
	{
		ofs << '{' << k.getName() << " = " << k.getString();
		k.rewindMeta();
		while (const Key m = k.nextMeta())
		{
			ofs << '{' << m.getName() << " = " << m.getString();
			ofs << '}';
		}
		ofs << '}';
	}
	ofs << '}';
}

void unserialize(istream &in, KeySet & output)
{
	namespace qi = boost::spirit::qi;

	using boost::spirit::ascii::space;

	typedef std::istreambuf_iterator<char> base_iterator_type;

	/*
	boost::spirit::multi_pass<base_iterator_type> begin =
		boost::spirit::make_default_multi_pass(base_iterator_type(in));
	boost::spirit::multi_pass<base_iterator_type> end =
		boost::spirit::make_default_multi_pass(base_iterator_type());
	*/

	std::string str = std::string(std::istreambuf_iterator<char>(in),
			std::istreambuf_iterator<char>());

	std::string::iterator begin = str.begin();
	std::string::iterator end = str.end();

	key_value_sequence<std::string::iterator> p;

	bool result = boost::spirit::qi::phrase_parse(begin, end, p, space);

	if (!result)
	{
		std::cout << "Failed parsing input file!" << std::endl;
	}
}

}
