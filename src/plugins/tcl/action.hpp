#include <keyset>
#include <ostream>

#include <boost/spirit/include/qi.hpp>
#include <boost/fusion/include/std_pair.hpp>

#include <boost/bind.hpp>

#include <iostream>
#include <iterator>
#include <fstream>
#include <map>

namespace elektra
{

struct Printer
{
	int nr_keys;
	int nr_meta;

	std::string keyname;
	std::string metaname;

	Printer () :
		nr_keys(0),
		nr_meta(0)
	{}

	void add_key(std::vector<char> const& c)
	{
		std::string s(c.begin(), c.end());
		++nr_keys;
		keyname = s;
		std::cout << "[" << nr_keys << "] add key " << s << std::endl;
	}

	void add_val(std::vector<char> const& c)
	{
		std::string s(c.begin(), c.end());
		std::cout << "[" << nr_keys << "] add val " << s << " to keyname " << keyname << std::endl;
	}

	void add_metakey(std::vector<char> const& c)
	{
		std::string s(c.begin(), c.end());
		++nr_meta;
		metaname = s;
		std::cout << "[" << nr_meta << "] add metakey " << s << std::endl;
	}

	void add_metaval(std::vector<char> const& c)
	{
		std::string s(c.begin(), c.end());
		std::cout << "[" << nr_meta << "] add metaval " << s << " to metaname " << metaname << std::endl;
	}
};


namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

template <typename Iterator>
struct key_value_sequence: qi::grammar<Iterator, ascii::space_type>
{
	key_value_sequence()
		: key_value_sequence::base_type(query),
		p()
	{
		query =  '{' >> pair >> *(pair) >> '}';
		pair  =  '{' >> key >> '=' >> val >>
			*('{' >> metakey >> '=' >> metaval >> '}') >>
			'}';
		key     =  (+(qi::char_ - qi::char_("={}[]<>"))) [boost::bind(&Printer::add_key, &p, _1)];
		val     =  (+(qi::char_ - qi::char_("={}[]<>"))) [boost::bind(&Printer::add_val, &p, _1)];
		metakey =  (+(qi::char_ - qi::char_("={}[]<>"))) [boost::bind(&Printer::add_metakey, &p, _1)];
		metaval =  (+(qi::char_ - qi::char_("={}[]<>"))) [boost::bind(&Printer::add_metaval, &p, _1)];
	}

	Printer p;

	qi::rule<Iterator, ascii::space_type> query;
	qi::rule<Iterator, ascii::space_type> pair;
	qi::rule<Iterator, ascii::space_type> key, val, metakey, metaval;
};

} // namespace elektra
