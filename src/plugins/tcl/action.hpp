/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ACTION_HPP
#define ACTION_HPP

#include <keyset.hpp>

#include <ostream>

#include <boost/bind.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/spirit/include/qi.hpp>

#include <fstream>
#include <iostream>
#include <iterator>
#include <map>

#include "printer.hpp"

namespace elektra
{


namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

template <typename Iterator>
struct Action : qi::grammar<Iterator, ascii::space_type>
{
	Action (kdb::KeySet & ks) : Action::base_type (query), p (ks)
	{
		query = '{' >> *(pair) > '}';
		pair = '{' >> key > '=' >> val >> *('{' >> metakey > '=' >> metaval > '}') > '}';

		key = (+(qi::char_ - qi::char_ ("={}[]<>")))[boost::bind (&Printer::add_key, &p, _1)];
		val = (+(qi::char_ - qi::char_ ("={}[]<>")))[boost::bind (&Printer::add_val, &p, _1)];
		metakey = (+(qi::char_ - qi::char_ ("={}[]<>")))[boost::bind (&Printer::add_metakey, &p, _1)];
		metaval = (+(qi::char_ - qi::char_ ("={}[]<>")))[boost::bind (&Printer::add_metaval, &p, _1)];
	}

	Printer p;

	qi::rule<Iterator, ascii::space_type> query;
	qi::rule<Iterator, ascii::space_type> pair;
	qi::rule<Iterator, ascii::space_type> key, val, metakey, metaval;
};

} // namespace elektra

#endif
