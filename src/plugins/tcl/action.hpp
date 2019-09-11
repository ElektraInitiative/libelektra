/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ACTION_HPP
#define ACTION_HPP

#include <keyset.hpp>

#include <ostream>

#include <boost/bind.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_standard.hpp>

#include <fstream>
#include <iostream>
#include <iterator>
#include <map>

#include "printer.hpp"

namespace elektra
{


namespace qi = boost::spirit::qi;
namespace unicode = boost::spirit::standard;

template <typename Iterator>
struct Action : qi::grammar<Iterator, unicode::space_type>
{
	Action (kdb::KeySet & ks, kdb::Key & parent) : Action::base_type (query), p (ks, parent)
	{
		query = '{' >> *(pair) > '}';
		pair = '{' >> key > '=' >> val >> *('{' >> metakey > '=' >> metaval > '}') > '}'; // lgtm [cpp/comparison-precedence]


		key = (+(qi::char_ - qi::char_ ("={}[]<>")))[boost::bind (&Printer::add_key, &p, _1)];
		val = (+(qi::char_ - qi::char_ ("={}[]<>")))[boost::bind (&Printer::add_val, &p, _1)];
		metakey = (+(qi::char_ - qi::char_ ("={}[]<>")))[boost::bind (&Printer::add_metakey, &p, _1)];
		metaval = (+(qi::char_ - qi::char_ ("={}[]<>")))[boost::bind (&Printer::add_metaval, &p, _1)];
	}

	Printer p;

	qi::rule<Iterator, unicode::space_type> query;
	qi::rule<Iterator, unicode::space_type> pair;
	qi::rule<Iterator, unicode::space_type> key, val, metakey, metaval;
};

} // namespace elektra

#endif
