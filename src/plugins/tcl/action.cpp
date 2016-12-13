/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <keyset.hpp>

#include <fstream>
#include <iostream>
#include <iterator>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>

#include <boost/bind.hpp>

#include "action.hpp"

using namespace std;

namespace elektra
{

using namespace kdb;

void serialise (ostream & ofs, KeySet & output)
{

	ofs << '{' << endl;
	output.rewind ();
	while (Key k = output.next ())
	{
		ofs << "\t{" << endl;
		ofs << "\t\t" << k.getName () << " = " << (k.isString () ? k.getString () : k.getBinary ()) << endl;
		k.rewindMeta ();
		while (const Key m = k.nextMeta ())
		{
			ofs << "\t\t{" << endl;
			ofs << "\t\t\t" << m.getName () << " = " << m.getString () << endl;
			ofs << "\t\t}" << endl;
		}
		ofs << "\t}" << endl;
	}
	ofs << '}' << endl;
}

void unserialise (istream & in, KeySet & input)
{
	namespace qi = boost::spirit::qi;

	using boost::spirit::ascii::space;

	in.unsetf (std::ios::skipws);
	boost::spirit::istream_iterator begin (in);
	boost::spirit::istream_iterator end;

	Action<boost::spirit::istream_iterator> p (input);

	if (!boost::spirit::qi::phrase_parse (begin, end, p, space))
	{
		throw std::runtime_error ("boost::spirit::qi::phrase_parse returned unsuccessfully");
	}
}

} // end namepace elektra
