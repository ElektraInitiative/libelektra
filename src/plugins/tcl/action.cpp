/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <keyset.hpp>

#include <fstream>
#include <iostream>
#include <iterator>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>

#include <boost/bind.hpp>

#include "action.hpp"

#include <kdbease.h> // elektraKeyGetRelativeName

using namespace std;

namespace elektra
{

using namespace kdb;

void serialise (ostream & ofs, KeySet & output, Key & parent)
{

	ofs << '{' << endl;
	output.rewind ();
	while (Key k = output.next ())
	{
		ofs << "\t{" << endl;
		string const name = elektraKeyGetRelativeName (*k, *parent);
		ofs << "\t\t" << name << " = " << k.getString () << endl;
		k.rewindMeta ();
		while (const Key m = k.nextMeta ())
		{
			ofs << "\t\t{" << endl;
			ofs << "\t\t\t" << m.getName ().substr (sizeof ("meta:/") - 1) << " = " << m.getString () << endl;
			ofs << "\t\t}" << endl;
		}
		ofs << "\t}" << endl;
	}
	ofs << '}' << endl;
}

void unserialise (istream & in, KeySet & input, Key & parent)
{
	namespace qi = boost::spirit::qi;

	using boost::spirit::qi::space;

	in.unsetf (std::ios::skipws);
	boost::spirit::istream_iterator begin (in);
	boost::spirit::istream_iterator end;

	Action<boost::spirit::istream_iterator> p (input, parent);

	if (!boost::spirit::qi::phrase_parse (begin, end, p, space))
	{
		throw std::runtime_error ("boost::spirit::qi::phrase_parse returned unsuccessfully");
	}
}

} // namespace elektra
