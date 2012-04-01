#include <keyset.hpp>

#include <iostream>
#include <iterator>
#include <fstream>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>

#include <boost/bind.hpp>

#include "action.hpp"

using namespace std;

namespace elektra
{

using namespace kdb;

void serialize(ostream &ofs, KeySet & output)
{

	ofs << '{' << endl;
	while (Key k = output.next())
	{
		ofs << "\t{" << endl;
		ofs << "\t\t" <<  k.getName() << " = " << k.getString() << endl;
		k.rewindMeta();
		while (const Key m = k.nextMeta())
		{
			ofs << "\t\t{" << endl;
			ofs << "\t\t\t" << m.getName() << " = " << m.getString() << endl;
			ofs << "\t\t}" << endl;
		}
		ofs << "\t}" << endl;
	}
	ofs << '}' << endl;
}

void unserialize(istream &in, KeySet & input)
{
	namespace qi = boost::spirit::qi;

	using boost::spirit::ascii::space;

	typedef std::istreambuf_iterator<char> base_iterator_type;

	in.unsetf (std::ios::skipws);
	boost::spirit::istream_iterator begin (in);
	boost::spirit::istream_iterator end;

	Action<boost::spirit::istream_iterator> p (input);

	bool result = boost::spirit::qi::phrase_parse(begin, end, p, space);

	if (!result)
	{
		std::cout << "Failed parsing input file!" << std::endl;
	}
}

} // end namepace elektra
