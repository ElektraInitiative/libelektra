/**
 * @file
 *
 * @brief Sourcefile of Struct checker
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include "checker.hpp"
#include "factory.hpp"

namespace elektra
{

Checker::~Checker ()
{
}

void StructChecker::buildup (Factory &, std::string const &)
{
}

void ListChecker::buildup (Factory & f, std::string const & templateParameter)
{
	structure = f.get (templateParameter);
	CheckerPtr c = f.get (templateParameter);
	if (!c.get ())
		throw "Could not create structure of template Parameter";

	// recursive handling code would belong here, but we don't have config
	// at the moment in ListChecker...
	c->buildup (f, "");

	structure = move (c);
}

} // end namespace elektra
