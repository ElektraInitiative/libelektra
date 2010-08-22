#include "checker.hpp"
#include "factory.hpp"

namespace elektra {

Checker::~Checker()
{}

void StructChecker::buildup (Factory &, std::string const&)
{
}

void ListChecker::buildup (Factory &f, std::string const& templateParameter)
{
	structure = f.get(templateParameter);
	std::auto_ptr<Checker> c = f.get(templateParameter);
	if (!c.get()) throw "Could not create structure of template Parameter";

	// recursive handling code would belong here, but we dont have config
	// at the moment in ListChecker...
	c->buildup(f, "");

	structure = c;
}

} // end namespace elektra

