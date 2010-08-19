#include "checker.hpp"
#include "factory.hpp"

namespace elektra {

Checker::~Checker()
{}

void StructChecker::buildup (Factory &, std::string const&)
{
	/* TODO should have map */
	std::cout << "buildup struct checker" << std::endl;
}

void ListChecker::buildup (Factory &f, std::string const& templateParameter)
{
	structure = f.get(templateParameter);
	std::cout << "buildup list checker with parameter " <<
		templateParameter << std::endl;
	std::auto_ptr<Checker> c = f.get(templateParameter);
	if (!c.get()) throw "Could not create structure of template Parameter";

	// mmh, cant go on to do that recursive, dont have config
	// here...
	c->buildup(f, "");

	structure = c;
}

} // end namespace elektra

