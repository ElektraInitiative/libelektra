#include "checker.hpp"
#include "factory.hpp"

namespace elektra {

Checker::~Checker()
{}

void StructChecker::buildup (Factory &, std::string const&)
{
	/* TODO should have map */
}

void ListChecker::buildup (Factory &f, std::string const& templateParameter)
{
	structure = f.get(templateParameter);
}

} // end namespace elektra

