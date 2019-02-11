/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <gen.hpp>

#include "gen/template.hpp"
#include <cmdline.hpp>
#include <kdb.hpp>

#include <fstream>

using namespace kdb;
using namespace std;

int GenCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () < 3)
	{
		throw invalid_argument ("need at least 3 arguments");
	}

	auto templateName = cl.arguments[0];
	auto parentKey = cl.arguments[1];
	auto outputName = cl.arguments[2];

	std::unordered_map<std::string, std::string> parameters;
	if (cl.arguments.size () > 3)
	{
		std::transform (cl.arguments.begin () + 3, cl.arguments.end (), std::inserter (parameters, parameters.end ()),
				[](const std::string & param) {
					auto search = param.find ('=');
					if (search == std::string::npos)
					{
						throw invalid_argument ("parameters have to be of format name=value");
					}

					return std::make_pair (std::string (param.begin (), param.begin () + search),
							       std::string (param.begin () + search + 1, param.end ()));
				});
	}

	const auto & templates = GenTemplateList::getInstance ();
	const auto tmpl = templates.getTemplate (templateName, parameters);

	KDB kdb;
	KeySet ks;
	kdb.get (ks, parentKey);

	for (const auto & part : tmpl->getParts ())
	{
		auto output = std::ofstream (outputName + part);
		tmpl->render (output, outputName, part, ks, parentKey);
	}

	return 0;
}
