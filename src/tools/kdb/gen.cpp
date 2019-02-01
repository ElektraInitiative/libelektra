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
	auto & templates = GenTemplateList::getInstance ();

	if (cl.arguments.size () < 2)
	{
		throw invalid_argument ("need at least 2 arguments");
	}

	auto templateName = cl.arguments[0];
	auto outputName = cl.arguments[1];

	std::unordered_map<std::string, std::string> parameters = {};

	const auto tmpl = templates.getTemplate (templateName, outputName, parameters);

	KeySet ks;
	ks.append (Key ("/test", KEY_META, "type", "long", KEY_END));
	ks.append (Key ("/test", KEY_META, "type", "enum", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#2", "two", KEY_END));

	for (const auto & part : tmpl->getParts ())
	{
		auto output = std::ofstream (outputName + part);
		tmpl->render (output, part, ks);
	}

	return 0;
}
