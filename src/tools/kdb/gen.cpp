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

#include <unistd.h>

using namespace kdb;
using namespace std;

int GenCommand::execute (Cmdline const & cl)
{
	GenTemplateList templates;

	KeySet ks;
	ks.append (Key ("/test", KEY_META, "type", "long", KEY_END));
	ks.append (Key ("/test", KEY_META, "type", "enum", KEY_META, "check/enum", "#3", KEY_META, "check/enum/#2", "two", KEY_END));

	auto tmpl = templates.getTemplate ("elektra");
	tmpl->setParameter ("outputName", "test_example");
	std::cout << "=== Header File ===" << std::endl;
	tmpl->render (std::cout, ".h", ks);
	std::cout << std::endl << std::endl;
	std::cout << "=== Code File ===" << std::endl;
	tmpl->render (std::cout, ".c", ks);

	return 0;
}
