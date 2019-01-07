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
	GenTemplateList templates (std::cout);

	auto tmpl = templates.getTemplate ("html");
	tmpl->render ();

	return 0;
}
