/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "template.hpp"
#include "elektragen.hpp"
#include <fstream>

#include "gen/templates.hpp"
#include "htmlgen.hpp"

#include <algorithm>

GenTemplate::GenTemplate (std::ostream & output) : _output (output)
{
}

void GenTemplate::render ()
{
	using namespace kainjow::mustache;

	auto name = getTemplateName ();
	std::replace_if (name.begin (), name.end (), std::not1 (std::ptr_fun (isalnum)), '_');

	auto tmpl = mustache (kdbgenTemplates.at (name));
	tmpl.render (getTemplateData (), [&](const std::string & str) { _output << str; });
}

template <class genClass>
void GenTemplateList::addTemplate (std::ostream & output)
{
	std::unique_ptr<GenTemplate> tmpl (new genClass (output));
	_templates[tmpl->getName ()] = std::move (tmpl);
}

GenTemplate * GenTemplateList::getTemplate (const std::string & name)
{
	return _templates[name].get ();
}

GenTemplateList::GenTemplateList (std::ostream & output) : _templates ()
{
	addTemplate<ElektraGenTemplate> (output);
	addTemplate<HtmlGenTemplate> (output);
}
