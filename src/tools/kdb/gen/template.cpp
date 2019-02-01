/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "template.hpp"

#include <algorithm>
#include <fstream>

#include "gen/templates.hpp"

#include "elektragen.hpp"

void GenTemplate::render (std::ostream & output, const std::string & part, const kdb::KeySet & ks)
{
	using namespace kainjow::mustache;

	if (std::find (_parts.begin (), _parts.end (), part) == _parts.end ())
	{
		return;
	}

	auto name = _templateBaseName + part;
	std::replace_if (name.begin (), name.end (), std::not1 (std::ptr_fun (isalnum)), '_');

	auto tmpl = mustache (kdbgenTemplates.at (name));
	tmpl.render (getTemplateData (ks), [&](const std::string & str) { output << str; });
}

void GenTemplate::setParameter (const std::string & name, const std::string & value)
{
	auto param = _parameters.find (name);
	if (param != _parameters.end ())
	{
		param->second = value;
	}
}

std::string GenTemplate::getName ()
{
	return _name;
}

std::string GenTemplate::getParameter (const std::string & name)
{
	auto param = _parameters.find (name);
	return param != _parameters.end () ? param->second : "";
}

template <class genClass>
void GenTemplateList::addTemplate ()
{
	std::unique_ptr<GenTemplate> tmpl (new genClass ());
	_templates[tmpl->getName ()] = std::move (tmpl);
}

GenTemplate * GenTemplateList::getTemplate (const std::string & name)
{
	return _templates[name].get ();
}

GenTemplateList::GenTemplateList () : _templates ()
{
	addTemplate<ElektraGenTemplate> ();
}
