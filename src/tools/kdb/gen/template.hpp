#include <utility>

#include <utility>

#include <utility>

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_TEMPLATE_HPP
#define ELEKTRA_TEMPLATE_HPP

#include "mustache.hpp"

#include <algorithm>
#include <iostream>
#include <memory>
#include <unordered_set>

#include <kdb.hpp>

class GenTemplate
{
protected:
	GenTemplate (std::string name, std::string templateBaseName, std::vector<std::string> parts,
		     const std::unordered_set<std::string> & parameters)
	: _name (std::move (name)), _templateBaseName (std::move (templateBaseName)), _parts (std::move (parts)), _parameters ()
	{
		std::transform (parameters.begin (), parameters.end (), std::inserter (_parameters, _parameters.begin ()),
				[](std::string p) { return std::make_pair (p, ""); });
	}

	virtual kainjow::mustache::data getTemplateData (const kdb::KeySet & ks) = 0;

	std::string getParameter (const std::string & name);

public:
	std::string getName ();
	void setParameter (const std::string & name, const std::string & value);

	void render (std::ostream & output, const std::string & part, const kdb::KeySet & ks);

private:
	std::string _name;
	std::string _templateBaseName;
	std::vector<std::string> _parts;
	std::unordered_map<std::string, std::string> _parameters;
};

class GenTemplateList
{
public:
	GenTemplateList ();

	GenTemplate * getTemplate (const std::string & name);

private:
	template <class genClass>
	void addTemplate ();
	std::unordered_map<std::string, std::unique_ptr<GenTemplate>> _templates;
};

#endif // ELEKTRA_TEMPLATE_HPP
