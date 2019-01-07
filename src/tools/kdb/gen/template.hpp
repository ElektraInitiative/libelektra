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
#include <iostream>
#include <memory>

class GenTemplate
{
protected:
	virtual kainjow::mustache::data getTemplateData () = 0;
	virtual std::string getTemplateName () = 0;

public:
	virtual std::string getName () = 0;

	explicit GenTemplate (std::ostream & output);

	void render ();

private:
	std::ostream & _output;
};

class GenTemplateList
{
public:
	explicit GenTemplateList (std::ostream & output);

	GenTemplate * getTemplate (const std::string & name);

private:
	template <class genClass>
	void addTemplate (std::ostream & output);
	std::unordered_map<std::string, std::unique_ptr<GenTemplate>> _templates;
};

#endif // ELEKTRA_TEMPLATE_HPP
