/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_HTMLGEN_HPP
#define ELEKTRA_HTMLGEN_HPP

#include "template.hpp"

class HtmlGenTemplate : public GenTemplate
{
public:
	HtmlGenTemplate (std::ostream & output) : GenTemplate (output){};

	std::string getName () override
	{
		return "html";
	}

protected:
	std::string getTemplateName () override
	{
		return "template.html.mustache";
	}

	kainjow::mustache::data getTemplateData () override;
};

#endif // ELEKTRA_HTMLGEN_HPP
