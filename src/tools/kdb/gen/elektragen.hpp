/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_ELEKTRAGEN_HPP
#define ELEKTRA_ELEKTRAGEN_HPP

#include "template.hpp"

class ElektraGenTemplate : public GenTemplate
{
public:
	ElektraGenTemplate (std::ostream & output) : GenTemplate (output){};

	std::string getName () override
	{
		return "elektra";
	}

protected:
	std::string getTemplateName () override
	{
		return "elektra.mustache";
	}

	kainjow::mustache::data getTemplateData () override;
};

#endif // ELEKTRA_ELEKTRAGEN_HPP
