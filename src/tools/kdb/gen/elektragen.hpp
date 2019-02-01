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
	struct Params
	{
		static const char * OutputName;
	};

public:
	ElektraGenTemplate () : GenTemplate ("elektra", { ".c", ".h" }, { { Params::OutputName, true } })
	{
	}

protected:
	kainjow::mustache::data getTemplateData (const kdb::KeySet & ks) const override;
};

#endif // ELEKTRA_ELEKTRAGEN_HPP
