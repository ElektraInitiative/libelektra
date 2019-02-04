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
		static const char * InitFunctionName;
	};

public:
	ElektraGenTemplate () : GenTemplate ("elektra", { ".c", ".h" }, { { Params::InitFunctionName, false } })
	{
	}

protected:
	kainjow::mustache::data getTemplateData (const std::string & outputName, const kdb::KeySet & ks,
						 const std::string & parentKey) const override;
};

#endif // ELEKTRA_ELEKTRAGEN_HPP
