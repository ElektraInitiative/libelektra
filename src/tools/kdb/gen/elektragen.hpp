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
		static const char * HelpFunctionName;
		static const char * SpecloadFunctionName;
		static const char * OptimizeEnumFromString;
		static const char * AdditionalHeaders;
	};

public:
	ElektraGenTemplate ()
	: GenTemplate ("elektra", { ".c", ".h" },
		       { { Params::InitFunctionName, false },
			 { Params::HelpFunctionName, false },
			 { Params::SpecloadFunctionName, false },
			 { Params::OptimizeEnumFromString, false },
			 { Params::AdditionalHeaders, false } })
	{
	}

protected:
	kainjow::mustache::data getTemplateData (const std::string & outputName, const kdb::KeySet & ks,
						 const std::string & parentKey) const override;
};

#endif // ELEKTRA_ELEKTRAGEN_HPP
