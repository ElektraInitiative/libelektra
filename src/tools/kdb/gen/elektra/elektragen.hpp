/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_ELEKTRAGEN_HPP
#define ELEKTRA_ELEKTRAGEN_HPP

#include "../template.hpp"

class ElektraGenTemplate : public GenTemplate
{
	struct Params
	{
		static const char * InitFunctionName;
		static const char * HelpFunctionName;
		static const char * SpecloadFunctionName;
		static const char * EnumConversion;
		static const char * AdditionalHeaders;
	};

public:
	ElektraGenTemplate ()
	: GenTemplate ("elektra", { ".c", ".h" },
		       { "enum.c", "struct.c", "struct.alloc.fields.c", "enum.decl.h", "struct.decl.h", "keys.fun.h", "keys.fun.struct.h",
			 "keys.fun.structref.h", "keys.tags.h", "context.fun.h", "context.tags.h" },
		       { { Params::InitFunctionName, false },
			 { Params::HelpFunctionName, false },
			 { Params::SpecloadFunctionName, false },
			 { Params::EnumConversion, false },
			 { Params::AdditionalHeaders, false } })
	{
	}

protected:
	kainjow::mustache::data getTemplateData (const std::string & outputName, const kdb::KeySet & ks,
						 const std::string & parentKey) const override;
};

#endif // ELEKTRA_ELEKTRAGEN_HPP
