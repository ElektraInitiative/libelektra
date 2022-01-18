/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_GEN_HIGHLEVEL_HPP
#define ELEKTRA_KDB_GEN_HIGHLEVEL_HPP

#include <gen/template.hpp>

class HighlevelGenTemplate : public GenTemplate
{
	struct Params
	{
		static const char * InitFunctionName;
		static const char * HelpFunctionName;
		static const char * SpecloadFunctionName;
		static const char * RunCommandsFunctionName;
		static const char * TagPrefix;
		static const char * EnumConversion;
		static const char * AdditionalHeaders;
		static const char * GenerateSetters;
		static const char * EmbeddedSpec;
		static const char * InstallPrefix;
		static const char * EmbedHelpFallback;
		static const char * UseCommands;
		static const char * InitWithPointers;
	};

public:
	HighlevelGenTemplate ()
	: GenTemplate ("highlevel", { ".c", ".h", ".spec.eqd", ".mount.sh", ".commands.h" },
		       { "enum.c", "union.c", "struct.c", "struct.alloc.fields.c", "enum.decl.h", "struct.decl.h", "union.decl.h",
			 "keys.fun.h", "keys.fun.struct.h", "keys.fun.structref.h", "keys.tags.h", "context.fun.h", "context.tags.h" },
		       { { Params::InitFunctionName, false },
			 { Params::HelpFunctionName, false },
			 { Params::SpecloadFunctionName, false },
			 { Params::RunCommandsFunctionName, false },
			 { Params::TagPrefix, false },
			 { Params::EnumConversion, false },
			 { Params::AdditionalHeaders, false },
			 { Params::GenerateSetters, false },
			 { Params::EmbeddedSpec, false },
			 { Params::InstallPrefix, false },
			 { Params::EmbedHelpFallback, false },
			 { Params::UseCommands, false },
			 { Params::InitWithPointers, false } })
	{
	}

protected:
	kainjow::mustache::data getTemplateData (const std::string & outputName, const std::string & part, const kdb::KeySet & ks,
						 const std::string & parentKey) const override;

	std::string escapeFunction (const std::string & str) const override;
	std::vector<std::string> getActualParts () const override;
};

#endif // ELEKTRA_KDB_GEN_HIGHLEVEL_HPP
