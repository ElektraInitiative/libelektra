
#include <elektra/core/errors.h> // for code and description constants

#include <errors/errorFactory.hpp>
#include <errors/errorTypes.hpp>
#include <errors/warningFactory.hpp>

#include <keyset.hpp>

namespace kdb
{
namespace tools
{
namespace errors
{

Error * ErrorFactory::create (const std::string & type, const std::string & description, const std::string & reason,
			      const std::string & module, const std::string & file, const std::string & mountPoint,
			      const std::string & configFile, kdb::long_t line)
{
	if (type == ELEKTRA_ERROR_RESOURCE)
		return new ResourceError (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_OUT_OF_MEMORY)
		return new OutOfMemoryError (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_INSTALLATION)
		return new InstallationError (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_INTERNAL)
		return new InternalError (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_INTERFACE)
		return new InterfaceError (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR)
		return new PluginMisbehaviorError (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_CONFLICTING_STATE)
		return new ConflictingStateError (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_VALIDATION_SYNTACTIC)
		return new ValidationSyntacticError (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_VALIDATION_SEMANTIC)
		return new ValidationSemanticError (description, reason, module, file, mountPoint, configFile, line);
	else
		return nullptr;
}


Error * ErrorFactory::fromKey (kdb::Key key)
{
	Error * err = nullptr;
	if (key.isNull () || !key.isValid () || key.isBinary () || (!key.hasMeta ("error") && !key.hasMeta ("warnings")))
	{
		return nullptr;
	}

	if (!key.hasMeta ("error"))
	{
		err = new PureWarningError ();
	}
	else
	{
		std::string errCode = key.getMeta<std::string> ("error/number");
		std::string errDesc = key.getMeta<std::string> ("error/description");

		std::string module = key.getMeta<std::string> ("error/module");
		std::string file = key.getMeta<std::string> ("error/file");
		std::string reason = key.getMeta<std::string> ("error/reason");
		std::string mountPoint = key.getMeta<std::string> ("error/mountpoint");
		std::string configFile = key.getMeta<std::string> ("error/configfile");
		kdb::long_t line = key.getMeta<kdb::long_t> ("error/line");

		err = create (errCode, errDesc, reason, module, file, mountPoint, configFile, line);
	}

	/* process warnings *
	 * Code for extracting warnings was adapted from src/libs/highlevel/elektra_error.c:elektraErrorFromKey (Key * key)
	 * and /src/tools/kdb/coloredkdbio.h:printWarnings()
	// TODO: use C++ binding version of keyMeta */
	KeySet metaKeys (ckdb::ksDup (ckdb::keyMeta (key.getKey ())));
	Key warningsParent ("meta:/warnings", KEY_END);
	KeySet warningKeys = metaKeys.cut (warningsParent);

	if (warningKeys.size () > 0)
	{
		for (auto it = warningKeys.begin () + 1; it != warningKeys.end (); ++it)
		{
			if (it->isDirectBelow (warningsParent))
			{
				auto name = it->getName ();

				std::string warnCode = warningKeys.get<std::string> (name + "/number");
				std::string warnDescription = warningKeys.get<std::string> (name + "/description");

				std::string warnReason = warningKeys.get<std::string> (name + "/reason");
				std::string warnModule = warningKeys.get<std::string> (name + "/module");
				std::string warnFile = warningKeys.get<std::string> (name + "/file");

				std::string warnMountPoint = warningKeys.get<std::string> (name + "/mountpoint");
				std::string warnConfigFile = warningKeys.get<std::string> (name + "/configfile");

				kdb::long_t warnLine = warningKeys.get<kdb::long_t> (name + "/line");

				Warning * w = WarningFactory::create (warnCode, warnDescription, warnReason, warnModule, warnFile,
								      warnMountPoint, warnConfigFile, warnLine);
				err->addWarning (*w);

				/* Warning gets copied by addWarning(Warning &) */
				delete w;
			}
		}
	}

	return err;
}

} // namespace errors
} // namespace tools
} // namespace kdb
