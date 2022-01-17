
#include <keyset.hpp>
#include <kdberrors.h> // for code and description constants
#include "errors/errorFactory.hpp"
#include "errors/warningFactory.hpp"
#include "errors/errorTypes.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

Error* ErrorFactory::create(const std::string & type, const std::string & reason, const std::string & module,
				  const std::string & file, const std::string & mountPoint, const std::string & configFile, kdb::long_t line)
{
	if (type == ELEKTRA_ERROR_RESOURCE || type == ELEKTRA_ERROR_RESOURCE_NAME)
		return new ResourceError (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_OUT_OF_MEMORY || type == ELEKTRA_ERROR_OUT_OF_MEMORY_NAME)
		return new OutOfMemoryError (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_INSTALLATION || type == ELEKTRA_ERROR_INSTALLATION_NAME)
		return new InstallationError (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_INTERNAL || type == ELEKTRA_ERROR_INTERNAL_NAME)
		return new InternalError (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_INTERFACE || type == ELEKTRA_ERROR_INTERFACE_NAME)
		return new InterfaceError (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR || type == ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR_NAME)
		return new PluginMisbehaviorError (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_CONFLICTING_STATE || type == ELEKTRA_ERROR_CONFLICTING_STATE_NAME)
		return new ConflictingStateError (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_VALIDATION_SYNTACTIC || type == ELEKTRA_ERROR_VALIDATION_SYNTACTIC_NAME)
		return new ValidationSyntacticError (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_ERROR_VALIDATION_SEMANTIC || type == ELEKTRA_ERROR_VALIDATION_SEMANTIC_NAME)
		return new ValidationSemanticError (reason, module, file, mountPoint, configFile, line);
	else
		return nullptr;
}


/**
 * @brief Create an error from a given key
 *
 * Reads meta-keys of given key to find error and warnings meta-keys. If no error exists a PureWarningError is created that contains the
 * key's warnings.
 *
 * @param key the key that has the error and warnings
 *
 * @return the error with warnings
 */
/* TODO: Test method */
Error * ErrorFactory::fromKey (kdb::Key key)
{
	Error *err = nullptr;
	if (key.isNull() || !key.isValid() || key.isBinary() || (!key.hasMeta("error") && !key.hasMeta("warnings")))
	{
		return nullptr;
	}

	if (!key.hasMeta ("error"))
	{
		err = new PureWarningError();
	}
	else
	{
		std::string errCode = key.getMeta<std::string>("error/number");
		std::string errDesc = key.getMeta<std::string>("error/description");

		if (!checkErrorCodeDesc (errCode, errDesc))
		{
			/* TODO: throw exception */
			return nullptr;
		}
		else
		{
			std::string module = key.getMeta<std::string>("error/module");
			std::string file = key.getMeta<std::string>("error/file");
			std::string reason = key.getMeta<std::string>("error/reason");
			std::string mountPoint = key.getMeta<std::string>("error/mountpoint");
			std::string configFile = key.getMeta<std::string>("error/configfile");
			kdb::long_t line = key.getMeta<kdb::long_t>("error/line");

			err = create (errCode, reason, module, file, mountPoint, configFile, line);
		}
	}

	/* process warnings *
	 * Code for extracting warnings was adapted from src/libs/highlevel/elektra_error.c:elektraErrorFromKey (Key * key)
	 * and /src/tools/kdb/coloredkdbio.h:printWarnings()
	// TODO: use C++ binding version of keyMeta */
	KeySet metaKeys (ckdb::ksDup (ckdb::keyMeta (key.getKey())));
	Key warningsParent ("meta:/warnings", KEY_END);
	KeySet warningKeys = metaKeys.cut (warningsParent);

	if (warningKeys.size() > 0)
	{
		for(auto it = warningKeys.begin() + 1; it != warningKeys.end (); ++it)
		{
			if (it->isDirectBelow(warningsParent))
			{
				auto name = it->getName ();

				std::string warnCode = warningKeys.get<std::string>(name + "/number");
				std::string warnDescription = warningKeys.get<std::string>(name + "/description");

				if (!WarningFactory::checkWarningCodeDesc (warnCode, warnDescription))
				{
					// TODO: throw exception
					return nullptr;
				}

				std::string warnReason = warningKeys.get<std::string>(name + "/reason");
				std::string warnModule = warningKeys.get<std::string>(name + "/module");
				std::string warnFile = warningKeys.get<std::string>(name + "/file");

				std::string warnMountPoint = warningKeys.get<std::string>(name + "/mountpoint");
				std::string warnConfigFile = warningKeys.get<std::string>(name + "/configfile");

				kdb::long_t warnLine = warningKeys.get<kdb::long_t>(name + "/line");

				Warning *w = WarningFactory::create (warnCode, warnReason, warnModule, warnFile,
								      warnMountPoint, warnConfigFile, warnLine);
				err->addWarning (*w);

				/* Warning gets copied by addWarning(Warning &) */
				delete w;
			}
		}
	}

	return err;
}

bool ErrorFactory::checkErrorCodeDesc(const std::string & code, const std::string & description)
{
	if (code == ELEKTRA_ERROR_RESOURCE)
		return (description == ELEKTRA_ERROR_RESOURCE_NAME);
	else if (code == ELEKTRA_ERROR_OUT_OF_MEMORY)
		return (description == ELEKTRA_ERROR_OUT_OF_MEMORY_NAME);
	else if (code == ELEKTRA_ERROR_INSTALLATION)
		return (description == ELEKTRA_ERROR_INSTALLATION_NAME);
	else if (code == ELEKTRA_ERROR_INTERNAL)
		return (description == ELEKTRA_ERROR_INTERNAL_NAME);
	else if (code == ELEKTRA_ERROR_INTERFACE)
		return (description == ELEKTRA_ERROR_INTERFACE_NAME);
	else if (code == ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR)
		return (description == ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR_NAME);
	else if (code == ELEKTRA_ERROR_CONFLICTING_STATE)
		return (description == ELEKTRA_ERROR_CONFLICTING_STATE_NAME);
	else if (code == ELEKTRA_ERROR_VALIDATION_SYNTACTIC)
		return (description == ELEKTRA_ERROR_VALIDATION_SYNTACTIC_NAME);
	else if (code == ELEKTRA_ERROR_VALIDATION_SEMANTIC)
		return (description == ELEKTRA_ERROR_VALIDATION_SEMANTIC_NAME);
	else
		return false;
}

} // namespace errors
} // namespace tools
} // namespace kdb