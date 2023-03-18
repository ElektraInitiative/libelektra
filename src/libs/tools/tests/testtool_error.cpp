/**
 * @file
 *
 * @brief Tests for the errors and warnings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <errors/errorFactory.hpp>
#include <errors/errorTypes.hpp>
#include <errors/warningTypes.hpp>
#include <gtest/gtest.h>
#include <elektra/kdb/errors.h> // for code and description constants

TEST (Error, TestWarnings)
{
	/* Resource warning */
	std::cout << std::endl << "TEST WARNINGS:" << std::endl;
	kdb::tools::errors::ResourceWarning resourceWarning ("resourceWarningDescription", "resourceWarningReason", "resourceWarningModule",
							     "resourceWarningFile", "resourceWarningMountPoint",
							     "resourceWarningConfigFile", 0);
	ASSERT_EQ (resourceWarning.code (), ELEKTRA_WARNING_RESOURCE);
	ASSERT_EQ (resourceWarning.description (), "resourceWarningDescription");
	ASSERT_EQ (resourceWarning.reason (), "resourceWarningReason");
	ASSERT_EQ (resourceWarning.module (), "resourceWarningModule");
	ASSERT_EQ (resourceWarning.file (), "resourceWarningFile");
	ASSERT_EQ (resourceWarning.mountPoint (), "resourceWarningMountPoint");
	ASSERT_EQ (resourceWarning.configFile (), "resourceWarningConfigFile");
	ASSERT_EQ (resourceWarning.line (), 0);

	/* Out of memory warning */
	kdb::tools::errors::OutOfMemoryWarning outOfMemoryWarning ("outOfMemoryWarningDescription", "outOfMemoryWarningReason",
								   "outOfMemoryWarningModule", "outOfMemoryWarningFile",
								   "outOfMemoryWarningMountPoint", "outOfMemoryWarningConfigFile", 1);
	ASSERT_EQ (outOfMemoryWarning.code (), ELEKTRA_WARNING_OUT_OF_MEMORY);
	ASSERT_EQ (outOfMemoryWarning.description (), "outOfMemoryWarningDescription");
	ASSERT_EQ (outOfMemoryWarning.reason (), "outOfMemoryWarningReason");
	ASSERT_EQ (outOfMemoryWarning.module (), "outOfMemoryWarningModule");
	ASSERT_EQ (outOfMemoryWarning.file (), "outOfMemoryWarningFile");
	ASSERT_EQ (outOfMemoryWarning.mountPoint (), "outOfMemoryWarningMountPoint");
	ASSERT_EQ (outOfMemoryWarning.configFile (), "outOfMemoryWarningConfigFile");
	ASSERT_EQ (outOfMemoryWarning.line (), 1);

	/* Installation warning */
	kdb::tools::errors::InstallationWarning installationWarning ("installationWarningDescription", "installationWarningReason",
								     "installationWarningModule", "installationWarningFile",
								     "installationWarningMountPoint", "installationWarningConfigFile", -1);
	ASSERT_EQ (installationWarning.code (), ELEKTRA_WARNING_INSTALLATION);
	ASSERT_EQ (installationWarning.description (), "installationWarningDescription");
	ASSERT_EQ (installationWarning.reason (), "installationWarningReason");
	ASSERT_EQ (installationWarning.module (), "installationWarningModule");
	ASSERT_EQ (installationWarning.file (), "installationWarningFile");
	ASSERT_EQ (installationWarning.mountPoint (), "installationWarningMountPoint");
	ASSERT_EQ (installationWarning.configFile (), "installationWarningConfigFile");
	ASSERT_EQ (installationWarning.line (), -1);

	/* Internal warning */
	kdb::tools::errors::InternalWarning internalWarning ("internalWarningDescription", "internalWarningReason", "internalWarningModule",
							     "internalWarningFile", "internalWarningMountPoint",
							     "internalWarningConfigFile", 999);
	ASSERT_EQ (internalWarning.code (), ELEKTRA_WARNING_INTERNAL);
	ASSERT_EQ (internalWarning.description (), "internalWarningDescription");
	ASSERT_EQ (internalWarning.reason (), "internalWarningReason");
	ASSERT_EQ (internalWarning.module (), "internalWarningModule");
	ASSERT_EQ (internalWarning.file (), "internalWarningFile");
	ASSERT_EQ (internalWarning.mountPoint (), "internalWarningMountPoint");
	ASSERT_EQ (internalWarning.configFile (), "internalWarningConfigFile");
	ASSERT_EQ (internalWarning.line (), 999);

	/* Interface warning */
	kdb::tools::errors::InterfaceWarning interfaceWarning ("interfaceWarningDescription", "interfaceWarningReason",
							       "interfaceWarningModule", "interfaceWarningFile",
							       "interfaceWarningMountPoint", "interfaceWarningConfigFile", 2);
	ASSERT_EQ (interfaceWarning.code (), ELEKTRA_WARNING_INTERFACE);
	ASSERT_EQ (interfaceWarning.description (), "interfaceWarningDescription");
	ASSERT_EQ (interfaceWarning.reason (), "interfaceWarningReason");
	ASSERT_EQ (interfaceWarning.module (), "interfaceWarningModule");
	ASSERT_EQ (interfaceWarning.file (), "interfaceWarningFile");
	ASSERT_EQ (interfaceWarning.mountPoint (), "interfaceWarningMountPoint");
	ASSERT_EQ (interfaceWarning.configFile (), "interfaceWarningConfigFile");
	ASSERT_EQ (interfaceWarning.line (), 2);

	/* PluginMisbehavior warning */
	kdb::tools::errors::PluginMisbehaviorWarning pluginMisbehaviorWarning (
		"pluginMisbehaviorWarningDescription", "pluginMisbehaviorWarningReason", "pluginMisbehaviorWarningModule",
		"pluginMisbehaviorWarningFile", "pluginMisbehaviorWarningMountPoint", "pluginMisbehaviorWarningConfigFile", 3);
	ASSERT_EQ (pluginMisbehaviorWarning.code (), ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR);
	ASSERT_EQ (pluginMisbehaviorWarning.description (), "pluginMisbehaviorWarningDescription");
	ASSERT_EQ (pluginMisbehaviorWarning.reason (), "pluginMisbehaviorWarningReason");
	ASSERT_EQ (pluginMisbehaviorWarning.module (), "pluginMisbehaviorWarningModule");
	ASSERT_EQ (pluginMisbehaviorWarning.file (), "pluginMisbehaviorWarningFile");
	ASSERT_EQ (pluginMisbehaviorWarning.mountPoint (), "pluginMisbehaviorWarningMountPoint");
	ASSERT_EQ (pluginMisbehaviorWarning.configFile (), "pluginMisbehaviorWarningConfigFile");
	ASSERT_EQ (pluginMisbehaviorWarning.line (), 3);

	/* ConflictingStatie warning */
	kdb::tools::errors::ConflictingStateWarning conflictingStateWarning (
		"conflictingStateWarningDescription", "conflictingStateWarningReason", "conflictingStateWarningModule",
		"conflictingStateWarningFile", "conflictingStateWarningMountPoint", "conflictingStateWarningConfigFile", 4);
	ASSERT_EQ (conflictingStateWarning.code (), ELEKTRA_WARNING_CONFLICTING_STATE);
	ASSERT_EQ (conflictingStateWarning.description (), "conflictingStateWarningDescription");
	ASSERT_EQ (conflictingStateWarning.reason (), "conflictingStateWarningReason");
	ASSERT_EQ (conflictingStateWarning.module (), "conflictingStateWarningModule");
	ASSERT_EQ (conflictingStateWarning.file (), "conflictingStateWarningFile");
	ASSERT_EQ (conflictingStateWarning.mountPoint (), "conflictingStateWarningMountPoint");
	ASSERT_EQ (conflictingStateWarning.configFile (), "conflictingStateWarningConfigFile");
	ASSERT_EQ (conflictingStateWarning.line (), 4);

	/* ValidationSyntactic warning */
	kdb::tools::errors::ValidationSyntacticWarning validationSyntacticWarning (
		"validationSyntacticWarningDescription", "validationSyntacticWarningReason", "validationSyntacticWarningModule",
		"validationSyntacticWarningFile", "validationSyntacticMountPoint", "validationSyntacticWarningConfigFile", 5);
	ASSERT_EQ (validationSyntacticWarning.code (), ELEKTRA_WARNING_VALIDATION_SYNTACTIC);
	ASSERT_EQ (validationSyntacticWarning.description (), "validationSyntacticWarningDescription");
	ASSERT_EQ (validationSyntacticWarning.reason (), "validationSyntacticWarningReason");
	ASSERT_EQ (validationSyntacticWarning.module (), "validationSyntacticWarningModule");
	ASSERT_EQ (validationSyntacticWarning.file (), "validationSyntacticWarningFile");
	ASSERT_EQ (validationSyntacticWarning.mountPoint (), "validationSyntacticMountPoint");
	ASSERT_EQ (validationSyntacticWarning.configFile (), "validationSyntacticWarningConfigFile");
	ASSERT_EQ (validationSyntacticWarning.line (), 5);

	/* ValidationSemantic warning */
	kdb::tools::errors::ValidationSemanticWarning validationSemanticWarning (
		"validationSemanticWarningDescription", "validationSemanticWarningReason", "validationSemanticWarningModule",
		"validationSemanticWarningFile", "validationSemanticWarningMountPoint", "validationSemanticWarningConfigFile", 6);
	ASSERT_EQ (validationSemanticWarning.code (), ELEKTRA_WARNING_VALIDATION_SEMANTIC);
	ASSERT_EQ (validationSemanticWarning.description (), "validationSemanticWarningDescription");
	ASSERT_EQ (validationSemanticWarning.reason (), "validationSemanticWarningReason");
	ASSERT_EQ (validationSemanticWarning.module (), "validationSemanticWarningModule");
	ASSERT_EQ (validationSemanticWarning.file (), "validationSemanticWarningFile");
	ASSERT_EQ (validationSemanticWarning.mountPoint (), "validationSemanticWarningMountPoint");
	ASSERT_EQ (validationSemanticWarning.configFile (), "validationSemanticWarningConfigFile");
	ASSERT_EQ (validationSemanticWarning.line (), 6);
}

TEST (Error, TestErrors)
{
	/* Resource error */
	std::cout << std::endl << "TEST ERRORS:" << std::endl;
	kdb::tools::errors::ResourceError resourceError ("resourceErrorDescription", "resourceErrorReason", "resourceErrorModule",
							 "resourceErrorFile", "resourceErrorMountPoint", "resourceErrorConfigFile", 0);
	ASSERT_EQ (resourceError.code (), ELEKTRA_ERROR_RESOURCE);
	ASSERT_EQ (resourceError.description (), "resourceErrorDescription");
	ASSERT_EQ (resourceError.reason (), "resourceErrorReason");
	ASSERT_EQ (resourceError.module (), "resourceErrorModule");
	ASSERT_EQ (resourceError.file (), "resourceErrorFile");
	ASSERT_EQ (resourceError.mountPoint (), "resourceErrorMountPoint");
	ASSERT_EQ (resourceError.configFile (), "resourceErrorConfigFile");
	ASSERT_EQ (resourceError.line (), 0);

	/* Out of memory error */
	kdb::tools::errors::OutOfMemoryError outOfMemoryError ("outOfMemoryErrorDescription", "outOfMemoryErrorReason",
							       "outOfMemoryErrorModule", "outOfMemoryErrorFile",
							       "outOfMemoryErrorMountPoint", "outOfMemoryErrorConfigFile", 1);
	ASSERT_EQ (outOfMemoryError.code (), ELEKTRA_ERROR_OUT_OF_MEMORY);
	ASSERT_EQ (outOfMemoryError.description (), "outOfMemoryErrorDescription");
	ASSERT_EQ (outOfMemoryError.reason (), "outOfMemoryErrorReason");
	ASSERT_EQ (outOfMemoryError.module (), "outOfMemoryErrorModule");
	ASSERT_EQ (outOfMemoryError.file (), "outOfMemoryErrorFile");
	ASSERT_EQ (outOfMemoryError.mountPoint (), "outOfMemoryErrorMountPoint");
	ASSERT_EQ (outOfMemoryError.configFile (), "outOfMemoryErrorConfigFile");
	ASSERT_EQ (outOfMemoryError.line (), 1);

	/* Installation error */
	kdb::tools::errors::InstallationError installationError ("installationErrorDescription", "installationErrorReason",
								 "installationErrorModule", "installationErrorFile",
								 "installationErrorMountPoint", "installationErrorConfigFile", -1);
	ASSERT_EQ (installationError.code (), ELEKTRA_ERROR_INSTALLATION);
	ASSERT_EQ (installationError.description (), "installationErrorDescription");
	ASSERT_EQ (installationError.reason (), "installationErrorReason");
	ASSERT_EQ (installationError.module (), "installationErrorModule");
	ASSERT_EQ (installationError.file (), "installationErrorFile");
	ASSERT_EQ (installationError.mountPoint (), "installationErrorMountPoint");
	ASSERT_EQ (installationError.configFile (), "installationErrorConfigFile");
	ASSERT_EQ (installationError.line (), -1);

	/* Internal error */
	kdb::tools::errors::InternalError internalError ("internalErrorDescription", "internalErrorReason", "internalErrorModule",
							 "internalErrorFile", "internalErrorMountPoint", "internalErrorConfigFile", 999);

	ASSERT_EQ (internalError.code (), ELEKTRA_ERROR_INTERNAL);
	ASSERT_EQ (internalError.description (), "internalErrorDescription");
	ASSERT_EQ (internalError.reason (), "internalErrorReason");
	ASSERT_EQ (internalError.module (), "internalErrorModule");
	ASSERT_EQ (internalError.file (), "internalErrorFile");
	ASSERT_EQ (internalError.mountPoint (), "internalErrorMountPoint");
	ASSERT_EQ (internalError.configFile (), "internalErrorConfigFile");
	ASSERT_EQ (internalError.line (), 999);

	/* Interface error */
	kdb::tools::errors::InterfaceError interfaceError ("interfaceErrorDescription", "interfaceErrorReason", "interfaceErrorModule",
							   "interfaceErrorFile", "interfaceErrorMountPoint", "interfaceErrorConfigFile", 2);
	ASSERT_EQ (interfaceError.code (), ELEKTRA_ERROR_INTERFACE);
	ASSERT_EQ (interfaceError.description (), "interfaceErrorDescription");
	ASSERT_EQ (interfaceError.reason (), "interfaceErrorReason");
	ASSERT_EQ (interfaceError.module (), "interfaceErrorModule");
	ASSERT_EQ (interfaceError.file (), "interfaceErrorFile");
	ASSERT_EQ (interfaceError.mountPoint (), "interfaceErrorMountPoint");
	ASSERT_EQ (interfaceError.configFile (), "interfaceErrorConfigFile");
	ASSERT_EQ (interfaceError.line (), 2);

	/* PluginMisbehavior error */
	kdb::tools::errors::PluginMisbehaviorError pluginMisbehaviorError (
		"pluginMisbehaviorErrorDescription", "pluginMisbehaviorErrorReason", "pluginMisbehaviorErrorModule",
		"pluginMisbehaviorErrorFile", "pluginMisbehaviorErrorMountPoint", "pluginMisbehaviorErrorConfigFile", 3);
	ASSERT_EQ (pluginMisbehaviorError.code (), ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR);
	ASSERT_EQ (pluginMisbehaviorError.description (), "pluginMisbehaviorErrorDescription");
	ASSERT_EQ (pluginMisbehaviorError.reason (), "pluginMisbehaviorErrorReason");
	ASSERT_EQ (pluginMisbehaviorError.module (), "pluginMisbehaviorErrorModule");
	ASSERT_EQ (pluginMisbehaviorError.file (), "pluginMisbehaviorErrorFile");
	ASSERT_EQ (pluginMisbehaviorError.mountPoint (), "pluginMisbehaviorErrorMountPoint");
	ASSERT_EQ (pluginMisbehaviorError.configFile (), "pluginMisbehaviorErrorConfigFile");
	ASSERT_EQ (pluginMisbehaviorError.line (), 3);

	/* ConflictingStatie error */
	kdb::tools::errors::ConflictingStateError conflictingStateError (
		"conflictingStateErrorDescription", "conflictingStateErrorReason", "conflictingStateErrorModule",
		"conflictingStateErrorFile", "conflictingStateErrorMountPoint", "conflictingStateErrorConfigFile", 4);
	ASSERT_EQ (conflictingStateError.code (), ELEKTRA_ERROR_CONFLICTING_STATE);
	ASSERT_EQ (conflictingStateError.description (), "conflictingStateErrorDescription");
	ASSERT_EQ (conflictingStateError.reason (), "conflictingStateErrorReason");
	ASSERT_EQ (conflictingStateError.module (), "conflictingStateErrorModule");
	ASSERT_EQ (conflictingStateError.file (), "conflictingStateErrorFile");
	ASSERT_EQ (conflictingStateError.mountPoint (), "conflictingStateErrorMountPoint");
	ASSERT_EQ (conflictingStateError.configFile (), "conflictingStateErrorConfigFile");
	ASSERT_EQ (conflictingStateError.line (), 4);

	/* ValidationSyntactic error */
	kdb::tools::errors::ValidationSyntacticError validationSyntacticError (
		"validationSyntacticErrorDescription", "validationSyntacticErrorReason", "validationSyntacticErrorModule",
		"validationSyntacticErrorFile", "validationSyntacticErrorMountPoint", "validationSyntacticErrorConfigFile", 5);
	ASSERT_EQ (validationSyntacticError.code (), ELEKTRA_ERROR_VALIDATION_SYNTACTIC);
	ASSERT_EQ (validationSyntacticError.description (), "validationSyntacticErrorDescription");
	ASSERT_EQ (validationSyntacticError.reason (), "validationSyntacticErrorReason");
	ASSERT_EQ (validationSyntacticError.module (), "validationSyntacticErrorModule");
	ASSERT_EQ (validationSyntacticError.file (), "validationSyntacticErrorFile");
	ASSERT_EQ (validationSyntacticError.mountPoint (), "validationSyntacticErrorMountPoint");
	ASSERT_EQ (validationSyntacticError.configFile (), "validationSyntacticErrorConfigFile");
	ASSERT_EQ (validationSyntacticError.line (), 5);

	/* ValidationSemantic error */
	kdb::tools::errors::ValidationSemanticError validationSemanticError (
		"validationSemanticErrorDescription", "validationSemanticErrorReason", "validationSemanticErrorModule",
		"validationSemanticErrorFile", "validationSemanticErrorMountPoint", "validationSemanticErrorConfigFile", 6);
	ASSERT_EQ (validationSemanticError.code (), ELEKTRA_ERROR_VALIDATION_SEMANTIC);
	ASSERT_EQ (validationSemanticError.description (), "validationSemanticErrorDescription");
	ASSERT_EQ (validationSemanticError.reason (), "validationSemanticErrorReason");
	ASSERT_EQ (validationSemanticError.module (), "validationSemanticErrorModule");
	ASSERT_EQ (validationSemanticError.file (), "validationSemanticErrorFile");
	ASSERT_EQ (validationSemanticError.mountPoint (), "validationSemanticErrorMountPoint");
	ASSERT_EQ (validationSemanticError.configFile (), "validationSemanticErrorConfigFile");
	ASSERT_EQ (validationSemanticError.line (), 6);
}

TEST (Error, ErrWarn)
{
	std::cout << std::endl << "TEST ERRORS WITH WARNINGS" << std::endl;
	kdb::tools::errors::PluginMisbehaviorError pmE ("pmeDescription", "pmeReason", "pmeModule", "pmeFile", "pmeMountPoint",
							"pmeConfigFile", 100);
	kdb::tools::errors::ConflictingStateWarning csW ("cswDescription", "cswReason", "cswModule", "cswFile", "cswMountPoint",
							 "cswConfigFile", 101);
	kdb::tools::errors::InterfaceWarning ifW ("ifwDescription", "ifwReason", "ifwModule", "ifwFile", "ifwMountPoint", "ifwConfigFile",
						  102);

	pmE.addWarning (csW);
	pmE.addWarning (ifW);

	ASSERT_EQ (pmE.warningCount (), 2);

	int i = 0;
	for (kdb::tools::errors::Warning * w : pmE)
	{
		std::cout << std::endl << "WARNING IN ERROR: " << *w << std::endl;

		if (i++ == 0)
			ASSERT_EQ (*w, csW);
		else
			ASSERT_EQ (*w, ifW);
	}
}

TEST (Error, Equality)
{
	std::cout << std::endl << "TEST EQUALITY OF WARNINGS AND ERRORS" << std::endl;

	kdb::tools::errors::InstallationWarning installationWarning ("Description", "Reason", "Module", "File", "MountPoint", "ConfigFile",
								     42);
	kdb::tools::errors::InstallationWarning installationWarning2 ("Description", "Reason", "Module", "File", "MountPoint", "ConfigFile",
								      42);
	kdb::tools::errors::InterfaceWarning interfaceWarning ("Description", "Reason", "Module", "File", "MountPoint", "ConfigFile", 42);
	kdb::tools::errors::InstallationError installationError ("Description", "Reason", "Module", "File", "MountPoint", "ConfigFile", 42);

	ASSERT_EQ (installationWarning, installationWarning);
	ASSERT_EQ (installationWarning, installationWarning2);
	ASSERT_EQ (installationError, installationError);
	ASSERT_NE (installationWarning, interfaceWarning);
	ASSERT_NE (installationWarning, installationError);
	ASSERT_NE (installationError, installationWarning);

	kdb::tools::errors::InstallationError installationError1 ("Description", "Reason", "Module", "File", "MountPoint", "ConfigFile",
								  42);
	kdb::tools::errors::ResourceError resourceError ("Description", "Reason", "Module", "File", "MountPoint", "ConfigFile", 42);

	ASSERT_NE (installationError, resourceError);
	ASSERT_EQ (installationError, installationError1);

	installationError.addWarning (installationWarning);
	installationError.addWarning (installationWarning2);
	installationError.addWarning (interfaceWarning);
	ASSERT_NE (installationError, installationError1);

	installationError1.addWarning (interfaceWarning);
	installationError1.addWarning (installationWarning);
	ASSERT_NE (installationError, installationError1);

	installationError1.addWarning (installationWarning2);
	/* Should be equal because same ordering of warnings is not necessary for equality */
	ASSERT_EQ (installationError, installationError1);

	resourceError.addWarning (installationWarning);
	resourceError.addWarning (installationWarning2);
	resourceError.addWarning (interfaceWarning);
	ASSERT_NE (installationError, resourceError);

	/* currently the Warning gets copied when it is added to the Error */
	ASSERT_EQ (installationError[2].file (), installationError1[0].file ());
	installationError[2].file () = "changed file in warning";
	ASSERT_NE (installationError[2].file (), installationError1[0].file ());
}

TEST (Error, FactoryNull)
{
	kdb::Key errKey;
	kdb::tools::errors::Error * result = kdb::tools::errors::ErrorFactory::fromKey (errKey);
	ASSERT_EQ (result, nullptr);

	/* should only be necessary if previous assertion failed */
	delete result;
}
/* The names of the metakeys are based on /src/libs/elektra/errors.c */
TEST (Error, FactoryErr)
{
	kdb::Key errKey;
	errKey.setMeta ("error", "number description module file line mountpoint configfile reason");
	errKey.setMeta ("error/number", ELEKTRA_ERROR_INTERNAL);
	errKey.setMeta ("error/description", "errDesc");
	errKey.setMeta ("error/module", "errModule");
	errKey.setMeta ("error/file", "errFile");
	errKey.setMeta ("error/line", 123);
	errKey.setMeta ("error/mountpoint", "errMountPoint");
	errKey.setMeta ("error/configfile", "errConfigFile");
	errKey.setMeta ("error/reason", "errReason");


	kdb::tools::errors::Error * result = kdb::tools::errors::ErrorFactory::fromKey (errKey);

	ASSERT_TRUE (dynamic_cast<const kdb::tools::errors::InternalError *> (result));

	ASSERT_EQ (result->code (), ELEKTRA_ERROR_INTERNAL);
	ASSERT_EQ (result->description (), "errDesc");
	ASSERT_EQ (result->module (), "errModule");
	ASSERT_EQ (result->file (), "errFile");
	ASSERT_EQ (result->line (), 123);
	ASSERT_EQ (result->mountPoint (), "errMountPoint");
	ASSERT_EQ (result->configFile (), "errConfigFile");
	ASSERT_EQ (result->reason (), "errReason");

	delete result;
}

/* The names of the metakeys are based on /src/libs/elektra/errors.c */
TEST (Error, FactoryPureWarning)
{
	kdb::Key errKey;

	errKey.setMeta ("warnings", "#02");
	errKey.setMeta ("warnings/#01", "number description module file line mountpoint configfile reason");
	errKey.setMeta ("warnings/#01/number", ELEKTRA_WARNING_INTERFACE);
	errKey.setMeta ("warnings/#01/description", "warn1Desc");
	errKey.setMeta ("warnings/#01/module", "warn1Module");
	errKey.setMeta ("warnings/#01/file", "warn1File");
	errKey.setMeta ("warnings/#01/line", 4);
	errKey.setMeta ("warnings/#01/mountpoint", "warn1MountPoint");
	errKey.setMeta ("warnings/#01/configfile", "warn1ConfigFile");
	errKey.setMeta ("warnings/#01/reason", "warn1Reason");

	errKey.setMeta ("warnings/#02", "number description module file line mountpoint configfile reason");
	errKey.setMeta ("warnings/#02/number", ELEKTRA_WARNING_OUT_OF_MEMORY);
	errKey.setMeta ("warnings/#02/description", "warn2Desc");
	errKey.setMeta ("warnings/#02/module", "warn2Module");
	errKey.setMeta ("warnings/#02/file", "warn2File");
	errKey.setMeta ("warnings/#02/line", 5);
	errKey.setMeta ("warnings/#02/mountpoint", "warn2MountPoint");
	errKey.setMeta ("warnings/#02/configfile", "warn2ConfigFile");
	errKey.setMeta ("warnings/#02/reason", "warn2Reason");


	kdb::tools::errors::Error * result = kdb::tools::errors::ErrorFactory::fromKey (errKey);

	ASSERT_TRUE (dynamic_cast<const kdb::tools::errors::PureWarningError *> (result));
	ASSERT_EQ (result->warningCount (), 2);

	ASSERT_TRUE (dynamic_cast<const kdb::tools::errors::InterfaceWarning *> (&((*result)[0])));
	ASSERT_EQ ((*result)[0].code (), ELEKTRA_WARNING_INTERFACE);
	ASSERT_EQ ((*result)[0].description (), "warn1Desc");
	ASSERT_EQ ((*result)[0].module (), "warn1Module");
	ASSERT_EQ ((*result)[0].file (), "warn1File");
	ASSERT_EQ ((*result)[0].line (), 4);
	ASSERT_EQ ((*result)[0].mountPoint (), "warn1MountPoint");
	ASSERT_EQ ((*result)[0].configFile (), "warn1ConfigFile");
	ASSERT_EQ ((*result)[0].reason (), "warn1Reason");

	ASSERT_TRUE (dynamic_cast<const kdb::tools::errors::OutOfMemoryWarning *> (&((*result)[1])));
	ASSERT_EQ ((*result)[1].code (), ELEKTRA_WARNING_OUT_OF_MEMORY);
	ASSERT_EQ ((*result)[1].description (), "warn2Desc");
	ASSERT_EQ ((*result)[1].module (), "warn2Module");
	ASSERT_EQ ((*result)[1].file (), "warn2File");
	ASSERT_EQ ((*result)[1].line (), 5);
	ASSERT_EQ ((*result)[1].mountPoint (), "warn2MountPoint");
	ASSERT_EQ ((*result)[1].configFile (), "warn2ConfigFile");
	ASSERT_EQ ((*result)[1].reason (), "warn2Reason");

	EXPECT_THROW ((*result)[2].code (), std::out_of_range);

	/* internal warnings get deleted by destructor of the error object */
	delete result;
}

/* The names of the metakeys are based on /src/libs/elektra/errors.c */
TEST (Error, FactoryErrWarn)
{
	kdb::Key errKey;

	errKey.setMeta ("error", "number description module file line mountpoint configfile reason");
	errKey.setMeta ("error/number", ELEKTRA_ERROR_CONFLICTING_STATE);
	errKey.setMeta ("error/description", "errDesc");
	errKey.setMeta ("error/module", "errModule");
	errKey.setMeta ("error/file", "errFile");
	errKey.setMeta ("error/line", 321);
	errKey.setMeta ("error/mountpoint", "errMountPoint");
	errKey.setMeta ("error/configfile", "errConfigFile");
	errKey.setMeta ("error/reason", "errReason");

	errKey.setMeta ("warnings", "#01");
	errKey.setMeta ("warnings/#01", "number description module file line mountpoint configfile reason");
	errKey.setMeta ("warnings/#01/number", ELEKTRA_WARNING_RESOURCE);
	errKey.setMeta ("warnings/#01/description", "warnDesc");
	errKey.setMeta ("warnings/#01/module", "warnModule");
	errKey.setMeta ("warnings/#01/file", "warnFile");
	errKey.setMeta ("warnings/#01/line", 6);
	errKey.setMeta ("warnings/#01/mountpoint", "warnMountPoint");
	errKey.setMeta ("warnings/#01/configfile", "warnConfigFile");
	errKey.setMeta ("warnings/#01/reason", "warnReason");

	kdb::tools::errors::Error * result = kdb::tools::errors::ErrorFactory::fromKey (errKey);

	ASSERT_TRUE (dynamic_cast<const kdb::tools::errors::ConflictingStateError *> (result));
	ASSERT_EQ (result->code (), ELEKTRA_ERROR_CONFLICTING_STATE);
	ASSERT_EQ (result->description (), "errDesc");
	ASSERT_EQ (result->module (), "errModule");
	ASSERT_EQ (result->file (), "errFile");
	ASSERT_EQ (result->line (), 321);
	ASSERT_EQ (result->mountPoint (), "errMountPoint");
	ASSERT_EQ (result->configFile (), "errConfigFile");
	ASSERT_EQ (result->reason (), "errReason");

	ASSERT_EQ (result->warningCount (), 1);

	ASSERT_TRUE (dynamic_cast<const kdb::tools::errors::ResourceWarning *> (&((*result)[0])));
	ASSERT_EQ ((*result)[0].code (), ELEKTRA_WARNING_RESOURCE);
	ASSERT_EQ ((*result)[0].description (), "warnDesc");
	ASSERT_EQ ((*result)[0].module (), "warnModule");
	ASSERT_EQ ((*result)[0].file (), "warnFile");
	ASSERT_EQ ((*result)[0].line (), 6);
	ASSERT_EQ ((*result)[0].mountPoint (), "warnMountPoint");
	ASSERT_EQ ((*result)[0].configFile (), "warnConfigFile");
	ASSERT_EQ ((*result)[0].reason (), "warnReason");

	EXPECT_THROW ((*result)[1].code (), std::out_of_range);

	/* internal warnings get deleted by destructor of the error object */
	delete result;
}
