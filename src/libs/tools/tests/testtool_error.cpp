/**
 * @file
 *
 * @brief Tests for the errors and warnings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <errors/errorTypes.hpp>
#include <errors/warningTypes.hpp>
#include <gtest/gtest.h>
#include <kdberrors.h> // for code and description constants

TEST (Error, TestWarnings)
{
	/* Resource warning */
	std::cout << std::endl << "TEST WARNINGS:" << std::endl;
	kdb::tools::errors::ResourceWarning resourceWarning ("resourceWarningReason", "resourceWarningModule", "resourceWarningFile",
							     "resourceWarningMountPoint", "resourceWarningConfigFile", 0);
	ASSERT_EQ (resourceWarning.code (), ELEKTRA_WARNING_RESOURCE);
	ASSERT_EQ (resourceWarning.description (), ELEKTRA_WARNING_RESOURCE_NAME);
	ASSERT_EQ (resourceWarning.reason (), "resourceWarningReason");
	ASSERT_EQ (resourceWarning.module (), "resourceWarningModule");
	ASSERT_EQ (resourceWarning.file (), "resourceWarningFile");
	ASSERT_EQ (resourceWarning.mountPoint (), "resourceWarningMountPoint");
	ASSERT_EQ (resourceWarning.configFile (), "resourceWarningConfigFile");
	ASSERT_EQ (resourceWarning.line (), 0);

	/* Out of memory warning */
	kdb::tools::errors::OutOfMemoryWarning outOfMemoryWarning ("outOfMemoryWarningReason", "outOfMemoryWarningModule",
								   "outOfMemoryWarningFile", "outOfMemoryWarningMountPoint",
								   "outOfMemoryWarningConfigFile", 1);
	ASSERT_EQ (outOfMemoryWarning.code (), ELEKTRA_WARNING_OUT_OF_MEMORY);
	ASSERT_EQ (outOfMemoryWarning.description (), ELEKTRA_WARNING_OUT_OF_MEMORY_NAME);
	ASSERT_EQ (outOfMemoryWarning.reason (), "outOfMemoryWarningReason");
	ASSERT_EQ (outOfMemoryWarning.module (), "outOfMemoryWarningModule");
	ASSERT_EQ (outOfMemoryWarning.file (), "outOfMemoryWarningFile");
	ASSERT_EQ (outOfMemoryWarning.mountPoint (), "outOfMemoryWarningMountPoint");
	ASSERT_EQ (outOfMemoryWarning.configFile (), "outOfMemoryWarningConfigFile");
	ASSERT_EQ (outOfMemoryWarning.line (), 1);

	/* Installation warning */
	kdb::tools::errors::InstallationWarning installationWarning ("installationWarningReason", "installationWarningModule",
								     "installationWarningFile", "installationWarningMountPoint",
								     "installationWarningConfigFile", -1);
	ASSERT_EQ (installationWarning.code (), ELEKTRA_WARNING_INSTALLATION);
	ASSERT_EQ (installationWarning.description (), ELEKTRA_WARNING_INSTALLATION_NAME);
	ASSERT_EQ (installationWarning.reason (), "installationWarningReason");
	ASSERT_EQ (installationWarning.module (), "installationWarningModule");
	ASSERT_EQ (installationWarning.file (), "installationWarningFile");
	ASSERT_EQ (installationWarning.mountPoint (), "installationWarningMountPoint");
	ASSERT_EQ (installationWarning.configFile (), "installationWarningConfigFile");
	ASSERT_EQ (installationWarning.line (), -1);

	/* Internal warning */
	kdb::tools::errors::InternalWarning internalWarning ("internalWarningReason", "internalWarningModule", "internalWarningFile",
							     "internalWarningMountPoint", "internalWarningConfigFile", 999);
	ASSERT_EQ (internalWarning.code (), ELEKTRA_WARNING_INTERNAL);
	ASSERT_EQ (internalWarning.description (), ELEKTRA_WARNING_INTERNAL_NAME);
	ASSERT_EQ (internalWarning.reason (), "internalWarningReason");
	ASSERT_EQ (internalWarning.module (), "internalWarningModule");
	ASSERT_EQ (internalWarning.file (), "internalWarningFile");
	ASSERT_EQ (internalWarning.mountPoint (), "internalWarningMountPoint");
	ASSERT_EQ (internalWarning.configFile (), "internalWarningConfigFile");
	ASSERT_EQ (internalWarning.line (), 999);

	/* Interface warning */
	kdb::tools::errors::InterfaceWarning interfaceWarning ("interfaceWarningReason", "interfaceWarningModule", "interfaceWarningFile",
							       "interfaceWarningMountPoint", "interfaceWarningConfigFile", 2);
	ASSERT_EQ (interfaceWarning.code (), ELEKTRA_WARNING_INTERFACE);
	ASSERT_EQ (interfaceWarning.description (), ELEKTRA_WARNING_INTERFACE_NAME);
	ASSERT_EQ (interfaceWarning.reason (), "interfaceWarningReason");
	ASSERT_EQ (interfaceWarning.module (), "interfaceWarningModule");
	ASSERT_EQ (interfaceWarning.file (), "interfaceWarningFile");
	ASSERT_EQ (interfaceWarning.mountPoint (), "interfaceWarningMountPoint");
	ASSERT_EQ (interfaceWarning.configFile (), "interfaceWarningConfigFile");
	ASSERT_EQ (interfaceWarning.line (), 2);

	/* PluginMisbehavior warning */
	kdb::tools::errors::PluginMisbehaviorWarning pluginMisbehaviorWarning (
		"pluginMisbehaviorWarningReason", "pluginMisbehaviorWarningModule", "pluginMisbehaviorWarningFile",
		"pluginMisbehaviorWarningMountPoint", "pluginMisbehaviorWarningConfigFile", 3);
	ASSERT_EQ (pluginMisbehaviorWarning.code (), ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR);
	ASSERT_EQ (pluginMisbehaviorWarning.description (), ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR_NAME);
	ASSERT_EQ (pluginMisbehaviorWarning.reason (), "pluginMisbehaviorWarningReason");
	ASSERT_EQ (pluginMisbehaviorWarning.module (), "pluginMisbehaviorWarningModule");
	ASSERT_EQ (pluginMisbehaviorWarning.file (), "pluginMisbehaviorWarningFile");
	ASSERT_EQ (pluginMisbehaviorWarning.mountPoint (), "pluginMisbehaviorWarningMountPoint");
	ASSERT_EQ (pluginMisbehaviorWarning.configFile (), "pluginMisbehaviorWarningConfigFile");
	ASSERT_EQ (pluginMisbehaviorWarning.line (), 3);

	/* ConflictingStatie warning */
	kdb::tools::errors::ConflictingStateWarning conflictingStateWarning (
		"conflictingStateWarningReason", "conflictingStateWarningModule", "conflictingStateWarningFile",
		"conflictingStateWarningMountPoint", "conflictingStateWarningConfigFile", 4);
	ASSERT_EQ (conflictingStateWarning.code (), ELEKTRA_WARNING_CONFLICTING_STATE);
	ASSERT_EQ (conflictingStateWarning.description (), ELEKTRA_WARNING_CONFLICTING_STATE_NAME);
	ASSERT_EQ (conflictingStateWarning.reason (), "conflictingStateWarningReason");
	ASSERT_EQ (conflictingStateWarning.module (), "conflictingStateWarningModule");
	ASSERT_EQ (conflictingStateWarning.file (), "conflictingStateWarningFile");
	ASSERT_EQ (conflictingStateWarning.mountPoint (), "conflictingStateWarningMountPoint");
	ASSERT_EQ (conflictingStateWarning.configFile (), "conflictingStateWarningConfigFile");
	ASSERT_EQ (conflictingStateWarning.line (), 4);

	/* ValidationSyntactic warning */
	kdb::tools::errors::ValidationSyntacticWarning validationSyntacticWarning (
		"validationSyntacticWarningReason", "validationSyntacticWarningModule", "validationSyntacticWarningFile",
		"validationSyntacticMountPoint", "validationSyntacticWarningConfigFile", 5);
	ASSERT_EQ (validationSyntacticWarning.code (), ELEKTRA_WARNING_VALIDATION_SYNTACTIC);
	ASSERT_EQ (validationSyntacticWarning.description (), ELEKTRA_WARNING_VALIDATION_SYNTACTIC_NAME);
	ASSERT_EQ (validationSyntacticWarning.reason (), "validationSyntacticWarningReason");
	ASSERT_EQ (validationSyntacticWarning.module (), "validationSyntacticWarningModule");
	ASSERT_EQ (validationSyntacticWarning.file (), "validationSyntacticWarningFile");
	ASSERT_EQ (validationSyntacticWarning.mountPoint (), "validationSyntacticMountPoint");
	ASSERT_EQ (validationSyntacticWarning.configFile (), "validationSyntacticWarningConfigFile");
	ASSERT_EQ (validationSyntacticWarning.line (), 5);

	/* ValidationSemantic warning */
	kdb::tools::errors::ValidationSemanticWarning validationSemanticWarning (
		"validationSemanticWarningReason", "validationSemanticWarningModule", "validationSemanticWarningFile",
		"validationSemanticWarningMountPoint", "validationSemanticWarningConfigFile", 6);
	ASSERT_EQ (validationSemanticWarning.code (), ELEKTRA_WARNING_VALIDATION_SEMANTIC);
	ASSERT_EQ (validationSemanticWarning.description (), ELEKTRA_WARNING_VALIDATION_SEMANTIC_NAME);
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
	kdb::tools::errors::ResourceError resourceError ("resourceErrorReason", "resourceErrorModule", "resourceErrorFile",
							 "resourceErrorMountPoint", "resourceErrorConfigFile", 0);
	ASSERT_EQ (resourceError.code (), ELEKTRA_ERROR_RESOURCE);
	ASSERT_EQ (resourceError.description (), ELEKTRA_ERROR_RESOURCE_NAME);
	ASSERT_EQ (resourceError.reason (), "resourceErrorReason");
	ASSERT_EQ (resourceError.module (), "resourceErrorModule");
	ASSERT_EQ (resourceError.file (), "resourceErrorFile");
	ASSERT_EQ (resourceError.mountPoint (), "resourceErrorMountPoint");
	ASSERT_EQ (resourceError.configFile (), "resourceErrorConfigFile");
	ASSERT_EQ (resourceError.line (), 0);

	/* Out of memory error */
	kdb::tools::errors::OutOfMemoryError outOfMemoryError ("outOfMemoryErrorReason", "outOfMemoryErrorModule", "outOfMemoryErrorFile",
							       "outOfMemoryErrorMountPoint", "outOfMemoryErrorConfigFile", 1);
	ASSERT_EQ (outOfMemoryError.code (), ELEKTRA_ERROR_OUT_OF_MEMORY);
	ASSERT_EQ (outOfMemoryError.description (), ELEKTRA_ERROR_OUT_OF_MEMORY_NAME);
	ASSERT_EQ (outOfMemoryError.reason (), "outOfMemoryErrorReason");
	ASSERT_EQ (outOfMemoryError.module (), "outOfMemoryErrorModule");
	ASSERT_EQ (outOfMemoryError.file (), "outOfMemoryErrorFile");
	ASSERT_EQ (outOfMemoryError.mountPoint (), "outOfMemoryErrorMountPoint");
	ASSERT_EQ (outOfMemoryError.configFile (), "outOfMemoryErrorConfigFile");
	ASSERT_EQ (outOfMemoryError.line (), 1);

	/* Installation error */
	kdb::tools::errors::InstallationError installationError ("installationErrorReason", "installationErrorModule",
								 "installationErrorFile", "installationErrorMountPoint",
								 "installationErrorConfigFile", -1);
	ASSERT_EQ (installationError.code (), ELEKTRA_ERROR_INSTALLATION);
	ASSERT_EQ (installationError.description (), ELEKTRA_ERROR_INSTALLATION_NAME);
	ASSERT_EQ (installationError.reason (), "installationErrorReason");
	ASSERT_EQ (installationError.module (), "installationErrorModule");
	ASSERT_EQ (installationError.file (), "installationErrorFile");
	ASSERT_EQ (installationError.mountPoint (), "installationErrorMountPoint");
	ASSERT_EQ (installationError.configFile (), "installationErrorConfigFile");
	ASSERT_EQ (installationError.line (), -1);

	/* Internal error */
	kdb::tools::errors::InternalError internalError ("internalErrorReason", "internalErrorModule", "internalErrorFile",
							 "internalErrorMountPoint", "internalErrorConfigFile", 999);

	ASSERT_EQ (internalError.code (), ELEKTRA_ERROR_INTERNAL);
	ASSERT_EQ (internalError.description (), ELEKTRA_ERROR_INTERNAL_NAME);
	ASSERT_EQ (internalError.reason (), "internalErrorReason");
	ASSERT_EQ (internalError.module (), "internalErrorModule");
	ASSERT_EQ (internalError.file (), "internalErrorFile");
	ASSERT_EQ (internalError.mountPoint (), "internalErrorMountPoint");
	ASSERT_EQ (internalError.configFile (), "internalErrorConfigFile");
	ASSERT_EQ (internalError.line (), 999);

	/* Interface error */
	kdb::tools::errors::InterfaceError interfaceError ("interfaceErrorReason", "interfaceErrorModule", "interfaceErrorFile",
							   "interfaceErrorMountPoint", "interfaceErrorConfigFile", 2);
	ASSERT_EQ (interfaceError.code (), ELEKTRA_ERROR_INTERFACE);
	ASSERT_EQ (interfaceError.description (), ELEKTRA_ERROR_INTERFACE_NAME);
	ASSERT_EQ (interfaceError.reason (), "interfaceErrorReason");
	ASSERT_EQ (interfaceError.module (), "interfaceErrorModule");
	ASSERT_EQ (interfaceError.file (), "interfaceErrorFile");
	ASSERT_EQ (interfaceError.mountPoint (), "interfaceErrorMountPoint");
	ASSERT_EQ (interfaceError.configFile (), "interfaceErrorConfigFile");
	ASSERT_EQ (interfaceError.line (), 2);

	/* PluginMisbehavior error */
	kdb::tools::errors::PluginMisbehaviorError pluginMisbehaviorError ("pluginMisbehaviorErrorReason", "pluginMisbehaviorErrorModule",
									   "pluginMisbehaviorErrorFile", "pluginMisbehaviorErrorMountPoint",
									   "pluginMisbehaviorErrorConfigFile", 3);
	ASSERT_EQ (pluginMisbehaviorError.code (), ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR);
	ASSERT_EQ (pluginMisbehaviorError.description (), ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR_NAME);
	ASSERT_EQ (pluginMisbehaviorError.reason (), "pluginMisbehaviorErrorReason");
	ASSERT_EQ (pluginMisbehaviorError.module (), "pluginMisbehaviorErrorModule");
	ASSERT_EQ (pluginMisbehaviorError.file (), "pluginMisbehaviorErrorFile");
	ASSERT_EQ (pluginMisbehaviorError.mountPoint (), "pluginMisbehaviorErrorMountPoint");
	ASSERT_EQ (pluginMisbehaviorError.configFile (), "pluginMisbehaviorErrorConfigFile");
	ASSERT_EQ (pluginMisbehaviorError.line (), 3);

	/* ConflictingStatie error */
	kdb::tools::errors::ConflictingStateError conflictingStateError ("conflictingStateErrorReason", "conflictingStateErrorModule",
									 "conflictingStateErrorFile", "conflictingStateErrorMountPoint",
									 "conflictingStateErrorConfigFile", 4);
	ASSERT_EQ (conflictingStateError.code (), ELEKTRA_ERROR_CONFLICTING_STATE);
	ASSERT_EQ (conflictingStateError.description (), ELEKTRA_ERROR_CONFLICTING_STATE_NAME);
	ASSERT_EQ (conflictingStateError.reason (), "conflictingStateErrorReason");
	ASSERT_EQ (conflictingStateError.module (), "conflictingStateErrorModule");
	ASSERT_EQ (conflictingStateError.file (), "conflictingStateErrorFile");
	ASSERT_EQ (conflictingStateError.mountPoint (), "conflictingStateErrorMountPoint");
	ASSERT_EQ (conflictingStateError.configFile (), "conflictingStateErrorConfigFile");
	ASSERT_EQ (conflictingStateError.line (), 4);

	/* ValidationSyntactic error */
	kdb::tools::errors::ValidationSyntacticError validationSyntacticError (
		"validationSyntacticErrorReason", "validationSyntacticErrorModule", "validationSyntacticErrorFile",
		"validationSyntacticErrorMountPoint", "validationSyntacticErrorConfigFile", 5);
	ASSERT_EQ (validationSyntacticError.code (), ELEKTRA_ERROR_VALIDATION_SYNTACTIC);
	ASSERT_EQ (validationSyntacticError.description (), ELEKTRA_ERROR_VALIDATION_SYNTACTIC_NAME);
	ASSERT_EQ (validationSyntacticError.reason (), "validationSyntacticErrorReason");
	ASSERT_EQ (validationSyntacticError.module (), "validationSyntacticErrorModule");
	ASSERT_EQ (validationSyntacticError.file (), "validationSyntacticErrorFile");
	ASSERT_EQ (validationSyntacticError.mountPoint (), "validationSyntacticErrorMountPoint");
	ASSERT_EQ (validationSyntacticError.configFile (), "validationSyntacticErrorConfigFile");
	ASSERT_EQ (validationSyntacticError.line (), 5);

	/* ValidationSemantic error */
	kdb::tools::errors::ValidationSemanticError validationSemanticError (
		"validationSemanticErrorReason", "validationSemanticErrorModule", "validationSemanticErrorFile",
		"validationSemanticErrorMountPoint", "validationSemanticErrorConfigFile", 6);
	ASSERT_EQ (validationSemanticError.code (), ELEKTRA_ERROR_VALIDATION_SEMANTIC);
	ASSERT_EQ (validationSemanticError.description (), ELEKTRA_ERROR_VALIDATION_SEMANTIC_NAME);
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
	kdb::tools::errors::PluginMisbehaviorError pmE ("pmeReason", "pmeModule", "pmeFile", "pmeMountPoint", "pmeConfigFile", 100);
	kdb::tools::errors::ConflictingStateWarning csW ("cswReason", "cswModule", "cswFile", "cswMountPoint", "cswConfigFile", 101);
	kdb::tools::errors::InterfaceWarning ifW ("ifwReason", "ifwModule", "ifwFile", "ifwMountPoint", "ifwConfigFile", 102);

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

	kdb::tools::errors::InstallationWarning installationWarning ("Reason", "Module", "File", "MountPoint", "ConfigFile", 42);
	kdb::tools::errors::InstallationWarning installationWarning2 ("Reason", "Module", "File", "MountPoint", "ConfigFile", 42);
	kdb::tools::errors::InterfaceWarning interfaceWarning ("Reason", "Module", "File", "MountPoint", "ConfigFile", 42);
	kdb::tools::errors::InstallationError installationError ("Reason", "Module", "File", "MountPoint", "ConfigFile", 42);

	ASSERT_EQ (installationWarning, installationWarning);
	ASSERT_EQ (installationWarning, installationWarning2);
	ASSERT_EQ (installationError, installationError);
	ASSERT_NE (installationWarning, interfaceWarning);
	ASSERT_NE (installationWarning, installationError);
	ASSERT_NE (installationError, installationWarning);

	kdb::tools::errors::InstallationError installationError1 ("Reason", "Module", "File", "MountPoint", "ConfigFile", 42);
	kdb::tools::errors::ResourceError resourceError ("Reason", "Module", "File", "MountPoint", "ConfigFile", 42);

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