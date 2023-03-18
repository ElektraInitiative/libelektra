/**
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 * @brief Add a type to the internalnotification plugin.
 *
 * Additional required steps:
 * - Export the register function using INTERNALNOTIFICATION_EXPORT_FUNCTION in elektraInternalnotificationGet()
 * - Update testmod_internalnotification.c: Generate additional test cases using the create_type_tests supermacro
 * - Update kdbnotification.h: add a ELEKTRA_NOTIFICATION_TYPE_DECLARATION
 * - Update libs/notification/notification.c: add a ELEKTRA_NOTIFICATION_TYPE_DEFINITION
 *
 * This supermacro creates the following functions:
 * - void elektraInternalnotificationConvertTYPE_NAME (Key * key, void * context)
 * - int elektraInternalnotificationRegisterTYPE_NAME (Plugin * handle, Key * key, TYPE * variable)
 *
 * @param  TYPE             valid C type (e.g. int or kdb_short_t)
 * @param  TYPE_NAME        name suffix for the functions (e.g. Int or UnsignedLong)
 * @param  VALUE_TYPE       optional, defaults to TYPE. Ideally a larger type assigned to variable `value` for
 *                          checking the range before the variable is updated
 * @param  TO_VALUE         expression for converting `string` (variable containing the key value) to VALUE_TYPE
 * @param  CHECK_CONVERSION optional, defaults to true. A boolean expression. Allows to check the range after
 *                          conversion. Use INTERNALNOTIFICATION_CHECK_CONVERSION to check if a conversion using
 *                          strto*()-functions was successful and INTERNALNOTIFICATION_CHECK_CONVERSION_RANGE (RANGE)
 *                          to check additionally for a specified range.
 */
#ifndef TYPE
#error "You have to #define TYPE, TYPE_NAME and TO_VALUE before including the addType supermacro"
#endif
#ifndef VALUE_TYPE
// use type as default if not set
#define VALUE_TYPE TYPE
#endif
#ifndef TYPE_NAME
#error "You have to #define TYPE, TYPE_NAME and TO_VALUE before including the addType supermacro"
#endif
#ifndef TO_VALUE
#error "You have to #define TYPE, TYPE_NAME and TO_VALUE before including the addType supermacro"
#endif
#ifndef CHECK_CONVERSION
#define CHECK_CONVERSION 1
#endif

#define INTERNALNOTIFICATION_CONVERSION_FUNCTION_NAME(TYPE_NAME) ELEKTRA_CONCAT (elektraInternalnotificationConvert, TYPE_NAME)
#define INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME(TYPE_NAME) ELEKTRA_CONCAT (elektraInternalnotificationConvertCallback, TYPE_NAME)

#define INTERNALNOTIFICATION_REGISTER_SIGNATURE(TYPE, TYPE_NAME)                                                                           \
	int INTERNALNOTIFICATION_REGISTER_NAME (TYPE_NAME) (Plugin * handle, Key * key, TYPE * variable)

#define INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE(TYPE_NAME)                                                                      \
	void INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME (TYPE_NAME) (Key * key, void * context)

#define DISABLE_UNDEF_PARAMETERS
#define NAME_MACRO INTERNALNOTIFICATION_CONVERSION_FUNCTION_NAME
#include <internal/macros/type_create_to_value.h>

INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE (TYPE_NAME)
{
	_ElektraInternalnotificationConversionContext * ctx = (_ElektraInternalnotificationConversionContext *) context;
	TYPE * variable = (TYPE *) ctx->variable;
	if (!INTERNALNOTIFICATION_CONVERSION_FUNCTION_NAME (TYPE_NAME) (key, variable))
	{
		if (ctx->errorCallback)
		{
			ctx->errorCallback (key, ctx->errorCallbackContext);
		}
	}
}

INTERNALNOTIFICATION_REGISTER_SIGNATURE (TYPE, TYPE_NAME)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");

	_ElektraInternalnotificationConversionContext * context = elektraMalloc (sizeof *context);
	if (context == NULL)
	{
		return 0;
	}
	context->errorCallback = pluginState->conversionErrorCallback;
	context->errorCallbackContext = pluginState->conversionErrorCallbackContext;
	context->variable = variable;

	KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (
		pluginState, key, INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME (TYPE_NAME), context, 1);
	if (registeredKey == NULL)
	{
		return 0;
	}
	return 1;
}

#undef INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME
#undef INTERNALNOTIFICATION_CONVERSION_FUNCTION_NAME_SIGNATURE
#undef INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE
#undef INTERNALNOTIFICATION_REGISTER_SIGNATURE
#undef NAME_MACRO

#undef TYPE
#undef VALUE_TYPE
#undef TYPE_NAME
#undef TO_VALUE
#undef CHECK_CONVERSION
#undef PRE_CHECK_BLOCK
#undef PRE_CHECK_CONVERSION
