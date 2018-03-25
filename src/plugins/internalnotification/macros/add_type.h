/**
 * Add a type to the internalnotification plugin.
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

#define ELEKTRA_CONCAT(X, Y) ELEKTRA_CONCAT2 (X, Y)
#define ELEKTRA_CONCAT2(X, Y) X##Y

#define INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME(TYPE_NAME) ELEKTRA_CONCAT (elektraInternalnotificationConvert, TYPE_NAME)

#define INTERNALNOTIFICATION_REGISTER_SIGNATURE(TYPE, TYPE_NAME)                                                                           \
	int INTERNALNOTIFICATION_REGISTER_NAME (TYPE_NAME) (Plugin * handle, Key * key, TYPE * variable)

#define INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE(TYPE_NAME)                                                                      \
	void INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME (TYPE_NAME) (Key * key, void * context)

INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE (TYPE_NAME)
{
	TYPE * variable = (TYPE *) context;
	char * end ELEKTRA_UNUSED;
	const char * string = keyValue (key);
	errno = 0;
	/* convert string to target type */
	VALUE_TYPE value = TO_VALUE;
	/* only update if conversion was successful */
	if (CHECK_CONVERSION)
	{
		*(variable) = value;
	}
	else
	{
		ELEKTRA_LOG_WARNING ("conversion failed! string=%s, stopped=%c errno=%d", keyString (key), *end, errno);
	}
}

INTERNALNOTIFICATION_REGISTER_SIGNATURE (TYPE, TYPE_NAME)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");
	KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (
		pluginState, key, INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME (TYPE_NAME), variable);
	if (registeredKey == NULL)
	{
		return 0;
	}
	return 1;
}

#undef ELEKTRA_CONCAT
#undef ELEKTRA_CONCAT2
#undef INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME
#undef INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE
#undef INTERNALNOTIFICATION_REGISTER_SIGNATURE

#undef TYPE
#undef VALUE_TYPE
#undef TYPE_NAME
#undef TO_VALUE
#undef CHECK_CONVERSION
