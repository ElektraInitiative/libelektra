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

#define CONCAT(X, Y) CONCAT2 (X, Y)
#define CONCAT2(X, Y) X##Y

#define INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME(TYPE_NAME) CONCAT (elektraInternalnotificationConvert, TYPE_NAME)

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

#undef CONCAT
#undef CONCAT2
#undef INTERNALNOTIFICATION_CONVERSION_CALLBACK_NAME
#undef INTERNALNOTIFICATION_CONVERSION_CALLBACK_SIGNATURE
#undef INTERNALNOTIFICATION_REGISTER_SIGNATURE

#undef TYPE
#undef VALUE_TYPE
#undef TYPE_NAME
#undef TO_VALUE
#undef CHECK_CONVERSION
