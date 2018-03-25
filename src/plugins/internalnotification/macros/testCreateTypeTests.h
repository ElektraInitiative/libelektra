#ifndef TYPE
#error "You have to #define TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE and CHECK_VALUE before including the testCreateTypeRegister supermacro"
#endif
#ifndef TYPE_NAME
#error "You have to #define TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE and CHECK_VALUE before including the testCreateTypeRegister supermacro"
#endif
#ifndef FORMAT_STRING
#error "You have to #define TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE and CHECK_VALUE before including the testCreateTypeRegister supermacro"
#endif
#ifndef TEST_VALUE
#error "You have to #define TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE and CHECK_VALUE before including the testCreateTypeRegister supermacro"
#endif
#ifndef CHECK_VALUE
#define CHECK_VALUE (value == TEST_VALUE)
#endif
#ifndef CHECK_INVALID
#define CHECK_INVALID (value == 0)
#endif

#define CONCAT(X, Y) CONCAT2 (X, Y)
#define CONCAT2(X, Y) X##Y

#define STRINGIFY(X) STRINGIFY2 (X)
#define STRINGIFY2(X) #X

#define REGISTER_FUNC_NAME(TYPE_NAME) CONCAT (internalnotificationRegister, TYPE_NAME)

#define TEST_CASE_UPDATE_SIGNATURE(TYPE_NAME) static void TEST_CASE_UPDATE_NAME (TYPE_NAME) (void)
#define TEST_CASE_NO_UPDATE_SIGNATURE(TYPE_NAME) static void TEST_CASE_NO_UPDATE_NAME (TYPE_NAME) (void)

static int REGISTER_FUNC_NAME (TYPE_NAME) (Plugin * plugin, Key * key, TYPE * variable)
{
	size_t address = elektraPluginGetFunction (plugin, STRINGIFY (CONCAT (register, TYPE_NAME)));
	if (!address) yield_error ("function not exported");

	/* register key with plugin */
	ELEKTRA_NOTIFICATION_REGISTERFUNC_TYPEDEF (RegisterFuncType, TYPE)
	return ((RegisterFuncType) address) (plugin, key, variable);
}

TEST_CASE_UPDATE_SIGNATURE (TYPE_NAME)
{
	printf ("test update\n");
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");
	Key * registeredKey = keyNew ("/test/internalnotification/value", KEY_END);
	TYPE value = 0;
	succeed_if (REGISTER_FUNC_NAME (TYPE_NAME) (plugin, registeredKey, &value) == 1, "registration was not successful");
	char * valueStr = elektraFormat (FORMAT_STRING, TEST_VALUE);
	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_VALUE, valueStr, KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);
	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);
	succeed_if (CHECK_VALUE, "registered value was not updated");
	free (valueStr);
	keyDel (registeredKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

#ifdef INVALID_VALUE
TEST_CASE_NO_UPDATE_SIGNATURE (TYPE_NAME)
{
	printf ("test no update with invalid value\n");
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");
	Key * valueKey = keyNew ("user/test/internalnotification/value", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);
	TYPE value = 0;
	succeed_if (REGISTER_FUNC_NAME (TYPE_NAME) (plugin, valueKey, &value) == 1, "registration was not successful");
	keySetString (valueKey, INVALID_VALUE);
	elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);
	succeed_if (CHECK_INVALID, "registered value was updated");
	ksDel (ks);
	PLUGIN_CLOSE ();
}
#endif

#undef CONCAT
#undef CONCAT2
#undef STRINGIFY
#undef STRINGIFY2

#undef TYPE
#undef TYPE_NAME
#undef FORMAT_STRING
#undef TEST_VALUE
#undef CHECK_VALUE
#undef CHECK_INVALID
#undef INVALID_VALUE
