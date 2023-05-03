/**
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 * @brief Create test cases for internalnotification type.
 *
 * This supermacro creates the following functions:
 * - int internalnotificationRegisterTYPE_NAME (Plugin * plugin, Key * key, TYPE * variable)
 * - static void test_updateTYPE_NAME (void)
 * - static void test_noUpdateTYPE_NAME (void) (only if INVALID_VALUE is defined)
 *
 * @param  TYPE           valid C type (e.g. int or kdb_short_t)
 * @param  TYPE_NAME      name suffix for the functions (e.g. Int or UnsignedLong)
 * @param  TEST_VALUE     value of type TYPE. Used for the "update" test case
 * @param  FORMAT_STRING  format to convert TEST_VALUE to string (passed to elektraFormat())
 * @param  CHECK_VALUE    optional, default is (value == TEST_VALUE). Boolean expression to check if `value` equals the test value
 * @param  INVALID_VALUE  optional. Value of type string. Used for the no update test case. If not defined, "no update" test case is
 * omitted
 * @param  CHECK_INVALID  optioal, defaults to (value == 0). Check if the variable `value` has not been updated. Value should be 0.
 */
#ifndef TYPE
#error "You have to #define TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE and CHECK_VALUE before including the create_type_tests supermacro"
#endif
#ifndef TYPE_NAME
#error "You have to #define TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE and CHECK_VALUE before including the create_type_tests supermacro"
#endif
#ifndef FORMAT_STRING
#error "You have to #define TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE and CHECK_VALUE before including the create_type_tests supermacro"
#endif
#ifndef TEST_VALUE
#error "You have to #define TYPE, TYPE_NAME, FORMAT_STRING, TEST_VALUE and CHECK_VALUE before including the create_type_tests supermacro"
#endif
#ifndef CHECK_VALUE
#define CHECK_VALUE (value == TEST_VALUE)
#endif
#ifndef CHECK_INVALID
#define CHECK_INVALID (value == 0)
#endif

#define REGISTER_FUNC_NAME(TYPE_NAME) ELEKTRA_CONCAT (internalnotificationRegister, TYPE_NAME)

#define TEST_CASE_UPDATE_SIGNATURE(TYPE_NAME) static void TEST_CASE_UPDATE_NAME (TYPE_NAME) (void)
#define TEST_CASE_NO_UPDATE_SIGNATURE(TYPE_NAME) static void TEST_CASE_NO_UPDATE_NAME (TYPE_NAME) (void)

static int REGISTER_FUNC_NAME (TYPE_NAME) (Plugin * plugin, Key * key, TYPE * variable)
{
	size_t address = elektraPluginGetFunction (plugin, ELEKTRA_STRINGIFY (ELEKTRA_CONCAT (register, TYPE_NAME)));
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
	Key * valueKey = keyNew ("user:/test/internalnotification/value", KEY_VALUE, valueStr, KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);

	ElektraDiff * diff = elektraDiffNew (NULL, NULL, ks, NULL);
	ksIncRef (ks);
	elektraInternalnotificationNotifyChangedKeys (plugin, diff);
	elektraDiffDel (diff);

	// elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);

	succeed_if (CHECK_VALUE, "registered value was not updated");
	free (valueStr);
	keyDel (registeredKey);
	ksDecRef (ks);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

#ifdef INVALID_VALUE
TEST_CASE_NO_UPDATE_SIGNATURE (TYPE_NAME)
{
	printf ("test no update with invalid value\n");
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("internalnotification");
	Key * valueKey = keyNew ("user:/test/internalnotification/value", KEY_END);
	KeySet * ks = ksNew (1, valueKey, KS_END);
	TYPE value = 0;
	succeed_if (REGISTER_FUNC_NAME (TYPE_NAME) (plugin, valueKey, &value) == 1, "registration was not successful");
	keySetString (valueKey, INVALID_VALUE);

	ElektraDiff * diff = elektraDiffNew (NULL, NULL, ks, NULL);
	ksIncRef (ks);
	elektraInternalnotificationNotifyChangedKeys (plugin, diff);
	elektraDiffDel (diff);

	// elektraInternalnotificationUpdateRegisteredKeys (plugin, ks);


	succeed_if (CHECK_INVALID, "registered value was updated");
	ksDecRef (ks);

	ksDel (ks);
	PLUGIN_CLOSE ();
}
#endif

#undef TYPE
#undef TYPE_NAME
#undef FORMAT_STRING
#undef TEST_VALUE
#undef CHECK_VALUE
#undef CHECK_INVALID
#undef INVALID_VALUE
