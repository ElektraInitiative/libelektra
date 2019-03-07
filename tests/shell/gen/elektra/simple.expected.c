/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#include "simple.actual.h"



#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>
#include <kdbinvoke.h>

#include <elektra/conversion.h>


/**
 * Initializes an instance of Elektra for the application 'tests/script/gen/elektra/simple'.
 *
 * This MUST be called before anything was written to stdout, otherwise specload will fail.
 * If you have to write to stdout before calling this, you must handle the specload
 * communication yourself. You may use  and exit
 *            @p elektra and @p error are both unchanged
 *
 * @see elektraOpen
 */// 
int loadConfiguration (Elektra ** elektra, ElektraError ** error)
{
	KeySet * defaults = ksNew (6,
	keyNew ("spec/tests/script/gen/elektra/simple/elektra/specload", KEY_META, "default", "0", KEY_META, "opt",
	"--elektra-spec", KEY_META, "opt/arg", "none", KEY_META, "type", "boolean", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/mydouble", KEY_META, "default", "0.0", KEY_META, "type", "double",
	KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myfloatarray/#", KEY_META, "default", "0.0", KEY_META, "type", "float",
	KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myint", KEY_META, "default", "0", KEY_META, "type", "long", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/mystring", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/print", KEY_META, "default", "0", KEY_META, "type", "boolean", KEY_END),
	KS_END);
;
	Elektra * e = elektraOpen ("tests/script/gen/elektra/simple", defaults, error);

	if (e == NULL)
	{
		return -1;
	}

	if (elektraGetBoolean (e, "spec/tests/script/gen/elektra/simple/elektra/specload"))
	{
		elektraClose (e);
		return specloadSend ();
	}

	if (0 /* TODO: check if help mode */)
	{
		elektraClose (e);
		printHelpMessage ();
		return 2;
	}

	*elektra = e;
	return 0;
}

/**
 * Sends the specification over stdout in the format expected by specload.
 *
 * You MUST not output anything to stdout before or after invoking this function
 * and should exit as soon as possible after calling this function.
 *
 * @retval 1 on success
 * @retval -1 on error
 */
int specloadSend (void)
{
	KeySet * spec = ksNew (6,
	keyNew ("spec/tests/script/gen/elektra/simple/elektra/specload", KEY_META, "default", "0", KEY_META, "opt",
	"--elektra-spec", KEY_META, "opt/arg", "none", KEY_META, "type", "boolean", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/mydouble", KEY_META, "default", "0.0", KEY_META, "type", "double",
	KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myfloatarray/#", KEY_META, "default", "0.0", KEY_META, "type", "float",
	KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/myint", KEY_META, "default", "0", KEY_META, "type", "long", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/mystring", KEY_META, "default", "", KEY_META, "type", "string", KEY_END),
	keyNew ("spec/tests/script/gen/elektra/simple/print", KEY_META, "default", "0", KEY_META, "type", "boolean", KEY_END),
	KS_END);
;

	Key * errorKey = keyNew (0, KEY_END);

	KeySet * specloadConf = ksNew (1, keyNew ("system/sendspec", KEY_END), KS_END);
	ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, errorKey);

	int result = elektraInvoke2Args (specload, "sendspec", spec, NULL);

	elektraInvokeClose (specload, errorKey);
	keyDel (errorKey);
	ksDel (specloadConf);
	ksDel (spec);

	return result == ELEKTRA_PLUGIN_STATUS_SUCCESS ? 1 : -1;
}

/**
 * Outputs the help message
 */
void printHelpMessage (void)
{
	// TODO
}

// clang-format off

// clang-format on

// -------------------------
// Enum conversion functions
// -------------------------



// -------------------------
// Enum accessor functions
// -------------------------




// clang-format off

// clang-format on

// -------------------------
// Struct accessor functions
// -------------------------





#ifdef __cplusplus
}
#endif
