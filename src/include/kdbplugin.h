/**
 * @file
 *
 * @brief Methods for plugin programing.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBPLUGIN_H
#define KDBPLUGIN_H

#include <elektra/kdb.h>

#include <errno.h>
#include <kdbmacros.h>
#include <string.h>

#ifdef ELEKTRA_STATIC
#define ELEKTRA_PLUGIN_EXPORT ELEKTRA_PLUGIN_EXPORT2 (ELEKTRA_PLUGIN_NAME_C)
#define ELEKTRA_PLUGIN_EXPORT2(module) ELEKTRA_PLUGIN_EXPORT3 (module)
#define ELEKTRA_PLUGIN_EXPORT3(module) libelektra_##module##_LTX_elektraPluginSymbol (void)
#else
#define ELEKTRA_PLUGIN_EXPORT elektraPluginSymbol (void)
#endif

#ifdef ELEKTRA_VARIANT
#define ELEKTRA_PLUGIN_FUNCTION(function) ELEKTRA_PLUGIN_FUNCTION2 (ELEKTRA_PLUGIN_NAME_C, ELEKTRA_VARIANT, function)
#define ELEKTRA_PLUGIN_FUNCTION2(module, variant, function) ELEKTRA_PLUGIN_FUNCTION3 (module, variant, function)
#define ELEKTRA_PLUGIN_FUNCTION3(module, variant, function) libelektra_##module##_##variant##_LTX_elektraPlugin##function
#else
/**
 * @brief Declare a plugin's function name suitable for
 * compilation variants (see doc/tutorials).
 *
 * It can be used in the same way as elektraPluginExport().
 * @see ELEKTRA_PLUGIN_EXPORT
 *
 * @ingroup plugin
 *
 * @param plugin the name of the plugin
 * @param function which function it is (open, close, get, set, error, commit)
 */
#define ELEKTRA_PLUGIN_FUNCTION(function) ELEKTRA_PLUGIN_FUNCTION2 (ELEKTRA_PLUGIN_NAME_C, function)
#define ELEKTRA_PLUGIN_FUNCTION2(module, function) ELEKTRA_PLUGIN_FUNCTION3 (module, function)
#define ELEKTRA_PLUGIN_FUNCTION3(module, function) libelektra_##module##_LTX_elektraPlugin##function
#endif

/**
 * @brief The filename for inclusion of the readme for
 * compilation variants (see doc/tutorials).
 *
 * @ingroup plugin
 *
 * @param plugin the name of the plugin
 */
#define ELEKTRA_README ELEKTRA_README2 (ELEKTRA_PLUGIN_NAME_C)
#define ELEKTRA_README2(module) ELEKTRA_README3 (module)
#define ELEKTRA_README3(module) ELEKTRA_QUOTE (readme_##module.c)


/**
 * Switches to denote the backend methods. Used in calls to elektraPluginExport().
 *
 * @ingroup backend
 */
typedef enum
{
	// clang-format off
	ELEKTRA_PLUGIN_OPEN=1,		/*!< Next arg is backend for kdbOpen() */
	ELEKTRA_PLUGIN_CLOSE=1<<1,	/*!< Next arg is backend for kdbClose() */
	ELEKTRA_PLUGIN_GET=1<<2,	/*!< Next arg is backend for kdbGet() */
	ELEKTRA_PLUGIN_SET=1<<3,	/*!< Next arg is backend for kdbSet() */
	ELEKTRA_PLUGIN_ERROR=1<<4,	/*!< Next arg is backend for kdbError() */
	ELEKTRA_PLUGIN_COMMIT=1<<5,	/*!< Next arg is backend for kdbCommit()*/
	ELEKTRA_PLUGIN_END=0		/*!< End of arguments */
	// clang-format on
} plugin_t;

/* Status values for plugin functions */

/** An error occurred inside the plugin function */
#define ELEKTRA_PLUGIN_STATUS_ERROR -1

/** Everything went fine */
#define ELEKTRA_PLUGIN_STATUS_SUCCESS 1

/** Everything went fine and the function **did not** update the given keyset/configuration */
#define ELEKTRA_PLUGIN_STATUS_NO_UPDATE 0

/** Everything went fine and we have a cache hit */
#define ELEKTRA_PLUGIN_STATUS_CACHE_HIT 2

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _Plugin Plugin;

Plugin * elektraPluginExport (const char * pluginName, ...);

KeySet * elektraPluginGetConfig (Plugin * handle);
void elektraPluginSetData (Plugin * plugin, void * handle);
void * elektraPluginGetData (Plugin * plugin);

KeySet * elektraPluginGetGlobalKeySet (Plugin * plugin);

#define PLUGINVERSION "1"


#ifdef __cplusplus
}
}
#endif


#endif
