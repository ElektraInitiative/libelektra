/**
 * @file
 *
 * @brief Methods for plugin programing.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBPLUGIN_H
#define KDBPLUGIN_H

#include <elektra/core/keyset.h>
#include <errno.h>
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
#define ELEKTRA_README3(module) ELEKTRA_README4 (readme_##module.c)
#define ELEKTRA_README4(file) #file


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
	ELEKTRA_PLUGIN_INIT=1<<6,	/*!< Next arg is backend for kdbInit()*/
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

typedef uint8_t ElektraKdbPhase;

enum
{
	ELEKTRA_KDB_GET_PHASE_RESOLVER = 0x01,

	ELEKTRA_KDB_GET_PHASE_CACHECHECK = 0x11,

	ELEKTRA_KDB_GET_PHASE_PRE_STORAGE = 0x71,
	ELEKTRA_KDB_GET_PHASE_STORAGE = 0x78,
	ELEKTRA_KDB_GET_PHASE_POST_STORAGE = 0x7F,

	ELEKTRA_KDB_SET_PHASE_RESOLVER = 0x01,

	ELEKTRA_KDB_SET_PHASE_PRE_STORAGE = 0x71,
	ELEKTRA_KDB_SET_PHASE_STORAGE = 0x78,
	ELEKTRA_KDB_SET_PHASE_POST_STORAGE = 0x7F,

	ELEKTRA_KDB_SET_PHASE_PRE_COMMIT = 0xE1,
	ELEKTRA_KDB_SET_PHASE_COMMIT = 0xE8,
	ELEKTRA_KDB_SET_PHASE_POST_COMMIT = 0xEF,

	ELEKTRA_KDB_SET_PHASE_PRE_ROLLBACK = 0xF1,
	ELEKTRA_KDB_SET_PHASE_ROLLBACK = 0xF8,
	ELEKTRA_KDB_SET_PHASE_POST_ROLLBACK = 0xFF,
};

typedef struct _Plugin Plugin;

Plugin * elektraPluginExport (const char * pluginName, ...);

KeySet * elektraPluginGetConfig (Plugin * handle);
void elektraPluginSetData (Plugin * plugin, void * handle);
void * elektraPluginGetData (Plugin * plugin);

KeySet * elektraPluginGetGlobalKeySet (Plugin * plugin);
ElektraKdbPhase elektraPluginGetPhase (Plugin * plugin);
Plugin * elektraPluginFromMountpoint (Plugin * plugin, const char * ref);

#define PLUGINVERSION "1"


#ifdef __cplusplus
}
}
#endif


#endif
