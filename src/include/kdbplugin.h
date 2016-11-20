/**
 * @file
 *
 * @brief Methods for plugin programing.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef KDBPLUGIN_H
#define KDBPLUGIN_H

#include <kdb.h>

#include <errno.h>
#include <string.h>

#define ELEKTRA_SET_ERROR_GET(parentKey)                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		if (errno == EACCES)                                                                                                       \
			ELEKTRA_SET_ERROR (109, parentKey, strerror (errno));                                                              \
		else                                                                                                                       \
			ELEKTRA_SET_ERROR (110, parentKey, strerror (errno));                                                              \
	} while (0)

#define ELEKTRA_SET_ERROR_SET(parentKey)                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		if (errno == EACCES)                                                                                                       \
			ELEKTRA_SET_ERROR (9, parentKey, strerror (errno));                                                                \
		else                                                                                                                       \
			ELEKTRA_SET_ERROR (75, parentKey, strerror (errno));                                                               \
	} while (0)

#define ELEKTRA_QUOTE(x) #x

#ifdef ELEKTRA_STATIC
#ifdef ELEKTRA_VARIANT
#define ELEKTRA_PLUGIN_EXPORT(module) ELEKTRA_PLUGIN_EXPORT2 (module, ELEKTRA_VARIANT)
#define ELEKTRA_PLUGIN_EXPORT2(module, variant) ELEKTRA_PLUGIN_EXPORT3 (module, variant)
#define ELEKTRA_PLUGIN_EXPORT3(module, variant) libelektra_##module##_##variant##_LTX_elektraPluginSymbol (void)
#else
#define ELEKTRA_PLUGIN_EXPORT(module) libelektra_##module##_LTX_elektraPluginSymbol (void)
#endif
#else
#define ELEKTRA_PLUGIN_EXPORT(module) elektraPluginSymbol (void)
#endif

#ifdef ELEKTRA_VARIANT
#define ELEKTRA_PLUGIN_FUNCTION(module, function) ELEKTRA_PLUGIN_FUNCTION2 (module, ELEKTRA_VARIANT, function)
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
	 * @param function which function it is (open, close, get, set, error)
	 */
#define ELEKTRA_PLUGIN_FUNCTION(module, function) libelektra_##module##_LTX_elektraPlugin##function
#endif

#ifdef ELEKTRA_VARIANT
#define ELEKTRA_README(module) ELEKTRA_README2 (module, ELEKTRA_VARIANT)
#define ELEKTRA_README2(module, variant) ELEKTRA_README3 (module, variant)
#define ELEKTRA_README3(module, variant) ELEKTRA_QUOTE (readme_##module##_##variant.c)
#else
/**
	 * @brief The filename for inclusion of the readme for
	 * compilation variants (see doc/tutorials).
	 *
	 * @ingroup plugin
	 *
	 * @param plugin the name of the plugin
	 */
#define ELEKTRA_README(module) ELEKTRA_README2 (module)
#define ELEKTRA_README2(module) ELEKTRA_QUOTE (readme_##module.c)
#endif


/**
 * Switches to denote the backend methods. Used in calls to elektraPluginExport().
 *
 * @ingroup backend
 */
typedef enum {
	// clang-format off
	ELEKTRA_PLUGIN_OPEN=1,		/*!< Next arg is backend for kdbOpen() */
	ELEKTRA_PLUGIN_CLOSE=1<<1,	/*!< Next arg is backend for kdbClose() */
	ELEKTRA_PLUGIN_GET=1<<2,	/*!< Next arg is backend for kdbGet() */
	ELEKTRA_PLUGIN_SET=1<<3,	/*!< Next arg is backend for kdbSet() */
	ELEKTRA_PLUGIN_ERROR=1<<4,	/*!< Next arg is backend for kdbError() */
	ELEKTRA_PLUGIN_END=0		/*!< End of arguments */
	// clang-format on
} plugin_t;


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


#define PLUGINVERSION "1"


#ifdef __cplusplus
}
}
#endif


#endif
