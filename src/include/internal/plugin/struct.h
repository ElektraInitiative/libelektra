#ifndef ELEKTRA_PLUGIN_STRUCT_H
#define ELEKTRA_PLUGIN_STRUCT_H

#include <elektra/core/keyset.h>

#include <internal/plugin/functions.h>

#include <stddef.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**
 * Holds all information related to a plugin.
 *
 * Since Elektra 0.8 a Backend consists of many plugins.
 *
 * A plugin should be reusable and only implement a single concern.
 * Plugins which are supplied with Elektra are located below src/plugins.
 * It is no problem that plugins are developed external too.
 *
 * @ingroup backend
 */
struct _Plugin
{
	KeySet * config; /*!< This keyset contains configuration for the plugin.
	 Direct below system:/ there is the configuration supplied for the backend.
	 Direct below user:/ there is the configuration supplied just for the
	 plugin, which should be of course preferred to the backend configuration.
	 The keys inside contain information like /path which path should be used
	 to write configuration to or /host to which host packets should be send.
	 @see elektraPluginGetConfig() */

	kdbOpenPtr kdbOpen;   /*!< The pointer to kdbOpen_template() of the backend. */
	kdbClosePtr kdbClose; /*!< The pointer to kdbClose_template() of the backend. */

	kdbInitPtr kdbInit;	/*!< The pointer to kdbInit_template() of the backend. */
	kdbGetPtr kdbGet;	/*!< The pointer to kdbGet_template() of the backend. */
	kdbSetPtr kdbSet;	/*!< The pointer to kdbSet_template() of the backend. */
	kdbErrorPtr kdbError;	/*!< The pointer to kdbError_template() of the backend. */
	kdbCommitPtr kdbCommit; /*!< The pointer to kdbCommit_template() of the backend. */

	const char * name; /*!< The name of the module responsible for that plugin. */

	size_t refcounter; /*!< This refcounter shows how often the plugin
	   is used.  Not shared plugins have 1 in it */

	void * data; /*!< This handle can be used for a plugin to store
	 any data its want to. */

	KeySet * global; /*!< This keyset can be used by plugins to pass data through
			the KDB and communicate with other plugins. Plugins shall clean
			up their parts of the global keyset, which they do not need any more.*/

	KeySet * modules; /*!< A list of all currently loaded modules.*/
};

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_PLUGIN_STRUCT_H
