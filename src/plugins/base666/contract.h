/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

keyNew ("system/elektra/modules/base666", KEY_VALUE, "base666 plugin waits for your orders", KEY_END),
	keyNew ("system/elektra/modules/base666/exports", KEY_END),
	keyNew ("system/elektra/modules/base666/exports/get", KEY_FUNC, elektraBase666Get, KEY_END),
	keyNew ("system/elektra/modules/base666/exports/set", KEY_FUNC, elektraBase666Set, KEY_END),
#include ELEKTRA_README (base666)
	keyNew ("system/elektra/modules/base666/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
