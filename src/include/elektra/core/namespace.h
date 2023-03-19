/**
 * @file
 *
 * @brief Elektra Core Namespaces.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_CORE_NAMESPACE_H
#define ELEKTRA_CORE_NAMESPACE_H

enum
{
	KEY_NS_NONE = 0,
	KEY_NS_CASCADING = 1,
	KEY_NS_META = 2,
	KEY_NS_SPEC = 3,
	KEY_NS_PROC = 4,
	KEY_NS_DIR = 5,
	KEY_NS_USER = 6,
	KEY_NS_SYSTEM = 7,
	KEY_NS_DEFAULT = 8,
};
typedef int elektraNamespace;

static const elektraNamespace KEY_NS_FIRST = KEY_NS_META;
static const elektraNamespace KEY_NS_LAST = KEY_NS_DEFAULT;

#endif // ELEKTRA_CORE_NAMESPACE_H