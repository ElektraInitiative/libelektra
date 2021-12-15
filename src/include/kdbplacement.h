/**
 * @file
 *
 * @brief INTERNAL header for libelektra-opts
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_KDBPLACEMENT_H
#define ELEKTRA_KDBPLACEMENT_H

#include "kdbmount.h"

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/*
infos/ordering - barely used, should be supported via placements
infos/stacking - only used by glob, should be supported via separate placements for get/set
infos/placements - needs changes

--> use this

# <pvariant> is the variant of placement, different backends may use different variants
contract/placements/<pvariant>... = ...

# infos/placements corresponds to contract/placements/basic
contract/placements/basic = ...
# but we can also split get and set
contract/placements/basic/get = ...
contract/placements/basic/set = ...

# odering constraints in contract/placements/basic
# <op> is get or set
contract/placements/basic/<op>/before = ... # during <op> run this plugin before the listed ones
contract/placements/basic/<op>/after = ... # during <op> run this plugin after the listed ones

# every backend plugins should support the 'basic' variant of placement
# sometimes it may make sense to ignore some parts e.g. the resolver or storage placements are useless in an SQL backend
# backend plugins that don't use other plugins (e.g. version) are obviously an exception
# if possible the 'basic' variant should be mapped to whatever placement variant the backend plugin uses natively

# other placement variants could enhance backend functionality e.g. there could be a variant that groups plugins
# by functionality (e.g. generation, transformation, validation) and calls them when appropriate. For example the
# backend plugin could do generation -> transformation -> validation in a loop until nothing changes. Obviously something
# like this couldn't be supported by every plugin, so requiring the declaration of a separate placement makes sense.

# parsing of the 'basic' variant (and in future possibly others) is implemented by libelektra-placement

*/

// TODO (kodebach) [Q]: should this be a public struct?
// the struct should be stable, unless there is some new drastic change to libelektra-kdb
// if we make the struct opaque we need creation, deletion and accessor functions
typedef struct
{
	char * resolver; // used for get/resolver, set/resolver, commit and rollback
	char * storage;	 // used for get/storage and set/storage

	struct
	{
		ElektraPluginList * prestorage;
		ElektraPluginList * poststorage;
	} get;

	struct
	{
		ElektraPluginList * prestorage;
		ElektraPluginList * poststorage;
		ElektraPluginList * precommit;
		ElektraPluginList * postcommit;
		ElektraPluginList * prerollback;
		ElektraPluginList * postrollback;
	} set;
} ElektraBasicPlacedPlugins;

/*
// if the struct above is opaque we also need these
ElektraBasicPlacedPlugins * elektraPlacementBasicCreate (void);
const char * elektraPlacementBasicResolver (ElektraBasicPlacedPlugins * placedPlugins);
const char * elektraPlacementBasicStorage (ElektraBasicPlacedPlugins * placedPlugins);

typedef enum
{
	ELEKTRA_BASIC_PLACED_GET_PRESTORAGE,
	ELEKTRA_BASIC_PLACED_GET_POSTSTORAGE,
	ELEKTRA_BASIC_PLACED_SET_PRESTORAGE,
	ELEKTRA_BASIC_PLACED_SET_POSTSTORAGE,
	ELEKTRA_BASIC_PLACED_SET_PRECOMMIT,
	ELEKTRA_BASIC_PLACED_SET_POSTCOMMIT,
	ELEKTRA_BASIC_PLACED_SET_PREROLLBACK,
	ELEKTRA_BASIC_PLACED_SET_POSTROLLBACK,
} ElektraBasicPlacedPluginsList;

ElektraPluginList * elektraPlacementBasicGetList (ElektraBasicPlacedPlugins * placedPlugins, ElektraBasicPlacedPluginsList listType);

void elektraPlacementBasicDelete (ElektraBasicPlacedPlugins * placedPlugins);
*/

int elektraPlacementBasicPlacePlugin (ElektraBasicPlacedPlugins * placedPlugins, const char * plugin);

/* Usage example:

ElektraPluginList * plugins = elektraMountResolveAllPlugins (aliases, metaKeys, withRecommends);

ElektraBasicPlacedPlugins * placedPlugins;
// init placedPlugins on stack, or via elektraPlacementBasicCreate depending on API
for (size_t i = 0; i < elektraPluginListSize (plugins); ++i)
{
	elektraPlacementBasicPlacePlugin (placedPlugins, plugin);
}

// process placedPlugins into plugin configuration
*/

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBPLACEMENT_H
