/**
 * @file
 *
 * @brief INTERNAL header for libelektra-opts
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_KDBMOUNT_H
#define ELEKTRA_KDBMOUNT_H

#include "kdb.h"
#include <stdbool.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

// private struct
struct _ElektraPluginList
{
	char ** plugins;
	size_t size;
	size_t alloc;
};

typedef struct _ElektraPluginList ElektraPluginList;
typedef struct _ElektraPluginList ElektraPluginAliasList;

ElektraPluginList * elektraPluginListCreate (size_t alloc);
size_t elektraPluginListSize (const ElektraPluginList * list);
const char * elektraPluginListGet (const ElektraPluginList * list, size_t index);
const char * elektraPluginListSet (const ElektraPluginList * list, size_t index, const char * plugin);
const char * elektraPluginListEnsureAlloc (const ElektraPluginList * list, size_t alloc);
void elektraPluginListDelete (ElektraPluginList * list);

// finds all plugins for an alias; contract/provides, contract/name (= plugin->name), file name
ElektraPluginList * elektraMountFindPluginsByAlias (const char * alias);

// (for spec-mount) finds all plugins that support metakey; contract/metadata
ElektraPluginList * elektraMountFindPluginsByMeta (const char * metaName);

// selects the best plugin from a set of candidates; based on contract/status
const char * elektraMountSelectBestPlugin (const ElektraPluginList * candidates);

// gets the list of other plugins recommneded by a concrete(!) plugin; contract/recommends
ElektraPluginAliasList * elektraMountGetRecommendedPlugins (const char * plugin);

// resolves all plugins in aliases (NULL terminated) with elektraMountFindPluginsByAlias
// finds plugins for all meta:/ keys in metaKeys
// if withRecommends==true also resolves recommended plugins with elektraMountGetRecommendedPlugins and elektraMountFindPluginsByAlias
// returns the list of all resolved plugins
ElektraPluginList * elektraMountResolveAllPlugins (const char ** aliases, KeySet * metaKeys, bool withRecommends);

/*
 replace infos/needs, config/needs with contract/needs

 all of this is processed at runtime by libelektra-kdb

 # general config (was config/needs)
 contract/needs/config/... = ...

 # plugin (was infos/needs) + config (not possible before)
 contract/needs/<plugin>/name = <some_plugin>
 contract/needs/<plugin>/config/... = ...
*/

/* Usage example:

ElektraPluginList * tomlCandidates = elektraMountFindPluginsByAlias ("storage/toml");
const char * toml = elektraMountSelectBestPlugin (tomlCandidates);

ElektraPluginAliasList * tomlRecommendsAliases = elektraMountGetRecommendedPlugins (toml);
size_t aliasCount = elektraPluginListSize (tomlRecommendsAliases);
ElektraPluginList * tomlRecommended = elektraPluginListCreate (aliasCount);
for (size_t i = 0; i < aliasCount; ++i)
{
	const char * alias = elektraPluginListGet (tomlRecommendsAliases, i);
	ElektraPluginList * candidates = elektraMountFindPluginsByAlias (alias);
	const char * recommended = elektraMountSelectBestPlugin (candidates);
	elektraPluginListSet (tomlRecommended, i, recommended);
}

ElektraPluginList * typeCandidates = elektraMountFindPluginsByMeta ("meta:/type");
const char * type = elektraMountSelectBestPlugin (typeCandidates);

// create mountpoint configuration with plugins

elektraPluginListDelete (tomlCandidates);
elektraPluginListDelete (tomlRecommendsAliases);
elektraPluginListDelete (tomlRecommended);
elektraPluginListDelete (typeCandidates);
*/

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBMOUNT_H
