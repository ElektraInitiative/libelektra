/**
 * @file utility.h
 *
 * @brief Contains functionality for handling Keys used throughout writing and reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_TOML_UTILITY_H
#define ELEKTRA_PLUGIN_TOML_UTILITY_H

#include <kdb.h>
#include <stdbool.h>
#include <stddef.h>

ElektraKey * keyAppendIndex (size_t index, const ElektraKey * parent);
void keyUpdateArrayMetakey (ElektraKey * key, size_t newIndex);
char * indexToArrayString (size_t index);
size_t arrayStringToIndex (const char * indexStr);
bool isArrayIndex (const char * basename);
void setPlainIntMeta (ElektraKey * key, const char * metaKeyName, size_t value);
char * intToStr (size_t i);
void setOrderForKey (ElektraKey * key, size_t order);
bool isArrayElement (const ElektraKey * key);
bool isEmptyArray (ElektraKey * key);
bool isBareString (const char * str);
size_t getArrayMax (ElektraKey * key);
bool isArray (ElektraKey * key);
bool isSimpleTable (ElektraKey * key);
bool isTableArray (ElektraKey * key);
bool isInlineTable (ElektraKey * key);
bool isTomlType (ElektraKey * key, const char * type);
char * getRelativeName (ElektraKey * parent, ElektraKey * key);
char * getDirectSubKeyName (const ElektraKey * parent, const ElektraKey * key);
void keySetDiff (ElektraKeyset * whole, ElektraKeyset * part);
ElektraKeyset * keysByPredicate (ElektraKeyset * ks, bool (*pred) (ElektraKey *));
ElektraKeyset * collectSubKeys (ElektraKeyset * ks, ElektraKey * parent);
ElektraKeyset * extractSubKeys (ElektraKeyset * ks, ElektraKey * parent);
bool isLeaf (ElektraKey * leafCandidate, ElektraKeyset * ks);
bool isBase64String (const char * str);
bool isNullString (const char * str);

#endif // ELEKTRA_PLUGIN_TOML_UTILITY_H
