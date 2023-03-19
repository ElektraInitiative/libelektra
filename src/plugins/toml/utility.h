/**
 * @file utility.h
 *
 * @brief Contains functionality for handling Keys used throughout writing and reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_TOML_UTILITY_H
#define ELEKTRA_PLUGIN_TOML_UTILITY_H

#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <stdbool.h>
#include <stddef.h>

Key * keyAppendIndex (size_t index, const Key * parent);
void keyUpdateArrayMetakey (Key * key, size_t newIndex);
char * indexToArrayString (size_t index);
size_t arrayStringToIndex (const char * indexStr);
bool isArrayIndex (const char * basename);
void setPlainIntMeta (Key * key, const char * metaKeyName, size_t value);
char * intToStr (size_t i);
void setOrderForKey (Key * key, size_t order);
bool isArrayElement (const Key * key);
bool isEmptyArray (Key * key);
bool isBareString (const char * str);
size_t getArrayMax (Key * key);
bool isArray (Key * key);
bool isSimpleTable (Key * key);
bool isTableArray (Key * key);
bool isInlineTable (Key * key);
bool isTomlType (Key * key, const char * type);
char * getRelativeName (Key * parent, Key * key);
char * getDirectSubKeyName (const Key * parent, const Key * key);
void keySetDiff (KeySet * whole, KeySet * part);
KeySet * keysByPredicate (KeySet * ks, bool (*pred) (Key *));
KeySet * collectSubKeys (KeySet * ks, Key * parent);
KeySet * extractSubKeys (KeySet * ks, Key * parent);
bool isLeaf (Key * leafCandidate, KeySet * ks);
bool isBase64String (const char * str);
bool isNullString (const char * str);

#endif // ELEKTRA_PLUGIN_TOML_UTILITY_H
