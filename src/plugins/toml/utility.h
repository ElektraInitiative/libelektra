#ifndef ELEKTRA_PLUGIN_TOML_UTILITY_H
#define ELEKTRA_PLUGIN_TOML_UTILITY_H

#include <kdb.h>
#include <stddef.h>
#include <stdbool.h>

void dumpKS(KeySet * keys);
void dumpMemKS(Key ** keys, size_t size);

Key * keyAppendIndex (size_t index, const Key * parent);
void keyUpdateArrayMetakey (Key * key, size_t newIndex);
char * indexToArrayString (size_t index);
size_t arrayStringToIndex (const char * indexStr);
bool isArrayIndex(const char * basename);
void setPlainIntMeta (Key * key, const char * metaKeyName, size_t value);
char * intToStr (size_t i);
void setOrderForKey (Key * key, size_t order);
bool isArrayElement(const Key * key);
bool isEmptyArray(Key * key);
bool isBareString(const char * str);
size_t getArrayMax (Key * key);
const Key * findMetaKey (Key * key, const char * metakeyName);
bool isArray (Key * key);
bool isSimpleTable (Key * key);
bool isTableArray (Key * key);
bool isInlineTable (Key * key);
bool isTomlType (Key * key, const char * type);
char * getRelativeName (Key * parent, Key * key);
char * getDirectSubKeyName (const Key * parent, const Key * key);
void keySetDiff(KeySet * whole, KeySet * part);
KeySet * keysByPredicate(KeySet * ks, bool (*pred)(Key *));
KeySet * collectSubKeys(KeySet * ks, Key * parent);
KeySet * extractSubKeys (KeySet * ks, Key * parent);
bool isLeaf(Key * leafCandidate, KeySet * ks);

#endif // ELEKTRA_PLUGIN_TOML_UTILITY_H
