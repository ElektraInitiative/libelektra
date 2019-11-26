#ifndef ELEKTRA_PLUGIN_TOML_UTILITY_H
#define ELEKTRA_PLUGIN_TOML_UTILTIY_H

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
bool isEmptyArray(Key * key);
size_t getArrayMax (Key * key);
const Key * findMetaKey (Key * key, const char * metakeyName);
bool isArray (Key * key);
bool isTomlType (Key * key, const char * type);
bool isTableArray (Key * key);
bool isInlineTable (Key * key);
char * getRelativeKeyName (const Key * parent, const Key * key);
char * getDirectSubKeyName (const Key * parent, const Key * key);

#endif // ELEKTRA_PLUGIN_TOML_UTILITY_H
