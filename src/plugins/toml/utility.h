#ifndef ELEKTRA_PLUGIN_TOML_UTILITY_H
#define ELEKTRA_PLUGIN_TOML_UTILTIY_H

#include <stddef.h>
#include <kdb.h>

Key * keyAppendIndex (size_t index, const Key * parent);
void keyUpdateArrayMetakey (Key * key, size_t newIndex);
char * indexToArrayString (size_t index);
void setPlainIntMeta (Key * key, const char * metaKeyName, size_t value);
char * intToStr (size_t i);
void setOrderForKey (Key * key, size_t order);

#endif // ELEKTRA_PLUGIN_TOML_UTILITY_H
