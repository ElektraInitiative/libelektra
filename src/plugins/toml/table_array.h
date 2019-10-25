#ifndef ELEKTRA_PLUGIN_TOML_TABLE_ARRAY_H
#define ELEKTRA_PLUGIN_TOML_TABLE_ARRAY_H

#include <kdb.h>
#include <stddef.h>

typedef struct _TableArrayList
{
	Key * key;
	char * keyStr;
	size_t currIndex;
	struct _TableArrayList * next;
} TableArrayList;

TableArrayList * pushTableArray (TableArrayList * top, Key * key);
TableArrayList * popTableArray (TableArrayList * top);
Key * buildTableArrayKeyName (const TableArrayList * ta);

#endif // ELEKTRA_PLUGIN_TOML_TABLE_ARRAY_LIST_H

