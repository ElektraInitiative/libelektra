/**
 * @file table_array.c
 *
 * @brief Contains functionaly for handling table arrays on reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_PLUGIN_TOML_TABLE_ARRAY_H
#define ELEKTRA_PLUGIN_TOML_TABLE_ARRAY_H

#include <elektra/old_kdb.h>
#include <stddef.h>

typedef struct _TableArrayList
{
	Key * key;
	char * keyStr;
	size_t currIndex;
	struct _TableArrayList * next;
} TableArrayList;

/*
 * @brief Pushes a new table array entry onto the list.
 *
 * @param top The current top element of the list.
 * @param key Table array key to push onto the list.
 *
 * @retval Pointer New table array top.
 * @retval NULL On error.
 */
TableArrayList * pushTableArray (TableArrayList * top, Key * key);

/*
 * @brief Pops a table array entry from the list.
 *
 * Frees the top element.
 *
 * @param top The current top element of the list.
 *
 * @retval Pointer New table array top.
 * @retval NULL On error.
 */

TableArrayList * popTableArray (TableArrayList * top);

/*
 * @brief Build a key name, bases on the current table array list.
 *
 * @param ta TableArray list.
 *
 * @retval Pointer Key with a keyname derived from the table array list.
 * @retval NULL On error.
 */

Key * buildTableArrayKeyName (const TableArrayList * ta);

#endif // ELEKTRA_PLUGIN_TOML_TABLE_ARRAY_LIST_H
