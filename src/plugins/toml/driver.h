/**
 * @file driver.h
 *
 * @brief Used by the TOML lexer/parser for generating appropriate Elektra Key/Values.
 *
 * All functions of the format driverEnter/driverExit are strongly bound to their similarly named grammar rules in the bison parser.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TOML_DRIVER_H
#define ELEKTRA_PLUGIN_TOML_DRIVER_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "./comment_list.h"
#include "./scalar.h"
#include "./table_array.h"

typedef struct _ParentList
{
	Key * key;
	struct _ParentList * next;
} ParentList;

typedef struct _IndexList
{
	size_t value;
	struct _IndexList * next;
} IndexList;

typedef struct
{
	KeySet * keys;
	Key * root;
	ParentList * parentStack;
	Key * currKey;
	Key * prevKey;
	IndexList * indexStack;
	TableArrayList * tableArrayStack;
	CommentList * commentRoot;
	CommentList * commentBack;
	Scalar * lastScalar;
	char * filename;
	size_t order;
	size_t newlineCount;
	size_t currLine;
	bool simpleTableActive;
	bool drainCommentsOnKeyExit;
	bool errorSet;
} Driver;


/**
 * @brief Reads a TOML file into the given KeySet.
 *
 * @param keys KeySet used for writing the keys read from the TOML file.
 * @param parent The parent key of the mountpoint of the TOML file. Must contain the TOML filepath.
 *
 * @retval 0 If the file could be read successfully.
 * @retval non-zero If the file could not be opened or parsed without errors.
 */
int tomlRead (KeySet * keys, Key * parent);


/**
 * @brief Sets an error message on the mount parent key.
 *
 * Does not overwrite existing error messages on the parent key.
 *
 * @param driver Driver for which to set the error.
 * @param err Error number describing the type of the error.
 * @param lineno Linenumber in the TOML file on which the error was emitted. If zero, no line number will be written.
 * @param format Format string of the error message.
 * @param ... Varags for the format string.
 */
void driverError (Driver * driver, int err, int lineno, const char * format, ...);

/**
 * @brief Sets generic error message on the mount parent key.
 *
 * @param driver Driver for which to set the error.
 * @param err Error number describing the type of the error.
 * @param caller Name of function, which called the errorneous function.
 * @param callee Name of function causing the error.
 */
void driverErrorGeneric (Driver * driver, int err, const char * caller, const char * callee);


/**
 * @brief Called on exiting a Toml grammar rule in @link parser.y
 *
 * Writes the file trailing comments to the file parent key.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitToml (Driver * driver);


/**
 * @brief Called on entering a TopKey grammar rule in @link parser.y
 *
 * Sets the current key stored in the driver to the top element of the parent stack.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverEnterKey (Driver * driver);

/**
 * @brief Called on exiting a TopKey grammar rule in @link parser.y
 *
 * Checks, if the read Key is already in the KeySet and will emit an error on a duplicate key.
 * Pushes the read Key on the parent stack in the file, drains comment to the key and will set the order metakey for it.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitKey (Driver * driver);


/**
 * @brief Called on exiting a KeyPair grammar rule in @link parser.y
 *
 * Writes the last read scalar value to the current parent key. Updates the driver prev key variable to the last key.
 * Pops one element from the parent stack.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitKeyValue (Driver * driver);


/**
 * @brief Called after an KeyPair with optional comment rule in @link parser.y
 *
 * Will assign a possible inline comment to the last previous key in the driver.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 *
 * @retval
 */
void driverExitOptCommentKeyPair (Driver * driver);

/**
 * @brief Called after an Table with optional comment rule in @link parser.y
 *
 * Will assign a possible inline comment to the last previous key in the driver.
 * It will emit a table array index key to the KeySet, if we must the comment to a table array key.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitOptCommentTable (Driver * driver);


/**
 * @brief Called at the end of a SimpleKey grammar rule in @link parser.y
 *
 * SimpleKeys, as understood by the parser, are key names without any dots in them.
 * They get accumulated in the current key attribute of the driver during parsing.
 * Checks the scalar describing the key name for validity. Will also split up a possible Float scalar into two simple keys.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 * @param name Scalar which contains the key name.
 */
void driverExitSimpleKey (Driver * driver, Scalar * name);


/**
 * @brief Called after a Scalar was matched in the Value grammar rule in @link parser.y
 *
 * Checks, if a valid Scalar type was read. Will assigned the scalar to the lastScalar attribute of the driver.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 * @param scalar The Scalar value that was read from the file.
 */
void driverExitValue (Driver * driver, Scalar * scalar);


/**
 * @brief Called when entering a TableSimple grammar rule in @link parser.y
 *
 * Will pop the parent stack, if we are in a simple table already.
 * Resets the current key attribute in driver to the parent stack top.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverEnterSimpleTable (Driver * driver);

/**
 * @brief Called when leaving a TableSimple grammar rule in @link parser.y
 *
 * Adds a tomltype metakey of value "simpletable" the top parent stack key.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitSimpleTable (Driver * driver);

/**
 * @brief Called when entering a TableArray grammar rule in @link parser.y
 *
 * Will pop the parent stack and clear the simple table state, if we are in a simple table.
 * Will also pop the parent stack, if the table array stack is not empty.
 * Sets a driver attribute, to not drain comments to a key on exit.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverEnterTableArray (Driver * driver);

/**
 * @brief Called when exiting a TableArray grammar rule in @link parser.y
 *
 * Builds a keyname with correct table array indices, based on the table array stack, that will be pushed onto the parent stack.
 * Adds the appropriate tomltype metakey to that key.
 * Drains comments to that key, and - if comments were present - adds the table array key to the KeySet.
 * Sets a driver attribute, to allow draining of comments to keys on exit again.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 *
 * @retval
 */
void driverExitTableArray (Driver * driver);


/**
 * @brief Called when entering the non-empty Array rule in @link parser.y
 *
 * Pushes an index with initial value zero onto the index stack of the driver.
 * Assigns a metakey array with empty value to the parent stack top key.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 *
 * @retval
 */
void driverEnterArray (Driver * driver);


/**
 * @brief Called when exiting the non-empty Array rule in @link parser.y
 *
 * Handles remaining comments in the array and will pop the index stack.
 * Adds the top parent stack key (the Key of the array) into the KeySet.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitArray (Driver * driver);


/**
 * @brief Called in the ArrayEmpty grammar rule in @link parser.y
 *
 * Calls the driverEnterArray and driverExitArray functions.
 * Does nothing if an error was set.
 *
 * @param driver Used drivcer.
 */
void driverEmptyArray (Driver * driver);


/**
 * @brief Called when entering the ArrayElement rule in @link parser.y
 *
 * Assigns comments to the appropriate array element keys.
 * Pushes an array index key onto the parent stack, based on the top index stack value (which will be incremented afterwards) of the driver.
 * Updates the array root metakey array to the new highest index value.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverEnterArrayElement (Driver * driver);


/**
 * @brief Called when leaving the ArrayElement rule in @link parser.y
 *
 * Assigns the last read scalar of the driver to the top parent key.
 * Pops the parent key stack.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitArrayElement (Driver * driver);


/**
 * @brief Called when entering the non-empty InlineTable grammar rule in @link parser.y
 *
 * Adds a tomltype metakey of value "inlinetable" to the top parent key.
 * Adds that key to the KeySet.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverEnterInlineTable (Driver * driver);


/**
 * @brief Called when leaving the non-empty InlineTable grammar rule in @link parser.y
 *
 * Clears the last scalar stored in the driver.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitInlineTable (Driver * driver);


/**
 * @brief Called when leaving the empty InlineTable grammar rule in @link parser.y
 *
 * Calls driverEnterInlineTable.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverEmptyInlineTable (Driver * driver);


/**
 * @brief Called after COMMENT terminal symbols in @link parser.y
 *
 * Appends the given scalar to the comment list stored in the driver.
 * Will also add newline entry before the comment entry, if there were any newlines counted by the driver.
 * Does nothing if an error was set.
 *
 * @param driver Uses driver.
 * @param comment Scalar containing the comment string. Will be freed by the function.
 */
void driverExitComment (Driver * driver, Scalar * comment);


/**
 * @brief Called when encountering a NEWLINE terminal symbol in @parser.y that should be counted
 *
 * Increments the newline counter in the driver.
 * Does nothing if an error was set.
 *
 * @param driver Used driver.
 */
void driverExitNewline (Driver * driver);


#endif // ELEKTRA_PLUGIN_TOML_DRIVER_H
