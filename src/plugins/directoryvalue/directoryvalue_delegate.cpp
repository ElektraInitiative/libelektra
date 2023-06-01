/**
 * @file
 *
 * @brief Delegate implementation for the `directoryvalue` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <numeric>

#include <elektra/core/errors.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>

#include <kdbplugin.hpp>

#include "./directoryvalue_delegate.hpp"
#include "./log.hpp"

using std::accumulate;
using std::ignore;
using std::make_pair;
using std::pair;
using std::range_error;
using std::string;
using std::tie;

using KeySetPair = pair<kdb::KeySet, kdb::KeySet>;

#include <elektra/ease/array.h>
#include <elektra/ease/name.h>


// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace
{

/**
 * @brief This function splits the given keyset into directory leaves (marked with `DIRECTORY_POSTFIX`) and other keys.
 *
 * @param input The function searches for directory leaves in this key set.
 *
 * @return A pair of key sets, where the first key set contains all directory leaves and the second key set contains all other keys
 */
KeySetPair splitDirectoryLeavesOther (kdb::KeySet const & input)
{
	kdb::KeySet directoryLeaves;
	kdb::KeySet other;

	for (auto key : input)
	{
		if (key.getBaseName () == DIRECTORY_POSTFIX)
		{
			directoryLeaves.append (key);
		}
		else
		{
			other.append (key);
		}
	}
	return make_pair (directoryLeaves, other);
}

/**
 * @brief This function splits the given keyset into array leaves (first element of an array parent) and other keys.
 *
 * @param input The function searches for array leaves in this key set.
 *
 * @return A pair of key sets, where the first key set contains all array leaves and the second key set contains all other keys
 */
KeySetPair splitArrayLeavesOther (kdb::KeySet const & arrayParents, kdb::KeySet const & keys)
{
	bool isFirstElement = false;
	kdb::KeySet firstElements;
	kdb::KeySet others;

	for (auto key : keys)
	{
		bool const isArrayLeaf = isFirstElement && key.isString () && key.getStringSize () > arrayValuePrefixSize &&
					 strncmp (key.getString ().c_str (), ARRAY_VALUE_PREFIX, arrayValuePrefixSize) == 0;
		(isArrayLeaf ? firstElements : others).append (key);
		isFirstElement = arrayParents.lookup (key);
	}

	return make_pair (firstElements, others);
}

/**
 * @brief This function removes the basename (last level) from the given keys.
 *
 * @param keys This parameter contains the keys for which this function removes the basename.
 *
 * @return A copy of the input, where the last level of each key was removed
 */
kdb::KeySet removeBaseName (kdb::KeySet const & keys)
{
	kdb::KeySet directories;

	for (auto key : keys)
	{
		ELEKTRA_LOG_DEBUG ("Remove basename from “%s”: “%s”", key.getName ().c_str (),
				   key.getBinarySize () == 0 ? "NULL" :
				   key.isBinary ()	     ? "binary value!" :
							       key.getString ().c_str ());
		kdb::Key directory = key.dup ();
		directory.delBaseName ();
		directories.append (directory);
	}
	return directories;
}

/**
 * @brief This function converts the given array leaves to directory keys.
 *
 * @param arrayLeaves This parameter contains array leaves (that start with `ARRAY_VALUE_PREFIX` and have the baseName = `#0`).
 *
 * @return A copy of `arrayLeaves`, where key values does not start with `ARRAY_VALUE_PREFIX` any more and the baseName of each key (`#0`)
 *         was removed
 */
kdb::KeySet convertArrayLeaves (kdb::KeySet const & arrayLeaves)
{
	kdb::KeySet directories = removeBaseName (arrayLeaves);
	for (auto key : directories)
	{
		ELEKTRA_LOG_DEBUG ("Convert array leaf “%s”", key.getName ().c_str ());
		if (key.getStringSize () == arrayValuePrefixSize + 1)
		{
			key.setBinary (0, 0);
			ELEKTRA_LOG_DEBUG ("Set value of “%s” to NULL", key.getName ().c_str ());
		}
		else
		{
			key.setString (key.getString ().substr (arrayValuePrefixSize + 1));
			ELEKTRA_LOG_DEBUG ("Set value of “%s” to “%s”", key.getName ().c_str (), key.getString ().c_str ());
		}
	}
	return directories;
}

/**
 * @brief This function returns a modified copy of `child`, where child is directly below `parent`.
 *
 * For example, if `child` has the name `user:/parent/level1/level2/level3` and parent has the name `user:/parent`, then the function will
 * return a key with the name `user:/parent/level1`.
 *
 * @pre The key `child` has to be below `parent`.
 *
 * @param parent This parameter specifies a parent key of `child`.
 * @param keys This variable stores a child key of `parent`.
 *
 * @return A copy of `child` that is directly below `parent`
 */
kdb::Key convertToDirectChild (kdb::Key const & parent, kdb::Key const & child)
{
	ELEKTRA_ASSERT (child.isBelow (parent), "The key `child` is not located below `parent`");

	kdb::Key directChild = child.dup ();
	while (!directChild.isDirectBelow (parent))
	{
		keySetBaseName (*directChild, 0);
	}
	return directChild;
}

/**
 * @brief Return all array parents of the given key set.
 *
 * @param keys This parameter contains the key set for which this function determines all array parent keys.
 *
 * @return A key set containing all array parents of `keys`
 */
kdb::KeySet getArrayParents (kdb::KeySet const & keys)
{
	kdb::KeySet arrayParents;

	for (auto const & key : keys)
	{
		if (key.hasMeta ("array")) arrayParents.append (key);
	}

#ifdef HAVE_LOGGER
	ELEKTRA_LOG_DEBUG ("Array parents:");
	logKeySet (arrayParents);
#endif

	return arrayParents;
}

/**
 * @brief This function splits `keys` into two key sets, one for array parents and elements, and the other one for all other keys.
 *
 * @param arrayParents This key set contains a (copy) of all array parents of `keys`.
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all array parents and elements,
 *         and the second key set contains all other keys
 */
KeySetPair splitArrayOther (kdb::KeySet const & arrayParents, kdb::KeySet const & keys)
{
	kdb::KeySet others = keys.dup ();
	kdb::KeySet arrays;

	for (auto parent : arrayParents)
	{
		arrays.append (others.cut (parent));
	}

	return make_pair (arrays, others);
}

/**
 * @brief This function splits `keys` into two key sets, one for empty array parents that do not contain a value and one for all other keys.
 *
 * @param arrayParents This key set contains array parents.
 *
 * @return A pair of key sets, where the first key set contains all array parents without values, and the second key set contains all other
 *         keys
 */
KeySetPair splitEmptyArrayParents (kdb::KeySet const & arrayParents)
{
	kdb::KeySet emptyParents;
	kdb::KeySet nonEmptyParents;

	for (auto arrayParent : arrayParents)
	{
		if (arrayParent.getBinarySize () != 0 || !arrayParent.hasMeta ("array"))
		{
			// has value, or is not actually array parent -> non-empty
			nonEmptyParents.append (arrayParent);
			continue;
		}


		/* TODO: Using ksGetSize() instead of keyNextMeta() to determine the number of metakeys produces
		 * additional "___dirdata:" entries when using the yajl-plugin with arrays! */
		// ckdb::KeySet * metaKeys = ckdb::keyMeta (arrayParent.getKey ());
		// ssize_t metaKeysSizeWithKsGetSize = ckdb::ksGetSize (metaKeys);

		ssize_t metaKeysSize = 0;
		while (ckdb::keyNextMeta (arrayParent.getKey ()))
			metaKeysSize++;

		if (arrayParent.hasMeta ("binary") && metaKeysSize > 2)
		{
			// has meta:/binary and at least 2 other metakeys (incl. meta:/array) -> non-empty
			nonEmptyParents.append (arrayParent);
			continue;
		}

		if (metaKeysSize > 1)
		{
			// no meta:/binary, but at least 2 other metakeys (incl. meta:/array) -> non-empty
			nonEmptyParents.append (arrayParent);
			continue;
		}

		// only one metakey, which must be meta:/array -> empty
		emptyParents.append (arrayParent);
	}

#ifdef HAVE_LOGGER
	ELEKTRA_LOG_DEBUG ("Empty array parents:");
	logKeySet (emptyParents);
#endif

	return make_pair (emptyParents, nonEmptyParents);
}

/**
 * @brief This function changes an array index of the given array element by one.
 *
 * @param parent This key stores an array parent of `element`. The function will change the index of the `element` that is directly below
 *		 this key.
 * @param element This parameter stores an array element.
 * @param increment This boolean parameter specifies if the function should increase or decrease the index by one.
 *
 * @return A key containing a copy of `element`, where the index below `parent` was increased or decreased by one and the value of the
 *         updated index (e.g. `#_10`)
 */
pair<kdb::Key, string> changeArrayIndexByOne (kdb::Key const & parent, kdb::Key const & element, bool increment = true)
{
	kdb::Key elementNewIndex = convertToDirectChild (parent, element);
	string postfix = elektraKeyGetRelativeName (*element, *elementNewIndex);

	if (increment ? elektraArrayIncName (*elementNewIndex) : elektraArrayDecName (*elementNewIndex))
	{
		throw range_error (string ("Unable to ") + (increment ? "increment" : "decrement") + " index of key “" +
				   elementNewIndex.getName () + "”");
	}
	string newIndex = elementNewIndex.getBaseName ();
	elementNewIndex.addName (postfix);

	ELEKTRA_LOG_DEBUG ("New name of “%s” is “%s”", element.getName ().c_str (), elementNewIndex.getName ().c_str ());

	return make_pair (elementNewIndex, newIndex);
}

/**
 * @brief Decrease the array index of array elements by one.
 *
 * @param parents This parameter contains the array parents for which this function decrease the index by one.
 * @param arrays This variable stores the arrays elements (and parents) this function updates.
 *
 * @return A copy of `arrays`, where all indices specified by `parents` are decreased by one
 */
kdb::KeySet decreaseArrayIndices (kdb::KeySet const & parents, kdb::KeySet const & arrays)
{
	kdb::KeySet arraysIndexDecreased = arrays.dup ();
	kdb::KeySet arrayParents = parents.dup ();

	while (kdb::Key parent = arrayParents.pop ())
	{
		ELEKTRA_LOG_DEBUG ("Decrease indices for array parent “%s”", parent.getName ().c_str ());

		parent.setMeta ("array", ""); // Set meta key for empty arrays

		arraysIndexDecreased =
			accumulate (arraysIndexDecreased.begin (), arraysIndexDecreased.end (), kdb::KeySet{},
				    [&parent] (kdb::KeySet collected, kdb::Key key) {
					    if (key.isBelow (parent))
					    {
						    string newIndex;
						    tie (key, newIndex) = changeArrayIndexByOne (parent, key, false);
						    parent.setMeta ("array", newIndex);
						    ELEKTRA_LOG_DEBUG ("New last index of “%s” is “%s”", parent.getName ().c_str (),
								       parent.getMeta<string> ("array").c_str ());
					    }
					    collected.append (key);
					    return collected;
				    });

		arraysIndexDecreased.append (parent); // Update meta data of parent key in `arrays`
	}

	return arraysIndexDecreased;
}

/**
 * @brief Split `keys` into two key sets, one for directories (keys without children) and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all directories and the second key set contains all leaves
 */
KeySetPair splitDirectoriesLeaves (kdb::KeySet const & keys)
{

	kdb::KeySet leaves;
	kdb::KeySet directories;

	kdb::Key previous = keys.at (0);
	for (elektraCursor it = 1; it < keys.size (); ++it)
	{
		kdb::Key current = keys.at (it);


		if (current.isBelow (previous))
		{
			directories.append (previous);
		}
		else
		{
			leaves.append (previous);
		}
		previous = current;
	}
	leaves.append (previous);
	return make_pair (directories, leaves);
}

/**
 * @brief Convert all keys in `parents` to an empty array parent and an array element with index `#0` storing the data of the old key.
 *
 * @param parents This parameter contains the set of array parent this function converts.
 *
 * @return A key set containing only empty array parents and corresponding array elements storing the values of the old array parent
 */
kdb::KeySet convertArrayParentsToLeaves (kdb::KeySet const & parents)
{
	kdb::KeySet converted;

	ELEKTRA_LOG_DEBUG ("Convert array parents to leaves");

	for (auto parent : parents)
	{
		kdb::Key directory{ parent.getName (), KEY_END };
		// The plugin still stores the `array` metadata in the parent and not the first child. Otherwise storage plugins pick up
		// the wrong key as parent.
		kdb::Key lastElement{ parent.getName (), KEY_END };
		auto secondToLastIndex = parent.getMeta<string> ("array");
		if (secondToLastIndex.empty ()) secondToLastIndex += "#";
		lastElement.addBaseName (secondToLastIndex);
		elektraArrayIncName (*lastElement);
		directory.setMeta ("array", lastElement.getBaseName ());
		kdb::Key leaf = parent.dup ();
		leaf.delMeta ("array");
		leaf.addBaseName ("#0");
		leaf.setString (ARRAY_VALUE_PREFIX + (parent.isBinary () ? "" : (" " + parent.getString ())));
		converted.append (directory);
		converted.append (leaf);
	}

#ifdef HAVE_LOGGER
	ELEKTRA_LOG_DEBUG ("Converted keys:");
	logKeySet (converted);
#endif

	return converted;
}

/**
 * @brief Convert all keys in `directories` to an empty key and a leaf key containing the data of the old key.
 *
 * @param directories This parameter contains a set of directory keys this function converts.
 *
 * @return A key set containing only empty directory keys and corresponding leaf keys storing the values of the old directory keys
 */
kdb::KeySet convertDirectoriesToLeaves (kdb::KeySet const & directories)
{
	kdb::KeySet directoryLeaves;

	for (auto directory : directories)
	{
		kdb::Key emptyDirectory{ directory.getName (), KEY_END };
		kdb::Key leaf = directory.dup ();
		leaf.addBaseName (DIRECTORY_POSTFIX);
		directoryLeaves.append (leaf);
		directoryLeaves.append (emptyDirectory);
	}

	return directoryLeaves;
}

} // end namespace

namespace elektra
{

/**
 * @brief Increase the array index of array elements by one.
 *
 * Since it is also possible that one of the array parents is part of another array, this function also updates the indices of the given
 * array parents.
 *
 * @param parents This parameter contains the array parents for which this function increases the index by one.
 * @param arrays This variable stores the arrays elements this function updates.
 *
 * @return A pair containing a copy of `parents` and `arrays`, where all indices specified by `parents` are increased by one
 */
KeySetPair increaseArrayIndices (kdb::KeySet const & parents, kdb::KeySet const & arrays)
{
	kdb::KeySet arraysIncreasedIndex = arrays.dup ();
	kdb::KeySet arrayParents = parents.dup ();
	kdb::KeySet updatedParents = parents.dup ();

	while (kdb::Key parent = arrayParents.pop ())
	{
		ELEKTRA_LOG_DEBUG ("Increase indices for array parent “%s”", parent.getName ().c_str ());

		kdb::KeySet newArrays;
		for (auto key : arraysIncreasedIndex)
		{
			if (key.isBelow (parent))
			{
				kdb::Key updated;
				tie (updated, ignore) = changeArrayIndexByOne (parent, key);
				if (updatedParents.lookup (key, KDB_O_POP)) updatedParents.append (updated);
				newArrays.append (updated);
			}
			else
			{
				newArrays.append (key);
			}
		}
		arraysIncreasedIndex = newArrays;
	}

	return make_pair (updatedParents, arraysIncreasedIndex);
}

// -- Class --------------------------------------------------------------------------------------------------------------------------------

/**
 * @brief This constructor creates a new delegate object used by the `directoryvalue` plugin
 *
 * @param config This key set contains configuration values provided by the `directoryvalue` plugin
 */
DirectoryValueDelegate::DirectoryValueDelegate (kdb::KeySet config ELEKTRA_UNUSED)
{
}

/**
 * @brief This method converts all leaf keys in the given key set to directory keys.
 *
 * @param keys This parameter specifies the key set this function converts.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS If the plugin converted any value in the given key set
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE If the plugin did not update `keys`
 */
int DirectoryValueDelegate::convertToDirectories (kdb::KeySet & keys)
{
	kdb::KeySet directoryLeaves;
	kdb::KeySet nonDirectoryLeaves;
	kdb::KeySet arrayParents;
	kdb::KeySet notArrayParents;
	kdb::KeySet arrays;
	kdb::KeySet arrayLeaves;
	kdb::KeySet maps;

	ELEKTRA_LOG_DEBUG ("Convert (special) leaf keys to directory keys");

	/**
	 * - Split array parents
	 * - Split first array child containing directory value prefix, others
	 * - Convert first array child back to array parent
	 * - Decrease index of arrays
	 * - Merge everything back together and convert directories to leaves
	 */

	arrayParents = getArrayParents (keys);
	tie (arrays, maps) = splitArrayOther (arrayParents, keys);
	tie (arrayLeaves, arrays) = splitArrayLeavesOther (arrayParents, arrays);

	arrayParents = convertArrayLeaves (arrayLeaves);
	notArrayParents = decreaseArrayIndices (arrayParents, arrays);
	notArrayParents.append (maps);

	tie (directoryLeaves, nonDirectoryLeaves) = splitDirectoryLeavesOther (notArrayParents);

	bool const status =
		directoryLeaves.size () > 0 || arrayLeaves.size () > 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	auto directories = removeBaseName (directoryLeaves);

	keys.clear ();
	keys.append (nonDirectoryLeaves);
	keys.append (directories);

	return status;
}

/**
 * @brief This method converts all directory keys in the given key set to leaf keys.
 *
 * @param keys This parameter specifies the key set this function converts.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS If the plugin converted any value in the given key set
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE If the plugin did not update `keys`
 */
int DirectoryValueDelegate::convertToLeaves (kdb::KeySet & keys)
{
	kdb::KeySet notArrayParents;
	kdb::KeySet arrayParents;
	kdb::KeySet emptyArrayParents;
	kdb::KeySet arrays;
	kdb::KeySet nonArrays;
	kdb::KeySet directories;
	kdb::KeySet leaves;

	ELEKTRA_LOG_DEBUG ("Convert directory keys to leaf keys");

	/*
	 * - Determine array parents
	 * - Split array keys and other keys
	 * - Split empty arrays
	 * - Increase array indices
	 */

	arrayParents = getArrayParents (keys);
	tie (arrays, nonArrays) = splitArrayOther (arrayParents, keys);

	tie (emptyArrayParents, arrayParents) = splitEmptyArrayParents (arrayParents);
	tie (arrayParents, arrays) = increaseArrayIndices (arrayParents, arrays);

	notArrayParents.append (arrays);
	notArrayParents.append (nonArrays);
	notArrayParents = accumulate (notArrayParents.begin (), notArrayParents.end (), kdb::KeySet{},
				      [&arrayParents, &emptyArrayParents] (kdb::KeySet collected, kdb::Key key) {
					      if (!arrayParents.lookup (key) && !emptyArrayParents.lookup (key)) collected.append (key);
					      return collected;
				      });
	arrayParents = convertArrayParentsToLeaves (arrayParents);

	tie (directories, leaves) = splitDirectoriesLeaves (notArrayParents);
	bool const status =
		directories.size () > 0 || arrayParents.size () > 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	auto directoryLeaves = convertDirectoriesToLeaves (directories);

	keys.clear ();
	keys.append (arrays);
	keys.append (arrayParents);
	keys.append (emptyArrayParents);
	keys.append (directoryLeaves);
	keys.append (leaves);

	return status;
}

} // end namespace elektra
