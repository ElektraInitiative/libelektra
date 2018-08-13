/**
 * @file
 *
 * @brief Delegate implementation for the `leaf` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <numeric>

#include <kdbassert.h>
#include <kdbease.h>
#include <kdblogger.h>

#include "leaf_delegate.hpp"

using std::accumulate;
using std::ignore;
using std::make_pair;
using std::pair;
using std::range_error;
using std::string;
using std::tie;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

namespace elektra
{
using CppKey = kdb::Key;

/**
 * @brief This function splits the given keyset into directory leaves (marked with `DIRECTORY_POSTFIX`) and other keys.
 *
 * @param input The function searches for directory leaves in this key set.
 *
 * @return A pair of key sets, where the first key set contains all directory leaves and the second key set contains all other keys
 */
pair<CppKeySet, CppKeySet> splitDirectoryLeavesOther (CppKeySet const & input)
{
	CppKeySet directoryLeaves;
	CppKeySet other;

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
pair<CppKeySet, CppKeySet> splitArrayLeavesOther (CppKeySet const & arrayParents, CppKeySet const & keys)
{
	bool isFirstElement = false;
	CppKeySet firstElements;
	CppKeySet others;

	for (auto key : keys)
	{
		(isFirstElement ? firstElements : others).append (key);
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
CppKeySet removeBaseName (CppKeySet const & keys)
{
	CppKeySet directories;

	for (auto key : keys)
	{
		CppKey directory = key.dup ();
		keySetBaseName (*directory, 0);
		directories.append (directory);
	}
	return directories;
}

/**
 * @brief This function returns a modified copy of `child`, where child is directly below `parent`.
 *
 * For example, if `child` has the name `user/parent/level1/level2/level3` and parent has the name `user/parent`, then the function will
 * return a key with the name `user/parent/level1`.
 *
 * @pre The key `child` has to be below `parent`.
 *
 * @param parent This parameter specifies a parent key of `child`.
 * @param keys This variable stores a child key of `parent`.
 *
 * @return A copy of `child` that is directly below `parent`
 */
CppKey convertToDirectChild (CppKey const & parent, CppKey const & child)
{
	ELEKTRA_ASSERT (child.isBelow (parent), "The key `child` is not located below `parent`");

	CppKey directChild = child.dup ();
	while (!directChild.isDirectBelow (parent))
	{
		keySetBaseName (*directChild, 0);
	}
	return directChild;
}

/**
 * @brief This function determines if the given key is an array parent.
 *
 * @param parent This parameter specifies a possible array parent.
 * @param keys This variable stores the key set of `parent`.
 *
 * @retval true If `parent` is the parent key of an array
 * @retval false Otherwise
 */
bool isArrayParent (CppKey const & parent, CppKeySet const & keys)
{
	CppKeySet children = accumulate (keys.begin (), keys.end (), CppKeySet{}, [&parent](CppKeySet collected, CppKey key) {
		if (key.isBelow (parent)) collected.append (key);
		return collected;
	});

	for (auto child : children)
	{
		CppKey directChild = convertToDirectChild (parent, child);
		if (elektraArrayValidateName (*directChild) < 0) return false;
	}

	return true;
}

/**
 * @brief Split `keys` into two key sets, one for array parents and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all array parents and the second key set contains all other keys
 */
pair<CppKeySet, CppKeySet> splitArrayParentsOther (CppKeySet const & keys)
{
	CppKeySet arrayParents;
	CppKeySet others;

	keys.rewind ();
	CppKey previous;
	for (previous = keys.next (); keys.next (); previous = keys.current ())
	{
		bool previousIsArray =
			previous.hasMeta ("array") ||
			(keys.current ().isBelow (previous) && keys.current ().getBaseName ()[0] == '#' && isArrayParent (previous, keys));

		(previousIsArray ? arrayParents : others).append (previous);
	}
	(previous.hasMeta ("array") ? arrayParents : others).append (previous);

	ELEKTRA_ASSERT (arrayParents.size () + others.size () == keys.size (),
			"Number of keys in split key sets: %zu ≠ number in given key set %zu", arrayParents.size () + others.size (),
			keys.size ());

	return make_pair (arrayParents, others);
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
pair<CppKeySet, CppKeySet> splitArrayOther (CppKeySet const & arrayParents, CppKeySet const & keys)
{
	CppKeySet others = keys.dup ();
	CppKeySet arrays;

	for (auto parent : arrayParents)
	{
		arrays.append (others.cut (parent));
	}

	return make_pair (arrays, others);
}

/**
 * @brief This function changes an array index of the given array element by one.
 *
 * @param parent This key set stores an array parent of `element`. The function will change the index of `element` that is directly below
 *               this key.
 * @param element This parameter stores an array element.
 * @param increment This boolean parameter specifies if the function should increase or decrease the index by one.
 *
 * @return A copy of `element`, where the index below `parent` was increased or decreased by one
 */
CppKey changeArrayIndexByOne (CppKey const & parent, CppKey const & element, bool increment = true)
{
	CppKey elementNewIndex = convertToDirectChild (parent, element);
	string postfix = elektraKeyGetRelativeName (*element, *elementNewIndex);

	if (increment ? elektraArrayIncName (*elementNewIndex) : elektraArrayDecName (*elementNewIndex))
	{
		throw range_error (string ("Unable to ") + (increment ? "increment" : "decrement") + " index of key “" +
				   elementNewIndex.getName () + "”");
	}
	elementNewIndex.addName (postfix);

	ELEKTRA_LOG_DEBUG ("New name of “%s” is “%s”", element.getName ().c_str (), elementNewIndex.getName ().c_str ());

	return elementNewIndex;
}

/**
 * @brief Decrease the array index of array elements by one.
 *
 * @param parents This parameter contains the array parents for which this function decrease the index by one.
 * @param arrays This variable stores the arrays elements (and parents) this function updates.
 *
 * @return A copy of `arrays`, where all indices specified by `parents` are decreased by one
 */
CppKeySet decreaseArrayIndices (CppKeySet const & parents, CppKeySet const & arrays)
{
	CppKeySet arraysIndexDecreased = arrays.dup ();
	CppKeySet arrayParents = parents.dup ();

	while (CppKey parent = arrayParents.pop ())
	{
		arraysIndexDecreased = accumulate (arraysIndexDecreased.begin (), arraysIndexDecreased.end (), CppKeySet{},
						   [&parent](CppKeySet collected, CppKey key) {
							   if (key.isBelow (parent)) changeArrayIndexByOne (parent, key, false);
							   collected.append (key);
							   return collected;
						   });
	}

	return arraysIndexDecreased;
}

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
pair<CppKeySet, CppKeySet> increaseArrayIndices (CppKeySet const & parents, CppKeySet const & arrays)
{
	CppKeySet arraysIncreasedIndex = arrays.dup ();
	CppKeySet arrayParents = parents.dup ();
	CppKeySet updatedParents = parents.dup ();

	while (CppKey parent = arrayParents.pop ())
	{
		ELEKTRA_LOG_DEBUG ("Increase indices for array parent “%s”", parent.getName ().c_str ());

		CppKeySet newArrays;
		for (auto key : arraysIncreasedIndex)
		{
			if (key.isBelow (parent))
			{
				auto updated = changeArrayIndexByOne (parent, key);
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

/**
 * @brief Split `keys` into two key sets, one for directories (keys without children) and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all directories and the second key set contains all leaves
 */
pair<CppKeySet, CppKeySet> splitDirectoriesLeaves (CppKeySet const & keys)
{
	CppKeySet leaves;
	CppKeySet directories;

	keys.rewind ();
	CppKey previous;
	for (previous = keys.next (); keys.next (); previous = keys.current ())
	{
		if (keys.current ().isBelow (previous))
		{
			directories.append (previous);
		}
		else
		{
			leaves.append (previous);
		}
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
CppKeySet convertArrayParentsToLeaves (CppKeySet const & parents)
{
	CppKeySet converted;

	for (auto parent : parents)
	{
		CppKey directory{ parent.getName (), KS_END };
		CppKey leaf = parent.dup ();
		leaf.addBaseName ("#0");
		converted.append (directory);
		converted.append (leaf);
	}

	return converted;
}

/**
 * @brief Convert all keys in `directories` to an empty key and a leaf key containing the data of the old key.
 *
 * @param directories This parameter contains a set of directory keys this function converts.
 *
 * @return A key set containing only empty directory keys and corresponding leaf keys storing the values of the old directory keys
 */
CppKeySet convertDirectoriesToLeaves (CppKeySet const & directories)
{
	CppKeySet directoryLeaves;

	for (auto directory : directories)
	{
		CppKey emptyDirectory{ directory.getName (), KS_END };
		CppKey leaf = directory.dup ();
		leaf.addBaseName (DIRECTORY_POSTFIX);
		directoryLeaves.append (leaf);
		directoryLeaves.append (emptyDirectory);
	}

	return directoryLeaves;
}

// -- Class --------------------------------------------------------------------------------------------------------------------------------

/**
 * @brief This constructor creates a new delegate object used by the `leaf` plugin
 *
 * @param config This key set contains configuration values provided by the `leaf` plugin
 */
LeafDelegate::LeafDelegate (CppKeySet config ELEKTRA_UNUSED)
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
int LeafDelegate::convertToDirectories (CppKeySet & keys)
{
	CppKeySet directoryLeaves;
	CppKeySet nonDirectoryLeaves;
	CppKeySet arrayParents;
	CppKeySet notArrayParents;
	CppKeySet arrays;
	CppKeySet arrayLeaves;
	CppKeySet maps;

	/**
	 * - Split array parents
	 * - Split first array child containing directory value prefix, others
	 * - Convert first array child back to array parent
	 * - Decrease index of arrays
	 * - Merge everything back together and convert directories to leaves
	 */

	tie (arrayParents, notArrayParents) = splitArrayParentsOther (keys);
	tie (arrays, maps) = splitArrayOther (arrayParents, keys);
	tie (arrayLeaves, arrays) = splitArrayLeavesOther (arrayParents, arrays);

	arrayParents = removeBaseName (arrayLeaves);

	tie (directoryLeaves, nonDirectoryLeaves) = splitDirectoryLeavesOther (notArrayParents);

	bool status = directoryLeaves.size () > 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

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
int LeafDelegate::convertToLeaves (CppKeySet & keys)
{
	CppKeySet notArrayParents;
	CppKeySet arrayParents;
	CppKeySet arrays;
	CppKeySet nonArrays;
	CppKeySet directories;
	CppKeySet leaves;

	tie (arrayParents, ignore) = splitArrayParentsOther (keys);
	tie (arrays, nonArrays) = splitArrayOther (arrayParents, keys);

	tie (arrayParents, arrays) = increaseArrayIndices (arrayParents, arrays);
	notArrayParents.append (arrays);
	notArrayParents.append (nonArrays);
	notArrayParents = accumulate (notArrayParents.begin (), notArrayParents.end (), CppKeySet{},
				      [&arrayParents](CppKeySet collected, CppKey key) {
					      if (!arrayParents.lookup (key)) collected.append (key);
					      return collected;
				      });
	arrayParents = convertArrayParentsToLeaves (arrayParents);

	tie (directories, leaves) = splitDirectoriesLeaves (notArrayParents);
	bool status = directories.size () > 0 || arrayParents.size () > 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	auto directoryLeaves = convertDirectoriesToLeaves (directories);

	keys.clear ();
	keys.append (arrays);
	keys.append (arrayParents);
	keys.append (directoryLeaves);
	keys.append (leaves);

	return status;
}

} // end namespace elektra
