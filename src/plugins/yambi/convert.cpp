/**
 * @file
 *
 * @brief This file contains a function to convert a YAML file to a key set.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include "kdberrors.h"

#include "convert.hpp"
#include "driver.hpp"

using std::string;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

// -- Function -----------------------------------------------------------------

/**
 * @brief This function converts the given YAML file to keys and adds the
 *        result to `keySet`.
 *
 * @param keySet The function adds the converted keys to this variable.
 * @param parent The function uses this parent key of `keySet` to emit error
 *               information.
 * @param filename This parameter stores the path of the YAML file this
 *                 function converts.
 *
 * @retval -3 if the file could not be opened for reading
 * @retval -2 if parsing was unsuccessful due to memory exhaustion
 * @retval -1 if there was an syntax error converting the YAML file
 * @retval  0 if parsing was successful and the function did not change the given keyset
 * @retval  1 if parsing was successful and the function did change `keySet`
 */
int addToKeySet (CppKeySet & keySet, CppKey & parent, string const & filename)
{
	Driver driver{ parent };

	int status = driver.parse (filename);

	if (status < 0)
	{
		if (status == -3)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parent.getKey (), "Unable to open file '%s'", filename.c_str ());
		}
		else if (status == -2)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parent.getKey (), "Parsing failed due to memory exhaustion");
		}
		else if (status == -1)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parent.getKey (), driver.getErrorMessage ().c_str ());
		}

		return status;
	}

	CppKeySet keys = driver.getKeySet ();
	status = (keys.size () <= 0) ? 0 : 1;

	keySet.append (keys);

	return status;
}
