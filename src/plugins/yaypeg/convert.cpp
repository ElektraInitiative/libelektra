/**
 * @file
 *
 * @brief This file contains a function to convert a YAML file to a key set.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include "convert.hpp"
#include "listener.hpp"
#include "parser.hpp"
#include "state.hpp"
#include "walk.hpp"

#define TAO_PEGTL_NAMESPACE yaypeg

#include <tao/pegtl/contrib/parse_tree.hpp>

namespace yaypeg
{

using kdb::Key;
using kdb::KeySet;
using std::string;

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
 * @retval -1 if there was an error converting the YAML file
 * @retval  0 if parsing was successful and the function did not change the
 *            given keyset
 * @retval  1 if parsing was successful and the function did change `keySet`
 */
int addToKeySet (KeySet & keySet, Key & parent, string const & filename)
{
	using std::runtime_error;
	using tao::TAO_PEGTL_NAMESPACE::analyze;
	using tao::TAO_PEGTL_NAMESPACE::file_input;
	using tao::TAO_PEGTL_NAMESPACE::parse_error;
	using tao::TAO_PEGTL_NAMESPACE::parse_tree::parse;
	using TAO_PEGTL_NAMESPACE::errors;

	State state;

#if DEBUG
	// Check grammar for problematic code
	if (analyze<yaml> () != 0)
	{
		throw runtime_error ("PEGTL’s analyze function found problems while checking the top level grammar rule `yaml`!");
		return -1;
	}
#endif

	file_input<> input{ filename };
	auto root = parse<yaml, selector, action, errors> (input, state);

	Listener listener{ parent };
	walk (listener, *root);
	auto keys = listener.getKeySet ();

	int status = (keys.size () <= 0) ? 0 : 1;

	keySet.append (keys);

	return status;
}

} // namespace yaypeg
