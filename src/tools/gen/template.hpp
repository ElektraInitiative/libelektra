#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
@from util import util
@from cpp_util import cpp_util
@from cpp_support import *
@set support = CppSupport()
$util.header($args.output)
#include "kdb.hpp"
#include "kdbtypes.h"

#include <string>

namespace kdb
{

$cpp_util.generateenum($support, $parameters)

$cpp_util.generatebool($support)

class Parameters
{
public:

	/** \brief Constructor
	 * \param ks the keyset to work with.
	 */
	Parameters(kdb::KeySet & ks) : ks(ks)
	{}

@for $key, $info in $parameters.items()
	$support.typeof(info) $support.getfuncname($key)() const;
	void $support.setfuncname($key)($support.typeof(info) n);
@end for

private:
	kdb::KeySet &ks;
};

@for $key, $info in $parameters.items()
/** \brief Get parameter $key
 *
 * $util.doxygen(support, key, info)
 *
 * \see $support.setfuncname($key)
 *
 * \return the value of the parameter, default if it could not be found
 */
inline $support.typeof(info) Parameters::$support.getfuncname($key)() const
{
@if $len(support.override(info)) > 0
	// override
	kdb::Key found = ks.lookup("${support.override(info)[0]}", 0);
@for $o in $support.override(info)[1:]
	if (!found)
	{
		found = ks.lookup("$o", 0);
	}
@end for
	// now the key itself
	if(!found)
	{
		found = ks.lookup("$key", 0);
	}
@else
	kdb::Key found = ks.lookup("$key", 0);
@end if

@if $len(support.fallback(info)) > 0
	// fallback
@for $f in $support.fallback(info)
	if (!found)
	{
		found = ks.lookup("$f", 0);
	}
@end for
@end if

	$support.typeof(info) ret $support.valof(info)

	if(found)
	{
		ret = found.get<$support.typeof(info)>();
	}

	return ret;
}

/** \brief Set parameter $key
 *
 * $util.doxygen(support, key, info)
 *
 * \see $support.setfuncname($key)
 *
 * \param n is the value to set in the parameter
 */
inline void Parameters::$support.setfuncname($key)($support.typeof(info) n)
{
	kdb::Key found = ks.lookup("$key", 0);

	if(!found)
	{
		kdb::Key k("$support.userkey(key)", KEY_END);
		k.set<$support.typeof(info)>(n);
		ks.append(k);
	}
	else
	{
		found.set<$support.typeof(info)>(n);
	}
}

@end for

} // namespace kdb

$util.footer($args.output)
