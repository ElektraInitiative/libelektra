/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
useAutocalling = false
#end compiler-settings
@from support.nested import *
@from util import util
@from cpp_util import cpp_util
@set support = NestedSupport()
$util.header($args.output)
#include "kdb.hpp"
#include "kdbtypes.h"

#include <string>

namespace kdb
{

$cpp_util.generateenum($support, $parameters)
$cpp_util.generatebool($support)















@def outputClasses(support, hierarchy)
@if not hierarchy.children
@return
@end if

@for n in hierarchy.name.split('/')[1:-1]
namespace ${support.nsnpretty($n)}
{
@end for

/** @brief class */
class ${hierarchy.prettyclassname($support)}
{
public:


	/** @brief Constructor for * ${hierarchy.prettyclassname($support)}
	 * \param ks keyset to work with
	 */
	${hierarchy.prettyclassname($support)}(kdb::KeySet & ks) : ks(ks)
	{}

@for k in hierarchy.childrenWithChildren
@set lnsname = $support.nspretty(k.dirname)
@set nestedname = $support.nestedpretty(k.basename)
@set nestedclassname = $support.classpretty(k.basename)
	/** \return nested subclass */
	$lnsname$nestedclassname& ${nestedname}()
	{
		// works in C++11 because classes are layout compatible
		return reinterpret_cast<$lnsname$nestedclassname&>(*this);
	}

	/** \return nested subclass */
	$lnsname$nestedclassname const& ${nestedname}() const
	{
		// works in C++11 because classes are layout compatible
		return reinterpret_cast<$lnsname$nestedclassname const&>(*this);
	}
@end for

@for k in hierarchy.childrenWithType
	$support.typeof(k.info) get${support.funcname(k.name)}() const;
	void ${support.setfuncname(k.name)}($support.typeof(k.info) n);
@end for

private:
	kdb::KeySet &ks;
};

@for n in hierarchy.name.split('/')[1:-1]
}
@end for

@for $child in hierarchy.children
$outputClasses(support, child)
@end for

@end def









/*
hierarchy is
@set hierarchy = Hierarchy('/', {})
@for $key, $info in $parameters.items()
hierarchy.add(Hierarchy($key, $info))
$hierarchy.add(Hierarchy($key, $info))
$hierarchy
@end for
*/


$cpp_util.generateForwardDecl($support, $hierarchy)
$outputClasses(support, hierarchy)

@for $key, $info in $parameters.iteritems()
/** @brief Get parameter $key
 *
 * $util.doxygen(support, key, info)
 *
 * \see $support.setfuncname($key)
 *
 * \return the value of the parameter, default if it could not be found
 */
inline $support.typeof(info) $support.nsname($key)${support.classname($key)}::$support.getfuncname($key)() const
{
	$support.typeof(info) value $support.valof(info)

	$cpp_util.generateGetBySpec(support, key, info)

	return value;
}

/** @brief Set parameter $key
 *
 * $util.doxygen(support, key, info)
 *
 * \see $support.setfuncname($key)
 *
 * \param n is the value to set in the parameter
 */
inline void $support.nsname($key)${support.classname($key)}::$support.setfuncname($key)($support.typeof(info) n)
{
	kdb::Key found = ks.lookup("$key", 0);

	if (!found)
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
