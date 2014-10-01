#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
useAutocalling = false
#end compiler-settings
@from nested_support import *
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


@def outputForwardDecl(support, hierarchy)
@if not hierarchy.children
@return
@end if

@for n in hierarchy.name.split('/')[1:-1]
namespace ${support.nsnpretty($n)}
{
@end for

class ${hierarchy.prettyclassname($support)};

@for n in hierarchy.name.split('/')[1:-1]
}
@end for

@for $child in hierarchy.children
$outputForwardDecl(support, child)
@end for

@end def













@def outputClasses(support, hierarchy)
@if not hierarchy.children
@return
@end if

@for n in hierarchy.name.split('/')[1:-1]
namespace ${support.nsnpretty($n)}
{
@end for

/** \brief class */
class ${hierarchy.prettyclassname($support)}
{
public:


	/** \brief Constructor for * ${hierarchy.prettyclassname($support)}
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


$outputForwardDecl(support, hierarchy)
$outputClasses(support, hierarchy)

@for $key, $info in $parameters.iteritems()
/** \brief Get parameter $key
 *
 * $util.doxygen(support, key, info)
 *
 * \see $support.setfuncname($key)
 *
 * \return the value of the parameter, default if it could not be found
 */
inline $support.typeof(info) $support.nsname($key)${support.classname($key)}::$support.getfuncname($key)() const
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
inline void $support.nsname($key)${support.classname($key)}::$support.setfuncname($key)($support.typeof(info) n)
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
