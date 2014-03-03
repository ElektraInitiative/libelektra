#from nested_support import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
#ifndef $includeguard($args.output)
#define $includeguard($args.output)

/** \file
 * \warning this is a generated file, do not modify it
 * \warning this is a prototype and not production code
 */
#include "kdb.hpp"
#include "kdbtypes.h"

#include <string>

namespace kdb
{

##todo: duplicate
@for $key, $info in $parameters.iteritems()
@if $isenum(info):
/**
 * Class enum of $key
 */
enum class $typeof(info)
{
@for $enum in $enumval(info)
	$enum,
@end for
};

##todo: duplicate
/** \brief Convert enum to string
 *
 * \return string that holds value of enum
 * \param e the enum that should be converted
 */
template <>
inline void Key::set($enumname(info) e)
{
	switch(e)
	{
@for $enum in $enumval(info)
	case $typeof(info)::$enum: setString("$enum"); break;
@end for
	}
}

##todo: duplicate
/** \brief Convert enum from string
 *
 * \return enum from string s or default value
 * \param s the string that should be converted
 */
template <>
inline $enumname(info) Key::get() const
{
	$typeof(info) ret $valof(info)
@for $enum in $enumval(info)
	if(getString() == "$enum")
		ret = $typeof(info)::$enum;
@end for
	return ret;
}

@end if
@end for

##todo: duplicate
/** \brief Convert bool to string
 *
 * \return string that holds value of bool
 * \param e the bool that should be converted
 */
template <>
inline void Key::set(bool e)
{
	if(e)
	{
		setString("true");
	}
	else
	{
		setString("false");
	}
}

##todo: duplicate
/** \brief Convert bool from string
 *
 * \return bool from string s or default value
 * \param s the string that should be converted
 */
template <>
inline bool Key::get() const
{
	$typeof(info) ret = false;
	if(getString() == "${trueval()[0]}")
		ret = true;
@for $b in $trueval()[1:]
	else if(getString() == "$b")
		ret = true;
@end for
	return ret;
}

##todo: duplicate
@def doxygen($key, $info)
 * \par Type
 * $info['type']
 * \par Mapped Type
 * $typeof(info)
@if $info.get('unit'):
 * \par Unit
 * $info.get('unit')
@end if
 * \par Default Value
 * $info['default']
@if $info.get('explanation'):
 * \par Explanation
 * $info.get('explanation')
@end if
@if $info.get('rationale'):
 * \par Rationale
 * $info.get('rationale')
@end if
@if $info.get('override')
 * \par Override
<ul>
    @for $i in $override(info)
    <li>get${funcname($i)}()</li>
    @end for
</ul>
@end if
@if $info.get('fallback')
 * \par Fallback
<ul>
    @for $i in $fallback(info)
    <li>get${funcname($i)}()</li>
    @end for
</ul>
@end if
@if $info.get('see')
    @for $i in $see(info)
 * \see get${funcname($i)}
    @end for
@end if
@end def









@def outputForwardDecl(hierarchy)
@if not hierarchy.children
@return
@end if

@for n in hierarchy.name.split('/')[1:-1]
namespace $nsnpretty($n)
{
@end for

class $hierarchy.classname;

@for n in hierarchy.name.split('/')[1:-1]
}
@end for

@for $child in hierarchy.children
$outputForwardDecl(child)
@end for

@end def













@def outputClasses(hierarchy)
@if not hierarchy.children
@return
@end if

@for n in hierarchy.name.split('/')[1:-1]
namespace $nsnpretty($n)
{
@end for

/** \brief class */
class $hierarchy.classname
{
public:


	/** \brief Constructor for $hierarchy.classname
	 * \param ks keyset to work with
	 */
	${hierarchy.classname}(kdb::KeySet & ks) : ks(ks)
	{}

@for k in hierarchy.childrenWithChildren
@set nsname = $nspretty(k.dirname)
@set nestedname = $nestedpretty(k.basename)
@set nestedclassname = $classpretty(k.basename)
	/** \return nested subclass */
	$nsname$nestedclassname& ${nestedname}()
	{
		// works in C++11 because classes are layout compatible
		return reinterpret_cast<$nsname$nestedclassname&>(*this);
	}

	/** \return nested subclass */
	$nsname$nestedclassname const& ${nestedname}() const
	{
		// works in C++11 because classes are layout compatible
		return reinterpret_cast<$nsname$nestedclassname const&>(*this);
	}
@end for

@for k in hierarchy.childrenWithType
	$typeof(k.info) get${funcname(k.name)}() const;
	void set${funcname(k.name)}($typeof(k.info) n);
@end for

private:
	kdb::KeySet &ks;
};

@for n in hierarchy.name.split('/')[1:-1]
}
@end for

@for $child in hierarchy.children
$outputClasses(child)
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


$outputForwardDecl(hierarchy)
$outputClasses(hierarchy)

@for $key, $info in $parameters.iteritems()
/** \brief Get parameter $key
 *
 * $doxygen(key, info)
 *
 * \see set$funcname($key)
 *
 * \return the value of the parameter, default if it could not be found
 */
inline $typeof(info) $nsname($key)$classname($key)::get$funcname($key)() const
{
@if $info.get('override')
	// override
	kdb::Key found = ks.lookup("${override(info)[0]}", 0);
@for $o in $override(info)[1:]
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

@if $info.get('fallback')
	// fallback
@for $f in $fallback(info)
	if (!found)
	{
		found = ks.lookup("$f", 0);
	}
@end for
@end if

	$typeof(info) ret $valof(info)

	if(found)
	{
		ret = found.get<$typeof(info)>();
	}

	return ret;
}

/** \brief Set parameter $key
 *
 * $doxygen(key, info)
 *
 * \see set$funcname($key)
 *
 * \param n is the value to set in the parameter
 */
inline void $nsname($key)$classname($key)::set$funcname($key)($typeof(info) n)
{
	kdb::Key found = ks.lookup("$key", 0);

	if(!found)
	{
		kdb::Key k("$userkey(key)", KEY_END);
		k.set<$typeof(info)>(n);
		ks.append(k);
	}
	else
	{
		found.set<$typeof(info)>(n);
	}
}

@end for

} // namespace kdb

#endif // $includeguard($args.output)
