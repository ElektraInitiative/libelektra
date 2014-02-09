#from nested_support import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
#ifndef ELEKTRA_GEN_FILENAME_HPP
#define ELEKTRA_GEN_FILENAME_HPP
/** \file
 * \warning this is a generated file, do not modify it
 * \warning this is a prototype and not production code
 */
#include "kdb.hpp"
#include <string>

namespace kdb
{

@set hierarchy = Hierarchy('/test', {})

##todo: duplicate
@for $key, $info in $parameters.iteritems()
@silent hierarchy.add(Hierarchy($key, $info))
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

hierarchy is $hierarchy

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

@def outputx(mparameters, mkey)
@set $lparameters = $cut(mparameters, mkey)

@for n in mkey.split('/')[1:-2]
namespace $nspretty($n)
{
@end for

// mkey is $mkey
class $classname($mkey)
{
public:

	${classname($mkey)}(kdb::KeySet & ks) : ks(ks)
	{}

@for k in $nestedclasses(mparameters, dirname(mkey))
@set nsname = $nspretty(dirname(k))
@set nestedname = $nestedpretty(basename(k))
@set nestedclassname = $classpretty(basename(k))
	$nsname::$nestedclassname ${nestedname}() const
	{
		return static_cast<$nsname::$nestedclassname>(ks);
	}
@end for

@for k,i in $siblings(mparameters, mkey).iteritems()
	$typeof(i) get${funcname(k)}() const;
	void set${funcname(k)}($typeof(i) n);
@end for

private:
	kdb::KeySet &ks;
};

@for n in mkey.split('/')[1:-2]
}
@end for

@for $k in lparameters
@silent $sys.stderr.write("var is '$k'" + $getVar('k', 'default')+ ".\n")
$outputx(lparameters, k)
@end for

@end def

##TODO use hierarchy here
@set px = $parameters
$outputx(px, "/")

@for $key, $info in $parameters.iteritems()
/** \brief Get parameter $key
 *
 * $doxygen(key, info)
 *
 * \see set$funcname($key)
 *
 * \return the value of the parameter, default if it could not be found
 * \param ks the keyset where the parameter is searched
 */
inline $typeof(info) $nsname($key)::$classname($key)::get$funcname($key)()
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
 * \param ks the keyset where the parameter is added or replaced
 * \param n is the value to set in the parameter
 */
inline void Parameters::set$funcname($key)($typeof(info) n)
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

#endif
