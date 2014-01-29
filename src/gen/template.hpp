#from cpp_support import *
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

@for $key, $info in $parameters.items()
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
	case $typeof(info)::$enum: setString("$enum");
@end for
	}
}

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

@def doxygen(key, info)
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
    <li>get_${funcname($i)}()</li>
    @end for
</ul>
@end if
@if $info.get('fallback')
 * \par Fallback
<ul>
    @for $i in $fallback(info)
    <li>get_${funcname($i)}()</li>
    @end for
</ul>
@end if
@if $info.get('see')
    @for $i in $see(info)
 * \see get_${funcname($i)}
    @end for
@end if
@end def

@for $key, $info in $parameters.items()
/** \brief Get parameter $key
 *
 * $doxygen(key, info)
 *
 * \see set_$funcname($key)
 *
 * \return the value of the parameter, default if it could not be found
 * \param ks the keyset where the parameter is searched
 */
static inline $typeof(info) get_$funcname($key)(kdb::KeySet ks)
{
	kdb::Key found = ks.lookup("$key", 0);
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
 * \see set_$funcname($key)
 *
 * \param ks the keyset where the parameter is added or replaced
 * \param n is the value to set in the parameter
 */
static inline void set_$funcname($key)(kdb::KeySet ks, $typeof(info) n)
{
	kdb::Key found = ks.lookup("$key", 0);

	if(!found)
	{
		kdb::Key k("$key", KEY_END);
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
