#from context_support import *
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
#include "contextual.hpp"
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

class none_t
{};

template <>
inline void Key::set(kdb::none_t e)
{}

template <>
inline kdb::none_t Key::get() const
{
	kdb::none_t ret;
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

@for $child in hierarchy.children
$outputClasses(child)
@end for


@for n in hierarchy.name.split('/')[1:-1]
namespace $nsnpretty($n)
{
@end for

class ${hierarchy.classname}GetPolicy
{
public:
	static kdb::Key get(kdb::KeySet &ks, kdb::Key const& spec)
	{
	@if $hierarchy.info.get('override')
		// override
		kdb::Key found = ks.lookup("${override(hierarchy.info)[0]}", 0);
	@for $o in $override(hierarchy.info)[1:]
		if (!found)
		{
			found = ks.lookup("$o", 0);
		}
	@end for
		// now the key itself
		if(!found)
		{
			found = ks.lookup(spec.getName(), 0);
		}
	@else
		kdb::Key found = ks.lookup(spec.getName(), 0);
	@end if

	@if $hierarchy.info.get('fallback')
		// fallback
	@for $f in $fallback(hierarchy.info)
		if (!found)
		{
			found = ks.lookup("$f", 0);
		}
	@end for
	@end if

		return found;
	}
};

/** \brief class of $hierarchy.name
 * Full Name (with contextual placeholders):
 * $hierarchy.info.get('name')
 * Dirname: $hierarchy.dirname
 * Basename: $hierarchy.basename
 * */
class $hierarchy.classname : public ContextualValue
	<$typeof($hierarchy.info), GetPolicyIs<${hierarchy.classname}GetPolicy>>
{
public:


	/** \brief Constructor for $hierarchy.classname
	 * \param ks keyset to work with
	 */
	${hierarchy.classname}(kdb::KeySet & ks, kdb::Context & context)
		: ContextualValue<$typeof($hierarchy.info), GetPolicyIs<${hierarchy.classname}GetPolicy>>(ks,
			context,
			kdb::Key(
@if $hierarchy.info.get('name'):
				"$hierarchy.info.get('name')",
@else
				"/",
@end if
				ckdb::KDB_O_CASCADING_NAME,
@if $hierarchy.info.get('default'):
				KEY_META, "default", $quote($hierarchy.info.get('default')),
@end if
@if $hierarchy.info.get('unit'):
				KEY_META, "unit", $quote($hierarchy.info.get('unit')),
@end if
@if $hierarchy.info.get('explanation'):
				KEY_META, "explanation", $quote($hierarchy.info.get('explanation')),
@end if
@if $hierarchy.info.get('rationale'):
				KEY_META, "rationale", $quote($hierarchy.info.get('rationale')),
@end if
@if $hierarchy.info.get('fallback'):
				KEY_META, "fallback", $quote($hierarchy.info.get('fallback')),
@end if
@if $hierarchy.info.get('override'):
				KEY_META, "override", $quote($hierarchy.info.get('override')),
@end if
				KEY_END))


@for k in hierarchy.children
@set nestedname = $nestedpretty(k.basename)
		, ${nestedname}(ks, context)
@end for
	{}

	using ContextualValue<$typeof($hierarchy.info), GetPolicyIs<${hierarchy.classname}GetPolicy>>::operator =;

@for k in hierarchy.children
@set nsname = $nspretty(k.dirname)
@set nestedname = $nestedpretty(k.basename)
@set nestedclassname = $classpretty(k.basename)
	/** \return nested subclass */
	$nsname$nestedclassname ${nestedname};
@end for
};

@if $typeof($hierarchy.info)=='std::string':
inline std::ostream & operator<<(std::ostream & os,
		$hierarchy.classname const & c)
{
	os << static_cast<std::string>(c);
	return os;
}
@end if

@for n in hierarchy.name.split('/')[1:-1]
}
@end for

@end def









/*
hierarchy is
@set hierarchy = ContextHierarchy('/', {})
@for $key, $info in $parameters.items()
hierarchy.addWithContext(Hierarchy($key, $info))
$hierarchy.addWithContext(Hierarchy($key, $info))
$hierarchy
@end for
*/


$outputForwardDecl(hierarchy)
$outputClasses(hierarchy)

} // namespace kdb

#endif // $includeguard($args.output)
