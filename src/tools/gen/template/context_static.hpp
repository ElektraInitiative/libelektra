#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
@from support.context import *
@from util import util
@from cpp_util import cpp_util
@set support = ContextSupport()
$util.header($args.output)
#include "contextual.hpp"
#include "kdbtypes.h"

#include <string>

namespace kdb
{

$cpp_util.generateenum($support, $parameters)
$cpp_util.generatebool($support)

class none_t
{};

template <>
inline void Key::set(kdb::none_t)
{}

template <>
inline kdb::none_t Key::get() const
{
	kdb::none_t ret;
	return ret;
}












@def outputClasses(support, hierarchy)

@for $child in hierarchy.children
$outputClasses(support, child)
@end for


@for n in hierarchy.name.split('/')[1:-1]
namespace $support.nsnpretty($n)
{
@end for

class ${hierarchy.prettyclassname(support)}GetPolicy
{
public:
typedef $support.typeof($hierarchy.info) type;
static type get(kdb::KeySet &ks, kdb::Key const&)
{
	type value{};

	$cpp_util.generateGetBySpec(support,
			$hierarchy.name,
			hierarchy.info)

	return value;
}
};

/** \brief class of $hierarchy.name
 *
 * Full Name (with contextual placeholders):
 * $hierarchy.info.get('name')
 * Dirname: $hierarchy.dirname
 * Basename: $hierarchy.basename
 * */
class $hierarchy.prettyclassname(support) : public ContextualValue
	<$support.typeof($hierarchy.info),
	GetPolicyIs<${hierarchy.prettyclassname(support)}GetPolicy>>
{
public:


	/** \brief Constructor for $hierarchy.prettyclassname(support)
	 * \param ks keyset to work with
	 */
	${hierarchy.prettyclassname(support)}(kdb::KeySet & ks, kdb::Context & context)
		: ContextualValue<$support.typeof($hierarchy.info),
		  GetPolicyIs<${hierarchy.prettyclassname(support)}GetPolicy>>(ks,
			context,
			kdb::Key(
@if $hierarchy.info.get('name'):
				"$hierarchy.info.get('name')",
@else
				"/",
@end if
				ckdb::KDB_O_CASCADING_NAME,
@if $hierarchy.info.get('default'):
				KEY_META, "default", $support.quote($hierarchy.info.get('default')),
@end if
@if $hierarchy.info.get('unit'):
				KEY_META, "unit", $support.quote($hierarchy.info.get('unit')),
@end if
@if $hierarchy.info.get('explanation'):
				KEY_META, "explanation", $support.quote($hierarchy.info.get('explanation')),
@end if
@if $hierarchy.info.get('rationale'):
				KEY_META, "rationale", $support.quote($hierarchy.info.get('rationale')),
@end if
@set $fa = $support.fallback(hierarchy.info)
@for $f in $fa
				KEY_META, "fallback/#$fa.index($f)", "$f",
@end for
@set $ov = $support.override(hierarchy.info)
@for $o in $ov
				KEY_META, "override/#$ov.index($o)", "$o",
@end for
				KEY_END))


@for k in hierarchy.children
@set nestedname = $support.nestedpretty(k.basename)
		, ${nestedname}(ks, context)
@end for
	{}

	using ContextualValue<$support.typeof($hierarchy.info), GetPolicyIs<${hierarchy.prettyclassname(support)}GetPolicy>>::operator =;

@for k in hierarchy.children
@set nsname = $support.nspretty(k.dirname)
@set nestedname = $support.nestedpretty(k.basename)
@set nestedclassname = $support.classpretty(k.basename)
	/** \return nested subclass */
	$nsname$nestedclassname ${nestedname};
@end for
};

@if $support.typeof($hierarchy.info)=='std::string':
inline std::ostream & operator<<(std::ostream & os,
		$hierarchy.prettyclassname(support) const & c)
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


$cpp_util.generateForwardDecl($support, $hierarchy)
$outputClasses(support, hierarchy)

} // namespace kdb
$util.footer($args.output)
