#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
@from context_support import *
@set support = ContextSupport()
@from util import util
@from cpp_util import cpp_util
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
inline void Key::set(kdb::none_t e)
{}

template <>
inline kdb::none_t Key::get() const
{
	kdb::none_t ret;
	return ret;
}




@def outputForwardDecl(support, hierarchy)
@if not hierarchy.children
@return
@end if

@for n in hierarchy.name.split('/')[1:-1]
namespace $support.nsnpretty($n)
{
@end for

class $hierarchy.prettyclassname(support);

@for n in hierarchy.name.split('/')[1:-1]
}
@end for

@for $child in hierarchy.children
$outputForwardDecl(support, child)
@end for

@end def













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
	static kdb::Key get(kdb::KeySet &ks, kdb::Key const& spec)
	{
	@if len($support.override(hierarchy.info)) > 0
		// override
		kdb::Key found = ks.lookup("${support.override(hierarchy.info)[0]}", 0);
	@for $o in $support.override(hierarchy.info)[1:]
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

	@if len($support.fallback(hierarchy.info)) > 0
		// fallback
	@for $f in $support.fallback(hierarchy.info)
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
 *
 * Full Name (with contextual placeholders):
 * $hierarchy.info.get('name')
 * Dirname: $hierarchy.dirname
 * Basename: $hierarchy.basename
 * */
class $hierarchy.prettyclassname(support) : public ContextualValue
	<$support.typeof($hierarchy.info), GetPolicyIs<${hierarchy.prettyclassname(support)}GetPolicy>>
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


$outputForwardDecl(support, hierarchy)
$outputClasses(support, hierarchy)

} // namespace kdb
$util.footer($args.output)
