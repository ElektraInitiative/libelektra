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
#include "kdbproposal.h"

#include <string>

namespace kdb
{

$cpp_util.generateenum($support, $parameters)
$cpp_util.generatebool($support)
$cpp_util.generatenone()










@def outputPolicies(support, hierarchy)
@for $child in hierarchy.children
$outputPolicies(support, child)
@end for


@for n in hierarchy.name.split('/')[1:-1]
namespace $support.nsnpretty($n)
{
@end for

class ${hierarchy.prettyclassname(support)}GetPolicy
{
public:
typedef $support.typeof($hierarchy.info) type;
static type get(kdb::KeySet &ks, kdb::Key const& spec)
{
	type value{};

	Key found(ckdb::ksLookupBySpec(ks.getKeySet(), *spec));

	if (found)
	{
		value = found.get<$support.typeof(hierarchy.info)>();
	}

	return value;
}
};

@for n in hierarchy.name.split('/')[1:-1]
}
@end for
@end def







/*
dynamic hierarchy is
@set hierarchy = ContextHierarchy('/', {})
@for $key, $info in $parameters.items()
hierarchy.addWithContext(Hierarchy($key, $info))
$hierarchy.addWithContext(Hierarchy($key, $info))
$hierarchy
@end for
*/


$cpp_util.generateForwardDecl($support, $hierarchy)
$outputPolicies(support, hierarchy)
$cpp_util.outputClasses($support, $hierarchy)

} // namespace kdb
$util.footer($args.output)
