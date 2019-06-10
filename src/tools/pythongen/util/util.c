#from support.c import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings

@@staticmethod
@def help()
This is how a static method would work to allow shared code:
https://pythonhosted.org/Cheetah/recipes/staticmethod.html
@end def

@@staticmethod
@def header(filename)
/** @file
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * \warning this is a generated file, do not modify it
 * \warning this is a prototype and not production code
 */
#ifndef $includeguard(filename)
#define $includeguard(filename)
@end def

@@staticmethod
@def footer(filename)
#endif // $includeguard(filename)
@end def


@@staticmethod
@def doxygen(support, key, info)
 * \par Type
 * $info['type']
 * \par Mapped Type
 * $support.typeof(info)
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
    @for $i in $support.override(info)
    <li>${support.getfuncname($i)}()</li>
    @end for
</ul>
@end if
@if $info.get('fallback')
 * \par Fallback
<ul>
    @for $i in $support.fallback(info)
    <li>${support.getfuncname($i)}()</li>
    @end for
</ul>
@end if
@if $info.get('see')
    @for $i in $support.see(info)
 * \see ${support.getfuncname($i)}
    @end for
@end if
@end def
