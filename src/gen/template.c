#from gen_support import *
#from c_support import *

#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
// start of a generated file
#include "kdb.h"
#include <stdlib.h>
#include <stdint.h>

@for $key, $info in $parameters.items()
@if $isenum(info):
$typeof(info)
{
    @for $enum in $enumval(info)
    $enum,
    @end for
};
@end if
@end for

@for $key, $info in $parameters.items()
/**
 * Type: $info['type']
 * Mapped Type: $typeof(info)
 * Default Value: $info['default']
 * Description: $info.get('explanation')
 */
$typeof(info) get_$funcname($key)(KeySet *ks)
{
    Key * found = ksLookupByName(ks, "$key", 0);
    $typeof(info) ret $valof(info)

    if (found)
    {
        @if $info['type'] == 'unsigned_int_32'
        ret = atoi(keyString(found));
        @else if $info['type'] == 'double'
        ret = atof(keyString(found));
        @else if $info['type'] == 'string'
        ret = keyString(found);
        @end if
    }

    return ret;
}


@end for
// end of a generated file
