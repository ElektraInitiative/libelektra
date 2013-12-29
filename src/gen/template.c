#from os.path import basename, dirname
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
// start of a generated file
#include "kdb.h"
#include <stdlib.h>

@for $key, $info in $parameters.items()
/**
 * Type: $info['type']
 * Default: $info['default']
 * Description: $info.get('description')
 */
$info['type'] get_$basename($key)(KeySet *ks)
{
    Key * found = ksLookupByName(ks, "$key", 0);
    $info['type'] ret = ($info['type'])$info['default'];

    if (found)
    {
        @if $info['type'] == 'int'
        ret = atoi(keyString(found));
        @else if $info['type'] == 'double'
        ret = atof(keyString(found));
        @else if $info['type'] == 'char*'
        ret = keyString(found);
        @end if
    }

    return ret;
}


@end for
// end of a generated file
