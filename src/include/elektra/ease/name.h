#ifndef ELEKTRA_EASE_NAME_H
#define ELEKTRA_EASE_NAME_H

#include <elektra/core/key.h>


#ifdef __cplusplus
namespace ckdb
{
extern "C" {
using Key = ckdb::Key;
using KeySet = ckdb::KeySet;
#endif


const char * elektraKeyGetRelativeName (Key const * cur, Key const * parentKey);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_EASE_NAME_H
