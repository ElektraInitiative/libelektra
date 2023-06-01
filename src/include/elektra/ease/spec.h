#ifndef ELEKTRA_EASE_SPEC_H
#define ELEKTRA_EASE_SPEC_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/type/types.h>


#ifdef __cplusplus
namespace ckdb
{
extern "C" {
using Key = ckdb::Key;
using KeySet = ckdb::KeySet;
#endif


kdb_boolean_t calculateSpecificationToken (char hash_string[65], KeySet * ks, Key * parentKey);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_EASE_SPEC_H
