/**
 * @file
 *
 * @brief Private declarations.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBPRIVATE_H
#define KDBPRIVATE_H

#pragma region includes

#include <elektra/config.h>
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/core/namespace.h>
#include <elektra/ease/meta.h>
#include <elektra/highlevel.h>
#include <elektra/highlevel/errors.h>
#include <elektra/io/api.h>
#include <elektra/kdb/kdb.h>
#include <elektra/plugin/plugin.h>
#include <elektra/type/types.h>
#include <internal/notifications.h>
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
#include <internal/core/opmphm.h>
#include <internal/core/opmphmpredictor.h>
#endif
#include <internal/utility/alloc.h>

#include <limits.h>

#pragma endregion

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

#pragma region core /keyset


/*Private helper for keyset*/


#pragma endregion

#ifdef __cplusplus
}
}
#endif

#endif /* KDBPRIVATE_H */
