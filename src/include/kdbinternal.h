/**
 * @file
 *
 * @brief Includes most internal header files.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBINTERNAL_H
#define KDBINTERNAL_H

#include <kdb.h>
#include <kdbconfig.h>
#include <kdberrors.h>
#include <kdbextension.h>
#include <kdblogger.h>
#include <kdbmodule.h>
#include <kdbobsolete.h>
#include <kdbplugin.h>
#include <kdbprivate.h>
#include <kdbproposal.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

// can be made simply without elektra's internals, so better keep it as
// extension.
ssize_t keySetStringF (Key * key, const char * format, ...);

#ifdef __cplusplus
}
}
#endif

#endif
