/**
 * @file
 *
 * @brief Small wrapper to expose the static inline kdberrors functions to haskell
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef HS_KDBERRORS_H
#define HS_KDBERRORS_H

#include <kdb.h>

void hs_elektraTriggerError (int nr, Key * parentKey, const char * message);
void hs_elektraTriggerWarnings (int nr, Key * parentKey, const char * message);

#endif
