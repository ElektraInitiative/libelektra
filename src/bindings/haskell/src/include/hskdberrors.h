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

void hsElektraTriggerError (int nr, Key * parentKey, const char * message);
void hsElektraTriggerWarnings (int nr, Key * parentKey, const char * message);

#endif
