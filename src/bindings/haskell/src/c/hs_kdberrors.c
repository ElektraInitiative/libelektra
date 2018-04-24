/**
 * @file
 *
 * @brief Small wrapper to expose the static inline kdberrors functions to haskell
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#include <hs_kdberrors.h>
#include <kdberrors.h>

void hs_elektraTriggerError (int nr, Key * parentKey, const char * message)
{
	elektraTriggerError (nr, parentKey, message);
}

void hs_elektraTriggerWarnings (int nr, Key * parentKey, const char * message)
{
	elektraTriggerWarnings (nr, parentKey, message);
}
