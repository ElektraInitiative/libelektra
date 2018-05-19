/**
 * @file
 *
 * @brief Small wrapper to expose the static inline kdberrors functions to haskell
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#include <hskdberrors.h>
#include <kdberrors.h>

void hsElektraTriggerError (int nr, Key * parentKey, const char * message)
{
	elektraTriggerError (nr, parentKey, message);
}

void hsElektraTriggerWarnings (int nr, Key * parentKey, const char * message)
{
	elektraTriggerWarnings (nr, parentKey, message);
}
