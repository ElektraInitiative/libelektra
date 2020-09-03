/**
 * @file error.c
 *
 * @brief Contains functionality for handling errors.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "error.h"

#include <kdberrors.h>

void emitElektraError (Key * root, int err, const char * msg)
{
	switch (err)
	{
	case ERROR_INTERNAL:
		ELEKTRA_SET_INTERNAL_ERROR (root, msg);
		break;
	case ERROR_MEMORY:
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (root);
		break;
	case ERROR_SYNTACTIC:
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (root, msg);
		break;
	case ERROR_SEMANTIC:
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (root, msg);
		break;
	default:
		ELEKTRA_SET_INTERNAL_ERROR (root, msg);
		break;
	}
}
