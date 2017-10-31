/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <augeas.h>
#include <stddef.h>

int main (void)
{
	augeas * handle = aug_init (NULL, NULL, AUG_NONE);
	aug_text_store (handle, "Hosts", "/raw/contentnode", "/raw/tree");
	aug_text_retrieve (handle, "Hosts", "/raw/contentnode", "/raw/tree", "/raw/output");
	aug_close (handle);
	return 0;
}
