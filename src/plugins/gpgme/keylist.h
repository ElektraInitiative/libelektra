/**
 * @file
 *
 * @brief provides linked list of GPGME's gpgme_key_t for the gpgme plugin.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_GPGME_KEYLIST_H
#define ELEKTRA_PLUGIN_GPGME_KEYLIST_H

#include <gpgme.h>

// define linked list of gpgme_key_t elements
struct internal_keylist
{
	gpgme_key_t key;
	struct internal_keylist * next;
};

struct keylist
{
	struct internal_keylist * head;
	struct internal_keylist * iterator;
	unsigned long size;
};

typedef struct keylist keylist_t;

// declare keylist functions
void elektraGpgmeKeylistInit (keylist_t * list);
int elektraGpgmeKeylistAdd (keylist_t * list, gpgme_key_t key);
void elektraGpgmeKeylistRewind (keylist_t * list);
gpgme_key_t elektraGpgmeKeylistNext (keylist_t * list);
void elektraGpgmeKeylistFree (keylist_t * list);

#endif
