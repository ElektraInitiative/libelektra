/**
 * @file
 *
 * @brief Some common functions operating on internals.
 *
 * If you include this file you have full access to elektra's internals
 * and your test might not be ABI compatible with the next release.
 *
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef TESTS_INTERNAL_H
#define TESTS_INTERNAL_H

#include <tests.h>

#include <kdbinternal.h>

void clear_sync (KeySet * ks);
void output_plugin (Plugin * plugin);
void output_backend (Plugin * backend);

#endif
