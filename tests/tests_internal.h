/** @file
  *
  * Some common functions operating on internals.
  *
  * @copyright This program is free software; you can redistribute it and/or modify
  *            it under the terms of the BSD License (revised).
  *
  * If you include this file you have full access to elektra's internals
  * and your test might not be ABI compatible with the next release.
  *
  * @see tests.h
  */

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef TESTS_INTERNAL_H
#define TESTS_INTERNAL_H

#include <tests.h>

#include <kdbinternal.h>

void clear_sync (KeySet *ks);
void output_plugin(Plugin *plugin);
void output_backend(Backend *backend);

void output_trie(Trie *trie);

void generate_split (Split *split);
void output_split(Split *split);

#endif
