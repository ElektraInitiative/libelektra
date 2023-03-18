// clang-format off


// clang-format on
/**
 * @file
 *
 * This file was automatically generated using `kdb gen highlevel`.
 * Any changes will be overwritten, when the file is regenerated.
 *
 * @copyright BSD Zero Clause License
 *
 *     Copyright (c) Elektra Initiative (https://www.libelektra.org)
 *
 *     Permission to use, copy, modify, and/or distribute this software for any
 *     purpose with or without fee is hereby granted.
 *
 *     THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 *     REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *     FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 *     INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 *     LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 *     OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 *     PERFORMANCE OF THIS SOFTWARE.
 */


#ifndef COMMANDS_ACTUAL_COMMANDS_H
#define COMMANDS_ACTUAL_COMMANDS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <elektra/highlevel.h>

int commandKdb (Elektra * elektra, kdb_boolean_t terminal, void * userData);
int commandKdbGet (Elektra * elektra, kdb_boolean_t terminal, void * userData);
int commandKdbGetMeta (Elektra * elektra, kdb_boolean_t terminal, void * userData);
int commandKdbSet (Elektra * elektra, kdb_boolean_t terminal, void * userData);

#ifdef __cplusplus
}
#endif

#endif // COMMANDS_ACTUAL_COMMANDS_H
