/**
 * @file
 *
 * @brief Elektra macros for compiler symbol versioning.
 *        This file contains the macros only needed in the internal parts.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_MACROS_SYMVER_INTERNAL_H
#define ELEKTRA_MACROS_SYMVER_INTERNAL_H

#include <elektra/macros/symver.h>
#include <elektra/macros/utils.h>

#include <internal/config.h>

#ifndef ELEKTRA_SYMVER_COMMAND
#error "ELEKTRA_SYMVER_COMMAND must be externally defined to the correct .symver assembler pseudo command"
#endif

/**
 * Declares another version of a symbol using the `.symver` assembler pseudo command
 *
 * @param ver  the version name as declared versions.def
 * @param sym  the unversioned name of the symbol
 * @param impl the version suffix to use for this version
 */
#define ELEKTRA_SYMVER_DECLARE(ver, sym, impl) ELEKTRA_SYMVER_COMMAND (ELEKTRA_STRINGIFY (ELEKTRA_SYMVER (sym, impl)), #sym "@" ver)

#endif // ELEKTRA_MACROS_SYMVER_INTERNAL_H
