/**
 * @file
 *
 * @brief Basic support macros used by other macros and in code.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifndef ELEKTRA_MACROS_UTILS_H
#define ELEKTRA_MACROS_UTILS_H

/** Turn a token into a string literal (macros are not evaluated) */
#define ELEKTRA_QUOTE(x) #x
/** Turn a value into a string literal (macros are evaluated) */
#define ELEKTRA_STRINGIFY(x) ELEKTRA_QUOTE (x)

/** Concat two tokens (macros are not evaluated) */
#define ELEKTRA_CONCAT2(X, Y) X##Y
/** Concat two values (macros are evaluated) */
#define ELEKTRA_CONCAT(X, Y) ELEKTRA_CONCAT2 (X, Y)

#endif // ELEKTRA_MACROS_UTILS_H
