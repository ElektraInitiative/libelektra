
/**
 * @file
 *
 * @brief Helper macros to handle bitfields encoded in integers.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_MACROS_PLUGIN_BITFIELDS_H
#define ELEKTRA_MACROS_PLUGIN_BITFIELDS_H

#ifdef __cplusplus

/** Test a bit. @see set_bit(), clear_bit() */
#define test_bit(var, bit) ((static_cast<unsigned long long> (var)) & (static_cast<unsigned long long> (bit)))
/** Set a bit. @see clear_bit() */
#define set_bit(var, bit) ((var) |= (static_cast<unsigned long long> (bit)))
/** Clear a bit. @see set_bit() */
#define clear_bit(var, bit) ((var) &= ~(static_cast<unsigned long long> (bit)))

#else

/** Test a bit. @see set_bit(), clear_bit() */
#define test_bit(var, bit) (((unsigned long long) (var)) & ((unsigned long long) (bit)))
/** Set a bit. @see clear_bit() */
#define set_bit(var, bit) ((var) |= ((unsigned long long) (bit)))
/** Clear a bit. @see set_bit() */
#define clear_bit(var, bit) ((var) &= ~((unsigned long long) (bit)))

#endif

#endif // ELEKTRA_MACROS_PLUGIN_BITFIELDS_H
