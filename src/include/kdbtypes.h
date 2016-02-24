/**
 * @file
 *
 * @brief Elektra's data types for C and C++11.
 *
 * This header file defines portable Types used in Elektra for C and
 * C++11.
 *
 * They are not used within the API, but only for generated front ends
 * see "kdb gen" how to use these types properly.
 *
 * The CORBA Type System is currently used.
 *
 * See "OMG Language Mapping" if you want to map the types to another
 * programming language.
 *
 * This files defines mappings to C89, C++03 and C++11.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDBTYPES_H
#define KDBTYPES_H

#include "kdbconfig.h"

#ifdef __cplusplus
// for C++11 or later

namespace kdb
{
using short_t = int16_t;	       // default: 0
using long_t = int32_t;		       // default: 0
using long_long_t = int64_t;	   // default: 0
using unsigned_short_t = uint16_t;     // default: 0
using unsigned_long_t = uint32_t;      // default: 0
using unsigned_long_long_t = uint64_t; // default: 0
using float_t = float;		       // default: 0.0
using double_t = double;	       // default: 0.0
using long_double_t = long double;     // default: 0.0
using char_t = unsigned char;	  // default: 0
// using wchar_t; // default: 0 wchar_t not supported!
using boolean_t = bool;  // default: false
using octet_t = uint8_t; // default: 0
}
#endif // for c++


// for C (and C++)

typedef unsigned char kdb_boolean_t;
typedef unsigned char kdb_char_t;
typedef unsigned char kdb_octet_t;
typedef signed short kdb_short_t;
typedef unsigned short kdb_unsigned_short_t;

#if SIZEOF_LONG == 4
#define ELEKTRA_LONG_F "%ld"
typedef long kdb_long_t;
#define ELEKTRA_UNSIGNED_LONG_F "%lu"
typedef unsigned long kdb_unsigned_long_t;
#elif SIZEOF_INT == 4
#define ELEKTRA_LONG_F "%d"
typedef int kdb_long_t;
#define ELEKTRA_UNSIGNED_LONG_F "%u"
typedef unsigned int kdb_unsigned_long_t;
#else
#error "Can't map kdb_long_t (4 bytes, 32 bits) to a native type."
#endif

// typedef wchar_t                   kdb_wchar_t; // wchar_t not supported!

#if SIZEOF_LONG == 8
#define ELEKTRA_LONG_LONG_F "%ld"
#define ELEKTRA_LONG_LONG_S strtol
typedef long kdb_long_long_t;
#define ELEKTRA_UNSIGNED_LONG_LONG_F "%lu"
#define ELEKTRA_UNSIGNED_LONG_LONG_S strtoul
typedef unsigned long kdb_unsigned_long_long_t;
#elif defined(HAVE_SIZEOF_LONG_LONG) && (SIZEOF_LONG_LONG == 8)
#define ELEKTRA_LONG_LONG_F "%lld"
#define ELEKTRA_LONG_LONG_S strtoll
typedef long long kdb_long_long_t;
#define ELEKTRA_UNSIGNED_LONG_LONG_F "%llu"
#define ELEKTRA_UNSIGNED_LONG_LONG_S strtoull
typedef unsigned long long kdb_unsigned_long_long_t;
#endif

typedef float kdb_float_t;
typedef double kdb_double_t;

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16)
typedef long double kdb_long_double_t;
#elif defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 12)
// the long double data type represents an IEEE double-extended
// floating-point number, which has an exponent of at least 15 bits in
// length and a signed fraction of at least 64 bits
typedef long double kdb_long_double_t;
#endif

#endif
