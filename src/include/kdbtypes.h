/**
 * @file
 *
 * @brief Elektra’s data types for C and C++11.
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
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBTYPES_H
#define KDBTYPES_H

#include "kdbconfig.h"

#ifdef __cplusplus
#include <cinttypes>
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
} // namespace kdb

typedef kdb::octet_t kdb_octet_t;
typedef kdb::boolean_t kdb_boolean_t;
typedef kdb::short_t kdb_short_t;
typedef kdb::long_t kdb_long_t;
typedef kdb::long_long_t kdb_long_long_t;
typedef kdb::unsigned_short_t kdb_unsigned_short_t;
typedef kdb::unsigned_long_t kdb_unsigned_long_t;
typedef kdb::unsigned_long_long_t kdb_unsigned_long_long_t;

#define ELEKTRA_LONG_F "%" PRIi32
#define ELEKTRA_UNSIGNED_LONG_F "%" PRIu32
#define ELEKTRA_LONG_LONG_F "%" PRIi64
#define ELEKTRA_LONG_LONG_S strtoll
#define ELEKTRA_UNSIGNED_LONG_LONG_F "%" PRIu64
#define ELEKTRA_UNSIGNED_LONG_LONG_S strtoull

#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
// for C99+
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>

typedef uint8_t kdb_octet_t;
typedef bool kdb_boolean_t;
typedef int16_t kdb_short_t;
typedef int32_t kdb_long_t;
typedef int64_t kdb_long_long_t;
typedef uint16_t kdb_unsigned_short_t;
typedef uint32_t kdb_unsigned_long_t;
typedef uint64_t kdb_unsigned_long_long_t;

#define ELEKTRA_LONG_F "%" PRIi32
#define ELEKTRA_UNSIGNED_LONG_F "%" PRIu32
#define ELEKTRA_LONG_LONG_F "%" PRIi64
#define ELEKTRA_LONG_LONG_S strtoll
#define ELEKTRA_UNSIGNED_LONG_LONG_F "%" PRIu64
#define ELEKTRA_UNSIGNED_LONG_LONG_S strtoull

#elif !defined(__cplusplus) // for C89
typedef unsigned char kdb_boolean_t;
typedef unsigned char kdb_octet_t;
typedef signed short kdb_short_t;
typedef unsigned short kdb_unsigned_short_t;

#if SIZEOF_LONG == 4
// kdb_long = long
typedef long kdb_long_t;
typedef unsigned long kdb_unsigned_long_t;

#define ELEKTRA_LONG_F "%ld"
#define ELEKTRA_UNSIGNED_LONG_F "%lu"

#elif SIZEOF_INT == 4
// kdb_long = int
typedef int kdb_long_t;
typedef unsigned int kdb_unsigned_long_t;

#define ELEKTRA_LONG_F "%d"
#define ELEKTRA_UNSIGNED_LONG_F "%u"

#else
// no kdb_long
#error "Can't map kdb_long_t (4 bytes, 32 bits) to a native type."

#endif // kdb_long tests


#if SIZEOF_LONG == 8
// kdb_long_long = long
typedef long kdb_long_long_t;
typedef unsigned long kdb_unsigned_long_long_t;

#define ELEKTRA_LONG_LONG_F "%ld"
#define ELEKTRA_LONG_LONG_S strtol
#define ELEKTRA_UNSIGNED_LONG_LONG_F "%lu"
#define ELEKTRA_UNSIGNED_LONG_LONG_S strtoul

#elif defined(HAVE_SIZEOF_LONG_LONG) && (SIZEOF_LONG_LONG == 8)
// kdb_long_long = long long
typedef long long kdb_long_long_t;
typedef unsigned long long kdb_unsigned_long_long_t;

#define ELEKTRA_LONG_LONG_F "%lld"
#define ELEKTRA_LONG_LONG_S strtoll
#define ELEKTRA_UNSIGNED_LONG_LONG_F "%llu"
#define ELEKTRA_UNSIGNED_LONG_LONG_S strtoull

#else
// no kdb_long_long
#error "Can't map kdb_long_long_t (8 bytes, 64 bits) to a native type."

#endif // kdb_long_long tests

#endif // for C++11 or later/C99+/C89

// for C (and C++)

typedef unsigned char kdb_char_t;
typedef float kdb_float_t;
typedef double kdb_double_t;

// typedef wchar_t kdb_wchar_t; // wchar_t not supported!

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE >= 10)
#define ELEKTRA_HAVE_KDB_LONG_DOUBLE
// the long double data type represents an IEEE double-extended
// floating-point number, which has an exponent of at least 15 bits in
// length and a signed fraction of at least 64 bits
typedef long double kdb_long_double_t;
#endif

#if SIZEOF_TV_USEC == SIZEOF_LONG
#define ELEKTRA_TIME_USEC_F "%ld"
#elif SIZEOF_TV_USEC == SIZEOF_LONG_LONG
#define ELEKTRA_TIME_USEC_F "%lli"
#else
#define ELEKTRA_TIME_USEC_F "%d"
#endif

#if SIZEOF_STAT_ST_SIZE == 4
#define ELEKTRA_STAT_ST_SIZE_F "%" PRIu32
#elif SIZEOF_STAT_ST_SIZE == 8
#define ELEKTRA_STAT_ST_SIZE_F "%" PRIu64
#else
#define ELEKTRA_STAT_ST_SIZE_F "%llu"
#endif

// check floating point types
#ifdef __cplusplus
#include <limits>

static_assert (std::numeric_limits<float>::is_iec559, "float has to be IEEE-754 compliant");
static_assert (std::numeric_limits<float>::radix == 2 && std::numeric_limits<float>::digits == 24 &&
		       std::numeric_limits<float>::max_exponent >= 128 && std::numeric_limits<float>::min_exponent <= -125 &&
		       sizeof (float) == 4,
	       "float has to be IEEE-754 single precision");
static_assert (std::numeric_limits<double>::is_iec559, "double has to be IEEE-754 compliant");
static_assert (std::numeric_limits<double>::radix == 2 && std::numeric_limits<double>::digits == 53 &&
		       std::numeric_limits<double>::max_exponent >= 1024 && std::numeric_limits<double>::min_exponent <= -1021 &&
		       sizeof (double) == 8,
	       "double has to be IEEE-754 double precision");
static_assert (std::numeric_limits<long double>::is_iec559, "long double has to be IEEE-754 compliant");
#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
static_assert (std::numeric_limits<long double>::radix == 2 && std::numeric_limits<long double>::digits >= 64 &&
		       std::numeric_limits<long double>::max_exponent >= 1 << 14 &&
		       std::numeric_limits<long double>::min_exponent <= -(1 << 14) + 3 && sizeof (long double) >= 10,
	       "long double has to be at least 80 bits (1 bit sign, 15 bits exponent, 64 bits mantissa)");
#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE
#else
#include <float.h>
#include <math.h>

#if FLT_RADIX != 2 || FLT_MANT_DIG != 24 || FLT_MAX_EXP < 128 || FLT_MIN_EXP > -125 || SIZEOF_FLOAT != 4
#error "float must be 32 bit floating point type with 1 bit sign, 8 bits exponent and 23 bits mantissa"
#endif
#if FLT_RADIX != 2 || DBL_MANT_DIG != 53 || DBL_MAX_EXP < 1024 || DBL_MIN_EXP > -1021 || SIZEOF_DOUBLE != 8
#error "double must be 64 bit floating point type with 1 bit sign, 11 bits exponent and 52 bits mantissa"
#endif

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

#if FLT_RADIX != 2 || LDBL_MANT_DIG < 64 || LDBL_MAX_EXP < (1 << 14) || LDBL_MIN_EXP > -(1 << 14) + 3
#error "long double must be at least 80 bit floating point type with 1 bit sign, at least 15 bits exponent and at least 64 bits mantissa"
#endif

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE
#endif // __cplusplus

#endif
