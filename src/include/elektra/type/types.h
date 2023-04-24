/**
 * @file
 *
 * @brief Elektra’s data types for C and C++11.
 *
 * This header file defines portable Types used in Elektra for C and C++.
 *
 * The names used are based on the CORBA Type System,
 * but they are mapped 1:1 onto the C99 stdint types.
 * Therefore C99/C++11 is required.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

#ifndef ELEKTRA_TYPE_TYPES_H
#define ELEKTRA_TYPE_TYPES_H

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

#else // for C89
#error "C++11 or newer or C99 or newer is required"
#endif // for C++11 or later/C99+/C89

// for C (and C++)

typedef unsigned char kdb_char_t;
typedef float kdb_float_t;
typedef double kdb_double_t;
typedef long double kdb_long_double_t;
// typedef wchar_t kdb_wchar_t; // wchar_t not supported!

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
#ifdef ELEKTRA_REQUIRE_LONG_DOUBLE
static_assert (std::numeric_limits<long double>::radix == 2 && std::numeric_limits<long double>::digits >= 64 &&
		       std::numeric_limits<long double>::max_exponent >= 1 << 14 &&
		       std::numeric_limits<long double>::min_exponent <= -(1 << 14) + 3 && sizeof (long double) >= 10,
	       "long double has to be at least 80 bits (1 bit sign, 15 bits exponent, 64 bits mantissa)");
#else
static_assert (std::numeric_limits<long double>::radix == 2 && std::numeric_limits<long double>::digits >= 53 &&
		       std::numeric_limits<long double>::max_exponent >= 1024 && std::numeric_limits<long double>::min_exponent <= -1021 &&
		       sizeof (long double) >= 8,
	       "double has to be at least 64 bits (1 bit sign, 11 bits exponent, 52 bits mantissa)");
#endif
#else
#include <float.h>

#if FLT_RADIX != 2 || FLT_MANT_DIG != 24 || FLT_MAX_EXP < 128 || FLT_MIN_EXP > -125
#error "float must be 32 bit floating point type with 1 bit sign, 8 bits exponent and 23 bits mantissa"
#endif
#if FLT_RADIX != 2 || DBL_MANT_DIG != 53 || DBL_MAX_EXP < 1024 || DBL_MIN_EXP > -1021
#error "double must be 64 bit floating point type with 1 bit sign, 11 bits exponent and 52 bits mantissa"
#endif

#ifdef ELEKTRA_REQUIRE_LONG_DOUBLE
// only check size if asked to, otherwise accept if it is same as double
#if FLT_RADIX != 2 || LDBL_MANT_DIG < 64 || LDBL_MAX_EXP < (1 << 14) || LDBL_MIN_EXP > -(1 << 14) + 3
#error "long double must be at least 80 bit floating point type with 1 bit sign, at least 15 bits exponent and at least 64 bits mantissa"
#endif
#else
#if FLT_RADIX != 2 || LDBL_MANT_DIG < 53 || LDBL_MAX_EXP < 1024 || LDBL_MIN_EXP > -1021
#error "double must be at least 64 bit floating point type with 1 bit sign, at least 11 bits exponent and at least 52 bits mantissa"
#endif
#endif

#endif // __cplusplus

#endif // ELEKTRA_TYPE_TYPES_H
