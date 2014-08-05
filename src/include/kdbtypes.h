/***************************************************************************
             kdbtypes.h  -  Elektra's type system for C and C++11
                             -------------------
    begin                : Sat, 01 Mar 2014 17:49:01 +0100
    copyright            : (C) 2014 by Markus raab
    email                : elektra@markus-raab.org

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/* This header file defines portable Types used in Elektra for C and
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
 * This files defines mappings to C and C++11
 * */

#ifndef KDBTYPES_H
#define KDBTYPES_H

#include "kdbconfig.h"

#ifdef __cplusplus
#if __cplusplus <= 199711L
// for C++97 or older

#include <inttypes.h>

namespace kdb
{
	typedef int16_t short_t; // default: 0
	typedef int32_t long_t; // default: 0
	typedef int64_t long_long_t; // default: 0
	typedef uint16_t unsigned_short_t; // default: 0
	typedef uint32_t unsigned_long_t; // default: 0
	typedef uint64_t unsigned_long_long_t; // default: 0
	typedef float float_t; // default: 0.0
	typedef double double_t; // default: 0.0
	typedef long double long_double_t; // default: 0.0
	typedef unsigned char char_t; // default: 0
	typedef bool boolean_t; // default: false
	typedef uint8_t octet_t; // default: 0
	// using wchar_t; // default: 0 wchar_t not supported!
}
#else
// for C++11 or later

namespace kdb
{
	using short_t = int16_t; // default: 0
	using long_t = int32_t; // default: 0
	using long_long_t = int64_t; // default: 0
	using unsigned_short_t = uint16_t; // default: 0
	using unsigned_long_t = uint32_t; // default: 0
	using unsigned_long_long_t = uint64_t; // default: 0
	using float_t = float; // default: 0.0
	using double_t = double; // default: 0.0
	using long_double_t = long double; // default: 0.0
	using char_t = unsigned char; // default: 0
	// using wchar_t; // default: 0 wchar_t not supported!
	using boolean_t = bool; // default: false
	using octet_t = uint8_t; // default: 0
}
#endif
#else
// for C

typedef unsigned char             kdb_boolean_t;
typedef unsigned char             kdb_char_t;
typedef unsigned char             kdb_octet_t;
typedef signed short              kdb_short_t;
typedef unsigned short            kdb_unsigned_short_t;

#if SIZEOF_LONG == 4
#define ELEKTRA_LONG_F "%ld"
typedef long                      kdb_long_t;
#define ELEKTRA_UNSIGNED_LONG_F "%lu"
typedef unsigned long             kdb_unsigned_long_t;
#elif SIZEOF_INT == 4
#define ELEKTRA_LONG_F "%d"
typedef int                       kdb_long_t;
#define ELEKTRA_UNSIGNED_LONG_F "%u"
typedef unsigned int              kdb_unsigned_long_t;
#else
# error "Can't map kdb_long_t (4 bytes, 32 bits) to a native type."
#endif

// typedef wchar_t                   kdb_wchar_t; // wchar_t not supported!

#if SIZEOF_LONG == 8
#define ELEKTRA_LONG_LONG_F "%ld"
#define ELEKTRA_LONG_LONG_S strtol
typedef long                      kdb_long_long_t;
#define ELEKTRA_UNSIGNED_LONG_LONG_F "%lu"
#define ELEKTRA_UNSIGNED_LONG_LONG_S strtoul
typedef unsigned long             kdb_unsigned_long_long_t;
#elif defined(HAVE_SIZEOF_LONG_LONG) && (SIZEOF_LONG_LONG == 8)
#define ELEKTRA_LONG_LONG_F "%lld"
#define ELEKTRA_LONG_LONG_S strtoll
typedef long long                 kdb_long_long_t;
#define ELEKTRA_UNSIGNED_LONG_LONG_F "%llu"
#define ELEKTRA_UNSIGNED_LONG_LONG_S strtoull
typedef unsigned long long        kdb_unsigned_long_long_t;
#endif

typedef float                     kdb_float_t;
typedef double                    kdb_double_t;

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16)
typedef long double               kdb_long_double_t;
#elif defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 12) && defined(__i386__)
typedef long double               kdb_long_double_t;
#endif

#endif

#endif
