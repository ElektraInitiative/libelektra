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

#if __cplusplus > 199711L
// for C++11
namespace elektra
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
	using char_t = char; // default: 0
	// using wchar_t; // default: 0 wchar_t not supported!
	using boolean_t = bool; // default: false
	using octet_t = uint8_t; // default: 0
}
#else
// for C
typedef unsigned char             elektra_boolean;
typedef unsigned char             elektra_char;
typedef unsigned char             elektra_octet;
typedef short                     elektra_short;
typedef unsigned short            elektra_unsigned_short;
#if SIZEOF_LONG == 4
typedef long                      elektra_long;
typedef unsigned long             elektra_unsigned_long;
#elif SIZEOF_INT == 4
typedef int                       elektra_long;
typedef unsigned int              elektra_unsigned_long;
#else
# error "Can't map elektra_long (32 bits) to a native type."
#endif

// typedef wchar_t                   elektra_wchar; // wchar_t not supported!

#if SIZEOF_LONG == 8
typedef long                      elektra_long_long;
typedef unsigned long             elektra_unsigned_long_long;
#elif defined(HAVE_SIZEOF_LONG_LONG) && (SIZEOF_LONG_LONG == 8)
typedef long long                 elektra_long_long;
typedef unsigned long long        elektra_unsigned_long_long;
#endif

typedef float                     elektra_float;
typedef double                    elektra_double;

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16)
typedef long double               elektra_long_double;
#elif defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 12) && defined(__i386__)
typedef long double               elektra_long_double;
#endif

#endif

#endif
