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


#ifndef ENUM_ACTUAL_H
#define ENUM_ACTUAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <elektra/highlevel.h>

#include <string.h>

#include "colors.h"



// clang-format off

// clang-format on

typedef enum
{
	ELEKTRA_ENUM_DISJOINTED_BLACK = 0,
	ELEKTRA_ENUM_DISJOINTED_WHITE = 255,
} ElektraEnumDisjointed;


typedef enum
{
	COLORS_NONE = NO_VALUE,
	COLORS_RED = 1,
	COLORS_GREEN = 1 << 1,
	COLORS_BLUE = 1 << 2,
} Colors;

typedef enum
{
	ELEKTRA_ENUM_MYENUM_RED = 0,
	ELEKTRA_ENUM_MYENUM_GREEN = 1,
	ELEKTRA_ENUM_MYENUM_BLUE = 2,
	ELEKTRA_ENUM_MYENUM_BLUEISH = 3,
	ELEKTRA_ENUM_MYENUM_BROWN = 4,
	ELEKTRA_ENUM_MYENUM_GRAY = 5,
} ElektraEnumMyenum;


#define ELEKTRA_TO_CONST_STRING(typeName) ELEKTRA_CONCAT (ELEKTRA_CONCAT (elektra, typeName), ToConstString)
#define ELEKTRA_TO_CONST_STRING_SIGNATURE(cType, typeName) const char * ELEKTRA_TO_CONST_STRING (typeName) (cType value)

ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_TO_CONST_STRING_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);

ELEKTRA_GET_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_SET_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);

ELEKTRA_KEY_TO_SIGNATURE (ExistingColors, EnumExistingColors);
ELEKTRA_TO_STRING_SIGNATURE (ExistingColors, EnumExistingColors);
ELEKTRA_TO_CONST_STRING_SIGNATURE (ExistingColors, EnumExistingColors);

ELEKTRA_GET_SIGNATURE (ExistingColors, EnumExistingColors);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ExistingColors, EnumExistingColors);
ELEKTRA_SET_SIGNATURE (ExistingColors, EnumExistingColors);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ExistingColors, EnumExistingColors);

ELEKTRA_KEY_TO_SIGNATURE (Colors, EnumColors);
ELEKTRA_TO_STRING_SIGNATURE (Colors, EnumColors);
ELEKTRA_TO_CONST_STRING_SIGNATURE (Colors, EnumColors);

ELEKTRA_GET_SIGNATURE (Colors, EnumColors);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Colors, EnumColors);
ELEKTRA_SET_SIGNATURE (Colors, EnumColors);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (Colors, EnumColors);

ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_TO_CONST_STRING_SIGNATURE (ElektraEnumMyenum, EnumMyenum);

ELEKTRA_GET_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_SET_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum);



// clang-format off

// clang-format on

#define ELEKTRA_UNION_FREE(typeName) ELEKTRA_CONCAT (elektraFree, typeName)
#define ELEKTRA_UNION_FREE_SIGNATURE(cType, typeName, discrType) void ELEKTRA_UNION_FREE (typeName) (cType * ptr, discrType discriminator)

#define ELEKTRA_UNION_GET_SIGNATURE(cType, typeName, discrType)                                                                            \
	cType ELEKTRA_GET (typeName) (Elektra * elektra, const char * keyname, discrType discriminator)
#define ELEKTRA_UNION_GET_ARRAY_ELEMENT_SIGNATURE(cType, typeName, discrType)                                                              \
	cType ELEKTRA_GET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyname, kdb_long_long_t index, discrType discriminator)
#define ELEKTRA_UNION_SET_SIGNATURE(cType, typeName, discrType)                                                                            \
	void ELEKTRA_SET (typeName) (Elektra * elektra, const char * keyname, cType value, discrType discriminator, ElektraError ** error)
#define ELEKTRA_UNION_SET_ARRAY_ELEMENT_SIGNATURE(cType, typeName, discrType)                                                              \
	void ELEKTRA_SET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyname, kdb_long_long_t index, cType value,            \
						   discrType discriminator, ElektraError ** error)






// clang-format off

// clang-format on

#define ELEKTRA_STRUCT_FREE(typeName) ELEKTRA_CONCAT (elektraFree, typeName)
#define ELEKTRA_STRUCT_FREE_SIGNATURE(cType, typeName) void ELEKTRA_STRUCT_FREE (typeName) (cType * ptr)






// clang-format off

// clang-format on

// clang-format off

/**
* Tag name for 'disjointed'
* 
*/// 
#define ELEKTRA_TAG_DISJOINTED Disjointed

/**
* Tag name for 'existinggentype'
* 
*/// 
#define ELEKTRA_TAG_EXISTINGGENTYPE Existinggentype

/**
* Tag name for 'gentype'
* 
*/// 
#define ELEKTRA_TAG_GENTYPE Gentype

/**
* Tag name for 'gentype2'
* 
*/// 
#define ELEKTRA_TAG_GENTYPE2 Gentype2

/**
* Tag name for 'myenum'
* 
*/// 
#define ELEKTRA_TAG_MYENUM Myenum
// clang-format on


// clang-format off

// clang-format on

// local helper macros to determine the length of a 64 bit integer
#define elektra_len19(x) ((x) < 10000000000000000000ULL ? 19 : 20)
#define elektra_len18(x) ((x) < 1000000000000000000ULL ? 18 : elektra_len19 (x))
#define elektra_len17(x) ((x) < 100000000000000000ULL ? 17 : elektra_len18 (x))
#define elektra_len16(x) ((x) < 10000000000000000ULL ? 16 : elektra_len17 (x))
#define elektra_len15(x) ((x) < 1000000000000000ULL ? 15 : elektra_len16 (x))
#define elektra_len14(x) ((x) < 100000000000000ULL ? 14 : elektra_len15 (x))
#define elektra_len13(x) ((x) < 10000000000000ULL ? 13 : elektra_len14 (x))
#define elektra_len12(x) ((x) < 1000000000000ULL ? 12 : elektra_len13 (x))
#define elektra_len11(x) ((x) < 100000000000ULL ? 11 : elektra_len12 (x))
#define elektra_len10(x) ((x) < 10000000000ULL ? 10 : elektra_len11 (x))
#define elektra_len09(x) ((x) < 1000000000ULL ? 9 : elektra_len10 (x))
#define elektra_len08(x) ((x) < 100000000ULL ? 8 : elektra_len09 (x))
#define elektra_len07(x) ((x) < 10000000ULL ? 7 : elektra_len08 (x))
#define elektra_len06(x) ((x) < 1000000ULL ? 6 : elektra_len07 (x))
#define elektra_len05(x) ((x) < 100000ULL ? 5 : elektra_len06 (x))
#define elektra_len04(x) ((x) < 10000ULL ? 4 : elektra_len05 (x))
#define elektra_len03(x) ((x) < 1000ULL ? 3 : elektra_len04 (x))
#define elektra_len02(x) ((x) < 100ULL ? 2 : elektra_len03 (x))
#define elektra_len01(x) ((x) < 10ULL ? 1 : elektra_len02 (x))
#define elektra_len00(x) ((x) < 0ULL ? 0 : elektra_len01 (x))
#define elektra_len(x) elektra_len00 (x)

#define ELEKTRA_SIZE(tagName) ELEKTRA_CONCAT (elektraSize, tagName)




/**
 * Get the value of key 'disjointed' (tag #ELEKTRA_TAG_DISJOINTED).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'disjointed'.

 */// 
static inline ElektraEnumDisjointed ELEKTRA_GET (ELEKTRA_TAG_DISJOINTED) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumDisjointed) (elektra, "disjointed");
}


/**
 * Set the value of key 'disjointed' (tag #ELEKTRA_TAG_DISJOINTED).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'disjointed'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_DISJOINTED) (Elektra * elektra,
						      ElektraEnumDisjointed value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumDisjointed) (elektra, "disjointed", value, error);
}




/**
 * Get the value of key 'existinggentype' (tag #ELEKTRA_TAG_EXISTINGGENTYPE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'existinggentype'.

 */// 
static inline ExistingColors ELEKTRA_GET (ELEKTRA_TAG_EXISTINGGENTYPE) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumExistingColors) (elektra, "existinggentype");
}


/**
 * Set the value of key 'existinggentype' (tag #ELEKTRA_TAG_EXISTINGGENTYPE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'existinggentype'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_EXISTINGGENTYPE) (Elektra * elektra,
						      ExistingColors value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumExistingColors) (elektra, "existinggentype", value, error);
}




/**
 * Get the value of key 'gentype' (tag #ELEKTRA_TAG_GENTYPE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'gentype'.

 */// 
static inline Colors ELEKTRA_GET (ELEKTRA_TAG_GENTYPE) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumColors) (elektra, "gentype");
}


/**
 * Set the value of key 'gentype' (tag #ELEKTRA_TAG_GENTYPE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'gentype'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GENTYPE) (Elektra * elektra,
						      Colors value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumColors) (elektra, "gentype", value, error);
}




/**
 * Get the value of key 'gentype2' (tag #ELEKTRA_TAG_GENTYPE2).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'gentype2'.

 */// 
static inline Colors ELEKTRA_GET (ELEKTRA_TAG_GENTYPE2) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumColors) (elektra, "gentype2");
}


/**
 * Set the value of key 'gentype2' (tag #ELEKTRA_TAG_GENTYPE2).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'gentype2'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GENTYPE2) (Elektra * elektra,
						      Colors value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumColors) (elektra, "gentype2", value, error);
}




/**
 * Get the value of key 'myenum' (tag #ELEKTRA_TAG_MYENUM).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'myenum'.

 */// 
static inline ElektraEnumMyenum ELEKTRA_GET (ELEKTRA_TAG_MYENUM) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumMyenum) (elektra, "myenum");
}


/**
 * Set the value of key 'myenum' (tag #ELEKTRA_TAG_MYENUM).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myenum'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_MYENUM) (Elektra * elektra,
						      ElektraEnumMyenum value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumMyenum) (elektra, "myenum", value, error);
}


#undef elektra_len19
#undef elektra_len18
#undef elektra_len17
#undef elektra_len16
#undef elektra_len15
#undef elektra_len14
#undef elektra_len13
#undef elektra_len12
#undef elektra_len11
#undef elektra_len10
#undef elektra_len09
#undef elektra_len08
#undef elektra_len07
#undef elektra_len06
#undef elektra_len05
#undef elektra_len04
#undef elektra_len03
#undef elektra_len02
#undef elektra_len01
#undef elektra_len00
#undef elektra_len


int loadConfiguration (Elektra ** elektra,
				 int argc, const char * const * argv, const char * const * envp,
				 
				 ElektraError ** error);
void printHelpMessage (Elektra * elektra, const char * usage, const char * prefix);
void exitForSpecload (int argc, const char * const * argv);




/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The tag to look up.
 *
 * @return The value stored at the given key.
 *   The lifetime of returned pointers is documented in the ELEKTRA_GET(*) functions above.
 */// 
#define elektraGet(elektra, tag) ELEKTRA_GET (tag) (elektra)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The tag to look up.
 * @param ...     Variable arguments depending on the given tag.
 *
 * @return The value stored at the given key.
 *   The lifetime of returned pointers is documented in the ELEKTRA_GET(*) functions above.
 */// 
#define elektraGetV(elektra, tag, ...) ELEKTRA_GET (tag) (elektra, __VA_ARGS__)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param result  Points to the struct into which results will be stored.
 *   The lifetime of pointers in this struct is documented in the ELEKTRA_GET(*) functions above.
 * @param tag     The tag to look up.
 */// 
#define elektraFillStruct(elektra, result, tag) ELEKTRA_GET (tag) (elektra, result)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param result  Points to the struct into which results will be stored.
 *   The lifetime of pointers in this struct is documented in the ELEKTRA_GET(*) functions above.
 * @param tag     The tag to look up.
 * @param ...     Variable arguments depending on the given tag.
 */// 
#define elektraFillStructV(elektra, result, tag, ...) ELEKTRA_GET (tag) (elektra, result, __VA_ARGS__)


/**
 * @param elektra The elektra instance initialized with the loadConfiguration().
 * @param tag     The tag to write to.
 * @param value   The new value.
 * @param error   Pass a reference to an ElektraError pointer.
 */// 
#define elektraSet(elektra, tag, value, error) ELEKTRA_SET (tag) (elektra, value, error)


/**
 * @param elektra The elektra instance initialized with the loadConfiguration().
 * @param tag     The tag to write to.
 * @param value   The new value.
 * @param error   Pass a reference to an ElektraError pointer.
 * @param ...     Variable arguments depending on the given tag.
 */// 
#define elektraSetV(elektra, tag, value, error, ...) ELEKTRA_SET (tag) (elektra, value, __VA_ARGS__, error)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The array tag to look up.
 *
 * @return The size of the array below the given key.
 */// 
#define elektraSize(elektra, tag) ELEKTRA_SIZE (tag) (elektra)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The array tag to look up.
 * @param ...     Variable arguments depending on the given tag.
 *
 * @return The size of the array below the given key.
 */// 
#define elektraSizeV(elektra, tag, ...) ELEKTRA_SIZE (tag) (elektra, __VA_ARGS__)

#ifdef __cplusplus
}
#endif

#endif // ENUM_ACTUAL_H
