/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#ifndef ENUM_ACTUAL_H
#define ENUM_ACTUAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <elektra.h>

#include <kdbhelper.h>

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


ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);

ELEKTRA_GET_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_SET_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumDisjointed, EnumDisjointed);



ELEKTRA_GET_SIGNATURE (ExistingColors, EnumExistingColors);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ExistingColors, EnumExistingColors);
ELEKTRA_SET_SIGNATURE (ExistingColors, EnumExistingColors);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ExistingColors, EnumExistingColors);

ELEKTRA_KEY_TO_SIGNATURE (Colors, EnumColors);
ELEKTRA_TO_STRING_SIGNATURE (Colors, EnumColors);

ELEKTRA_GET_SIGNATURE (Colors, EnumColors);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Colors, EnumColors);
ELEKTRA_SET_SIGNATURE (Colors, EnumColors);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (Colors, EnumColors);

ELEKTRA_KEY_TO_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_TO_STRING_SIGNATURE (ElektraEnumMyenum, EnumMyenum);

ELEKTRA_GET_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_SET_SIGNATURE (ElektraEnumMyenum, EnumMyenum);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (ElektraEnumMyenum, EnumMyenum);



// clang-format off

// clang-format on

#define ELEKTRA_STRUCT_FREE(cType, typeName) elektraFree##typeName
#define ELEKTRA_STRUCT_FREE_SIGNATURE(cType, typeName) void ELEKTRA_STRUCT_FREE (cType, typeName) (cType * ptr)





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




/**
 * Get the value of 'disjointed'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'disjointed'.
 */// 
static inline ElektraEnumDisjointed ELEKTRA_GET (Disjointed) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumDisjointed) (elektra, "disjointed");
}


/**
 * Set the value of 'disjointed'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'disjointed'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Disjointed) (Elektra * elektra, ElektraEnumDisjointed value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumDisjointed) (elektra, "disjointed", value, error);
}



/**
 * Get the value of 'existinggentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'existinggentype'.
 */// 
static inline ExistingColors ELEKTRA_GET (Existinggentype) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumExistingColors) (elektra, "existinggentype");
}


/**
 * Set the value of 'existinggentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'existinggentype'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Existinggentype) (Elektra * elektra, ExistingColors value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumExistingColors) (elektra, "existinggentype", value, error);
}



/**
 * Get the value of 'gentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'gentype'.
 */// 
static inline Colors ELEKTRA_GET (Gentype) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumColors) (elektra, "gentype");
}


/**
 * Set the value of 'gentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'gentype'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Gentype) (Elektra * elektra, Colors value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumColors) (elektra, "gentype", value, error);
}



/**
 * Get the value of 'gentype2'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'gentype2'.
 */// 
static inline Colors ELEKTRA_GET (Gentype2) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumColors) (elektra, "gentype2");
}


/**
 * Set the value of 'gentype2'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'gentype2'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Gentype2) (Elektra * elektra, Colors value,  ElektraError ** error)
{
	
	ELEKTRA_SET (EnumColors) (elektra, "gentype2", value, error);
}



/**
 * Get the value of 'myenum'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'myenum'.
 */// 
static inline ElektraEnumMyenum ELEKTRA_GET (Myenum) (Elektra * elektra )
{
	
	return ELEKTRA_GET (EnumMyenum) (elektra, "myenum");
}


/**
 * Set the value of 'myenum'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myenum'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Myenum) (Elektra * elektra, ElektraEnumMyenum value,  ElektraError ** error)
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


int loadConfiguration (Elektra ** elektra, ElektraError ** error);
void printHelpMessage (void);
void specloadCheck (int argc, const char ** argv);


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The tag to look up.
 *
 * @return The value stored at the given key and index.
 */// 
#define elektraGet(elektra, tag) ELEKTRA_GET (tag) (elektra)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The tag to look up.
 * @param ...     Variable arguments depending on the given tag.
 *
 * @return The value stored at the given key and index.
 */// 
#define elektraGetV(elektra, tag, ...) ELEKTRA_GET (tag) (elektra, __VA_ARGS__)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The tag to look up.
 * @param result  Points to the struct into which results will be stored.
 */// 
#define elektraGet2(elektra, result, tag) ELEKTRA_GET (tag) (elektra, result)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param result  Points to the struct into which results will be stored.
 * @param tag     The tag to look up.
 * @param ...     Variable arguments depending on the given tag.
 */// 
#define elektraGet2V(elektra, result, tag, ...) ELEKTRA_GET (tag) (elektra, result, __VA_ARGS__)


/**
 * @param elektra The elektra instance initialized with the loadConfiguration().
 * @param tag     The codegenerated Tag to write to.
 * @param value   The new value.
 * @param error   Pass a reference to an ElektraError pointer.
 */// 
#define elektraSet(elektra, tag, value, error) ELEKTRA_GET (tag) (elektra, value, error)


/**
 * @param elektra The elektra instance initialized with the loadConfiguration().
 * @param tag     The codegenerated Tag to write to.
 * @param value   The new value.
 * @param error   Pass a reference to an ElektraError pointer.
 * @param ...     Variable arguments depending on the given tag.
 */// 
#define elektraSetV(elektra, tag, value, error, ...) ELEKTRA_GET (tag) (elektra, value, __VA_ARGS__, error)

#ifdef __cplusplus
}
#endif

#endif // ENUM_ACTUAL_H
