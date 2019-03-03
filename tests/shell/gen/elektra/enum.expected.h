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

#include <elektra.h>

#include "colors.h"

typedef enum
{
	ELEKTRA_ENUM_DISJOINTED_BLACK = 0,
	ELEKTRA_ENUM_DISJOINTED_WHITE = 2,
} ElektraEnumDisjointed;


typedef enum
{
	COLORS_RED = 0,
	COLORS_GREEN = 1,
	COLORS_BLUE = 2,
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
#define ELEKTRA_TAG_DISJOINTED DISJOINTED
#define ELEKTRA_TAG_EXISTINGGENTYPE EXISTINGGENTYPE
#define ELEKTRA_TAG_GENTYPE GENTYPE
#define ELEKTRA_TAG_GENTYPE2 GENTYPE2
#define ELEKTRA_TAG_MYENUM MYENUM
// clang-format on



/**
 * Get the value of 'disjointed'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'disjointed'.
 */// 
static inline ElektraEnumDisjointed ELEKTRA_GET (DISJOINTED) (Elektra * elektra)
{
	return ELEKTRA_GET (EnumDisjointed) (elektra, "disjointed");
}


/**
 * Get element from array 'disjointed'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'disjointed'.
 */// 
static inline ElektraEnumDisjointed ELEKTRA_GET_ARRAY_ELEMENT (DISJOINTED) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (EnumDisjointed) (elektra, "disjointed", index);
}


/**
 * Set the value of 'disjointed'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'disjointed'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (DISJOINTED) (Elektra * elektra, ElektraEnumDisjointed value, ElektraError ** error)
{
	ELEKTRA_SET (EnumDisjointed) (elektra, "disjointed", value, error);
}


/**
 * Set element of array 'disjointed'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'disjointed'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (DISJOINTED) (Elektra * elektra, kdb_long_long_t index, ElektraEnumDisjointed value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (EnumDisjointed) (elektra, "disjointed", index, value, error);
}


/**
 * Get the value of 'existinggentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'existinggentype'.
 */// 
static inline ExistingColors ELEKTRA_GET (EXISTINGGENTYPE) (Elektra * elektra)
{
	return ELEKTRA_GET (EnumExistingColors) (elektra, "existinggentype");
}


/**
 * Get element from array 'existinggentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'existinggentype'.
 */// 
static inline ExistingColors ELEKTRA_GET_ARRAY_ELEMENT (EXISTINGGENTYPE) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (EnumExistingColors) (elektra, "existinggentype", index);
}


/**
 * Set the value of 'existinggentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'existinggentype'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (EXISTINGGENTYPE) (Elektra * elektra, ExistingColors value, ElektraError ** error)
{
	ELEKTRA_SET (EnumExistingColors) (elektra, "existinggentype", value, error);
}


/**
 * Set element of array 'existinggentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'existinggentype'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (EXISTINGGENTYPE) (Elektra * elektra, kdb_long_long_t index, ExistingColors value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (EnumExistingColors) (elektra, "existinggentype", index, value, error);
}


/**
 * Get the value of 'gentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'gentype'.
 */// 
static inline Colors ELEKTRA_GET (GENTYPE) (Elektra * elektra)
{
	return ELEKTRA_GET (EnumColors) (elektra, "gentype");
}


/**
 * Get element from array 'gentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'gentype'.
 */// 
static inline Colors ELEKTRA_GET_ARRAY_ELEMENT (GENTYPE) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (EnumColors) (elektra, "gentype", index);
}


/**
 * Set the value of 'gentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'gentype'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (GENTYPE) (Elektra * elektra, Colors value, ElektraError ** error)
{
	ELEKTRA_SET (EnumColors) (elektra, "gentype", value, error);
}


/**
 * Set element of array 'gentype'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'gentype'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (GENTYPE) (Elektra * elektra, kdb_long_long_t index, Colors value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (EnumColors) (elektra, "gentype", index, value, error);
}


/**
 * Get the value of 'gentype2'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'gentype2'.
 */// 
static inline Colors ELEKTRA_GET (GENTYPE2) (Elektra * elektra)
{
	return ELEKTRA_GET (EnumColors) (elektra, "gentype2");
}


/**
 * Get element from array 'gentype2'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'gentype2'.
 */// 
static inline Colors ELEKTRA_GET_ARRAY_ELEMENT (GENTYPE2) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (EnumColors) (elektra, "gentype2", index);
}


/**
 * Set the value of 'gentype2'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'gentype2'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (GENTYPE2) (Elektra * elektra, Colors value, ElektraError ** error)
{
	ELEKTRA_SET (EnumColors) (elektra, "gentype2", value, error);
}


/**
 * Set element of array 'gentype2'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'gentype2'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (GENTYPE2) (Elektra * elektra, kdb_long_long_t index, Colors value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (EnumColors) (elektra, "gentype2", index, value, error);
}


/**
 * Get the value of 'myenum'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'myenum'.
 */// 
static inline ElektraEnumMyenum ELEKTRA_GET (MYENUM) (Elektra * elektra)
{
	return ELEKTRA_GET (EnumMyenum) (elektra, "myenum");
}


/**
 * Get element from array 'myenum'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'myenum'.
 */// 
static inline ElektraEnumMyenum ELEKTRA_GET_ARRAY_ELEMENT (MYENUM) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (EnumMyenum) (elektra, "myenum", index);
}


/**
 * Set the value of 'myenum'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myenum'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MYENUM) (Elektra * elektra, ElektraEnumMyenum value, ElektraError ** error)
{
	ELEKTRA_SET (EnumMyenum) (elektra, "myenum", value, error);
}


/**
 * Set element of array 'myenum'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'myenum'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (MYENUM) (Elektra * elektra, kdb_long_long_t index, ElektraEnumMyenum value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (EnumMyenum) (elektra, "myenum", index, value, error);
}

Elektra * loadConfiguration (ElektraError ** error);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The tag to look up.
 * @return The value stored at the given key and index.
 */
#define elektraGet(elektra, tag) ELEKTRA_GET (tag) (elektra)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The tag to look up.
 * @param result Points to the struct into which results will be stored.
 */
#define elektraGet2(elektra, tag, result) ELEKTRA_GET (tag) (elektra, result)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The tag to look up.
 * @param index The array index of the desired element, starting with 0.
 * @return The value stored at the given key and index.
 */
#define elektraGetArrayElement(elektra, tag, index) ELEKTRA_GET_ARRAY_ELEMENT (tag) (elektra, index)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The tag to look up.
 * @param index The array index of the desired element, starting with 0.
 * @param result Points to the struct into which results will be stored.
 */
#define elektraGetArrayElement2(elektra, tag, index, result) ELEKTRA_GET_ARRAY_ELEMENT (tag) (elektra, index, result)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The codegenerated Tag to write to.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
#define elektraSet(elektra, tag, value, error) ELEKTRA_GET (tag) (elektra, value, error)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The code-generated tag to write to.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 * @param ... Strings to replace dynamic parts (_, #) of keyname.
 */
#define elektraSetArrayElement(elektra, tag, index, value, error) ELEKTRA_SET_ARRAY_ELEMENT (tag) (elektra, index, value, error)

#endif // ENUM_ACTUAL_H
