/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#ifndef STRUCT_ACTUAL_H
#define STRUCT_ACTUAL_H

#include <elektra.h>







#define ELEKTRA_STRUCT_FREE(cType, typeName) elektraFree##typeName
#define ELEKTRA_STRUCT_FREE_SIGNATURE(cType, typeName) void ELEKTRA_STRUCT_FREE (cType, typeName) (cType * ptr)
typedef struct
{
	const char * a;
	kdb_long_t b;
} ElektraStructMystruct;



ELEKTRA_GET_OUT_PTR_SIGNATURE (ElektraStructMystruct, StructMystruct);
ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE (ElektraStructMystruct, StructMystruct);
ELEKTRA_SET_SIGNATURE (const ElektraStructMystruct *, StructMystruct);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const ElektraStructMystruct *, StructMystruct);


// clang-format off
#define ELEKTRA_TAG_MYSTRUCT MYSTRUCT
#define ELEKTRA_TAG_MYSTRUCT_A MYSTRUCT_A
#define ELEKTRA_TAG_MYSTRUCT_B MYSTRUCT_B
// clang-format on



/**
 * Get the value of 'mystruct'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param result  The value will be stored in the referenced variable.
 */// 
static inline void ELEKTRA_GET (MYSTRUCT) (Elektra * elektra, ElektraStructMystruct *result)
{
	ELEKTRA_GET (StructMystruct) (elektra, "mystruct", result);
}


/**
 * Get the value of 'mystruct'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param result  The value will be stored in the referenced variable.
 */// 
static inline void ELEKTRA_GET_ARRAY_ELEMENT (MYSTRUCT) (Elektra * elektra, kdb_long_long_t index, ElektraStructMystruct *result)
{
	ELEKTRA_GET_ARRAY_ELEMENT (StructMystruct) (elektra, "mystruct", index, result);
}


/**
 * Set the value of 'mystruct'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MYSTRUCT) (Elektra * elektra, const ElektraStructMystruct * value, ElektraError ** error)
{
	ELEKTRA_SET (StructMystruct) (elektra, "mystruct", value, error);
}


/**
 * Set element of array 'mystruct'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'mystruct'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (MYSTRUCT) (Elektra * elektra, kdb_long_long_t index,
								  const ElektraStructMystruct * value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (StructMystruct) (elektra, "mystruct", index, value, error);
}



/**
 * Get the value of 'mystruct/a'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'mystruct/a'.
 */// 
static inline const char * ELEKTRA_GET (MYSTRUCT_A) (Elektra * elektra)
{
	return ELEKTRA_GET (String) (elektra, "mystruct/a");
}


/**
 * Get element from array 'mystruct/a'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'mystruct/a'.
 */// 
static inline const char * ELEKTRA_GET_ARRAY_ELEMENT (MYSTRUCT_A) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (String) (elektra, "mystruct/a", index);
}


/**
 * Set the value of 'mystruct/a'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct/a'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MYSTRUCT_A) (Elektra * elektra, const char * value, ElektraError ** error)
{
	ELEKTRA_SET (String) (elektra, "mystruct/a", value, error);
}


/**
 * Set element of array 'mystruct/a'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'mystruct/a'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (MYSTRUCT_A) (Elektra * elektra, kdb_long_long_t index, const char * value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (String) (elektra, "mystruct/a", index, value, error);
}


/**
 * Get the value of 'mystruct/b'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'mystruct/b'.
 */// 
static inline kdb_long_t ELEKTRA_GET (MYSTRUCT_B) (Elektra * elektra)
{
	return ELEKTRA_GET (Long) (elektra, "mystruct/b");
}


/**
 * Get element from array 'mystruct/b'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'mystruct/b'.
 */// 
static inline kdb_long_t ELEKTRA_GET_ARRAY_ELEMENT (MYSTRUCT_B) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (Long) (elektra, "mystruct/b", index);
}


/**
 * Set the value of 'mystruct/b'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct/b'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MYSTRUCT_B) (Elektra * elektra, kdb_long_t value, ElektraError ** error)
{
	ELEKTRA_SET (Long) (elektra, "mystruct/b", value, error);
}


/**
 * Set element of array 'mystruct/b'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'mystruct/b'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (MYSTRUCT_B) (Elektra * elektra, kdb_long_long_t index, kdb_long_t value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (Long) (elektra, "mystruct/b", index, value, error);
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

#endif // STRUCT_ACTUAL_H
