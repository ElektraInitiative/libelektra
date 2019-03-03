/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#ifndef SIMPLE_ACTUAL_H
#define SIMPLE_ACTUAL_H

#include <elektra.h>









// clang-format off
#define ELEKTRA_TAG_MYDOUBLE MYDOUBLE
#define ELEKTRA_TAG_MYINT MYINT
#define ELEKTRA_TAG_MYSTRING MYSTRING
#define ELEKTRA_TAG_PRINT PRINT
// clang-format on



/**
 * Get the value of 'mydouble'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'mydouble'.
 */// 
static inline kdb_double_t ELEKTRA_GET (MYDOUBLE) (Elektra * elektra)
{
	return ELEKTRA_GET (Double) (elektra, "mydouble");
}


/**
 * Get element from array 'mydouble'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'mydouble'.
 */// 
static inline kdb_double_t ELEKTRA_GET_ARRAY_ELEMENT (MYDOUBLE) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (Double) (elektra, "mydouble", index);
}


/**
 * Set the value of 'mydouble'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mydouble'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MYDOUBLE) (Elektra * elektra, kdb_double_t value, ElektraError ** error)
{
	ELEKTRA_SET (Double) (elektra, "mydouble", value, error);
}


/**
 * Set element of array 'mydouble'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'mydouble'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (MYDOUBLE) (Elektra * elektra, kdb_long_long_t index, kdb_double_t value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (Double) (elektra, "mydouble", index, value, error);
}


/**
 * Get the value of 'myint'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'myint'.
 */// 
static inline kdb_long_t ELEKTRA_GET (MYINT) (Elektra * elektra)
{
	return ELEKTRA_GET (Long) (elektra, "myint");
}


/**
 * Get element from array 'myint'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'myint'.
 */// 
static inline kdb_long_t ELEKTRA_GET_ARRAY_ELEMENT (MYINT) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (Long) (elektra, "myint", index);
}


/**
 * Set the value of 'myint'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myint'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MYINT) (Elektra * elektra, kdb_long_t value, ElektraError ** error)
{
	ELEKTRA_SET (Long) (elektra, "myint", value, error);
}


/**
 * Set element of array 'myint'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'myint'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (MYINT) (Elektra * elektra, kdb_long_long_t index, kdb_long_t value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (Long) (elektra, "myint", index, value, error);
}


/**
 * Get the value of 'mystring'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'mystring'.
 */// 
static inline const char * ELEKTRA_GET (MYSTRING) (Elektra * elektra)
{
	return ELEKTRA_GET (String) (elektra, "mystring");
}


/**
 * Get element from array 'mystring'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'mystring'.
 */// 
static inline const char * ELEKTRA_GET_ARRAY_ELEMENT (MYSTRING) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (String) (elektra, "mystring", index);
}


/**
 * Set the value of 'mystring'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystring'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MYSTRING) (Elektra * elektra, const char * value, ElektraError ** error)
{
	ELEKTRA_SET (String) (elektra, "mystring", value, error);
}


/**
 * Set element of array 'mystring'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'mystring'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (MYSTRING) (Elektra * elektra, kdb_long_long_t index, const char * value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (String) (elektra, "mystring", index, value, error);
}


/**
 * Get the value of 'print'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'print'.
 */// 
static inline kdb_boolean_t ELEKTRA_GET (PRINT) (Elektra * elektra)
{
	return ELEKTRA_GET (Boolean) (elektra, "print");
}


/**
 * Get element from array 'print'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 *
 * @return element @p index from array 'print'.
 */// 
static inline kdb_boolean_t ELEKTRA_GET_ARRAY_ELEMENT (PRINT) (Elektra * elektra, kdb_long_long_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT (Boolean) (elektra, "print", index);
}


/**
 * Set the value of 'print'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'print'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (PRINT) (Elektra * elektra, kdb_boolean_t value, ElektraError ** error)
{
	ELEKTRA_SET (Boolean) (elektra, "print", value, error);
}


/**
 * Set element of array 'print'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index   The array elements index.
 * @param value   The value of 'print'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET_ARRAY_ELEMENT (PRINT) (Elektra * elektra, kdb_long_long_t index, kdb_boolean_t value,
								  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT (Boolean) (elektra, "print", index, value, error);
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

#endif // SIMPLE_ACTUAL_H
