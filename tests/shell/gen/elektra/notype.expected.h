/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#ifndef NOTYPE_ACTUAL_H
#define NOTYPE_ACTUAL_H

#include <elektra.h>









// clang-format off

// clang-format on



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

#endif // NOTYPE_ACTUAL_H
