/**
 * @file
 *
 * @brief Included by elektra.h and code generator. Used by the code generator to override the default generic getters/setters. Therefore it has deliberately not an include guard.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#undef elektraSet
#undef elektraSetArrayElement
#undef elektraGet
#undef elektraGetArrayElement

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The codegenerated Tag to write to.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
#define elektraSet(ELEKTRA, TAG, VALUE, ERROR)                                                                                             \
	_Generic((TAG), ELEKTRA_TAG_NAMES_GEN (ELEKTRA_GENERIC_SET_ENTRY) ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_SET_ENTRY)      \
				ELEKTRA_TAG (String)                                                                                       \
		 : ELEKTRA_SET_BY_TAG (String)) (ELEKTRA, TAG, VALUE, ERROR)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The codegenerated Tag to write to.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
#define elektraSetEnum(ELEKTRA, TAG, VALUE, ERROR)                                                                                                \
_Generic((TAG), ELEKTRA_TAG_NAMES_GEN (ELEKTRA_GENERIC_SET_ENTRY) ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_SET_ENTRY)      \
char * : __elektraSetEnum \
) (ELEKTRA, TAG, VALUE, ERROR)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keynameOrTag The keyname (or a codegenerated Tag) to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
#define elektraSetArrayElement(ELEKTRA, TAG, VALUE, INDEX, ERROR)                                                                          \
	_Generic((TAG), ELEKTRA_TAG_NAMES_GEN (ELEKTRA_GENERIC_SET_ARRAY_ELEMENT_ENTRY)                                                    \
				ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_SET_ARRAY_ELEMENT_ENTRY) ELEKTRA_TAG (String)             \
		 : ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (String)) (ELEKTRA, TAG, VALUE, INDEX, ERROR)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyName The keyname (or a codegenerated Tag) to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0. \
 * @return The value stored at the given key and index.
 */
#define elektraSetEnumArrayElement(ELEKTRA, TAG, VALUE, INDEX, ERROR)                                                                                        \
_Generic((TAG), ELEKTRA_TAG_NAMES_GEN (ELEKTRA_GENERIC_SET_ARRAY_ELEMENT_ENTRY)                                                  \
char * : __elektraSetEnumArrayElement \
) (ELEKTRA, TAG, VALUE, INDEX, ERROR)


/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 * @return The value stored at the given key and index.
*/
#define elektraGet(ELEKTRA, TAG)                                                                                                           \
	_Generic((TAG), ELEKTRA_TAG_NAMES_GEN (ELEKTRA_GENERIC_GET_ENTRY) ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_GET_ENTRY)      \
				ELEKTRA_TAG (String)                                                                                       \
		 : ELEKTRA_GET_BY_TAG (String)) (ELEKTRA, TAG)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 * @return The value stored at the given key and index.
 */
#define elektraGetEnum(ELEKTRA, TAG)                                                                                                           \
_Generic((TAG), ELEKTRA_TAG_NAMES_GEN (ELEKTRA_GENERIC_GET_ENTRY) ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_GET_ENTRY)      \
char * : __elektraGetEnum \
) (ELEKTRA, TAG)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyName The keyname (or a codegenerated Tag) to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0. \
 * @return The value stored at the given key and index.
*/
#define elektraGetArrayElement(ELEKTRA, TAG, INDEX)                                                                                        \
	_Generic((TAG), ELEKTRA_TAG_NAMES_GEN (ELEKTRA_GENERIC_GET_ARRAY_ELEMENT_ENTRY)                                                    \
				ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_GET_ARRAY_ELEMENT_ENTRY) ELEKTRA_TAG (String)             \
		 : ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (String)) (ELEKTRA, TAG, INDEX)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyName The keyname (or a codegenerated Tag) to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0. \
 * @return The value stored at the given key and index.
 */
#define elektraGetEnumArrayElement(ELEKTRA, TAG, INDEX)                                                                                        \
_Generic((TAG), ELEKTRA_TAG_NAMES_GEN (ELEKTRA_GENERIC_GET_ARRAY_ELEMENT_ENTRY)                                                  \
char * : __elektraGetEnumArrayElement \
) (ELEKTRA, TAG, INDEX)
