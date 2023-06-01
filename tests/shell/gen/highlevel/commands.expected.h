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


#ifndef COMMANDS_ACTUAL_H
#define COMMANDS_ACTUAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <elektra/highlevel.h>

#include <internal/utility/format.h>
#include <internal/utility/alloc.h>
#include <string.h>



#include "commands.actual.commands.h"

// clang-format off

// clang-format on



#define ELEKTRA_TO_CONST_STRING(typeName) ELEKTRA_CONCAT (ELEKTRA_CONCAT (elektra, typeName), ToConstString)
#define ELEKTRA_TO_CONST_STRING_SIGNATURE(cType, typeName) const char * ELEKTRA_TO_CONST_STRING (typeName) (cType value)




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
* Tag name for 'dynamic/#'
* 
* Required arguments:
* 
* - kdb_long_long_t index1: Replaces occurrence no. 1 of # in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_DYNAMIC Dynamic

/**
* Tag name for 'get'
* 
*/// 
#define ELEKTRA_TAG_GET Get

/**
* Tag name for 'get/keyname'
* 
*/// 
#define ELEKTRA_TAG_GET_KEYNAME GetKeyname

/**
* Tag name for 'get/maxlength'
* 
*/// 
#define ELEKTRA_TAG_GET_MAXLENGTH GetMaxlength

/**
* Tag name for 'get/meta'
* 
*/// 
#define ELEKTRA_TAG_GET_META GetMeta

/**
* Tag name for 'get/meta/keyname'
* 
*/// 
#define ELEKTRA_TAG_GET_META_KEYNAME GetMetaKeyname

/**
* Tag name for 'get/meta/metaname'
* 
*/// 
#define ELEKTRA_TAG_GET_META_METANAME GetMetaMetaname

/**
* Tag name for 'get/meta/verbose'
* 
*/// 
#define ELEKTRA_TAG_GET_META_VERBOSE GetMetaVerbose

/**
* Tag name for 'get/verbose'
* 
*/// 
#define ELEKTRA_TAG_GET_VERBOSE GetVerbose

/**
* Tag name for 'printversion'
* 
*/// 
#define ELEKTRA_TAG_PRINTVERSION Printversion

/**
* Tag name for 'setter'
* 
*/// 
#define ELEKTRA_TAG_SETTER Setter

/**
* Tag name for 'setter/keyname'
* 
*/// 
#define ELEKTRA_TAG_SETTER_KEYNAME SetterKeyname

/**
* Tag name for 'setter/value'
* 
*/// 
#define ELEKTRA_TAG_SETTER_VALUE SetterValue
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
 * Get the value of key 'dynamic/#' (tag #ELEKTRA_TAG_DYNAMIC).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index1 Replaces occurrence no. 1 of # in the keyname.
 *
 * @return the value of 'dynamic/#'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_DYNAMIC) (Elektra * elektra ,
								       kdb_long_long_t index1   )
{
	char * name = elektraFormat ("dynamic/%*.*s%lld",  elektra_len (index1), elektra_len (index1), "#___________________", (long long) index1  );
	const char * result = ELEKTRA_GET (String) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of key 'dynamic/#' (tag #ELEKTRA_TAG_DYNAMIC).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'dynamic/#'.
 * @param index1 Replaces occurrence no. 1 of # in the keyname.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_DYNAMIC) (Elektra * elektra,
						      const char * value,  
						      kdb_long_long_t index1,
						        ElektraError ** error)
{
	char * name = elektraFormat ("dynamic/%*.*s%lld",  elektra_len (index1), elektra_len (index1), "#___________________", (long long) index1  );
	ELEKTRA_SET (String) (elektra, name, value, error);
	elektraFree (name);
	
}

/**
 * Get the size of the array 'dynamic/#' (tag #ELEKTRA_TAG_DYNAMIC).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 */// 
static inline kdb_long_long_t ELEKTRA_SIZE (ELEKTRA_TAG_DYNAMIC) (Elektra * elektra )
{
	
	return elektraArraySize (elektra, "dynamic");
}



/**
 * Get the value of key 'get' (tag #ELEKTRA_TAG_GET).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'get'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_GET) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "get");
}


/**
 * Set the value of key 'get' (tag #ELEKTRA_TAG_GET).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'get'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GET) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "get", value, error);
}




/**
 * Get the value of key 'get/keyname' (tag #ELEKTRA_TAG_GET_KEYNAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'get/keyname'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_GET_KEYNAME) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "get/keyname");
}


/**
 * Set the value of key 'get/keyname' (tag #ELEKTRA_TAG_GET_KEYNAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'get/keyname'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GET_KEYNAME) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "get/keyname", value, error);
}




/**
 * Get the value of key 'get/maxlength' (tag #ELEKTRA_TAG_GET_MAXLENGTH).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'get/maxlength'.

 */// 
static inline kdb_long_t ELEKTRA_GET (ELEKTRA_TAG_GET_MAXLENGTH) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Long) (elektra, "get/maxlength");
}


/**
 * Set the value of key 'get/maxlength' (tag #ELEKTRA_TAG_GET_MAXLENGTH).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'get/maxlength'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GET_MAXLENGTH) (Elektra * elektra,
						      kdb_long_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Long) (elektra, "get/maxlength", value, error);
}




/**
 * Get the value of key 'get/meta' (tag #ELEKTRA_TAG_GET_META).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'get/meta'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_GET_META) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "get/meta");
}


/**
 * Set the value of key 'get/meta' (tag #ELEKTRA_TAG_GET_META).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'get/meta'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GET_META) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "get/meta", value, error);
}




/**
 * Get the value of key 'get/meta/keyname' (tag #ELEKTRA_TAG_GET_META_KEYNAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'get/meta/keyname'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_GET_META_KEYNAME) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "get/meta/keyname");
}


/**
 * Set the value of key 'get/meta/keyname' (tag #ELEKTRA_TAG_GET_META_KEYNAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'get/meta/keyname'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GET_META_KEYNAME) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "get/meta/keyname", value, error);
}




/**
 * Get the value of key 'get/meta/metaname' (tag #ELEKTRA_TAG_GET_META_METANAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'get/meta/metaname'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_GET_META_METANAME) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "get/meta/metaname");
}


/**
 * Set the value of key 'get/meta/metaname' (tag #ELEKTRA_TAG_GET_META_METANAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'get/meta/metaname'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GET_META_METANAME) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "get/meta/metaname", value, error);
}




/**
 * Get the value of key 'get/meta/verbose' (tag #ELEKTRA_TAG_GET_META_VERBOSE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'get/meta/verbose'.

 */// 
static inline kdb_boolean_t ELEKTRA_GET (ELEKTRA_TAG_GET_META_VERBOSE) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Boolean) (elektra, "get/meta/verbose");
}


/**
 * Set the value of key 'get/meta/verbose' (tag #ELEKTRA_TAG_GET_META_VERBOSE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'get/meta/verbose'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GET_META_VERBOSE) (Elektra * elektra,
						      kdb_boolean_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Boolean) (elektra, "get/meta/verbose", value, error);
}




/**
 * Get the value of key 'get/verbose' (tag #ELEKTRA_TAG_GET_VERBOSE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'get/verbose'.

 */// 
static inline kdb_boolean_t ELEKTRA_GET (ELEKTRA_TAG_GET_VERBOSE) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Boolean) (elektra, "get/verbose");
}


/**
 * Set the value of key 'get/verbose' (tag #ELEKTRA_TAG_GET_VERBOSE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'get/verbose'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_GET_VERBOSE) (Elektra * elektra,
						      kdb_boolean_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Boolean) (elektra, "get/verbose", value, error);
}




/**
 * Get the value of key 'printversion' (tag #ELEKTRA_TAG_PRINTVERSION).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'printversion'.

 */// 
static inline kdb_boolean_t ELEKTRA_GET (ELEKTRA_TAG_PRINTVERSION) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Boolean) (elektra, "printversion");
}


/**
 * Set the value of key 'printversion' (tag #ELEKTRA_TAG_PRINTVERSION).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'printversion'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_PRINTVERSION) (Elektra * elektra,
						      kdb_boolean_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Boolean) (elektra, "printversion", value, error);
}




/**
 * Get the value of key 'setter' (tag #ELEKTRA_TAG_SETTER).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'setter'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_SETTER) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "setter");
}


/**
 * Set the value of key 'setter' (tag #ELEKTRA_TAG_SETTER).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'setter'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_SETTER) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "setter", value, error);
}




/**
 * Get the value of key 'setter/keyname' (tag #ELEKTRA_TAG_SETTER_KEYNAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'setter/keyname'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_SETTER_KEYNAME) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "setter/keyname");
}


/**
 * Set the value of key 'setter/keyname' (tag #ELEKTRA_TAG_SETTER_KEYNAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'setter/keyname'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_SETTER_KEYNAME) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "setter/keyname", value, error);
}




/**
 * Get the value of key 'setter/value' (tag #ELEKTRA_TAG_SETTER_VALUE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'setter/value'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_SETTER_VALUE) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "setter/value");
}


/**
 * Set the value of key 'setter/value' (tag #ELEKTRA_TAG_SETTER_VALUE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'setter/value'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_SETTER_VALUE) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "setter/value", value, error);
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

int runCommands (Elektra * elektra, void * userData);


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

#endif // COMMANDS_ACTUAL_H
