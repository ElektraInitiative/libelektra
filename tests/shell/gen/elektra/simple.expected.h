// clang-format off


// clang-format on
/**
 * @file
 *
 * This file was automatically generated using `kdb gen elektra`.
 * Any changes will be overwritten, when the file is regenerated.
 *
 * @copyright BSD Zero Clause License
 *
 *     Copyright (C) 2019 Elektra Initiative (https://libelektra.org)
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


#ifndef SIMPLE_ACTUAL_H
#define SIMPLE_ACTUAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <elektra.h>

#include <kdbhelper.h>
#include <string.h>



#define ELEKTRA_CONTEXT_SET(contextTag) elektraSetContextualValue##contextTag

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
* Tag name for 'mydouble'
* 
*/// 
#define ELEKTRA_TAG_MYDOUBLE Mydouble

/**
* Tag name for 'myfloatarray/#'
* 
* Required arguments:
* 
* - kdb_long_long_t index1: Replaces occurence no. 1 of # in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_MYFLOATARRAY Myfloatarray

/**
* Tag name for 'myint'
* 
*/// 
#define ELEKTRA_TAG_MYINT Myint

/**
* Tag name for 'mystring'
* 
*/// 
#define ELEKTRA_TAG_MYSTRING Mystring

/**
* Tag name for 'print'
* 
*/// 
#define ELEKTRA_TAG_PRINT Print
// clang-format on


// clang-format off

// clang-format on

// clang-format off

// clang-format on


// clang-format off

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
 * Get the value of 'mydouble'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'mydouble'.
 */// 
static inline kdb_double_t ELEKTRA_GET (Mydouble) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Double) (elektra, "mydouble");
}


/**
 * Set the value of 'mydouble'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mydouble'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Mydouble) (Elektra * elektra,
						    kdb_double_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Double) (elektra, "mydouble", value, error);
}




/**
 * Get the value of 'myfloatarray/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param index1 Replaces occurence no. 1 of # in the keyname.
 *
 * @return the value of 'myfloatarray/#'.
 */// 
static inline kdb_float_t ELEKTRA_GET (Myfloatarray) (Elektra * elektra ,
								     kdb_long_long_t index1   )
{
	

	char * name = elektraFormat ("myfloatarray/%*.*s%lld",  elektra_len (index1), elektra_len (index1), "#___________________", (long long) index1  );
	kdb_float_t result = ELEKTRA_GET (Float) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of 'myfloatarray/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myfloatarray/#'.
 * @param index1 Replaces occurence no. 1 of # in the keyname.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Myfloatarray) (Elektra * elektra,
						    kdb_float_t value,  
						    kdb_long_long_t index1,   ElektraError ** error)
{
	

	char * name = elektraFormat ("myfloatarray/%*.*s%lld",  elektra_len (index1), elektra_len (index1), "#___________________", (long long) index1  );
	ELEKTRA_SET (Float) (elektra, name, value, error);
	elektraFree (name);
	
}

/**
 * Get the size of the array 'myfloatarray/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 */// 
static inline kdb_long_long_t ELEKTRA_SIZE (Myfloatarray) (Elektra * elektra )
{
	
	return elektraArraySize (elektra, "myfloatarray");
}



/**
 * Get the value of 'myint'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'myint'.
 */// 
static inline kdb_long_t ELEKTRA_GET (Myint) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Long) (elektra, "myint");
}


/**
 * Set the value of 'myint'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myint'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Myint) (Elektra * elektra,
						    kdb_long_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Long) (elektra, "myint", value, error);
}




/**
 * Get the value of 'mystring'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'mystring'.
 */// 
static inline const char * ELEKTRA_GET (Mystring) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "mystring");
}


/**
 * Set the value of 'mystring'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystring'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Mystring) (Elektra * elektra,
						    const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "mystring", value, error);
}




/**
 * Get the value of 'print'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'print'.
 */// 
static inline kdb_boolean_t ELEKTRA_GET (Print) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Boolean) (elektra, "print");
}


/**
 * Set the value of 'print'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'print'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Print) (Elektra * elektra,
						    kdb_boolean_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Boolean) (elektra, "print", value, error);
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
void printHelpMessage (const char * usage, const char * prefix);
void specloadCheck (int argc, const char ** argv);


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The tag to look up.
 *
 * @return The value stored at the given key.
 */// 
#define elektraGet(elektra, tag) ELEKTRA_GET (tag) (elektra)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The tag to look up.
 * @param ...     Variable arguments depending on the given tag.
 *
 * @return The value stored at the given key.
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


/**
 * @param elektra    The elektra instance initialized with loadConfiguration().
 * @param contextTag The context tag for the contextual value you want to set.
 * @param value	     The actual value you want to set.
 */// 
#define elektraContextSet(elektra, contextTag, value) ELEKTRA_CONTEXT_SET (contextTag) (elektra, value)


/**
 * @param elektra    The elektra instance initialized with loadConfiguration().
 * @param contextTag The context tag for the contextual value you want to set.
 * @param value	     The actual value you want to set.
 * @param ...     Variable arguments depending on the given tag.
 */// 
#define elektraContextSetV(elektra, contextTag, value, ...) ELEKTRA_CONTEXT_SET (contextTag) (elektra, value, __VA_ARGS__)

#ifdef __cplusplus
}
#endif

#endif // SIMPLE_ACTUAL_H
