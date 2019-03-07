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

#include <kdbhelper.h>



// clang-format off

// clang-format on






// clang-format off

// clang-format on

#define ELEKTRA_STRUCT_FREE(cType, typeName) elektraFree##typeName
#define ELEKTRA_STRUCT_FREE_SIGNATURE(cType, typeName) void ELEKTRA_STRUCT_FREE (cType, typeName) (cType * ptr)





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
* - kdb_long_long_t index0: Replaces occurence no. 0 of # in the keyname.
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
static inline void ELEKTRA_SET (Mydouble) (Elektra * elektra, kdb_double_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Double) (elektra, "mydouble", value, error);
}



/**
 * Get the value of 'myfloatarray/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'myfloatarray/#'.
 */// 
static inline kdb_float_t ELEKTRA_GET (Myfloatarray) (Elektra * elektra ,
								    kdb_long_long_t index0 
								     )
{
	char * name = elektraFormat ("myfloatarray/%*.*s%lld",   elektra_len (index0),
				     elektra_len (index0), "#___________________",  index0  );
	kdb_float_t result = ELEKTRA_GET (Float) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of 'myfloatarray/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myfloatarray/#'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Myfloatarray) (Elektra * elektra, kdb_float_t value,  
						    kdb_long_long_t index0,
						      ElektraError ** error)
{
	char * name = elektraFormat ("myfloatarray/%*.*s%lld",   elektra_len (index0),
				     elektra_len (index0), "#___________________",  index0  );
	ELEKTRA_SET (Float) (elektra, name, value, error);
	elektraFree (name);
	
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
static inline void ELEKTRA_SET (Myint) (Elektra * elektra, kdb_long_t value,  ElektraError ** error)
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
static inline void ELEKTRA_SET (Mystring) (Elektra * elektra, const char * value,  ElektraError ** error)
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
static inline void ELEKTRA_SET (Print) (Elektra * elektra, kdb_boolean_t value,  ElektraError ** error)
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
void printHelpMessage (void);
int specloadSend (void);


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

#endif // SIMPLE_ACTUAL_H
