/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

// clang-format on

#ifndef CONTEXT_ACTUAL_H
#define CONTEXT_ACTUAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <elektra.h>

#include <kdbhelper.h>
#include <string.h>



#define ELEKTRA_CONTEXT_SET(contextTag) elektraSetContextualValue##contextTag

// clang-format off

// clang-format on






// clang-format off

// clang-format on

#define ELEKTRA_STRUCT_FREE(typeName) ELEKTRA_CONCAT (elektraFree, typeName)
#define ELEKTRA_STRUCT_FREE_SIGNATURE(cType, typeName) void ELEKTRA_STRUCT_FREE (typeName) (cType * ptr)





// clang-format off

// clang-format on

// clang-format off

/**
* Tag name for 'key/%date%/child'
* 
*/// 
#define ELEKTRA_TAG_KEY_DATE_CHILD KeyDateChild

/**
* Tag name for 'key/%profile%/child/_/grandchildren/#'
* 
* Required arguments:
* 
* - const char * name1: Replaces occurence no. 1 of _ in the keyname.
* 
* - kdb_long_long_t index1: Replaces occurence no. 1 of # in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_KEY_PROFILE_CHILD_GRANDCHILDREN KeyProfileChildGrandchildren

/**
* Tag name for 'key/%user\/name%/child'
* 
*/// 
#define ELEKTRA_TAG_KEY_USER__NAME_CHILD KeyUserNameChild
// clang-format on


// clang-format off

// clang-format on

// clang-format off

/**
* Context tag name for 'date'
*/// 
#define ELEKTRA_CONTEXT_DATE Date

/**
* Context tag name for 'profile'
*/// 
#define ELEKTRA_CONTEXT_PROFILE Profile

/**
* Context tag name for 'user/name'
*/// 
#define ELEKTRA_CONTEXT_USER_NAME UserName
// clang-format on


// clang-format off

// clang-format on

static inline void ELEKTRA_CONTEXT_SET (Date) (Elektra * elektra, const char * value)
{
	ksAppendKey (elektraContext (elektra), keyNew ("system/elektra/codegen/context/date", KEY_VALUE, value, KEY_END));
}
static inline void ELEKTRA_CONTEXT_SET (Profile) (Elektra * elektra, const char * value)
{
	ksAppendKey (elektraContext (elektra), keyNew ("system/elektra/codegen/context/profile", KEY_VALUE, value, KEY_END));
}
static inline void ELEKTRA_CONTEXT_SET (UserName) (Elektra * elektra, const char * value)
{
	ksAppendKey (elektraContext (elektra), keyNew ("system/elektra/codegen/context/user/name", KEY_VALUE, value, KEY_END));
}


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
 * Get the value of 'key/%date%/child'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'key/%date%/child'.
 */// 
static inline kdb_boolean_t ELEKTRA_GET (KeyDateChild) (Elektra * elektra )
{
	const char * date = keyString (ksLookupByName (elektraContext (elektra), "system/elektra/codegen/context/date", 0));

	char * name = elektraFormat ("key/%1$s/child",  date  
				     );
	kdb_boolean_t result = ELEKTRA_GET (Boolean) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of 'key/%date%/child'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'key/%date%/child'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (KeyDateChild) (Elektra * elektra,
						    kdb_boolean_t value,  ElektraError ** error)
{
	const char * date = keyString (ksLookupByName (elektraContext (elektra), "system/elektra/codegen/context/date", 0));

	char * name = elektraFormat ("key/%1$s/child",  date  
				     );
	ELEKTRA_SET (Boolean) (elektra, name, value, error);
	elektraFree (name);
	
}




/**
 * Get the value of 'key/%profile%/child/_/grandchildren/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param name1 Replaces occurence no. 1 of _ in the keyname.
 * @param index1 Replaces occurence no. 1 of # in the keyname.
 *
 * @return the value of 'key/%profile%/child/_/grandchildren/#'.
 */// 
static inline kdb_long_t ELEKTRA_GET (KeyProfileChildGrandchildren) (Elektra * elektra ,
								     const char * name1 ,
								      kdb_long_long_t index1   )
{
	const char * profile = keyString (ksLookupByName (elektraContext (elektra), "system/elektra/codegen/context/profile", 0));

	char * name = elektraFormat ("key/%1$s/child/%2$s/grandchildren/%5$*4$.*3$s%6$lld",  profile ,  
				       name1 , 
				       elektra_len (index1), elektra_len (index1),
				     "#___________________",  index1 
				     );
	kdb_long_t result = ELEKTRA_GET (Long) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of 'key/%profile%/child/_/grandchildren/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'key/%profile%/child/_/grandchildren/#'.
 * @param name1 Replaces occurence no. 1 of _ in the keyname.
 * @param index1 Replaces occurence no. 1 of # in the keyname.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (KeyProfileChildGrandchildren) (Elektra * elektra,
						    kdb_long_t value,  
						    const char * name1, 
						    kdb_long_long_t index1,   ElektraError ** error)
{
	const char * profile = keyString (ksLookupByName (elektraContext (elektra), "system/elektra/codegen/context/profile", 0));

	char * name = elektraFormat ("key/%1$s/child/%2$s/grandchildren/%5$*4$.*3$s%6$lld",  profile ,  
				       name1 , 
				       elektra_len (index1), elektra_len (index1),
				     "#___________________",  index1 
				     );
	ELEKTRA_SET (Long) (elektra, name, value, error);
	elektraFree (name);
	
}

/**
 * Get the size of the array 'key/%profile%/child/_/grandchildren/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param name1 Replaces occurence no. 1 of _ in the keyname.
 */// 
static inline kdb_long_long_t ELEKTRA_SIZE (KeyProfileChildGrandchildren) (Elektra * elektra ,
								 const char * name1   )
{
	const char * profile = keyString (ksLookupByName (elektraContext (elektra), "system/elektra/codegen/context/profile", 0));

	char * name = elektraFormat ("key/%1$s/child/%2$s/grandchildren",  profile ,  
				       name1 
				     );
	kdb_long_long_t size = elektraArraySize (elektra, name);
	elektraFree (name);
	return size;
	
}



/**
 * Get the value of 'key/%user\/name%/child'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'key/%user\/name%/child'.
 */// 
static inline const char * ELEKTRA_GET (KeyUserNameChild) (Elektra * elektra )
{
	const char * userName = keyString (ksLookupByName (elektraContext (elektra), "system/elektra/codegen/context/user/name", 0));

	char * name = elektraFormat ("key/%1$s/child",  userName  
				     );
	const char * result = ELEKTRA_GET (String) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of 'key/%user\/name%/child'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'key/%user\/name%/child'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (KeyUserNameChild) (Elektra * elektra,
						    const char * value,  ElektraError ** error)
{
	const char * userName = keyString (ksLookupByName (elektraContext (elektra), "system/elektra/codegen/context/user/name", 0));

	char * name = elektraFormat ("key/%1$s/child",  userName  
				     );
	ELEKTRA_SET (String) (elektra, name, value, error);
	elektraFree (name);
	
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

#endif // CONTEXT_ACTUAL_H
