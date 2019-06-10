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

#ifdef __cplusplus
extern "C" {
#endif

#include <elektra.h>

#include <kdbhelper.h>
#include <string.h>



// clang-format off

// clang-format on






// clang-format off

// clang-format on

#define ELEKTRA_STRUCT_FREE(typeName) elektraFree##typeName
#define ELEKTRA_STRUCT_FREE_SIGNATURE(cType, typeName) void ELEKTRA_STRUCT_FREE (typeName) (cType * ptr)

typedef struct
{
	const char *   a;
	kdb_long_t   b;
} ElektraStructMystruct;

typedef struct
{
	kdb_short_t   age;
	kdb_long_long_t   childrenSize;
	Person  * []  children;
	kdb_float_t   height;
	const char *   fullName;
} Person;



ELEKTRA_GET_OUT_PTR_SIGNATURE (ElektraStructMystruct, StructMystruct);
ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE (ElektraStructMystruct, StructMystruct);
ELEKTRA_SET_SIGNATURE (const ElektraStructMystruct *, StructMystruct);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const ElektraStructMystruct *, StructMystruct);

ELEKTRA_STRUCT_FREE_SIGNATURE (Person *, StructPerson);

ELEKTRA_GET_SIGNATURE (Person *, StructPerson);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Person *, StructPerson);

ELEKTRA_SET_SIGNATURE (const Person *, StructPerson);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const Person *, StructPerson);



// clang-format off

// clang-format on

// clang-format off

/**
* Tag name for 'mystruct'
* 
*/// 
#define ELEKTRA_TAG_MYSTRUCT Mystruct

/**
* Tag name for 'mystruct/a'
* 
*/// 
#define ELEKTRA_TAG_MYSTRUCT_A MystructA

/**
* Tag name for 'mystruct/b'
* 
*/// 
#define ELEKTRA_TAG_MYSTRUCT_B MystructB

/**
* Tag name for 'people/#'
* 
* Required arguments:
* 
* - kdb_long_long_t index0: Replaces occurence no. 0 of # in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PEOPLE People

/**
* Tag name for 'person/_'
* 
* Required arguments:
* 
* - const char * name0: Replaces occurence no. 0 of _ in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON Person

/**
* Tag name for 'person/_/age'
* 
* Required arguments:
* 
* - const char * name0: Replaces occurence no. 0 of _ in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON_AGE PersonAge

/**
* Tag name for 'person/_/children/#'
* 
* Required arguments:
* 
* - const char * name0: Replaces occurence no. 0 of _ in the keyname.
* 
* - kdb_long_long_t index1: Replaces occurence no. 1 of # in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON_CHILDREN PersonChildren

/**
* Tag name for 'person/_/height'
* 
* Required arguments:
* 
* - const char * name0: Replaces occurence no. 0 of _ in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON_HEIGHT PersonHeight

/**
* Tag name for 'person/_/name'
* 
* Required arguments:
* 
* - const char * name0: Replaces occurence no. 0 of _ in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON_NAME PersonName
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

#define ELEKTRA_SIZE(tagName) elektraSize##tagName

// clang-format off

// clang-format on



/**
 * Get the value of 'mystruct'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param result  The value will be stored in the referenced variable.
 */// 
static inline void ELEKTRA_GET (Mystruct) (Elektra * elektra, ElektraStructMystruct *result )
{
	
	ELEKTRA_GET (StructMystruct) (elektra, "mystruct", result);
}


/**
 * Set the value of 'mystruct'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Mystruct) (Elektra * elektra, const ElektraStructMystruct * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (StructMystruct) (elektra, "mystruct", value, error);
}






/**
 * Get the value of 'mystruct/a'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'mystruct/a'.
 */// 
static inline const char * ELEKTRA_GET (MystructA) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "mystruct/a");
}


/**
 * Set the value of 'mystruct/a'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct/a'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MystructA) (Elektra * elektra, const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "mystruct/a", value, error);
}




/**
 * Get the value of 'mystruct/b'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'mystruct/b'.
 */// 
static inline kdb_long_t ELEKTRA_GET (MystructB) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Long) (elektra, "mystruct/b");
}


/**
 * Set the value of 'mystruct/b'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct/b'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (MystructB) (Elektra * elektra, kdb_long_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Long) (elektra, "mystruct/b", value, error);
}


// clang-format off

// clang-format on


/**
 * Get the value of 'people/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'people/#', free with ELEKTRA_STRUCT_FREE (Person, StructPerson).
 */// 
static inline Person * ELEKTRA_GET (People) (Elektra * elektra ,
								      kdb_long_long_t index0 
								       )
{
	char * name = elektraFormat ("people/%*.*s%lld",   elektra_len (index0),
				     elektra_len (index0), "#___________________",  index0  );
	const char * actualName = elektraGetRawString (elektra, name);
	elektraFree (name);
	

	if (actualName == NULL || strlen (actualName) == 0)
	{
		return NULL;
	}

	return ELEKTRA_GET (StructPerson) (elektra, actualName);
}



/**
 * Set the value of 'people/#'.
 *
 * WARNING: if the given value does not reference a valid struct, from this specification,
 * ELEKTRA_GET (People) will fail. Use the `reference` plugin to ensure valid values.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'people/#', you must prefix it with '/tests/script/gen/elektra/struct' yourself.
 *                This is because you may want to specify a namespace.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (People) (Elektra * elektra, const char * value,  
						    kdb_long_long_t index0,
						      ElektraError ** error)
{
	char * name = elektraFormat ("people/%*.*s%lld",   elektra_len (index0),
				     elektra_len (index0), "#___________________",  index0  );
	elektraSetRawString (elektra, name, value, "struct_ref", error);
	elektraFree (name);
	
}



/**
 * Get the size of the array 'people/#'.
 */// 
static inline kdb_long_long_t ELEKTRA_SIZE (People) (Elektra * elektra)
{
	
	kdb_long_long_t size = elektraArraySize (elektra, "people");
	return size;
}
// clang-format off

// clang-format on


/**
 * Get the value of 'person/_'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'person/_', free with ELEKTRA_STRUCT_FREE (Person, StructPerson).
 */// 
static inline Person * ELEKTRA_GET (Person) (Elektra * elektra ,
								      const char * name0 
								       )
{
	char * name = elektraFormat ("person/%s",   name0  );
	Person *result = ELEKTRA_GET (StructPerson) (elektra, name);
	elektraFree (name);
	return result;
	
}



/**
 * Set the value of 'person/_'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'person/_'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (Person) (Elektra * elektra, const Person * value,  
						    const char * name0,
						      ElektraError ** error)
{
	char * name = elektraFormat ("person/%s",   name0  );
	ELEKTRA_SET (StructPerson) (elektra, name, value, error);
	elektraFree (name);
	
}






/**
 * Get the value of 'person/_/age'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'person/_/age'.
 */// 
static inline kdb_short_t ELEKTRA_GET (PersonAge) (Elektra * elektra ,
								    const char * name0 
								     )
{
	char * name = elektraFormat ("person/%s/age",   name0  );
	kdb_short_t result = ELEKTRA_GET (Short) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of 'person/_/age'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'person/_/age'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (PersonAge) (Elektra * elektra, kdb_short_t value,  
						    const char * name0,
						      ElektraError ** error)
{
	char * name = elektraFormat ("person/%s/age",   name0  );
	ELEKTRA_SET (Short) (elektra, name, value, error);
	elektraFree (name);
	
}


// clang-format off

// clang-format on


/**
 * Get the value of 'person/_/children/#'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'person/_/children/#', free with ELEKTRA_STRUCT_FREE (Person, StructPerson).
 */// 
static inline Person * ELEKTRA_GET (PersonChildren) (Elektra * elektra ,
								      const char * name0 , 
								      kdb_long_long_t index1 
								       )
{
	char * name = elektraFormat ("person/%s/children/%*.*s%lld",   name0 ,
				        elektra_len (index1),
				     elektra_len (index1), "#___________________",  index1  );
	const char * actualName = elektraGetRawString (elektra, name);
	elektraFree (name);
	

	if (actualName == NULL || strlen (actualName) == 0)
	{
		return NULL;
	}

	return ELEKTRA_GET (StructPerson) (elektra, actualName);
}



/**
 * Set the value of 'person/_/children/#'.
 *
 * WARNING: if the given value does not reference a valid struct, from this specification,
 * ELEKTRA_GET (PersonChildren) will fail. Use the `reference` plugin to ensure valid values.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'person/_/children/#', you must prefix it with '/tests/script/gen/elektra/struct' yourself.
 *                This is because you may want to specify a namespace.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (PersonChildren) (Elektra * elektra, const char * value,  
						    const char * name0,
						    kdb_long_long_t index1,
						      ElektraError ** error)
{
	char * name = elektraFormat ("person/%s/children/%*.*s%lld",   name0 ,
				        elektra_len (index1),
				     elektra_len (index1), "#___________________",  index1  );
	elektraSetRawString (elektra, name, value, "struct_ref", error);
	elektraFree (name);
	
}



/**
 * Get the size of the array 'person/_/children/#'.
 */// 
static inline kdb_long_long_t ELEKTRA_SIZE (PersonChildren) (Elektra * elektra)
{
	char * name = elektraFormat ("person/%s/children",   name0 ,
				      );
	kdb_long_long_t size = elektraArraySize (elektra, name);
	elektraFree (name);
	
	return size;
}



/**
 * Get the value of 'person/_/height'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'person/_/height'.
 */// 
static inline kdb_float_t ELEKTRA_GET (PersonHeight) (Elektra * elektra ,
								    const char * name0 
								     )
{
	char * name = elektraFormat ("person/%s/height",   name0  );
	kdb_float_t result = ELEKTRA_GET (Float) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of 'person/_/height'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'person/_/height'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (PersonHeight) (Elektra * elektra, kdb_float_t value,  
						    const char * name0,
						      ElektraError ** error)
{
	char * name = elektraFormat ("person/%s/height",   name0  );
	ELEKTRA_SET (Float) (elektra, name, value, error);
	elektraFree (name);
	
}




/**
 * Get the value of 'person/_/name'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 *
 * @return the value of 'person/_/name'.
 */// 
static inline const char * ELEKTRA_GET (PersonName) (Elektra * elektra ,
								    const char * name0 
								     )
{
	char * name = elektraFormat ("person/%s/name",   name0  );
	const char * result = ELEKTRA_GET (String) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of 'person/_/name'.
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'person/_/name'.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (PersonName) (Elektra * elektra, const char * value,  
						    const char * name0,
						      ElektraError ** error)
{
	char * name = elektraFormat ("person/%s/name",   name0  );
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
#define elektraSet(elektra, tag, value, error) ELEKTRA_GET (tag) (elektra, value, error)


/**
 * @param elektra The elektra instance initialized with the loadConfiguration().
 * @param tag     The tag to write to.
 * @param value   The new value.
 * @param error   Pass a reference to an ElektraError pointer.
 * @param ...     Variable arguments depending on the given tag.
 */// 
#define elektraSetV(elektra, tag, value, error, ...) ELEKTRA_GET (tag) (elektra, value, __VA_ARGS__, error)


/**
 * @param elektra The elektra instance initialized with loadConfiguration().
 * @param tag     The array tag to look up.
 *
 * @return The size of the array below the given key.
 */// 
#define elektraSize(elektra, tag) ELEKTRA_SIZE (tag) (elektra)

#ifdef __cplusplus
}
#endif

#endif // STRUCT_ACTUAL_H
