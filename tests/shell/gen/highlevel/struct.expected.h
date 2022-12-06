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


#ifndef STRUCT_ACTUAL_H
#define STRUCT_ACTUAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <elektra/elektra.h>

#include <elektra/kdbhelper.h>
#include <string.h>





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

typedef struct ElektraStructMyotherstruct
{
	
	kdb_long_t  x;

	
	kdb_long_t  xY;

} ElektraStructMyotherstruct;

typedef struct ElektraStructMystruct
{
	
	const char *  a;

	
	kdb_long_t  b;

} ElektraStructMystruct;

typedef struct Person
{
	
	kdb_short_t  age;

	
	kdb_long_long_t  childrenSize;

	
	struct Person *  *  children;

	
	kdb_float_t  height;

	
	const char *  fullName;

} Person;



ELEKTRA_GET_OUT_PTR_SIGNATURE (ElektraStructMyotherstruct, StructMyotherstruct);
ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE (ElektraStructMyotherstruct, StructMyotherstruct);
ELEKTRA_SET_SIGNATURE (const ElektraStructMyotherstruct *, StructMyotherstruct);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const ElektraStructMyotherstruct *, StructMyotherstruct);


ELEKTRA_GET_OUT_PTR_SIGNATURE (ElektraStructMystruct, StructMystruct);
ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE (ElektraStructMystruct, StructMystruct);
ELEKTRA_SET_SIGNATURE (const ElektraStructMystruct *, StructMystruct);
ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (const ElektraStructMystruct *, StructMystruct);

ELEKTRA_STRUCT_FREE_SIGNATURE (Person *, StructPerson);

ELEKTRA_GET_SIGNATURE (Person *, StructPerson);
ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Person *, StructPerson);





// clang-format off

// clang-format on

// clang-format off

/**
* Tag name for 'myotherstruct'
* 
*/// 
#define ELEKTRA_TAG_MYOTHERSTRUCT Myotherstruct

/**
* Tag name for 'myotherstruct/x'
* 
*/// 
#define ELEKTRA_TAG_MYOTHERSTRUCT_X MyotherstructX

/**
* Tag name for 'myotherstruct/x/y'
* 
*/// 
#define ELEKTRA_TAG_MYOTHERSTRUCT_X_Y MyotherstructXY

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
* - kdb_long_long_t index1: Replaces occurrence no. 1 of # in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PEOPLE People

/**
* Tag name for 'person/_'
* 
* Required arguments:
* 
* - const char * name1: Replaces occurrence no. 1 of _ in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON Person

/**
* Tag name for 'person/_/age'
* 
* Required arguments:
* 
* - const char * name1: Replaces occurrence no. 1 of _ in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON_AGE PersonAge

/**
* Tag name for 'person/_/children/#'
* 
* Required arguments:
* 
* - const char * name1: Replaces occurrence no. 1 of _ in the keyname.
* 
* - kdb_long_long_t index1: Replaces occurrence no. 1 of # in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON_CHILDREN PersonChildren

/**
* Tag name for 'person/_/height'
* 
* Required arguments:
* 
* - const char * name1: Replaces occurrence no. 1 of _ in the keyname.
* 
* 
*/// 
#define ELEKTRA_TAG_PERSON_HEIGHT PersonHeight

/**
* Tag name for 'person/_/name'
* 
* Required arguments:
* 
* - const char * name1: Replaces occurrence no. 1 of _ in the keyname.
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

#define ELEKTRA_SIZE(tagName) ELEKTRA_CONCAT (elektraSize, tagName)

// clang-format off

// clang-format on



/**
 * Get the value of key 'myotherstruct' (tag #ELEKTRA_TAG_MYOTHERSTRUCT).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param result  The value will be stored in the referenced variable.
 *   Pointers contained in the struct may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.

 */// 
static inline void ELEKTRA_GET (ELEKTRA_TAG_MYOTHERSTRUCT) (Elektra * elektra, ElektraStructMyotherstruct *result )
{
	
	ELEKTRA_GET (StructMyotherstruct) (elektra, "myotherstruct", result);
}


/**
 * Set the value of key 'myotherstruct' (tag #ELEKTRA_TAG_MYOTHERSTRUCT).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myotherstruct'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_MYOTHERSTRUCT) (Elektra * elektra, const ElektraStructMyotherstruct * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (StructMyotherstruct) (elektra, "myotherstruct", value, error);
}






/**
 * Get the value of key 'myotherstruct/x' (tag #ELEKTRA_TAG_MYOTHERSTRUCT_X).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'myotherstruct/x'.

 */// 
static inline kdb_long_t ELEKTRA_GET (ELEKTRA_TAG_MYOTHERSTRUCT_X) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Long) (elektra, "myotherstruct/x");
}


/**
 * Set the value of key 'myotherstruct/x' (tag #ELEKTRA_TAG_MYOTHERSTRUCT_X).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myotherstruct/x'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_MYOTHERSTRUCT_X) (Elektra * elektra,
						      kdb_long_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Long) (elektra, "myotherstruct/x", value, error);
}




/**
 * Get the value of key 'myotherstruct/x/y' (tag #ELEKTRA_TAG_MYOTHERSTRUCT_X_Y).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'myotherstruct/x/y'.

 */// 
static inline kdb_long_t ELEKTRA_GET (ELEKTRA_TAG_MYOTHERSTRUCT_X_Y) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Long) (elektra, "myotherstruct/x/y");
}


/**
 * Set the value of key 'myotherstruct/x/y' (tag #ELEKTRA_TAG_MYOTHERSTRUCT_X_Y).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'myotherstruct/x/y'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_MYOTHERSTRUCT_X_Y) (Elektra * elektra,
						      kdb_long_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Long) (elektra, "myotherstruct/x/y", value, error);
}

// clang-format off

// clang-format on



/**
 * Get the value of key 'mystruct' (tag #ELEKTRA_TAG_MYSTRUCT).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param result  The value will be stored in the referenced variable.
 *   Pointers contained in the struct may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.

 */// 
static inline void ELEKTRA_GET (ELEKTRA_TAG_MYSTRUCT) (Elektra * elektra, ElektraStructMystruct *result )
{
	
	ELEKTRA_GET (StructMystruct) (elektra, "mystruct", result);
}


/**
 * Set the value of key 'mystruct' (tag #ELEKTRA_TAG_MYSTRUCT).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_MYSTRUCT) (Elektra * elektra, const ElektraStructMystruct * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (StructMystruct) (elektra, "mystruct", value, error);
}






/**
 * Get the value of key 'mystruct/a' (tag #ELEKTRA_TAG_MYSTRUCT_A).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'mystruct/a'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_MYSTRUCT_A) (Elektra * elektra )
{
	
	return ELEKTRA_GET (String) (elektra, "mystruct/a");
}


/**
 * Set the value of key 'mystruct/a' (tag #ELEKTRA_TAG_MYSTRUCT_A).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct/a'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_MYSTRUCT_A) (Elektra * elektra,
						      const char * value,  ElektraError ** error)
{
	
	ELEKTRA_SET (String) (elektra, "mystruct/a", value, error);
}




/**
 * Get the value of key 'mystruct/b' (tag #ELEKTRA_TAG_MYSTRUCT_B).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 *
 * @return the value of 'mystruct/b'.

 */// 
static inline kdb_long_t ELEKTRA_GET (ELEKTRA_TAG_MYSTRUCT_B) (Elektra * elektra )
{
	
	return ELEKTRA_GET (Long) (elektra, "mystruct/b");
}


/**
 * Set the value of key 'mystruct/b' (tag #ELEKTRA_TAG_MYSTRUCT_B).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'mystruct/b'.

 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_MYSTRUCT_B) (Elektra * elektra,
						      kdb_long_t value,  ElektraError ** error)
{
	
	ELEKTRA_SET (Long) (elektra, "mystruct/b", value, error);
}


// clang-format off

// clang-format on


/**
 * Get the value of key 'people/#' (tag #ELEKTRA_TAG_PEOPLE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
succeed_if_same_string ($1)
 *
 * @return the value of 'people/#', free with ELEKTRA_STRUCT_FREE (StructPerson).
 *   Pointers contained in the struct may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline Person * ELEKTRA_GET (ELEKTRA_TAG_PEOPLE) (Elektra * elektra ,
									kdb_long_long_t index1 
									 )
{
	char * name = elektraFormat ("people/%*.*s%lld",  elektra_len (index1), elektra_len (index1), "#___________________", (long long) index1  );
	const char * actualName = elektraFindReference (elektra, name);
	elektraFree (name);
	

	if (actualName == NULL || strlen (actualName) == 0)
	{
		return NULL;
	}

	return ELEKTRA_GET (StructPerson) (elektra, actualName);
}






/**
 * Get the size of the array 'people/#' (tag #ELEKTRA_TAG_PEOPLE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().

 */// 
static inline kdb_long_long_t ELEKTRA_SIZE (ELEKTRA_TAG_PEOPLE) (Elektra * elektra )
{
	
	return elektraArraySize (elektra, "people");
}
// clang-format off

// clang-format on


/**
 * Get the value of key 'person/_' (tag #ELEKTRA_TAG_PERSON).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param name1 Replaces occurrence no. 1 of _ in the keyname.
 *
 * @return the value of 'person/_', free with ELEKTRA_STRUCT_FREE (StructPerson).
 *   Pointers contained in the struct may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline Person * ELEKTRA_GET (ELEKTRA_TAG_PERSON) (Elektra * elektra ,
									 const char * name1   )
{
	char * name = elektraFormat ("person/%s",  name1  );
	Person *result = ELEKTRA_GET (StructPerson) (elektra, name);
	elektraFree (name);
	return result;
	
}









/**
 * Get the value of key 'person/_/age' (tag #ELEKTRA_TAG_PERSON_AGE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param name1 Replaces occurrence no. 1 of _ in the keyname.
 *
 * @return the value of 'person/_/age'.

 */// 
static inline kdb_short_t ELEKTRA_GET (ELEKTRA_TAG_PERSON_AGE) (Elektra * elektra ,
								       const char * name1   )
{
	char * name = elektraFormat ("person/%s/age",  name1  );
	kdb_short_t result = ELEKTRA_GET (Short) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of key 'person/_/age' (tag #ELEKTRA_TAG_PERSON_AGE).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'person/_/age'.
 * @param name1 Replaces occurrence no. 1 of _ in the keyname.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_PERSON_AGE) (Elektra * elektra,
						      kdb_short_t value,  
						      const char * name1,
						        ElektraError ** error)
{
	char * name = elektraFormat ("person/%s/age",  name1  );
	ELEKTRA_SET (Short) (elektra, name, value, error);
	elektraFree (name);
	
}


// clang-format off

// clang-format on


/**
 * Get the value of key 'person/_/children/#' (tag #ELEKTRA_TAG_PERSON_CHILDREN).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
succeed_if_same_string ($1)
 *
 * @return the value of 'person/_/children/#', free with ELEKTRA_STRUCT_FREE (StructPerson).
 *   Pointers contained in the struct may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline Person * ELEKTRA_GET (ELEKTRA_TAG_PERSON_CHILDREN) (Elektra * elektra ,
									const char * name1 , 
									kdb_long_long_t index1 
									 )
{
	char * name = elektraFormat ("person/%s/children/%*.*s%lld",  name1 ,
				       elektra_len (index1), elektra_len (index1), "#___________________", (long long) index1  );
	const char * actualName = elektraFindReference (elektra, name);
	elektraFree (name);
	

	if (actualName == NULL || strlen (actualName) == 0)
	{
		return NULL;
	}

	return ELEKTRA_GET (StructPerson) (elektra, actualName);
}






/**
 * Get the size of the array 'person/_/children/#' (tag #ELEKTRA_TAG_PERSON_CHILDREN).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param name1 Replaces occurrence no. 1 of _ in the keyname.
 */// 
static inline kdb_long_long_t ELEKTRA_SIZE (ELEKTRA_TAG_PERSON_CHILDREN) (Elektra * elektra ,
								   const char * name1   )
{
	char * name = elektraFormat ("person/%s/children",  name1  );
	kdb_long_long_t size = elektraArraySize (elektra, name);
	elektraFree (name);
	return size;
	
}



/**
 * Get the value of key 'person/_/height' (tag #ELEKTRA_TAG_PERSON_HEIGHT).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param name1 Replaces occurrence no. 1 of _ in the keyname.
 *
 * @return the value of 'person/_/height'.

 */// 
static inline kdb_float_t ELEKTRA_GET (ELEKTRA_TAG_PERSON_HEIGHT) (Elektra * elektra ,
								       const char * name1   )
{
	char * name = elektraFormat ("person/%s/height",  name1  );
	kdb_float_t result = ELEKTRA_GET (Float) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of key 'person/_/height' (tag #ELEKTRA_TAG_PERSON_HEIGHT).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'person/_/height'.
 * @param name1 Replaces occurrence no. 1 of _ in the keyname.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_PERSON_HEIGHT) (Elektra * elektra,
						      kdb_float_t value,  
						      const char * name1,
						        ElektraError ** error)
{
	char * name = elektraFormat ("person/%s/height",  name1  );
	ELEKTRA_SET (Float) (elektra, name, value, error);
	elektraFree (name);
	
}




/**
 * Get the value of key 'person/_/name' (tag #ELEKTRA_TAG_PERSON_NAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param name1 Replaces occurrence no. 1 of _ in the keyname.
 *
 * @return the value of 'person/_/name'.
 *   The returned pointer may become invalid, if the internal state of @p elektra
 *   is modified. All calls to elektraSet* modify this state.
 */// 
static inline const char * ELEKTRA_GET (ELEKTRA_TAG_PERSON_NAME) (Elektra * elektra ,
								       const char * name1   )
{
	char * name = elektraFormat ("person/%s/name",  name1  );
	const char * result = ELEKTRA_GET (String) (elektra, name);
	elektraFree (name);
	return result;
	
}


/**
 * Set the value of key 'person/_/name' (tag #ELEKTRA_TAG_PERSON_NAME).
 *
 * @param elektra Instance of Elektra. Create with loadConfiguration().
 * @param value   The value of 'person/_/name'.
 * @param name1 Replaces occurrence no. 1 of _ in the keyname.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */// 
static inline void ELEKTRA_SET (ELEKTRA_TAG_PERSON_NAME) (Elektra * elektra,
						      const char * value,  
						      const char * name1,
						        ElektraError ** error)
{
	char * name = elektraFormat ("person/%s/name",  name1  );
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


int loadConfiguration (Elektra ** elektra,
				 int argc, const char * const * argv, const char * const * envp,
				 
				 ElektraError ** error);
void printHelpMessage (Elektra * elektra, const char * usage, const char * prefix);
void exitForSpecload (int argc, const char * const * argv);




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

#endif // STRUCT_ACTUAL_H
