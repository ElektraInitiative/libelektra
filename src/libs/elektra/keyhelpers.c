/**
 * @file
 *
 * @brief Helpers for key manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif


#include "kdb.h"
#include "kdbprivate.h"
#include "kdbtypes.h"


/**
 * @internal
 *
 * Returns one level of the escaped key name.
 *
 * Only needed for escaping engine, otherwise the unescaped key name
 * should be used! In the unescaped version, every level is null
 * terminated.
 *
 * Interface is not const-correct. It does a const-cast needed for
 * many clients.
 *
 * This method is used to skip repeating '/' and to find escaping chars.
 * Given @p keyName, this method returns a pointer to the next name level
 * found and changes @p size to the number of bytes on this level name.
 *
 * This method is used by keySetName() to cleanup parameters
 * before being accepted in the Key object.
 *
 * @code
// Lets define a key name with a lot of repeating '/' and escaped '/'
char *keyName="user////abc/def\/ghi////jkl///";
char *p=keyName;
size_t size=0;
int level=0;
char buffer[20]; // TODO: make sure buffer size is ok

p=keyName;
while (*(p=keyNameGetOneLevel(p+size,&size)))
{
	level++;

	// copy what we found to a buffer, so we can NULL-terminate it
	strncpy(buffer,p,size);
	buffer[size]=0;

	printf("Level %d name: \"%s\"\n",level,buffer);
}

 * The above example will produce the following output:
 *
 * @code
Level 1 name: user
Level 2 name: abc
Level 3 name: def\/ghi
Level 4 name: jkl
 * @endcode
 *
 * @pre name must be non-null and point to a null terminated string
 *
 * @param name the string that will be searched
 * @param size the number of bytes of level name found in @p keyName until
 * 	the next delimiter ('/')
 * @return a pointer to the first char of the next level name, it will point to
 * 	NULL when done.
 * @ingroup keyname
 */
char * keyNameGetOneLevel (const char * name, size_t * size)
{
	char * real = (char *) name;
	size_t cursor = 0;
	int end = 0;	 // bool to check for end of level
	int escapeCount = 0; // counter to check if / was escaped

	/* skip all repeating '/' in the beginning */
	while (*real && *real == KDB_PATH_SEPARATOR)
	{
		++real;
	}

	/* now see where this basename ends handling escaped chars with '\' */
	while (real[cursor] && !end)
	{
		switch (real[cursor])
		{
		case KDB_PATH_ESCAPE:
			++escapeCount;
			break;
		case KDB_PATH_SEPARATOR:
			if (!(escapeCount % 2))
			{
				end = 1;
			}
		// fallthrough
		default:
			escapeCount = 0;
		}
		++cursor;
	}

	/* if a '/' stopped our loop, balance the counter */
	if (end)
	{
		--cursor;
	}

	*size = cursor;
	return real;
}

int keyNameIsSpec (const char * name)
{
	if (!strcmp ("spec", name) || !strncmp ("spec/", name, sizeof ("spec/") - 1)) return 1;
	return 0;
}

int keyNameIsProc (const char * name)
{
	if (!strcmp ("proc", name) || !strncmp ("proc/", name, sizeof ("proc/") - 1)) return 1;
	return 0;
}

int keyNameIsDir (const char * name)
{
	if (!strcmp ("dir", name) || !strncmp ("dir/", name, sizeof ("dir/") - 1)) return 1;
	return 0;
}

/**
 * @internal
 *
 * Check whether a key name is under the @p user namespace or not
 *
 * @return 1 if string begins with @p user, 0 otherwise
 * @param keyName the name of a key
 * @see keyIsSystem(), keyIsUser(), keyNameIsSystem()
 * @ingroup keyname
 *
 */
int keyNameIsUser (const char * name)
{
	if (!strcmp ("user", name) || !strncmp ("user/", name, sizeof ("user/") - 1) || !strncmp ("user:", name, sizeof ("user:") - 1))
	{
		return 1;
	}
	return 0;
}

/**
 * @internal
 *
 * Check whether a key name is under the @p system namespace or not
 *
 * @return 1 if string begins with @p system , 0 otherwise
 * @param keyName the name of a key
 * @see keyIsSystem(), keyIsUser(), keyNameIsUser()
 * @ingroup keyname
 *
 */
int keyNameIsSystem (const char * name)
{
	if (!strcmp ("system", name) || !strncmp ("system/", name, sizeof ("system/") - 1)) return 1;
	return 0;
}


/**
 * @internal
 *
 * @pre key->meta must be NULL or a valid keyset
 *
 * clears key (all data members are set to zero)
 * Initializes an empty metadata keyset if null or clears it.
 */
void keyInit (Key * key)
{
	memset (key, 0, sizeof (Key));
}
