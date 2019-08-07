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

#include <stdio.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif


#include "elektra/kdb.h"
#include "kdbprivate.h"
#include "kdbtypes.h"
#include <kdbassert.h>


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
 * clears key (all data members are set to zero)
 */
int keyInit (Key * key)
{
	memset (key, 0, sizeof (struct _Key));

	return 0;
}

static int elektraSetMetaInt (Key * key, const char * meta, int value)
{
	char * str = 0;
	if ((str = elektraFormat ("%d", value)) == 0)
	{
		return -1;
	}

	keySetMeta (key, meta, str);
	elektraFree (str);
	return 0;
}

// duplicate of keySetMode in meta/meta.c
static int elektraSetMode (Key * key, mode_t mode)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT - 1, "%o", mode) < 0)
	{
		return -1;
	}

	keySetMeta (key, "mode", str);

	return 0;
}

/**
 * @internal
 *
 * helper functions for keyNew/keyVNew
 *
 * @pre caller must use va_start and va_end on va
 * @param key initialized Key
 * @param keyName a valid name to the key, or NULL to get a simple
 * initialized, but really empty, object
 * @param va the variadic argument list
 */
void keyVInit (Key * key, const char * name, va_list va)
{
	if (!key) return;

	keyInit (key);

	if (name)
	{
		keyswitch_t action = 0;
		size_t value_size = 0;
		void * value = 0;
		void (*func) (void) = 0;
		int flags = 0;
		char * owner = 0;
		int mode = 0;
		int hasMode = 0;

		while ((action = va_arg (va, keyswitch_t)))
		{
			switch (action)
			{
			/* flags with an argument */
			case KEY_SIZE:
				value_size = va_arg (va, size_t);
				break;
			case KEY_VALUE:
				value = va_arg (va, void *);
				if (value_size && keyIsBinary (key))
					keySetBinary (key, value, value_size);
				else if (keyIsBinary (key))
					keySetBinary (key, value, elektraStrLen (value));
				else
					keySetString (key, value);
				break;
			case KEY_FUNC:
				func = va_arg (va, void (*) (void));
				keySetBinary (key, &func, sizeof (func));
				break;
			case KEY_META:
				value = va_arg (va, char *);
				/* First parameter is name */
				keySetMeta (key, value, va_arg (va, char *));
				break;

			/* flags without an argument */
			case KEY_FLAGS:
				flags |= va_arg (va, int); // FALLTHROUGH
			case KEY_BINARY:
			case KEY_LOCK_NAME:
			case KEY_LOCK_VALUE:
			case KEY_LOCK_META:
			case KEY_CASCADING_NAME:
			case KEY_META_NAME:
			case KEY_EMPTY_NAME:
				if (action != KEY_FLAGS) flags |= action;
				if (test_bit (flags, KEY_BINARY)) keySetMeta (key, "binary", "");
				if (test_bit (flags, KEY_LOCK_NAME)) elektraKeyLock (key, KEY_LOCK_NAME);
				if (test_bit (flags, KEY_LOCK_VALUE)) elektraKeyLock (key, KEY_LOCK_VALUE);
				if (test_bit (flags, KEY_LOCK_META)) elektraKeyLock (key, KEY_LOCK_META);
				break;

			/* deprecated flags */
			case KEY_NAME:
				name = va_arg (va, char *);
				break;
			case KEY_OWNER:
				owner = va_arg (va, char *);
				break;
			case KEY_COMMENT:
				keySetMeta (key, "comment", va_arg (va, char *));
				break;
			case KEY_UID:
				elektraSetMetaInt (key, "uid", va_arg (va, int));
				break;
			case KEY_GID:
				elektraSetMetaInt (key, "gid", va_arg (va, int));
				break;
			case KEY_DIR:
				mode |= KDB_DIR_MODE;
				break;
			case KEY_MODE:
				hasMode = 1;
				mode |= va_arg (va, int);
				break;
			case KEY_ATIME:
				elektraSetMetaInt (key, "atime", va_arg (va, time_t));
				break;
			case KEY_MTIME:
				elektraSetMetaInt (key, "mtime", va_arg (va, time_t));
				break;
			case KEY_CTIME:
				elektraSetMetaInt (key, "ctime", va_arg (va, time_t));
				break;

			default:
				ELEKTRA_ASSERT (0, "Unknown option " ELEKTRA_UNSIGNED_LONG_LONG_F " in keyVInit",
						(kdb_unsigned_long_long_t) action);
				break;
			}
		}

		option_t name_options = flags & (KEY_CASCADING_NAME | KEY_META_NAME | KEY_EMPTY_NAME);
		elektraKeySetName (key, name, name_options);

		if (!hasMode && mode == KDB_DIR_MODE)
			elektraSetMode (key, KDB_FILE_MODE | KDB_DIR_MODE);
		else if (mode != 0)
			elektraSetMode (key, mode);

		if (owner) keySetOwner (key, owner);
	}
}
