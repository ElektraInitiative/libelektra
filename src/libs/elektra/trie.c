/**
 * @file
 *
 * @brief Interna of trie functionality.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if VERBOSE && defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include "kdbinternal.h"

static char * elektraTrieStartsWith (const char * str, const char * substr);
static Backend * elektraTriePrefixLookup (Trie * trie, const char * name);

/**
 * @brief Internal Datastructure for mountpoints
 *
 * @{
 */

/**
 * Lookups a backend inside the trie.
 *
 * @return the backend if found
 * @return 0 otherwise
 * @param trie the trie object to work with
 * @param key the name of this key will be looked up
 * @ingroup trie
 */
Backend * elektraTrieLookup (Trie * trie, const Key * key)
{
	char * where = 0;
	Backend * ret = 0;
	size_t len = 0;

	if (!key) return 0;
	if (!trie) return 0;

	len = keyGetNameSize (key) + 1;
	if (len <= 1) return 0; // would crash otherwise
	where = elektraMalloc (len);
	strncpy (where, keyName (key), len);
	where[len - 2] = '/';

	ret = elektraTriePrefixLookup (trie, where);
	elektraFree (where);

	return ret;
}

/**
 * Closes the trie and all opened backends within.
 *
 * @param trie the trie to close
 * @param errorKey the key used to report warnings
 * @ingroup trie
 * @retval 0 on success
 */
int elektraTrieClose (Trie * trie, Key * errorKey)
{
	size_t i;
	if (trie == NULL) return 0;
	for (i = 0; i < KDB_MAX_UCHAR; ++i)
	{
		if (trie->text[i] != NULL)
		{
			elektraTrieClose (trie->children[i], errorKey);
			if (trie->value[i]) elektraBackendClose (trie->value[i], errorKey);
			elektraFree (trie->text[i]);
		}
	}
	if (trie->empty_value)
	{
		elektraBackendClose (trie->empty_value, errorKey);
	}
	elektraFree (trie);
	return 0;
}
Trie * elektraTrieInsert (Trie * trie, const char * name, Backend * value)
{
	char * p;
	unsigned char idx;

	if (name == 0)
	{
		name = "";
	}
	idx = (unsigned char)name[0];

	if (trie == NULL)
	{
		trie = elektraCalloc (sizeof (Trie));

		if (!strcmp ("", name))
		{
			trie->empty_value = value;
			return trie;
		}

		trie->textlen[idx] = strlen (name);

		trie->text[idx] = elektraStrDup (name);

		trie->value[idx] = value;
		return trie;
	}

	if (!strcmp ("", name))
	{
		trie->empty_value = value;
		return trie;
	}

	if (trie->text[idx])
	{
		/* there exists an entry with the same first character */
		if ((p = elektraTrieStartsWith (name, trie->text[idx])) == 0)
		{
			/* the name in the trie is part of the searched name --> continue search */
			trie->children[idx] = elektraTrieInsert (trie->children[idx], name + trie->textlen[idx], value);
		}
		else
		{
			/* name in trie doesn't match name --> split trie */
			char * newname;
			Trie * child;
			unsigned char idx2;

			newname = elektraStrDup (p);
			*p = 0; /* shorten the old name in the trie */
			trie->textlen[idx] = strlen (trie->text[idx]);

			child = trie->children[idx];

			/* insert the name given as a parameter into the new trie entry */
			trie->children[idx] = elektraTrieInsert (NULL, name + (p - trie->text[idx]), value);

			/* insert the split try into the new trie entry */

			idx2 = (unsigned char)newname[0];
			trie->children[idx]->text[idx2] = newname;
			trie->children[idx]->textlen[idx2] = strlen (newname);
			trie->children[idx]->value[idx2] = trie->value[idx];
			trie->children[idx]->children[idx2] = child;

			trie->value[idx] = 0;
		}
	}
	else
	{
		/* there doesn't exist an entry with the same first character */
		trie->text[idx] = elektraStrDup (name);
		trie->value[idx] = (void *)value;
		trie->textlen[idx] = strlen (name);
	}

	return trie;
}


/******************
 * Private static declarations
 ******************/


#if 0

static Trie *delete_trie(Trie *trie, char *name, CloseMapper closemapper)
{
	Trie *tr;
	unsigned char idx;
	if (trie==NULL) {
		return NULL;
	}

	idx=(unsigned char) name[0];

	if (trie->text[idx]==NULL) {
		return NULL;
	}

	if (elektraTrieStartsWith(name,trie->text[idx])==0) {

		tr=delete_trie(trie->children[idx],name+trie->textlen[idx],closemapper);

		if (tr==NULL) {
			/* child trie has been deleted */
			trie->children[idx]=NULL;
			elektraFree (trie->text[idx]);
			closemapper(trie->value[idx]);
			trie->text[idx]=NULL;
		}

		return trie;
	}
	return NULL;
}

#endif

/**
 * return NULL if string starts with substring, except for the terminating '\0',
 * otherwise return a pointer to the first mismatch in substr.
 *
 * Takes const char* arguments but return char* pointer to it.
 * (like e.g. strchr)
 */
static char * elektraTrieStartsWith (const char * str, const char * substr)
{
	size_t i = 0;
	size_t sublen = strlen (substr);

	for (i = 0; i < sublen; i++)
	{
		if (substr[i] != str[i]) return (char *)substr + i;
	}
	return 0;
}

static Backend * elektraTriePrefixLookup (Trie * trie, const char * name)
{
	if (trie == NULL) return NULL;

	unsigned char idx = (unsigned char)name[0];
	const char * trieText = trie->text[idx];

	if (trieText == NULL)
	{
		return trie->empty_value;
	}

	void * ret = NULL;
	if (elektraTrieStartsWith (name, trieText) == 0)
	{
		ret = elektraTriePrefixLookup (trie->children[idx], name + trie->textlen[idx]);
	}
	else
	{
		return trie->empty_value;
	}

	if (ret == NULL && trie->value[idx] == NULL)
	{
		return trie->empty_value;
	}
	if (ret == NULL) return trie->value[idx];

	return ret;
}
