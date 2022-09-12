/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>

// for keySetComment
#include <kdbextension.h>

#include <stdlib.h>

/**A functional access to keys.
 *
 * Instead of writing your own loop you can write
 * a function working with a key and pass it to
 * this method.
 *
 * The function will be executed for all keys in
 * the keyset.
 *
 * @param ks the keyset to work with
 * @param func the function to execute on every key of the keyset
 * @return the sum of all return values
 * @retval -1 if any function returned -1, the execution will stop there, but
 * 	ksCurrent() will tell you where it stopped.
 * @see ksFilter()
 */
int ksForEach (ElektraKeyset * ks, int (*func) (ElektraKey * k))
{
	int ret = 0;
	ElektraKey * current;

	elektraCursor cursor = ksGetCursor (ks);
	ksRewind (ks);
	while ((current = ksNext (ks)) != 0)
	{
		int rc = func (current);
		if (rc == -1) return -1;
		ret += rc;
	}
	ksSetCursor (ks, cursor);
	return ret;
}


/**Filter a keyset.
 *
 * filter is executed for every key in the keyset result. When it returns 0,
 * the key will be dropped, when it returns 1 it will be ksAppendKey()ed to result,
 * when it returns -1 the processing will be stopped. You can use ksCurrent()
 * on input to see where the problem was. Because of this input is not const,
 * apart from ksCurrent() the input will not be changed. The keys that have
 * been in result before will stay untouched.
 *
 * @param result is the keyset where keys are added.
 * @param input is the keyset the filter works on.
 * @param filter is the function to execute on every key of the keyset to decide if
 * 	it should be ksAppendKey()ed to the result.
 * @return the number of keys added on success
 * @retval 0 when nothing was done
 * @retval -1 when filter returned an error (-1), ksCurrent() of input will
 * 	be the problematic key.
 * @see ksForEach()
 **/
int ksFilter (ElektraKeyset * result, ElektraKeyset * input, int (*filter) (ElektraKey * k))
{
	int ret = 0;
	ElektraKey * current;

	elektraCursor cursor = ksGetCursor (input);
	ksRewind (input);
	while ((current = ksNext (input)) != 0)
	{
		int rc = filter (current);
		if (rc == -1)
			return -1;
		else if (rc != 0)
		{
			++ret;
			ksAppendKey (result, keyDup (current, ELEKTRA_KEY_CP_ALL));
		}
	}
	ksSetCursor (input, cursor);
	return ret;
}


ElektraKey * global_a;

int add_string (ElektraKey * check)
{
	return keySetString (check, "string");
}
int add_comment (ElektraKey * check)
{
	return keySetMeta (check, "comment", "comment");
}
int has_a (ElektraKey * check)
{
	return keyName (check)[5] == 'a';
}
int below_a (ElektraKey * check)
{
	return keyIsBelow (global_a, check);
}
int direct_below_a (ElektraKey * check)
{
	return keyIsDirectlyBelow (global_a, check);
}

int sum_helper (ElektraKey * check)
{
	return atoi (keyValue (check));
}
int below_30 (ElektraKey * check)
{
	return atoi (keyValue (check)) < 30;
}
int find_80 (ElektraKey * check)
{
	int n = atoi (keyValue (check));
	return n > 70 ? -1 : 1;
}

int main (void)
{
	ElektraKeyset * out;
	ElektraKeyset * ks = ksNew (64, keyNew ("user:/a/1", ELEKTRA_KEY_END), keyNew ("user:/a/2", ELEKTRA_KEY_END), keyNew ("user:/a/b/1", ELEKTRA_KEY_END),
			     keyNew ("user:/a/b/2", ELEKTRA_KEY_END), keyNew ("user:/ab/2", ELEKTRA_KEY_END), keyNew ("user:/b/1", ELEKTRA_KEY_END),
			     keyNew ("user:/b/2", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * values = 0;
	ElektraKeyset * values_below_30 = 0;

	global_a = keyNew ("user:/a", ELEKTRA_KEY_END);

	ksForEach (ks, add_string);
	ksForEach (ks, add_comment);

	out = ksNew (0, ELEKTRA_KS_END);
	ksFilter (out, ks, has_a);
	ksDel (out);

	out = ksNew (0, ELEKTRA_KS_END);
	ksFilter (out, ks, below_a);
	ksDel (out);

	out = ksNew (0, ELEKTRA_KS_END);
	ksFilter (out, ks, direct_below_a);
	ksDel (out);

	ksDel (ks);
	keyDel (global_a);
	global_a = 0;

	values = ksNew (64, keyNew ("user:/a", ELEKTRA_KEY_VALUE, "40", ELEKTRA_KEY_END), keyNew ("user:/b", ELEKTRA_KEY_VALUE, "20", ELEKTRA_KEY_END),
			keyNew ("user:/c", ELEKTRA_KEY_VALUE, "80", ELEKTRA_KEY_END), keyNew ("user:/d", ELEKTRA_KEY_VALUE, "24", ELEKTRA_KEY_END),
			keyNew ("user:/e", ELEKTRA_KEY_VALUE, "32", ELEKTRA_KEY_END), keyNew ("user:/f", ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END),
			keyNew ("user:/g", ELEKTRA_KEY_VALUE, "43", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	/* add together */
	ksForEach (values, sum_helper);

	values_below_30 = ksNew (0, ELEKTRA_KS_END);
	ksFilter (values_below_30, values, below_30);
	ksForEach (values_below_30, sum_helper);

	ksForEach (values, find_80);
	ksCurrent (values);		       /* here is user:/c */
	ksLookupByName (values, "user:/c", 0); /* should find the same */
	ksDel (values);
	ksDel (values_below_30);
}
