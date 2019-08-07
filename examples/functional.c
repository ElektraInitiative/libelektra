/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>

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
int ksForEach (KeySet * ks, int (*func) (Key * k))
{
	int ret = 0;
	Key * current;

	cursor_t cursor = ksGetCursor (ks);
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
int ksFilter (KeySet * result, KeySet * input, int (*filter) (Key * k))
{
	int ret = 0;
	Key * current;

	cursor_t cursor = ksGetCursor (input);
	ksRewind (input);
	while ((current = ksNext (input)) != 0)
	{
		int rc = filter (current);
		if (rc == -1)
			return -1;
		else if (rc != 0)
		{
			++ret;
			ksAppendKey (result, keyDup (current));
		}
	}
	ksSetCursor (input, cursor);
	return ret;
}


Key * global_a;

int add_string (Key * check)
{
	return keySetString (check, "string");
}
int add_comment (Key * check)
{
	return keySetMeta (check, "comment", "comment");
}
int has_a (Key * check)
{
	return keyName (check)[5] == 'a';
}
int below_a (Key * check)
{
	return keyIsBelow (global_a, check);
}
int direct_below_a (Key * check)
{
	return keyIsDirectBelow (global_a, check);
}

int sum_helper (Key * check)
{
	return atoi (keyValue (check));
}
int below_30 (Key * check)
{
	return atoi (keyValue (check)) < 30;
}
int find_80 (Key * check)
{
	int n = atoi (keyValue (check));
	return n > 70 ? -1 : 1;
}

int main (void)
{
	KeySet * out;
	KeySet * ks = ksNew (64, keyNew ("user/a/1", KEY_END), keyNew ("user/a/2", KEY_END), keyNew ("user/a/b/1", KEY_END),
			     keyNew ("user/a/b/2", KEY_END), keyNew ("user/ab/2", KEY_END), keyNew ("user/b/1", KEY_END),
			     keyNew ("user/b/2", KEY_END), KS_END);
	KeySet * values = 0;
	KeySet * values_below_30 = 0;

	global_a = keyNew ("user/a", KEY_END);

	ksForEach (ks, add_string);
	ksForEach (ks, add_comment);

	out = ksNew (0, KS_END);
	ksFilter (out, ks, has_a);
	ksDel (out);

	out = ksNew (0, KS_END);
	ksFilter (out, ks, below_a);
	ksDel (out);

	out = ksNew (0, KS_END);
	ksFilter (out, ks, direct_below_a);
	ksDel (out);

	ksDel (ks);
	keyDel (global_a);
	global_a = 0;

	values = ksNew (64, keyNew ("user/a", KEY_VALUE, "40", KEY_END), keyNew ("user/b", KEY_VALUE, "20", KEY_END),
			keyNew ("user/c", KEY_VALUE, "80", KEY_END), keyNew ("user/d", KEY_VALUE, "24", KEY_END),
			keyNew ("user/e", KEY_VALUE, "32", KEY_END), keyNew ("user/f", KEY_VALUE, "12", KEY_END),
			keyNew ("user/g", KEY_VALUE, "43", KEY_END), KS_END);

	/* add together */
	ksForEach (values, sum_helper);

	values_below_30 = ksNew (0, KS_END);
	ksFilter (values_below_30, values, below_30);
	ksForEach (values_below_30, sum_helper);

	ksForEach (values, find_80);
	ksCurrent (values);		      /* here is user/c */
	ksLookupByName (values, "user/c", 0); /* should find the same */
	ksDel (values);
	ksDel (values_below_30);
}
