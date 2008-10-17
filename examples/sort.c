#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <kdb.h>

void ksUnsort (KeySet *ks)
{
	Key *cur;
	size_t size = 0;
	KeySet *randks=ksNew(0); /*This is the final randomized keyset*/
	KeySet *tempks=ksNew(0); /*Temporary storage for keys not chosen to be inserted*/

	while (ksGetSize(ks) > 0)
	{
		ksRewind(ks);
		size = ksGetSize(ks);
		/* printf ("iterating %d\n", size); */
		while ((cur=ksPop(ks)) != 0)
		{
			/* printf ("\titerating %s\n", keyName(cur)); */
			if (!(rand()%size)) ksAppendKey(randks, cur);
			else ksAppendKey(tempks,cur);
		}
		ksAppend(ks, tempks);
		ksCopy(tempks,0);
	}

	ksCopy (ks, randks);

	ksDel (randks);
	ksDel (tempks);
}

int main(void)
{
	Key *cur;
	Key *found;
	KeySet *ks = ksNew (30,
		keyNew ("user/rem2", KEY_REMOVE, KEY_DIR, KEY_END),
		keyNew ("user/rem1/key2", KEY_REMOVE, KEY_END),
		keyNew ("user/rem1/key1", KEY_REMOVE, KEY_END),
		keyNew ("user/rem1", KEY_REMOVE, KEY_DIR, KEY_END),
		keyNew ("user/dir1", KEY_DIR, KEY_END),
		keyNew ("user/dir1/key1", KEY_VALUE, "value1", KEY_END),
		keyNew ("user/dir1/key2", KEY_VALUE, "value2", KEY_END),
		keyNew ("user/dir1/key3", KEY_VALUE, "value3", KEY_END),
		keyNew ("user/dir1/key4", KEY_VALUE, "value4", KEY_END),
		keyNew ("user/dir1/.inactive1", KEY_COMMENT, "key is inactive", KEY_END),
		keyNew ("user/dir1/.inactive2", KEY_COMMENT, "additional information", KEY_END),
		keyNew ("user/dir2", KEY_DIR, KEY_END),
		keyNew ("user/dir2/key1", KEY_VALUE, "value1", KEY_END),
		keyNew ("user/dir2/key2", KEY_VALUE, "value2", KEY_END),
		keyNew ("user/dir2/key3", KEY_VALUE, "value3", KEY_END),
		keyNew ("user/dir2/key4", KEY_VALUE, "value4", KEY_END),
		keyNew ("user/dir3", KEY_DIR, KEY_END),
		keyNew ("user/dir3/key1", KEY_VALUE, "value1", KEY_END),
		keyNew ("user/dir3/.inactive1", KEY_COMMENT, "key is inactive", KEY_END),
		keyNew ("user/dir3/.inactive2", KEY_COMMENT, "a users comment", KEY_END),
		keyNew ("user/dir4", KEY_DIR, KEY_END),
		keyNew ("user/dir5", KEY_DIR, KEY_END),
		KS_END);

	srand (23);
	ksUnsort (ks);

	printf ("Keys are now unsorted:\n");
	ksRewind(ks);
	while ((cur=ksNext(ks)) != 0)
	{	/* Iterates over all keys and prints their name */
		if (keyNeedRemove(cur)) printf ("%s (Need Remove)\n", keyName(cur));
		else printf ("%s\n", keyName(cur));
	}

	found = ksLookupByName (ks, "user/dir1/key3", 0);
	printf ("\nKeys are sorted after ksLookupByName:\n");
	ksRewind(ks);
	while ((cur=ksNext(ks)) != 0)
	{	/* Iterates over all keys and prints their name */
		if (keyNeedRemove(cur)) printf ("%s (Need Remove)\n", keyName(cur));
		else printf ("%s\n", keyName(cur));
	}

	keySetName(found, "user/something/else");
	found = ksLookupByName (ks, "user/something/else", 0);
	printf ("\nKeys are *not* sorted after ksLookupByName because of changed name.\n");
	printf ("The search for user/something/else found: %s\n", keyName(found));
	printf ("The order is:\n");
	ksRewind(ks);
	while ((cur=ksNext(ks)) != 0)
	{	/* Iterates over all keys and prints their name */
		if (keyNeedRemove(cur)) printf ("%s (Need Remove)\n", keyName(cur));
		else printf ("%s\n", keyName(cur));
	}

	printf ("\nSo we have to sort manually using ksSort()!\n");
	ksSort (ks);
	printf ("The same is valid for keyRemove().");
	found = ksLookupByName (ks, "user/something/else", 0);
	keyRemove (found);
	found = ksLookupByName (ks, "user/something/else", 0);
	printf ("\nKeys are *not* sorted after ksLookupByName because of changed name.\n");
	printf ("The search for user/something/else (Need Remove) found: %s\n", keyName(found));
	printf ("The order is:\n");
	ksRewind(ks);
	while ((cur=ksNext(ks)) != 0)
	{	/* Iterates over all keys and prints their name */
		if (keyNeedRemove(cur)) printf ("%s (Need Remove)\n", keyName(cur));
		else printf ("%s\n", keyName(cur));
	}

	printf ("\nSo we have to sort manually using ksSort()!\n");
	ksSort (ks);
	printf ("Now we get:\n");
	ksRewind(ks);
	while ((cur=ksNext(ks)) != 0)
	{	/* Iterates over all keys and prints their name */
		if (keyNeedRemove(cur)) printf ("%s (Need Remove)\n", keyName(cur));
		else printf ("%s\n", keyName(cur));
	}


	return 0;
}
