#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <kdb.h>
#include <stdio.h>

/*
 * Save this program as binary.c
 * Compile and run it as bellow.
 *
 * Change the data type of 'i' from int to char, and to float,
 * and to double, and recompile it.
 *
 * You should not use binary types.
 * Consider a text representation of
 * your types and write them as string.
 *
 * cc -L/lib -lkdb binary.c -o binary; ./binary; cat user/tmp/bin
 *
 */

int main(void)
{
	int i=25;
	Key *key=keyNew("user/examples/bin",KEY_END);
	Key *back;
	KDB * handle= kdbOpen();

	keySetBinary(key,&i,sizeof(i));
	kdbSetKey(handle,key);
	keyDel(key);

	back=keyNew("user/examples/bin", KEY_END);
	kdbGetKey(handle,back);
	printf("Got back: %d\n",*(int*)keyValue(back));
	keyRemove(back);
	kdbSetKey(handle,key);
	keyDel(back);

	kdbClose(handle);
	return 0;
}
