#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <kdb.h>
#include <stdio.h>

/*
 * Save this program as binary.c
 * Compile and run it as bellow.
 * Change the data type of 'i' from int to char, and to float,
 * and to double, and recompile it.
 *
 * cc -L/lib -lkdb binary.c -o binary; ./binary; cat user/tmp/bin
 *
 */

int main(void) {
	int i=25;
	Key *key=keyNew("user/tmp/bin",KEY_SWITCH_END);
	KDBHandle handle;
	
	kdbOpen(&handle);
	
	keySetBinary(key,&i,sizeof(i));
	kdbSetKey(handle,key);
	
	printf("Size of the value in bytes: %d\n",sizeof(i));
	
	keyDel(key);
	
	kdbClose(&handle);
	
	return 0;
}
