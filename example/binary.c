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
	Key key;
	int i=25;
	
	kdbOpen();
	
	keyInit(&key);
	keySetName(&key,"user/tmp/bin");
	keySetBinary(&key,&i,sizeof(i));
	kdbSetKey(&key);
	
	printf("Size of the value in bytes: %d\n",sizeof(i));
	
	kdbClose();
	
	return 0;
}
