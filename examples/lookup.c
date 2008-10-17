#include <stdio.h>
#include <kdb.h>

int main(int argc,char **argv) {
	KeySet *ks;
	Key *key=0;
	KDB *handle = kdbOpen();

	ks=ksNew(0);

	kdbGetByName(handle,ks,"system/sw/xorg",0);
	kdbClose(handle);

	ksRewind(ks);
	key=ksLookupByName(ks,"system/sw/xorg/current/screens/screen0/displays/00/depth",
		KDB_O_NOCASE);

	printf("*************** Name matching\n\n");
	if (key) {
		/* keyToStream(key,stdout,0); */
		
		key=ksCurrent(ks);
		
		/* keyToStream(key,stdout,0);  should be the same */
	}
}
