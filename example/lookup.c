#include <kdb.h>
#include <stdio.h>

int main(int argc,char **argv) {
	KeySet *ks;
	Key *key=0;
	u_int32_t where,match;
	regex_t regex;
	
	ks=ksNew();
	
	kdbOpen();
	kdbGetChildKeys("system/sw/xorg",ks,KDB_O_RECURSIVE|KDB_O_SORT);
	kdbClose();
	
	ksRewind(ks);
	key=ksLookupByName(ks,"system/sw/xorg/current/screens/screen0/displays/00/depth",
		KDB_O_NOCASE);
	
	printf("*************** Name matching\n\n");
	if (key) {
		keyToStream(key,stdout,0);
		
		key=ksCurrent(ks);
		
		keyToStream(key,stdout,0); /* should be the same */
	}
	
	printf("*************** Value matching\n\n");
	
	ksRewind(ks);
	while (key=ksLookupByValue(ks,"24",0)) {
		/* show all keys which value="24" */
		keyToStream(key,stdout,0);
	}
	
	ksRewind(ks);
	while (key=ksLookupByValue(ks,"0",0)) {
		/* show all keys which value="0" */
		keyToStream(key,stdout,0);
	}

	
	printf("*************** Regex matching\n\n");
	
	regcomp(&regex,".*/InputDevices/.*/Options/.*",0);
	where=KEY_SWITCH_NAME;
	
	ksRewind(ks);
	do {
		match=ksLookupRE(ks,where,&regex,KDB_O_NOSPANPARENT);
		if (match) {
			key=ksCurrent(ks);
			keyToStream(key,stdout,0);
		}
	} while (match);
	
	regfree(&regex);
}
