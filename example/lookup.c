#include <kdbfuture.h>
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
	key=ksLookupByName(ks,"system/sw/xorg/current/Screens/Screen0/Displays/00/Depth");
	
	if (key) {
		keyToStream(key,stdout,0);
		
		key=ksCurrent(ks);
		
		keyToStream(key,stdout,0); /* should be the same */
	}
	
	ksRewind(ks);
	while (key=ksLookupByValue(ks,"24")) {
		/* show all keys which value="24" */
		keyToStream(key,stdout,0);
	}
	
	ksRewind(ks);
	while (key=ksLookupByValue(ks,"0")) {
		/* show all keys which value="0" */
		keyToStream(key,stdout,0);
	}

	
	printf("*************** Regex matching\n\n");
	
	regcomp(&regex,".*/InputDevices/.*/Options/.*",0);
	where=KEY_SWITCH_NAME;
	
	ksRewind(ks);
	do {
		match=ksLookupRE(ks,&regex,where,0);
		if (match) {
			key=ksCurrent(ks);
			keyToStream(key,stdout,0);
		}
	} while (match);
	
	regfree(&regex);
}
