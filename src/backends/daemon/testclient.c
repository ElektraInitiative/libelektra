/*
*  C Implementation: test
*
* Description: 
*
*
* Author: Avi Alkalay <avi@unix.sh>, (C) 2006
*
* Copyright: See COPYING file that comes with this distribution
*
*/

#include <stdio.h>
#include <kdb.h>


int main(int argc,char **argv) {
	KDBHandle handle;
	Key *key;
	
	kdbOpen(&handle);
	key=keyNew("user/env/alias/ll",KEY_SWITCH_END);
	
	kdbGetKey(handle,key);
	
	keyToStream(key,stdout,0);
}
