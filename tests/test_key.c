#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdb.h>

int nbError = 0;
int nbTest = 0;

struct test {
	char	*testName;
	char	*keyName;
	
	char	*expectedKeyName;
	char	*expectedBaseName;
	char	*expectedFRootName;
	char	*expectedParentName;
};

struct test tstKeyName[] = 
{
	{ "Normal key", "system/foo/bar",
		"system/foo/bar",
		"bar",
		"system",
		"system/foo"
	},
			
	{ "Key containing redundant & trailing separator", "system//foo//bar//",
		"system/foo/bar", 	/* keyStealName 	*/
		"bar", 			/* keyStealBaseName	*/
		"system",		/* keyGetFullRootName	*/
		"system/foo"		/* keyGetParentName	*/
	},
	
	{ "Key containing escaped separator", "user:yl///foo\\///bar\\/foo_bar\\",
		"user/foo\\//bar\\/foo_bar\\", 	/* keyStealName 	*/
		"bar\\/foo_bar\\", 		/* keyStealBaseName 	*/
		"user:yl",			/* keyGetFullRootName 	*/ 
		"user/foo\\/"			/* keyGetParentName	*/
	
	},
	
	{ NULL, NULL, NULL }
};

void succeed_if(int test, const char *failedMessage)
{
	extern int nbError;
	extern int nbTest;

	nbTest++;
	
	if ( !test ) {
		nbError++;
		printf("Test failed in %s: %s\n", __FILE__, failedMessage);
	}
}

void *my_malloc(size_t size)
{
	void *buf;

	if ( (buf = malloc(size)) == NULL ) {
		perror("my_malloc");
		exit(-1);
	}
	memset(buf, '@', size);	// Fill with '@' since '\0' mean end of string

	return buf;
}

void test_keyNew()
{
	Key     *key;

	printf("Test key creation\n");
	
	// Empty key
	key = keyNew(KEY_SWITCH_END);
	succeed_if(key != NULL, "keyNew: Unable to create a new empty key");
	succeed_if(keyIsInitialized(key), "keyNew: Doesn't set key as initialized");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete empty key");
		
	// Key with name
	key = keyNew("system/sw/test", KEY_SWITCH_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name");
	succeed_if(strcmp(keyStealName(key), "system/sw/test") == 0, "keyNew: Key's name setted incorrectly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name");
	
	
	// Key with name + value (default type must be KEY_TYPE_STRING)
	key = keyNew("system/sw/test",
			KEY_SWITCH_VALUE, "test",
			KEY_SWITCH_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsString(key), "keyNew: Default key value isn't set to KEY_TYPE_STRING");
	succeed_if( strcmp(keyStealValue(key), "test") == 0, "keyNew: Value not set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + value");
	
	// Key with name + UID/GID
	key = keyNew("system/sw/test",
			KEY_SWITCH_UID, 123,
			KEY_SWITCH_GID, 456,
			KEY_SWITCH_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + UID + GID");
	succeed_if(keyGetUID(key) == 123, "keyNew: UID no set correctly");
	succeed_if(keyGetGID(key) == 456, "keyNew: GID not set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + UID + GID");

	// Key with name + MODE
	key = keyNew("system/sw/test",
			KEY_SWITCH_MODE, 0644,
			KEY_SWITCH_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + access");
	succeed_if(keyGetAccess(key) == 0644, "keyNew: access no set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + access");
	
	// Key with name + owner
	key = keyNew("user/test/test",
			KEY_SWITCH_OWNER, "yl",
			KEY_SWITCH_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + owner");
	succeed_if( strcmp(keyStealOwner(key), "yl") == 0, "keyNew: owner not set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + owner");
}

void test_keyName()
{
	Key	*key;
	size_t	size;
	char	*buf;
	int	i;
	
	printf("Test key's name manipulation\n");
	for(i = 0 ; tstKeyName[i].testName != NULL ; i++) {
		key = keyNew(tstKeyName[i].keyName, KEY_SWITCH_END);
		
		/* keyStealName */
		succeed_if( (strcmp(keyStealName(key), tstKeyName[i].expectedKeyName) == 0) , "keyStealName" );

		/* keyStealBaseName */
		succeed_if( (strcmp(keyStealBaseName(key), tstKeyName[i].expectedBaseName) == 0), "keyStealBaseName" );

		/* keyGetFullRootNameSize */
		size = keyGetFullRootNameSize(key);
		succeed_if( (size == strblen(tstKeyName[i].expectedFRootName)), "keyGetFullRootNameSize" );
		
		/* keyGetFullRootName */
		size = keyGetFullRootNameSize(key);
		buf = my_malloc(size*sizeof(char));
		keyGetFullRootName(key, buf, size);
		succeed_if( (strncmp(buf, tstKeyName[i].expectedFRootName, size) == 0), "keyGetFullRootName" );
		free(buf);

		/* keyGetParentNameSize */
		size = keyGetParentNameSize(key);
		succeed_if( (size == strblen(tstKeyName[i].expectedParentName)), "ketGetParentNameSize" );

		/* keyGetParentName */
		size = keyGetParentNameSize(key)+1;
		buf = my_malloc(size*sizeof(char));
		keyGetParentName(key, buf, size);
		succeed_if( (strncmp(buf, tstKeyName[i].expectedParentName, size) == 0), "keyGetParentName" );
		free(buf);

		/* keyGetBaseNameSize */
		size = keyGetBaseNameSize(key);
		succeed_if( (size == strblen(tstKeyName[i].expectedBaseName)), "keyGetBaseNameSize" );

		/* keyGetBaseName */
		size = keyGetBaseNameSize(key)+1;
		buf = my_malloc(size*sizeof(char));
		keyGetBaseName(key, buf, size);
		succeed_if( (strncmp(buf, tstKeyName[i].expectedBaseName, size) == 0), "keyGetBaseName" );
		free(buf);

		/* keyGetNameSize */
		size = keyGetNameSize(key);
		succeed_if( (size == strblen(tstKeyName[i].expectedKeyName)), "keyGetKeyNameSize" );
		
		/* keyGetName */
		size = keyGetNameSize(key);
		buf = my_malloc(size*sizeof(char));
		keyGetName(key, buf, size);
		succeed_if( (strcmp(buf, tstKeyName[i].expectedKeyName) == 0), "keyGetName" );
		free(buf);	

		
		keyDel(key);	
	}
}

void test_keyDup()
{
	Key     *orig, *copy;

	printf("Test key duplication\n");
	
	// Create test key
	orig = keyNew("user:yl/foo/bar",
			KEY_SWITCH_VALUE, "foobar",
			KEY_SWITCH_UID, 123,
			KEY_SWITCH_GID, 456,
			KEY_SWITCH_MODE, 0644,
			KEY_SWITCH_TYPE, KEY_TYPE_STRING,
			KEY_SWITCH_END);

	// Create target
	copy = keyNew(KEY_SWITCH_END);
	
	// Copy the key
	succeed_if( keyDup(orig, copy) == 0, "keyDup failed");
	
	// Check the copy
	succeed_if( strcmp(keyStealName(copy), "user/foo/bar") == 0, "keyDup: key name copy error");
	succeed_if( strcmp(keyStealOwner(copy), "yl") == 0, "keyDup: key name owner copy error");
	succeed_if( strcmp(keyStealValue(copy), "foobar") == 0, "keyDup: key value copy error");
	succeed_if( keyGetUID(copy) == 123, "keyDup: key UID copy error");
	succeed_if( keyGetGID(copy) == 456, "keyDup: key GID copy error");
	succeed_if( (keyGetAccess(copy) & 0777) == 0644, "keyDup: key access copy error");
	succeed_if( keyIsString(copy), "keyDup: key type copy error");
		
	keyDel(orig);
	keyDel(copy);
}

int main()
{
	printf("KEY STRUCT TESTS\n");
	printf("----------------\n\n");
	test_keyNew();
	test_keyName();
	test_keyDup();

	printf("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return (nbError > 0);
}
