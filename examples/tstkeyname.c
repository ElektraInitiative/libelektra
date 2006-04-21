#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdb.h>

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


void *my_malloc(size_t size)
{
	void *buf;

	if ( (buf = malloc(size)) == NULL ) {
		fprintf(stderr, "malloc(%ld) error\n", size);
		exit(-1);
	}
	memset(buf, '@', size);

	return buf;
}

int main()
{
	Key	*key;
	size_t	size;
	char	*buf;
	int	i;
	int	success;
	
	printf("Test key name accessor :\n");
	printf("------------------------\n");

	for(i = 0 ; tstKeyName[i].testName != NULL ; i++) {
		printf("Test %d: %s: %s\n", i+1, tstKeyName[i].testName,tstKeyName[i].keyName);

		key = keyNew(tstKeyName[i].keyName, KEY_SWITCH_END);
		
		/* keyStealName */
		success = (strcmp(keyStealName(key), tstKeyName[i].expectedKeyName) == 0);
		printf("\tCheck keyStealName(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%s\", expecting \"%s\"\n", keyStealName(key), tstKeyName[i].expectedKeyName);

		/* keyStealBaseName */
		success = (strcmp(keyStealBaseName(key), tstKeyName[i].expectedBaseName) == 0);
		printf("\tCheck keyStealBaseName(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%s\", expecting \"%s\"\n", keyStealBaseName(key), tstKeyName[i].expectedBaseName);

		/* keyGetFullRootNameSize */
		size = keyGetFullRootNameSize(key);
		success = (size == strblen(tstKeyName[i].expectedFRootName));
		printf("\tCheck keyGetFullRootNameSize(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%d\", expecting \"%d\"\n", size, strblen(tstKeyName[i].expectedFRootName));
		
		/* keyGetFullRootName */
		size = keyGetFullRootNameSize(key);
		buf = my_malloc(size*sizeof(char));
		keyGetFullRootName(key, buf, size);
		success = (strncmp(buf, tstKeyName[i].expectedFRootName, size) == 0);
		printf("\tCheck keyGetFullRootName(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%s\", expecting \"%s\"\n", buf, tstKeyName[i].expectedFRootName);
		free(buf);

		/* keyGetParentNameSize */
		size = keyGetParentNameSize(key);
		success = (size == strblen(tstKeyName[i].expectedParentName));
		printf("\tCheck keyGetParentNameSize(): %s\n", (success ? "OK": "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%d\", expecting \"%d\"\n", size, strblen(tstKeyName[i].expectedParentName));

		/* keyGetParentName */
		size = keyGetParentNameSize(key)+1;
		buf = my_malloc(size*sizeof(char));
		keyGetParentName(key, buf, size);
		success = (strncmp(buf, tstKeyName[i].expectedParentName, size) == 0);
		printf("\tCheck keyGetParentName(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%s\", expecting \"%s\"\n", buf, tstKeyName[i].expectedParentName);
		free(buf);

		/* keyGetBaseNameSize */
		size = keyGetBaseNameSize(key);
		success = (size == strblen(tstKeyName[i].expectedBaseName));
		printf("\tCheck keyGetBaseNameSize(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%d\", expecting \"%d\"\n", size, strblen(tstKeyName[i].expectedBaseName));

		/* keyGetBaseName */
		size = keyGetBaseNameSize(key)+1;
		buf = my_malloc(size*sizeof(char));
		keyGetBaseName(key, buf, size);
		success = (strncmp(buf, tstKeyName[i].expectedBaseName, size) == 0);
		printf("\tCheck keyGetBaseName(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%s\", expecting \"%s\"\n", buf, tstKeyName[i].expectedBaseName);
		free(buf);

		/* keyGetNameSize */
		size = keyGetNameSize(key);
		success = (size == strblen(tstKeyName[i].expectedKeyName));
		printf("\tCheck keyGetKeyNameSize(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%d\", expecting \"%d\"\n", size, strblen(tstKeyName[i].expectedParentName));
		
		/* keyGetName */
		size = keyGetNameSize(key);
		buf = my_malloc(size*sizeof(char));
		keyGetName(key, buf, size);
		success = (strcmp(buf, tstKeyName[i].expectedKeyName) == 0);
		printf("\tCheck keyGetName(): %s\n", (success ? "OK" : "** FAILED **"));
		if ( !success )
			printf("\t\tGot \"%s\", expecting \"%s\"\n", buf, tstKeyName[i].expectedKeyName);
		free(buf);	

		
		keyDel(key);	
		printf("\n");
	}
}
