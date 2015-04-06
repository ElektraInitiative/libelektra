#include <kdb.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main()
{
	KeySet * myConfig = ksNew (0, KS_END);
	Key * key = keyNew ("/sw/MyApp", KEY_CASCADING_NAME, KEY_END);
	KDB * handle = kdbOpen (key);

	if( kdbGet(handle, myConfig, key) < 0)
	{
		printf ("Error occurred: %s\n",
		keyString(keyGetMeta (key,"error/description")));
		//fields for more information are listed in the value from
		//the Key returned by keyGetMeta(key,"error")
	}

	//check for warnings
	if(keyGetMeta (key,"warnings"))
	{
		int warn_count = strtol(keyString (keyGetMeta (key,"warnings")),NULL,10);
		int warn_iter = 0;

		char buffer [sizeof("warnings/#00/description")+1];

		do{
			if(warn_iter < 10)
				sprintf(&buffer[0],"warnings/#0%i/description",warn_iter);
			else
				sprintf(&buffer[0],"warnings/#%i/description",warn_iter);

			const Key * warnkey = keyGetMeta (key,buffer);
			printf ("Warning occurred: %s\n",keyString (warnkey));
			//fields for more information are listed in the value from
			//the Key returned by keyGetMeta(key,"warnings/#00")
			warn_iter++;
		} while (warn_iter <= warn_count);

		// if all meta data is desired, see following code

		// const Key * meta;
		// keyRewindMeta (key);
		// while ((meta = keyNextMeta (key))!=0)
		// {
		//	printf ("%s=%s\n", keyName (meta), keyString (meta));
		// }

	}

	keyDel (key);

	//lookup
	Key * result = ksLookupByName (myConfig,"/sw/MyApp/Tests/TestKey1", 0);
	if(result)
	{
		//do something with the key
	} else printf("Key not found in KeySet\n");

	const char * key_name = keyName(result);
	const char * key_value = keyString(result);
	const char * key_comment = keyString(keyGetMeta(result, "comment"));
	printf("key: %s value: %s comment: %s\n", key_name, key_value, key_comment);

	ksDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	kdbClose(handle, 0); // no more affairs with the key database.
}
