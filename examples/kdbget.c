#include <kdb.h>
#include <stdio.h>
#include <string.h>

int main()
{
	KeySet *myConfig = ksNew(0, KS_END);
	Key *key = keyNew("/sw/MyApp", KEY_CASCADING_NAME, KEY_END);
	KDB *handle = kdbOpen(key);

	kdbGet(handle, myConfig, key);

	//check for error
	if(keyGetMeta (key,"error"))
	{
		printf("Error occurred: %s\n",
				keyString(keyGetMeta(key,"error/description")));
		//fields for more information are listed in the value from
		//the Key returned by keyGetMeta(key,"error")
	}
	//check for warnings
	if(keyGetMeta (key,"warnings"))
	{
		const char * warning_count = keyString (keyGetMeta (key,"warnings"));
		char buffer[] = "warnings/#00/description";

		do {
			const Key *warnkey = keyGetMeta (key,buffer);
			printf ("Warning occurred: %s\n",keyString (warnkey));
			//fields for more information are listed in the value from
			//the Key returned by keyGetMeta(key,"warnings/#00")
			buffer[11]++;
			if (buffer[11] > '9')
			{
				buffer[11] = '0';
				buffer[10]++;
			}
		} while (strncmp (&buffer[10],warning_count,2)<=0);
	}

	keyDel (key);

	//insert a key so lookup has something to lookup
	ksAppendKey (myConfig,
				keyNew ("/sw/MyApp/Tests/TestKey1",
					KEY_VALUE, "foo",
					KEY_META, "comment", "bar",
					KEY_END));

	//lookup
	Key * result = ksLookupByName (myConfig,"/sw/MyApp/Tests/TestKey1", 0);
	if(result)
	{
		//do something with the key
		keySetString(result,"foobar");
	} else printf("Key not found in KeySet\n");


	const char * key_name = keyName(result);
	const char * key_value = keyString(result);
	const char * key_comment = keyString(keyGetMeta(result, "comment"));
	printf("key: %s value: %s comment: %s\n", key_name, key_value, key_comment);

	ksDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	kdbClose(handle, 0); // no more affairs with the key database.
}
