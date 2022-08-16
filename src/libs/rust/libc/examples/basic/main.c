#include <stdio.h>
#include <elektra_rs.h>

int main (void)
{
	CKey * key = elektraKeyNew ("user:/test/qwe/asd");
	printf ("%s\n", elektraKeyName (key));

	CKey * key2 = elektraKeyNew ("user:/test/qwe/asd/qwe");
	printf ("%s\n", elektraKeyName (key2));

	printf ("--------------\n");
	printf ("KeyIsBelow:\n");

	printf ("%i\n", elektraKeyIsBelow (key, key2));
	printf ("%i\n", elektraKeyIsBelow (key2, key));

	printf ("--------------\n");
	printf ("keyAddName:\n");

	printf ("%zu\n", elektraKeyAddName (key, "yyyyyyy"));
	printf ("%s\n", elektraKeyName (key));
	printf ("%s\n", elektraKeyBaseName (key));
	printf ("%zu\n", elektraKeyBaseNameSize (key));

	printf ("--------------\n");
	printf ("keySetName:\n");

	printf ("%zu\n", elektraKeySetName (key, "user:/asd/qwe/asd"));
	printf ("%s\n", elektraKeyName (key));
	printf ("%s\n", elektraKeyBaseName (key));
	printf ("%zu\n", elektraKeyBaseNameSize (key));

	printf ("--------------\n");
	printf ("keySetBaseName:\n");

	printf ("%zu\n", elektraKeySetBaseName (key, ""));
	printf ("%s\n", elektraKeyName (key));
	printf ("%s\n", elektraKeyBaseName (key));
	printf ("%zu\n", elektraKeyBaseNameSize (key));

	printf ("--------------\n");
	printf ("keyClear:\n");

	elektraKeyClear (key);
	printf ("%s\n", elektraKeyName (key));

	printf ("--------------\n");
	printf ("keyValue:\n");

	const char * value = elektraKeyValue (key);
	printf ("%p\n", value);

	printf ("keySetValue:\n");
	printf ("%zu\n", elektraKeySetValue (key, "abcd", 5));
	value = elektraKeyValue (key);
	printf ("%p\n", value);
	printf ("%s\n", value);

	printf ("keySetValue:\n");
	printf ("%zu\n", elektraKeySetValue (key, "abcd", 5));
	value = elektraKeyValue (key);
	printf ("%p\n", value);
	printf ("%s\n", value);

	printf ("--------------\n");
	printf ("keySet:\n");

	CKeySet * ks = elektraKeysetNew (1);
	printf ("%zu\n", elektraKeysetSize (ks));
	printf ("%i\n", elektraKeysetIncRef (ks));
	printf ("%i\n", elektraKeysetIncRef (ks));
	printf ("%i\n", elektraKeysetIncRef (ks));
	printf ("%i\n", elektraKeysetIncRef (ks));
	printf ("%i\n", elektraKeysetIncRef (ks));
	printf ("%zu\n", elektraKeysetSize (ks));

	printf ("%zu\n", elektraKeysetAdd (ks, key));
	printf ("%zu\n", elektraKeysetSize (ks));

	printf ("%zu\n", elektraKeysetAdd (ks, key2));
	printf ("%zu\n", elektraKeysetSize (ks));

	printf ("%s\n", elektraKeyName (elektraKeysetLookup (ks, key)));
	printf ("%s\n", elektraKeyName (elektraKeysetLookup (ks, key2)));

	elektraKeysetDel (ks);

	elektraKeyDel (key);
	elektraKeyDel (key2);
}