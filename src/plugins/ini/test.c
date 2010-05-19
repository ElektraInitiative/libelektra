#include "ini.h"
	
KDBHandle handle;

int key_info();
int read_in();

int main ()
{
	kdbOpenBackend(&handle, "ini");

	key_info();

	kdbClose(&handle);
	return 0;
}

int key_info()
{
	Key * new = keyNew ("user/file/mykey");
	kdbGetKey (handle, new);
	printf ("%s", keyStealValue (new));
}

int read_in ()
{
	FILE * f;
	size_t s;
	int sm;
	char buffer [BUFFER_SIZE];
	
	f = fopen ("value.txt", "w");
	
	s = fread (buffer, sizeof(char), BUFFER_SIZE, stdin);
	sm = convert_strlen (buffer, s);
	fprintf (stderr, "Read %d, will need %d bytes\n",s, sm);
	convert_stream (buffer, s, f);
}

