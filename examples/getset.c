#include <stdio.h>
#include <kdb.h>

#define BUFFER_SIZE 4000

int main(void)
{
	char buffer[BUFFER_SIZE+1];
	KDB *h = kdbOpen();
	kdbSetString(h, "user/example", "");
	kdbSetString(h, "user/example/hello", "Hello World");
	kdbGetString(h, "user/example/hello", buffer, BUFFER_SIZE);
	printf ("%s\n", buffer);
	kdbRemove(h, "user/example/hello");
	kdbRemove(h, "user/example");
	kdbClose (h);
}

